#!/usr/bin/env python3
"""Compile a Python source with CPython and pycomparse, then compare bytecode."""

from __future__ import annotations

import argparse
import difflib
import os
import re
import shlex
import subprocess
import sys
import tempfile
from pathlib import Path


COMPILE_WITH_CPYTHON = r"""
import py_compile
import sys

py_compile.compile(sys.argv[1], cfile=sys.argv[2], doraise=True)
"""


DUMP_PYC = r"""
import dis
import marshal
import sys
import types


def dump(code, indent=""):
    print(f"{indent}CODE {code.co_name!r}")
    print(
        f"{indent}  args={code.co_argcount} "
        f"posonly={code.co_posonlyargcount} "
        f"kwonly={code.co_kwonlyargcount} "
        f"nlocals={code.co_nlocals} "
        f"stack={code.co_stacksize} "
        f"flags=0x{code.co_flags:x}"
    )
    print(
        f"{indent}  names={code.co_names!r} "
        f"varnames={code.co_varnames!r}"
    )
    for instruction in dis.get_instructions(code):
        arg = ""
        if instruction.arg is not None:
            arg = " " + (instruction.argrepr or repr(instruction.argval))
        print(f"{indent}    {instruction.offset:04d} {instruction.opname}{arg}")
    for i, const in enumerate(code.co_consts):
        if isinstance(const, types.CodeType):
            print(f"{indent}  CONST_CODE[{i}]")
            dump(const, indent + "    ")


with open(sys.argv[1], "rb") as fp:
    fp.read(16)
    code = marshal.load(fp)
dump(code)
"""

DUMP_PYC_IGNORE_CODE_BYTES = r"""
import marshal
import pprint
import sys
import types


def normalize_const(const):
    if isinstance(const, types.CodeType):
        return ("code", normalize_code(const))
    if isinstance(const, tuple):
        return ("tuple", tuple(normalize_const(item) for item in const))
    if isinstance(const, frozenset):
        items = sorted((normalize_const(item) for item in const), key=repr)
        return ("frozenset", tuple(items))
    return ("literal", type(const).__name__, repr(const))


def normalize_code(code):
    normalized_consts = [normalize_const(const) for const in code.co_consts]
    normalized_consts.sort(key=repr)
    return (
        ("name", code.co_name),
        ("filename", code.co_filename),
        ("firstlineno", code.co_firstlineno),
        ("argcount", code.co_argcount),
        ("posonlyargcount", code.co_posonlyargcount),
        ("kwonlyargcount", code.co_kwonlyargcount),
        ("nlocals", code.co_nlocals),
        ("stacksize", code.co_stacksize),
        ("flags", code.co_flags),
        ("names", code.co_names),
        ("varnames", code.co_varnames),
        ("freevars", code.co_freevars),
        ("cellvars", code.co_cellvars),
        ("consts", tuple(normalized_consts)),
    )


with open(sys.argv[1], "rb") as fp:
    fp.read(16)
    code = marshal.load(fp)

pprint.pprint(normalize_code(code), width=120)
"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Compile .py files with both CPython and pycomparse and compare "
            "their bytecode disassembly."
        )
    )
    parser.add_argument(
        "sources",
        nargs="+",
        help="Python source file(s) to compile",
    )
    parser.add_argument(
        "--parser-test",
        default="build/parser_test",
        help="Path to pycomparse parser executable (default: build/parser_test)",
    )
    parser.add_argument(
        "--cpython-cmd",
        default="uv run python",
        help='Command used for reference CPython (default: "uv run python")',
    )
    parser.add_argument(
        "--keep-temp",
        action="store_true",
        help="Keep temporary directory with generated .pyc files and dumps",
    )
    parser.add_argument(
        "--ignore-larger-stackdepth",
        action="store_true",
        help=(
            "Ignore stackdepth differences when pycomparse stackdepth is "
            "greater than or equal to CPython stackdepth"
        ),
    )
    parser.add_argument(
        "--ignore-code-bytes",
        action="store_true",
        help=(
            "Ignore raw instruction stream differences; compare code object "
            "metadata and normalized constants instead"
        ),
    )
    return parser.parse_args()


def run_command(cmd: list[str], *, env: dict[str, str]) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)


def cpython_env(cpython_cmd: list[str]) -> dict[str, str]:
    env = os.environ.copy()
    if len(cpython_cmd) >= 2 and cpython_cmd[0] == "uv" and cpython_cmd[1] == "run":
        env.setdefault("UV_CACHE_DIR", "/tmp/.uvcache")
    return env


def compile_reference(
    source: Path, out_pyc: Path, cpython_cmd: list[str], env: dict[str, str]
) -> tuple[bool, str]:
    cmd = cpython_cmd + ["-c", COMPILE_WITH_CPYTHON, str(source), str(out_pyc)]
    proc = run_command(cmd, env=env)
    if proc.returncode != 0:
        return False, proc.stderr.decode("utf-8", errors="replace")
    return True, ""


def compile_pycomparse(source: Path, out_pyc: Path, parser_test: Path) -> tuple[bool, str]:
    with out_pyc.open("wb") as out_fp:
        proc = subprocess.run(
            [str(parser_test), str(source)],
            stdout=out_fp,
            stderr=subprocess.PIPE,
        )
    if proc.returncode != 0:
        return False, proc.stderr.decode("utf-8", errors="replace")
    return True, ""


def dump_pyc(
    pyc: Path, cpython_cmd: list[str], env: dict[str, str], ignore_code_bytes: bool
) -> str:
    dump_script = DUMP_PYC_IGNORE_CODE_BYTES if ignore_code_bytes else DUMP_PYC
    cmd = cpython_cmd + ["-c", dump_script, str(pyc)]
    proc = run_command(cmd, env=env)
    if proc.returncode != 0:
        raise RuntimeError(
            f"Failed to disassemble {pyc}:\n"
            + proc.stderr.decode("utf-8", errors="replace")
        )
    return proc.stdout.decode("utf-8", errors="replace")


STACK_LINE_RE = re.compile(
    r"^(?P<prefix>\s+args=\d+ posonly=\d+ kwonly=\d+ nlocals=\d+ stack=)"
    r"(?P<stack>\d+)"
    r"(?P<suffix>\s+flags=0x[0-9a-fA-F]+)$"
)


def normalize_larger_stackdepth_diffs(
    reference_dump: str, pycomparse_dump: str
) -> tuple[str, str]:
    ref_lines = reference_dump.splitlines()
    py_lines = pycomparse_dump.splitlines()
    if len(ref_lines) != len(py_lines):
        return reference_dump, pycomparse_dump

    normalized_ref = list(ref_lines)
    normalized_py = list(py_lines)
    for i, (ref_line, py_line) in enumerate(zip(ref_lines, py_lines)):
        ref_match = STACK_LINE_RE.match(ref_line)
        py_match = STACK_LINE_RE.match(py_line)
        if ref_match is None or py_match is None:
            continue
        if ref_match.group("prefix") != py_match.group("prefix"):
            continue
        if ref_match.group("suffix") != py_match.group("suffix"):
            continue
        ref_stack = int(ref_match.group("stack"))
        py_stack = int(py_match.group("stack"))
        if py_stack >= ref_stack:
            unified_line = (
                ref_match.group("prefix") + str(ref_stack) + ref_match.group("suffix")
            )
            normalized_ref[i] = unified_line
            normalized_py[i] = unified_line

    return "\n".join(normalized_ref), "\n".join(normalized_py)


def compare_one_file(
    source: Path,
    parser_test: Path,
    cpython_cmd: list[str],
    env: dict[str, str],
    temp_dir: Path,
    keep_temp: bool,
    ignore_larger_stackdepth: bool,
    ignore_code_bytes: bool,
) -> int:
    temp_dir.mkdir(parents=True, exist_ok=True)
    ref_pyc = temp_dir / "reference.pyc"
    pyc_pyc = temp_dir / "pycomparse.pyc"
    ref_dump_file = temp_dir / "reference.dump.txt"
    pyc_dump_file = temp_dir / "pycomparse.dump.txt"

    ref_ok, ref_err = compile_reference(source, ref_pyc, cpython_cmd, env)
    py_ok, py_err = compile_pycomparse(source, pyc_pyc, parser_test)

    if not ref_ok and not py_ok:
        print(f"SKIP: {source} (both compilers failed to compile)")
        return 0

    if ref_ok != py_ok:
        print(f"DIFF: {source}")
        if not ref_ok:
            print("reference compile failed:")
            print(ref_err.rstrip())
        if not py_ok:
            print("pycomparse compile failed:")
            print(py_err.rstrip())
        return 1

    try:
        reference_dump = dump_pyc(
            ref_pyc,
            cpython_cmd,
            env,
            ignore_code_bytes=ignore_code_bytes,
        )
        pycomparse_dump = dump_pyc(
            pyc_pyc,
            cpython_cmd,
            env,
            ignore_code_bytes=ignore_code_bytes,
        )
    except RuntimeError as err:
        print(f"ERROR: {source}")
        print(str(err).rstrip())
        return 2

    if ignore_larger_stackdepth:
        reference_dump, pycomparse_dump = normalize_larger_stackdepth_diffs(
            reference_dump, pycomparse_dump
        )

    ref_dump_file.write_text(reference_dump)
    pyc_dump_file.write_text(pycomparse_dump)

    if reference_dump == pycomparse_dump:
        print(f"MATCH: {source}")
        return 0

    print(f"DIFF: {source}")
    diff = difflib.unified_diff(
        reference_dump.splitlines(),
        pycomparse_dump.splitlines(),
        fromfile="reference",
        tofile="pycomparse",
        lineterm="",
    )
    for line in diff:
        print(line)
    if keep_temp:
        print(f"reference dump:  {ref_dump_file}")
        print(f"pycomparse dump: {pyc_dump_file}")
    else:
        print("re-run with --keep-temp to keep disassembly dumps")
    return 1


def main() -> int:
    args = parse_args()
    parser_test = Path(args.parser_test)
    cpython_cmd = shlex.split(args.cpython_cmd)

    if not parser_test.is_file():
        print(f"error: parser executable not found: {parser_test}", file=sys.stderr)
        return 2

    sources = [Path(source) for source in args.sources]
    missing = [source for source in sources if not source.is_file()]
    if missing:
        for source in missing:
            print(f"error: source file not found: {source}", file=sys.stderr)
        return 2

    env = cpython_env(cpython_cmd)
    temp_ctx = tempfile.TemporaryDirectory(prefix="pycomparse-compare-")
    try:
        base_temp_dir = Path(temp_ctx.name)
        overall_status = 0
        for i, source in enumerate(sources):
            status = compare_one_file(
                source=source,
                parser_test=parser_test,
                cpython_cmd=cpython_cmd,
                env=env,
                temp_dir=base_temp_dir / f"{i:03d}",
                keep_temp=args.keep_temp,
                ignore_larger_stackdepth=args.ignore_larger_stackdepth,
                ignore_code_bytes=args.ignore_code_bytes,
            )
            if status == 2:
                overall_status = 2
            elif status == 1 and overall_status == 0:
                overall_status = 1
        return overall_status
    finally:
        if args.keep_temp:
            print(f"Kept temp dir: {temp_ctx.name}")
        else:
            temp_ctx.cleanup()


if __name__ == "__main__":
    raise SystemExit(main())

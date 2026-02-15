#!/usr/bin/env python3

from __future__ import annotations

import argparse
import concurrent.futures
import fcntl
import fnmatch
import os
import re
import shlex
import subprocess
import sys
import tempfile
import uuid
from dataclasses import dataclass
from pathlib import Path
from typing import Dict

DEFAULT_TEST_ROOTS = (
    "test",
    "ctypes/test",
    "distutils/tests",
    "idlelib/idle_test",
    "lib2to3/tests",
    "sqlite3/test",
    "tkinter/test",
    "unittest/test",
)
DISCOVER_TEST_DIR_NAMES = {"test", "tests", "idle_test"}
MODE_CATEGORY_ORDER: dict[str, list[str]] = {
    "compile": [
        "compile_crash",
        "compile_timeout",
        "reference_compile_timeout",
        "compile_unexpected_error",
    ],
    "bytecode": [
        "bytecode_timeout",
        "bytecode_error",
        "bytecode_mismatch",
    ],
    "runtime": [
        "compile_fail",
        "compile_crash",
        "compile_timeout",
        "runtime_timeout",
        "runtime_exit_mismatch",
        "runtime_output_mismatch",
    ],
}


RAN_TESTS_RE = re.compile(
    r"^Ran (?P<count>\d+) test(?P<plural>s?) in [0-9]+(?:\.[0-9]+)?s$",
    re.MULTILINE,
)
COMPILE_WITH_CPYTHON = (
    "import py_compile,sys; "
    "py_compile.compile(sys.argv[1], cfile=sys.argv[2], doraise=True)"
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compile/check CPython Lib files with pycomparse."
    )
    parser.add_argument(
        "--mode",
        choices=("compile", "bytecode", "runtime"),
        default="compile",
        help=(
            "run mode: compile (syntax parity/crash checks), "
            "bytecode (compare disassembly), runtime (run source vs compiled)"
        ),
    )
    parser.add_argument(
        "--compile-only",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "--bytecode",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "--runtime",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "--lib",
        default=os.environ.get("CPYTHON_LIB", str(Path.home() / "cpython" / "Lib")),
        help="path to CPython Lib directory",
    )
    parser.add_argument(
        "--parser",
        default=os.environ.get("PARSER_TEST", "build/parser_test"),
        help="path to parser executable",
    )
    parser.add_argument(
        "--path-glob",
        action="append",
        help=(
            "glob over Lib-relative paths (repeatable). "
            "If omitted, defaults to known CPython test roots "
            "plus discovered test/tests/idle_test subdirs."
        ),
    )
    parser.add_argument(
        "--python-cmd",
        default="uv run --no-project python -B",
        help=(
            "reference python command used for compile/bytecode/runtime "
            "checks (default: uv run --no-project python -B)"
        ),
    )
    parser.add_argument(
        "--bytecode-script",
        default="scripts/compare_bytecode.py",
        help="bytecode comparator script path (default: scripts/compare_bytecode.py)",
    )
    parser.add_argument(
        "--bytecode-ignore-larger-stackdepth",
        action="store_true",
        help="pass --ignore-larger-stackdepth to the bytecode comparator",
    )
    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=max(1, (os.cpu_count() or 1)),
        help="parallel workers (default: CPU count)",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=120.0,
        help="per-process timeout in seconds (default: 120)",
    )
    parser.add_argument(
        "--no-normalize-unittest-timing",
        action="store_true",
        help="do not normalize unittest 'Ran ... in ...s' timing lines",
    )
    parser.add_argument(
        "--quiet-ok",
        action="store_true",
        help="print only failing files",
    )
    parser.add_argument(
        "--show-diff",
        action="store_true",
        help="print normalized stdout/stderr diffs for mismatches",
    )
    return parser.parse_args()


@dataclass(frozen=True)
class Config:
    mode: str
    parser_test: Path
    cpython_lib: Path
    path_globs: list[str]
    python_cmd: list[str]
    python_cmd_raw: str
    bytecode_script: Path
    bytecode_ignore_larger_stackdepth: bool
    runner_python: str
    jobs: int
    timeout: float
    normalize_unittest_timing: bool
    quiet_ok: bool
    show_diff: bool
    env: Dict[str, str]


@dataclass
class FileResult:
    file_path: Path
    ok: bool
    category: str
    detail: str = ""
    ref_rc: int | None = None
    pyc_rc: int | None = None
    ref_out: str = ""
    pyc_out: str = ""
    ref_err: str = ""
    pyc_err: str = ""
    tool_output: str = ""


def resolve_mode(args: argparse.Namespace) -> str:
    mode = args.mode
    legacy_flags = [
        ("compile", args.compile_only),
        ("bytecode", args.bytecode),
        ("runtime", args.runtime),
    ]
    enabled = [name for name, set_flag in legacy_flags if set_flag]
    if not enabled:
        return mode
    if len(enabled) > 1:
        raise ValueError("only one of --compile-only/--bytecode/--runtime may be set")
    legacy_mode = enabled[0]
    if mode != "compile" and mode != legacy_mode:
        raise ValueError(f"--mode={mode} conflicts with legacy flag for {legacy_mode}")
    return legacy_mode


def normalize_text(text: str, normalize_unittest_timing: bool) -> str:
    normalized = text.replace("\r\n", "\n")
    if normalize_unittest_timing:
        normalized = RAN_TESTS_RE.sub(
            lambda m: f"Ran {m.group('count')} test{m.group('plural')} in <TIME>s",
            normalized,
        )
    return normalized


def is_crash_return_code(rc: int) -> bool:
    # subprocess uses negative values for signals on POSIX.
    return rc < 0 or rc >= 128


def run_process(
    args: list[str],
    *,
    timeout: float,
    env: Dict[str, str],
    stdout_target=None,
) -> tuple[int, bytes, bytes, bool]:
    try:
        proc = subprocess.run(
            args,
            stdin=subprocess.DEVNULL,
            stdout=stdout_target if stdout_target is not None else subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout,
            env=env,
            check=False,
        )
        stdout = b"" if stdout_target is not None else proc.stdout
        return proc.returncode, stdout, proc.stderr, False
    except subprocess.TimeoutExpired as exc:
        out = b"" if exc.stdout is None else exc.stdout
        err = b"" if exc.stderr is None else exc.stderr
        return 124, out, err, True


def compile_reference_syntax(file_path: Path, cfg: Config) -> tuple[int, bool]:
    tmp_path = None
    try:
        with tempfile.NamedTemporaryFile(
            prefix="pycomparse-ref-compile-",
            suffix=".pyc",
            delete=False,
        ) as tmp:
            tmp_path = tmp.name
        rc, _, _, timed_out = run_process(
            [*cfg.python_cmd, "-c", COMPILE_WITH_CPYTHON, str(file_path), tmp_path],
            timeout=cfg.timeout,
            env=cfg.env,
        )
        return rc, timed_out
    finally:
        if tmp_path is not None:
            try:
                os.unlink(tmp_path)
            except FileNotFoundError:
                pass


def process_compile(file_path: Path, cfg: Config) -> FileResult:
    rc, _, _, timed_out = run_process(
        [str(cfg.parser_test), str(file_path)],
        timeout=cfg.timeout,
        env=cfg.env,
        stdout_target=subprocess.DEVNULL,
    )
    if timed_out:
        return FileResult(file_path, False, "compile_timeout")
    if is_crash_return_code(rc):
        return FileResult(file_path, False, "compile_crash", detail=f"code={rc}")
    if rc == 0:
        return FileResult(file_path, True, "ok")

    ref_rc, ref_timed_out = compile_reference_syntax(file_path, cfg)
    if ref_timed_out:
        return FileResult(file_path, False, "reference_compile_timeout")
    if ref_rc == 0:
        return FileResult(
            file_path,
            False,
            "compile_unexpected_error",
            detail=f"code={rc}/0",
        )
    return FileResult(file_path, True, "ok")


def process_bytecode(file_path: Path, cfg: Config) -> FileResult:
    compare_cmd = [
        cfg.runner_python,
        str(cfg.bytecode_script),
        "--parser-test",
        str(cfg.parser_test),
        "--cpython-cmd",
        cfg.python_cmd_raw,
    ]
    if cfg.bytecode_ignore_larger_stackdepth:
        compare_cmd.append("--ignore-larger-stackdepth")
    compare_cmd.append(str(file_path))

    compare_env = dict(cfg.env)
    compare_env["PYTHONPATH"] = os.environ.get("PYTHONPATH", "")

    rc, stdout, stderr, timed_out = run_process(
        compare_cmd,
        timeout=cfg.timeout,
        env=compare_env,
    )
    combined = (stdout + stderr).decode(errors="replace")
    if timed_out:
        return FileResult(file_path, False, "bytecode_timeout")
    if rc == 0:
        return FileResult(file_path, True, "ok")
    if rc == 1 and "DIFF:" in combined:
        return FileResult(
            file_path,
            False,
            "bytecode_mismatch",
            tool_output=combined,
        )
    return FileResult(
        file_path,
        False,
        "bytecode_error",
        detail=f"code={rc}",
        tool_output=combined,
    )


def process_runtime(file_path: Path, cfg: Config) -> FileResult:
    # Keep __file__ shape close to source runs for tests that derive paths via
    # os.path.splitext(__file__) + ".py".
    pyc_file = file_path.with_suffix(".pyc")
    lock_file = file_path.with_name(file_path.name + ".pycomparse-runtime.lock")
    backup_file: Path | None = None
    tmp_pyc_file: Path | None = None
    compiled_file_installed = False

    with lock_file.open("a+b") as lock_fd:
        fcntl.flock(lock_fd.fileno(), fcntl.LOCK_EX)
        try:
            if pyc_file.exists():
                backup_file = file_path.with_name(
                    f"{file_path.name}.pycomparse-bak."
                    f"{os.getpid()}.{uuid.uuid4().hex}.pyc"
                )
                pyc_file.rename(backup_file)

            with tempfile.NamedTemporaryFile(
                dir=file_path.parent,
                prefix=f"{file_path.stem}.pycomparse-tmp.",
                suffix=".pyc",
                delete=False,
            ) as out_file:
                tmp_pyc_file = Path(out_file.name)
                compile_rc, _, compile_err, compile_timed_out = run_process(
                    [str(cfg.parser_test), str(file_path)],
                    timeout=cfg.timeout,
                    env=cfg.env,
                    stdout_target=out_file,
                )
            if compile_timed_out:
                return FileResult(file_path, False, "compile_timeout")
            if compile_rc != 0:
                if is_crash_return_code(compile_rc):
                    return FileResult(
                        file_path, False, "compile_crash", detail=f"code={compile_rc}"
                    )
                return FileResult(
                    file_path,
                    False,
                    "compile_fail",
                    detail=f"code={compile_rc} stderr={compile_err.decode(errors='replace')}",
                )

            os.replace(tmp_pyc_file, pyc_file)
            tmp_pyc_file = None
            compiled_file_installed = True

            ref_args = [*cfg.python_cmd, str(file_path)]
            pyc_args = [*cfg.python_cmd, str(pyc_file)]
            ref_rc, ref_out_raw, ref_err_raw, ref_timed_out = run_process(
                ref_args, timeout=cfg.timeout, env=cfg.env
            )
            if ref_timed_out:
                return FileResult(file_path, False, "runtime_timeout", detail="source")

            pyc_rc, pyc_out_raw, pyc_err_raw, pyc_timed_out = run_process(
                pyc_args, timeout=cfg.timeout, env=cfg.env
            )
            if pyc_timed_out:
                return FileResult(file_path, False, "runtime_timeout", detail="compiled")

            ref_out = normalize_text(
                ref_out_raw.decode(errors="replace"), cfg.normalize_unittest_timing
            )
            ref_err = normalize_text(
                ref_err_raw.decode(errors="replace"), cfg.normalize_unittest_timing
            )
            pyc_out = normalize_text(
                pyc_out_raw.decode(errors="replace"), cfg.normalize_unittest_timing
            )
            pyc_err = normalize_text(
                pyc_err_raw.decode(errors="replace"), cfg.normalize_unittest_timing
            )

            if pyc_rc != ref_rc:
                return FileResult(
                    file_path,
                    False,
                    "runtime_exit_mismatch",
                    detail=f"code={pyc_rc}/{ref_rc}",
                    ref_rc=ref_rc,
                    pyc_rc=pyc_rc,
                )
            if pyc_out != ref_out or pyc_err != ref_err:
                return FileResult(
                    file_path,
                    False,
                    "runtime_output_mismatch",
                    detail=f"code={pyc_rc}",
                    ref_rc=ref_rc,
                    pyc_rc=pyc_rc,
                    ref_out=ref_out,
                    pyc_out=pyc_out,
                    ref_err=ref_err,
                    pyc_err=pyc_err,
                )
            return FileResult(file_path, True, "ok")
        finally:
            if tmp_pyc_file is not None:
                try:
                    tmp_pyc_file.unlink()
                except FileNotFoundError:
                    pass
            if compiled_file_installed:
                try:
                    pyc_file.unlink()
                except FileNotFoundError:
                    pass
            if backup_file is not None and backup_file.exists():
                backup_file.rename(pyc_file)


def build_default_path_globs(cpython_lib: Path) -> list[str]:
    globs: set[str] = {f"{root}/test_*.py" for root in DEFAULT_TEST_ROOTS}
    for dirpath, dirnames, _filenames in os.walk(cpython_lib):
        dirnames[:] = [name for name in dirnames if not name.startswith(".")]
        directory = Path(dirpath)
        if directory.name not in DISCOVER_TEST_DIR_NAMES:
            continue
        rel_dir = directory.relative_to(cpython_lib).as_posix()
        globs.add(f"{rel_dir}/test_*.py")
    return sorted(globs)


def collect_files_for_globs(cpython_lib: Path, path_globs: list[str]) -> list[Path]:
    files: list[Path] = []
    seen: set[Path] = set()
    for file_path in cpython_lib.rglob("*.py"):
        rel = file_path.relative_to(cpython_lib).as_posix()
        if any(fnmatch.fnmatch(rel, pattern) for pattern in path_globs):
            if file_path not in seen:
                seen.add(file_path)
                files.append(file_path)
    files.sort()
    return files


def print_diff_block(title: str, a: str, b: str) -> None:
    import difflib

    diff = list(
        difflib.unified_diff(
            a.splitlines(),
            b.splitlines(),
            fromfile="reference",
            tofile="compiled",
            lineterm="",
        )
    )
    if not diff:
        return
    print(f"  {title}:")
    for line in diff[:120]:
        print(f"    {line}")


def print_tool_output(output: str) -> None:
    if not output:
        return
    print("  tool output:")
    for line in output.splitlines()[:200]:
        print(f"    {line}")


def main() -> int:
    args = parse_args()
    try:
        mode = resolve_mode(args)
    except ValueError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 2

    parser_test = Path(args.parser)
    cpython_lib = Path(args.lib)
    if not parser_test.is_file() or not os.access(parser_test, os.X_OK):
        print(
            f"error: parser executable not found or not executable: {parser_test}",
            file=sys.stderr,
        )
        return 2
    if not cpython_lib.is_dir():
        print(f"error: CPython Lib directory not found: {cpython_lib}", file=sys.stderr)
        return 2

    bytecode_script = Path(args.bytecode_script)
    if mode == "bytecode" and not bytecode_script.is_file():
        print(f"error: bytecode script not found: {bytecode_script}", file=sys.stderr)
        return 2

    python_cmd = shlex.split(args.python_cmd)
    if not python_cmd:
        print("error: --python-cmd must not be empty", file=sys.stderr)
        return 2
    env = os.environ.copy()
    pythonpath = env.get("PYTHONPATH", "")
    env["PYTHONPATH"] = (
        str(cpython_lib) if not pythonpath else f"{cpython_lib}:{pythonpath}"
    )
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    env["PYTHONHASHSEED"] = "0"
    env.setdefault("UV_CACHE_DIR", "/tmp/uv-cache")

    cfg = Config(
        mode=mode,
        parser_test=parser_test,
        cpython_lib=cpython_lib,
        path_globs=(
            args.path_glob
            if args.path_glob
            else build_default_path_globs(cpython_lib)
        ),
        python_cmd=python_cmd,
        python_cmd_raw=args.python_cmd,
        bytecode_script=bytecode_script,
        bytecode_ignore_larger_stackdepth=args.bytecode_ignore_larger_stackdepth,
        runner_python=sys.executable,
        jobs=max(1, args.jobs),
        timeout=max(1.0, args.timeout),
        normalize_unittest_timing=not args.no_normalize_unittest_timing,
        quiet_ok=args.quiet_ok,
        show_diff=args.show_diff,
        env=env,
    )

    files = collect_files_for_globs(cfg.cpython_lib, cfg.path_globs)
    if not files:
        print("Summary: total=0 ok=0 fail=0")
        return 0

    counts: Dict[str, int] = {"ok": 0}
    fail_results: list[FileResult] = []

    def worker(path: Path) -> FileResult:
        if cfg.mode == "runtime":
            return process_runtime(path, cfg)
        if cfg.mode == "bytecode":
            return process_bytecode(path, cfg)
        return process_compile(path, cfg)

    with concurrent.futures.ThreadPoolExecutor(max_workers=cfg.jobs) as executor:
        futures = [executor.submit(worker, path) for path in files]
        for fut in concurrent.futures.as_completed(futures):
            result = fut.result()
            rel = result.file_path.relative_to(cfg.cpython_lib).as_posix()
            if result.ok:
                counts["ok"] += 1
                if not cfg.quiet_ok:
                    print(f"OK {cfg.cpython_lib / rel}")
            else:
                counts[result.category] = counts.get(result.category, 0) + 1
                fail_results.append(result)
                detail = f", {result.detail}" if result.detail else ""
                print(f"FAIL {cfg.cpython_lib / rel} [{result.category}{detail}]")
                if cfg.show_diff:
                    if result.category == "runtime_output_mismatch":
                        print_diff_block("stdout", result.ref_out, result.pyc_out)
                        print_diff_block("stderr", result.ref_err, result.pyc_err)
                    if result.category in {"bytecode_mismatch", "bytecode_error"}:
                        print_tool_output(result.tool_output)

    total = len(files)
    fails = total - counts["ok"]
    print(f"Summary: total={total} ok={counts['ok']} fail={fails}")
    categories = MODE_CATEGORY_ORDER.get(cfg.mode, [])
    summary_parts = [f"{name}={counts.get(name, 0)}" for name in categories]
    extra_categories = sorted(
        key for key in counts.keys() if key not in {"ok", *categories}
    )
    summary_parts.extend(f"{name}={counts[name]}" for name in extra_categories)
    if summary_parts:
        print("Summary categories: " + " ".join(summary_parts))

    return 1 if fail_results else 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Run parser/compiler and scanner tests for pycomparse.

This script supports three modes:
- ``scripts/test.py``: run parser + scanner suites
- ``scripts/test.py parser``: run parser/compiler integration tests only
- ``scripts/test.py scan``: run scanner tokenization tests only

The default ``all`` mode prints one combined summary line for both suites.
"""

from __future__ import annotations

import argparse
import difflib
import dis
import json
import marshal
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


def is_python38() -> bool:
    return sys.version_info[:2] == (3, 8)


def print_python_hint() -> None:
    found = subprocess.run(
        ["python3", "--version"],
        check=False,
        capture_output=True,
        text=True,
    )
    version = (found.stdout or found.stderr).strip() or "unknown"
    print(
        f"error: scripts/test.py requires Python 3.8 (found: {version})",
        file=sys.stderr,
    )
    print(
        "hint: run with 'uv run scripts/test.py parser' "
        "or 'uv run scripts/test.py scan'",
        file=sys.stderr,
    )


def cat_file(path: Path) -> None:
    if not path.exists():
        return
    sys.stdout.buffer.write(path.read_bytes())


def unified_diff(path_a: Path, path_b: Path) -> str:
    a_lines = path_a.read_text(encoding="utf-8", errors="replace").splitlines(
        keepends=True
    )
    b_lines = path_b.read_text(encoding="utf-8", errors="replace").splitlines(
        keepends=True
    )
    diff = difflib.unified_diff(
        a_lines,
        b_lines,
        fromfile=str(path_a),
        tofile=str(path_b),
        n=100,
    )
    return "".join(diff)


def maybe_relative(path: Path, root: Path) -> str:
    try:
        return str(path.relative_to(root))
    except ValueError:
        return str(path)


CODE_FLAG_BITS = {
    "CO_OPTIMIZED": 0x0001,
    "CO_NEWLOCALS": 0x0002,
    "CO_VARARGS": 0x0004,
    "CO_VARKEYWORDS": 0x0008,
    "CO_NESTED": 0x0010,
    "CO_GENERATOR": 0x0020,
    "CO_NOFREE": 0x0040,
    "CO_COROUTINE": 0x0080,
    "CO_ASYNC_GENERATOR": 0x0200,
    "CO_FUTURE_BARRY_AS_BDFL": 0x0400000,
    "CO_FUTURE_GENERATOR_STOP": 0x0800000,
    "CO_FUTURE_ANNOTATIONS": 0x1000000,
}


def load_test_manifest(test_file: Path) -> dict[str, object]:
    manifest_path = test_file.with_name(test_file.name + ".test.json")
    if not manifest_path.exists():
        return {}
    return json.loads(manifest_path.read_text(encoding="utf-8"))


def manifest_string_list(manifest: dict[str, object], key: str) -> list[str]:
    value = manifest.get(key, [])
    if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
        raise ValueError(f"{key} must be a list of strings")
    return value


def manifest_bool(manifest: dict[str, object], key: str, default: bool) -> bool:
    value = manifest.get(key, default)
    if not isinstance(value, bool):
        raise ValueError(f"{key} must be a boolean")
    return value


def manifest_status_list(manifest: dict[str, object], key: str) -> list[str]:
    value = manifest.get(key, [])
    if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
        raise ValueError(f"{key} must be a list of strings")
    return value


def load_pyc_code(path: Path):
    data = path.read_bytes()
    if len(data) < 16:
        raise ValueError(f"invalid pyc header in {path}")
    return marshal.loads(data[16:])


def nested_code_by_name(code, name: str):
    for const in code.co_consts:
        if hasattr(const, "co_name"):
            if const.co_name == name:
                return const
            nested = nested_code_by_name(const, name)
            if nested is not None:
                return nested
    return None


def check_code_assertions(code, spec: dict[str, object], path: str) -> list[str]:
    failures: list[str] = []
    opnames = {instruction.opname for instruction in dis.get_instructions(code)}

    for flag_name in spec.get("flags_all", []):
        bit = CODE_FLAG_BITS.get(flag_name)
        if bit is None:
            failures.append(f"{path}: unknown flag {flag_name}")
            continue
        if (code.co_flags & bit) == 0:
            failures.append(f"{path}: missing flag {flag_name}")

    for opname in spec.get("opnames_include", []):
        if opname not in opnames:
            failures.append(f"{path}: missing opcode {opname}")

    for opname in spec.get("opnames_exclude", []):
        if opname in opnames:
            failures.append(f"{path}: unexpected opcode {opname}")

    for name in spec.get("varnames_include", []):
        if name not in code.co_varnames:
            failures.append(f"{path}: missing varname {name}")

    for name in spec.get("freevars_include", []):
        if name not in code.co_freevars:
            failures.append(f"{path}: missing freevar {name}")

    for name in spec.get("cellvars_include", []):
        if name not in code.co_cellvars:
            failures.append(f"{path}: missing cellvar {name}")

    nested_specs = spec.get("nested", {})
    if isinstance(nested_specs, dict):
        for nested_name, nested_spec in nested_specs.items():
            nested_code = nested_code_by_name(code, nested_name)
            if nested_code is None:
                failures.append(f"{path}: missing nested code object {nested_name}")
                continue
            if not isinstance(nested_spec, dict):
                failures.append(f"{path}: invalid nested spec for {nested_name}")
                continue
            failures.extend(
                check_code_assertions(
                    nested_code,
                    nested_spec,
                    f"{path}.{nested_name}",
                )
            )

    return failures


def run_parser_tests(
    repo_root: Path,
    compiler: str,
    tmpdir: str,
    verbose: bool,
    print_summary: bool = True,
) -> tuple[int, int, int]:
    parser_bin = Path(compiler)
    if not parser_bin.is_absolute():
        parser_bin = repo_root / parser_bin

    total_tests = 0
    failed_tests = 0

    positive_tests = sorted((repo_root / "test").glob("*.py"))
    positive_tests.extend(sorted((repo_root / "test" / "options").rglob("*.py")))
    error_tests = sorted((repo_root / "test" / "errors").glob("*.py"))
    interactive_tests = sorted(
        (repo_root / "test" / "options" / "interactive").glob("*.txt")
    )

    with tempfile.TemporaryDirectory(
        dir=tmpdir,
        prefix="pycomparse-test.",
    ) as tmp_base:
        tmp = Path(tmp_base)

        pyc = tmp / "test.pyc"
        output = tmp / "output.txt"
        reference = tmp / "reference.txt"
        compile_err = tmp / "compile.err"
        run_err = tmp / "run.err"
        ref_err = tmp / "ref.err"
        err_out = tmp / "errors.txt"
        stderr_out = tmp / "compiler_stderr.txt"

        def report_fail(label: str) -> None:
            nonlocal failed_tests
            failed_tests += 1
            print(f"FAIL: {label}")

        def report_ok(label: str) -> None:
            if verbose:
                print(f"ok: {label}")

        for test_file in positive_tests:
            total_tests += 1
            rel = str(test_file.relative_to(repo_root))
            manifest = load_test_manifest(test_file)
            try:
                compile_args = manifest_string_list(manifest, "compile_args")
                reference_args = manifest_string_list(manifest, "reference_args")
                run_test = manifest_bool(manifest, "run", True)
            except ValueError as exc:
                report_fail(f"{rel} (invalid manifest)")
                print(exc)
                continue

            compile_cmd = [str(parser_bin), *compile_args, "--out", str(pyc), rel]
            with compile_err.open("wb") as compile_err_file:
                compile_proc = subprocess.run(
                    compile_cmd,
                    cwd=repo_root,
                    stdout=subprocess.DEVNULL,
                    stderr=compile_err_file,
                    check=False,
                )
            if compile_proc.returncode != 0:
                report_fail(f"{rel} (compile)")
                cat_file(compile_err)
                continue

            if run_test:
                with output.open("wb") as output_file, run_err.open(
                    "wb"
                ) as run_err_file:
                    run_proc = subprocess.run(
                        ["python3", str(pyc)],
                        cwd=repo_root,
                        stdout=output_file,
                        stderr=run_err_file,
                        check=False,
                    )
                if run_proc.returncode != 0:
                    report_fail(f"{rel} (compiled output runtime)")
                    cat_file(run_err)
                    continue

                ref_cmd = ["python3", *reference_args, rel]
                with reference.open("wb") as reference_file, ref_err.open(
                    "wb"
                ) as ref_err_file:
                    ref_proc = subprocess.run(
                        ref_cmd,
                        cwd=repo_root,
                        stdout=reference_file,
                        stderr=ref_err_file,
                        check=False,
                    )
                if ref_proc.returncode != 0:
                    report_fail(f"{rel} (reference runtime)")
                    cat_file(ref_err)
                    continue

                diff_text = unified_diff(reference, output)
                if diff_text:
                    report_fail(f"{rel} (output mismatch)")
                    print(diff_text, end="")
                    continue

            code_assertions = manifest.get("code_assertions")
            if code_assertions is not None:
                if not isinstance(code_assertions, dict):
                    report_fail(f"{rel} (invalid manifest)")
                    print("code_assertions must be an object")
                    continue
                try:
                    code = load_pyc_code(pyc)
                except Exception as exc:
                    report_fail(f"{rel} (code assertions)")
                    print(f"failed to load generated pyc: {exc}")
                    continue
                failures = check_code_assertions(code, code_assertions, rel)
                if failures:
                    report_fail(f"{rel} (code assertions)")
                    for failure in failures:
                        print(failure)
                    continue

            report_ok(f"{rel} ({'positive' if run_test else 'compile_only'})")

        for test_file in error_tests:
            total_tests += 1
            rel = str(test_file.relative_to(repo_root))

            with err_out.open("wb") as err_out_file:
                proc = subprocess.run(
                    [str(parser_bin), "--out", str(stderr_out), rel],
                    cwd=repo_root,
                    stdout=subprocess.DEVNULL,
                    stderr=err_out_file,
                    check=False,
                )
            if proc.returncode == 0:
                report_fail(f"{rel} (expected nonzero exit)")
                if stderr_out.exists() and stderr_out.stat().st_size > 0:
                    cat_file(stderr_out)
                continue

            expected = Path(f"{rel}.expected")
            diff_text = unified_diff(repo_root / expected, err_out)
            if diff_text:
                report_fail(f"{rel} (diagnostic mismatch)")
                print(diff_text, end="")
                continue

            if proc.returncode != 0:
                report_ok(f"{rel} (error)")

        for test_file in interactive_tests:
            total_tests += 1
            rel = str(test_file.relative_to(repo_root))
            label = test_file.stem
            manifest = load_test_manifest(test_file)
            try:
                compile_args = manifest_string_list(manifest, "compile_args")
                expected_statuses = manifest_status_list(manifest, "expected_statuses")
            except ValueError as exc:
                report_fail(f"{rel} (invalid manifest)")
                print(exc)
                continue

            input_text = test_file.read_text(encoding="utf-8")
            out_prefix = tmp / f"{label}.out"
            proc = subprocess.run(
                [
                    str(parser_bin),
                    *compile_args,
                    "--interactive-test",
                    "--interactive-out-prefix",
                    str(out_prefix),
                ],
                cwd=repo_root,
                input=input_text,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                check=False,
            )
            if proc.returncode != 0:
                report_fail(f"{label} (interactive exit)")
                if proc.stderr:
                    print(proc.stderr, end="")
                continue

            statuses: list[str] = []
            output_paths: list[Path] = []
            unexpected_line = None
            for raw_line in proc.stdout.splitlines():
                line = raw_line.strip()
                if not line:
                    continue
                if line == "incomplete":
                    statuses.append("incomplete")
                elif line == "error":
                    statuses.append("error")
                elif line.startswith("ok: "):
                    statuses.append("ok")
                    output_paths.append(Path(line[4:]))
                else:
                    unexpected_line = line
                    break

            if unexpected_line is not None:
                report_fail(f"{rel} (interactive output format)")
                print(f"unexpected output line: {unexpected_line}")
                continue

            if statuses != expected_statuses:
                report_fail(f"{rel} (interactive status sequence)")
                print(f"expected statuses: {expected_statuses}")
                print(f"actual statuses:   {statuses}")
                continue

            missing_output = False
            for output_path in output_paths:
                if not output_path.exists() or output_path.stat().st_size == 0:
                    missing_output = True
                    break
            if missing_output:
                report_fail(f"{rel} (interactive output file)")
                print("interactive mode reported output file that was not written")
                continue

            # Diagnostics are printed to stderr; allow stderr when the test
            # expects at least one error status.
            if proc.stderr and "error" not in expected_statuses:
                report_fail(f"{rel} (interactive stderr)")
                print(proc.stderr, end="")
                continue

            report_ok(f"{rel} (interactive)")

    passed_tests = total_tests - failed_tests
    if print_summary:
        print(f"Ran {total_tests} tests: {passed_tests} passed, {failed_tests} failed.")
    return (1 if failed_tests else 0), total_tests, failed_tests


def expand_input_patterns(repo_root: Path, inputs: list[str]) -> list[Path]:
    if not inputs:
        test_dir = repo_root / "test"
        scan_dir = test_dir / "scanner_only"
        return sorted(
            list(test_dir.glob("*.py"))
            + list(scan_dir.glob("*.py"))
            + list(scan_dir.glob("*.py.raw"))
        )

    expanded: list[Path] = []
    for raw in inputs:
        pattern = Path(raw)
        if pattern.is_absolute():
            matches = sorted(pattern.parent.glob(pattern.name))
        else:
            matches = sorted(repo_root.glob(raw))

        if matches:
            expanded.extend(matches)
            continue

        if pattern.is_absolute():
            expanded.append(pattern)
        else:
            expanded.append(repo_root / raw)

    # Keep first occurrence order while de-duplicating.
    seen: set[Path] = set()
    unique: list[Path] = []
    for path in expanded:
        resolved = path.resolve()
        if resolved in seen:
            continue
        seen.add(resolved)
        unique.append(path)
    return unique


def run_scan_tests(
    repo_root: Path,
    scanner_test: str,
    inputs: list[str],
    from_string: bool,
    allow_failures: bool,
    verbose: bool,
    print_summary: bool = True,
) -> tuple[int, int, int]:
    scanner_bin = Path(scanner_test)
    if not scanner_bin.is_absolute():
        scanner_bin = repo_root / scanner_bin

    tests = expand_input_patterns(repo_root, inputs)
    if not tests:
        print("No scan tests matched.")
        return 0, 0, 0

    tmp_dir = Path(tempfile.mkdtemp(prefix="pycomparse-scantest."))
    failed = 0

    try:
        tokens = tmp_dir / "tokens.txt"
        reference = tmp_dir / "tokens.reference.txt"
        scanner_err = tmp_dir / "scanner.err"
        ref_err = tmp_dir / "reference.err"

        for test_file in tests:
            rel = maybe_relative(test_file, repo_root)
            if verbose:
                print(f"...{rel}")

            with tokens.open("wb") as tokens_file, scanner_err.open(
                "wb"
            ) as scanner_err_file:
                cmd = [str(scanner_bin)]
                if from_string:
                    cmd.append("--from-string")
                cmd.append(rel)
                scanner_proc = subprocess.run(
                    cmd,
                    cwd=repo_root,
                    stdout=tokens_file,
                    stderr=scanner_err_file,
                    check=False,
                )
            if scanner_proc.returncode != 0:
                failed += 1
                print(f"FAIL: {rel} (scanner)")
                cat_file(scanner_err)
                continue

            with reference.open("wb") as reference_file, ref_err.open(
                "wb"
            ) as ref_err_file:
                ref_proc = subprocess.run(
                    ["python3", "scripts/pytokenize.py", rel],
                    cwd=repo_root,
                    stdout=reference_file,
                    stderr=ref_err_file,
                    check=False,
                )
            if ref_proc.returncode != 0:
                failed += 1
                print(f"FAIL: {rel} (reference tokenize)")
                cat_file(ref_err)
                continue

            diff_text = unified_diff(reference, tokens)
            if diff_text:
                failed += 1
                print(f"FAIL: {rel} (token mismatch)")
                print(diff_text, end="")
                continue

            if verbose:
                print(f"ok: {rel} (scan)")
    finally:
        shutil.rmtree(tmp_dir, ignore_errors=True)

    passed = len(tests) - failed
    if print_summary:
        print(f"Ran {len(tests)} scan tests: {passed} passed, {failed} failed.")

    if failed and allow_failures:
        return 0, len(tests), failed
    return (1 if failed else 0), len(tests), failed


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    sub = parser.add_subparsers(dest="mode")

    parser_mode = sub.add_parser("parser", help="Run parser/compiler tests")
    parser_mode.add_argument(
        "--compiler",
        default="build/pycomparse",
        help="Compiler binary path (default: build/pycomparse)",
    )
    parser_mode.add_argument(
        "--tmpdir",
        default=os.environ.get("TMPDIR", "/tmp"),
        help="Temporary directory root (default: $TMPDIR or /tmp)",
    )
    parser_mode.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print one line for each successful test.",
    )

    scan_mode = sub.add_parser("scan", help="Run scanner tokenization tests")
    scan_mode.add_argument(
        "inputs",
        nargs="*",
        help="Optional files/patterns (default: test/*.py and test/scanner_only/*)",
    )
    scan_mode.add_argument(
        "--scanner-test",
        default=os.environ.get("SCANNER_TEST", "build/scanner_test"),
        help="Scanner binary path (default: $SCANNER_TEST or build/scanner_test)",
    )
    scan_mode.add_argument(
        "--from-string",
        action="store_true",
        help="Pass --from-string to scanner_test (read file into buffer).",
    )
    scan_mode.add_argument(
        "--allow-failures",
        action="store_true",
        help="Always return success even if scan mismatches exist.",
    )
    scan_mode.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Print one line for each successful test.",
    )

    args = parser.parse_args()
    if args.mode is None:
        args.mode = "all"
        args.compiler = "build/pycomparse"
        args.tmpdir = os.environ.get("TMPDIR", "/tmp")
        args.scanner_test = os.environ.get("SCANNER_TEST", "build/scanner_test")
        args.from_string = False
        args.allow_failures = False
        args.inputs = []
        args.verbose = False
    return args


def main() -> int:
    if not is_python38():
        print_python_hint()
        return 2

    args = parse_args()
    repo_root = Path(__file__).resolve().parents[1]

    if args.mode == "all":
        parser_rc, parser_total, parser_failed = run_parser_tests(
            repo_root,
            args.compiler,
            args.tmpdir,
            args.verbose,
            print_summary=False,
        )
        scan_rc, scan_total, scan_failed = run_scan_tests(
            repo_root,
            args.scanner_test,
            args.inputs,
            args.from_string,
            args.allow_failures,
            args.verbose,
            print_summary=False,
        )
        total = parser_total + scan_total
        failed = parser_failed + scan_failed
        passed = total - failed
        print(
            f"Ran {total} tests ({parser_total} parser, {scan_total} scan): "
            f"{passed} passed, {failed} failed."
        )
        return parser_rc | scan_rc

    if args.mode == "parser":
        parser_rc, _, _ = run_parser_tests(
            repo_root, args.compiler, args.tmpdir, args.verbose
        )
        return parser_rc

    scan_rc, _, _ = run_scan_tests(
        repo_root,
        args.scanner_test,
        args.inputs,
        args.from_string,
        args.allow_failures,
        args.verbose,
    )
    return scan_rc


if __name__ == "__main__":
    raise SystemExit(main())

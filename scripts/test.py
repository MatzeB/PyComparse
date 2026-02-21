#!/usr/bin/env python3
"""Unified test runner for parser and scanner tests."""

from __future__ import annotations

import argparse
import difflib
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


def run_parser_tests(repo_root: Path, compiler: str, tmpdir: str, verbose: bool) -> int:
    parser_bin = Path(compiler)
    if not parser_bin.is_absolute():
        parser_bin = repo_root / parser_bin

    total_tests = 0
    failed_tests = 0

    positive_tests = sorted((repo_root / "test").glob("*.py"))
    error_tests = sorted((repo_root / "test" / "errors").glob("*.py"))
    compile_only_tests = sorted((repo_root / "test" / "compile_only").glob("*.py"))

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

            with compile_err.open("wb") as compile_err_file:
                compile_proc = subprocess.run(
                    [str(parser_bin), "--out", str(pyc), rel],
                    cwd=repo_root,
                    stdout=subprocess.DEVNULL,
                    stderr=compile_err_file,
                    check=False,
                )
            if compile_proc.returncode != 0:
                report_fail(f"{rel} (compile)")
                cat_file(compile_err)
                continue

            with output.open("wb") as output_file, run_err.open("wb") as run_err_file:
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

            with reference.open("wb") as reference_file, ref_err.open(
                "wb"
            ) as ref_err_file:
                ref_proc = subprocess.run(
                    ["python3", rel],
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

            report_ok(f"{rel} (positive)")

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

        for test_file in compile_only_tests:
            total_tests += 1
            rel = str(test_file.relative_to(repo_root))

            with compile_err.open("wb") as compile_err_file:
                proc = subprocess.run(
                    [str(parser_bin), "--out", str(pyc), rel],
                    cwd=repo_root,
                    stdout=subprocess.DEVNULL,
                    stderr=compile_err_file,
                    check=False,
                )
            if proc.returncode != 0:
                report_fail(f"{rel} (compile_only)")
                cat_file(compile_err)
                continue

            report_ok(f"{rel} (compile_only)")

    passed_tests = total_tests - failed_tests
    print(f"Ran {total_tests} tests: {passed_tests} passed, {failed_tests} failed.")
    return 1 if failed_tests else 0


def expand_input_patterns(repo_root: Path, inputs: list[str]) -> list[Path]:
    if not inputs:
        test_dir = repo_root / "test"
        return sorted(
            list(test_dir.glob("*.py")) + list(test_dir.glob("*.py.raw"))
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
) -> int:
    scanner_bin = Path(scanner_test)
    if not scanner_bin.is_absolute():
        scanner_bin = repo_root / scanner_bin

    tests = expand_input_patterns(repo_root, inputs)
    if not tests:
        print("No scan tests matched.")
        return 0

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
    print(f"Ran {len(tests)} scan tests: {passed} passed, {failed} failed.")

    if failed and allow_failures:
        return 0
    return 1 if failed else 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Pycomparse test runner")
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
        help="Optional files/patterns (default: test/*.py)",
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
        args.mode = "parser"
        args.compiler = "build/pycomparse"
        args.tmpdir = os.environ.get("TMPDIR", "/tmp")
        args.verbose = False
    return args


def main() -> int:
    if not is_python38():
        print_python_hint()
        return 2

    args = parse_args()
    repo_root = Path(__file__).resolve().parents[1]

    if args.mode == "parser":
        return run_parser_tests(repo_root, args.compiler, args.tmpdir, args.verbose)
    if args.mode == "scan":
        return run_scan_tests(
            repo_root,
            args.scanner_test,
            args.inputs,
            args.from_string,
            args.allow_failures,
            args.verbose,
        )

    raise AssertionError(f"unknown mode: {args.mode}")


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env bash

# Run this script with `uv run ./test.sh` so `python3` resolves to Python 3.8.

PARSER_TEST="${PARSER_TEST:-build/pycomparse}"
TMPDIR="${TMPDIR:-/tmp}"

total_tests=0
failed_tests=0

if ! python3 -c 'import sys; raise SystemExit(0 if sys.version_info[:2] == (3, 8) else 1)'; then
    echo "error: test.sh requires Python 3.8 (found: $(python3 --version 2>&1))" >&2
    echo "hint: run with 'uv run ./test.sh'" >&2
    exit 2
fi

tmp_base="$(mktemp -d "${TMPDIR%/}/pycomparse-test.XXXXXX")"
trap 'rm -rf "$tmp_base"' EXIT

report_fail() {
    failed_tests=$((failed_tests + 1))
    echo "FAIL: $1"
}

run_positive_test() {
    local t="$1"
    local pyc="$tmp_base/test.pyc"
    local output="$tmp_base/output.txt"
    local reference="$tmp_base/reference.txt"
    local compile_err="$tmp_base/compile.err"
    local run_err="$tmp_base/run.err"
    local ref_err="$tmp_base/ref.err"
    local diff_out="$tmp_base/diff.txt"

    total_tests=$((total_tests + 1))

    if ! "${PARSER_TEST}" --out "$pyc" "$t" 2> "$compile_err"; then
        report_fail "$t (compile)"
        cat "$compile_err"
        return
    fi

    if ! python3 "$pyc" > "$output" 2> "$run_err"; then
        report_fail "$t (compiled output runtime)"
        cat "$run_err"
        return
    fi

    if ! python3 "$t" > "$reference" 2> "$ref_err"; then
        report_fail "$t (reference runtime)"
        cat "$ref_err"
        return
    fi

    if ! diff -U 100 "$reference" "$output" > "$diff_out"; then
        report_fail "$t (output mismatch)"
        cat "$diff_out"
    fi
}

run_error_test() {
    local t="$1"
    local err_out="$tmp_base/errors.txt"
    local stderr_out="$tmp_base/compiler_stderr.txt"
    local diff_out="$tmp_base/diff.txt"

    total_tests=$((total_tests + 1))

    "${PARSER_TEST}" --out "$stderr_out" "$t" > /dev/null 2> "$err_out"
    local status=$?
    if [ "$status" -eq 0 ]; then
        report_fail "$t (expected nonzero exit)"
        if [ -s "$stderr_out" ]; then
            cat "$stderr_out"
        fi
    fi

    if ! diff -U 100 "$t.expected" "$err_out" > "$diff_out"; then
        report_fail "$t (diagnostic mismatch)"
        cat "$diff_out"
    fi
}

run_compile_only_test() {
    local t="$1"
    local pyc="$tmp_base/test.pyc"
    local compile_err="$tmp_base/compile.err"

    total_tests=$((total_tests + 1))

    if ! "${PARSER_TEST}" --out "$pyc" "$t" 2> "$compile_err"; then
        report_fail "$t (compile_only)"
        cat "$compile_err"
    fi
}

for t in test/*.py; do
    run_positive_test "$t"
done

for t in test/errors/*.py; do
    run_error_test "$t"
done

for t in test/compile_only/*.py; do
    run_compile_only_test "$t"
done

passed_tests=$((total_tests - failed_tests))
echo "Ran ${total_tests} tests: ${passed_tests} passed, ${failed_tests} failed."

if [ "$failed_tests" -ne 0 ]; then
    exit 1
fi

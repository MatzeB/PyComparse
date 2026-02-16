#!/usr/bin/env bash

# Run this script with `uv run ./scantest.sh` so `python3` resolves to Python 3.8.

SCANNER_TEST="${SCANNER_TEST:-build/scanner_test}"

if ! python3 -c 'import sys; raise SystemExit(0 if sys.version_info[:2] == (3, 8) else 1)'; then
    echo "error: scantest.sh requires Python 3.8 (found: $(python3 --version 2>&1))" >&2
    echo "hint: run with 'uv run ./scantest.sh'" >&2
    exit 2
fi

if [ -z "${1:-}" ]; then
    INPUTS=test/*.py
else
    INPUTS="$@"
fi

for t in ${INPUTS}; do
    echo "...$t"
    ${SCANNER_TEST} "$t" > /tmp/tokens.txt
    python3 utils/pytokenize.py "$t" > /tmp/tokens.reference.txt
    diff -U 100 /tmp/tokens.reference.txt /tmp/tokens.txt
done

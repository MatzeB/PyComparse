#!/usr/bin/env bash

SCANNER_TEST="${SCANNER_TEST:-build/scanner_test}"

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

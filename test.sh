#!/usr/bin/env bash

PARSER_TEST="${PARSER_TEST:-build/parser_test}"

for t in test/*.py; do
    echo "...$t"
    ${PARSER_TEST} $t > /tmp/test.pyc
    python3 /tmp/test.pyc > /tmp/output
    python3 $t > /tmp/reference
    diff -U 100 /tmp/reference /tmp/output
done

for t in test/errors/*.py; do
    echo "...$t"
    ${PARSER_TEST} $t 2> /tmp/errors.txt
    if [ $? == 0 ]; then
        echo 1>&2 "Exit with code 0, despite error"
    fi
    diff -U 100 $t.expected /tmp/errors.txt
done

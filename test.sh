#!/bin/sh
for t in test/*.py; do
    echo "...$t"
    build/parser_test $t > /tmp/test.pyc
    python3 /tmp/test.pyc > /tmp/output
    python3 $t > /tmp/reference
    diff -U 100 /tmp/reference /tmp/output
done

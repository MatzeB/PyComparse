#!/bin/sh
for t in test/*.py; do
    echo "...$t"
    build/parser_test $t > /tmp/test.pyc
    python3 /tmp/test.pyc > /tmp/output
    python3 $t > /tmp/reference
    diff -U 100 /tmp/reference /tmp/output
done

for t in test/errors/*.py; do
    echo "...$t"
    build/parser_test $t 2> /tmp/errors.txt
    if [ $? == 0 ]; then
        echo 1>&2 "Exit with code 0, despite error"
    fi
    diff -U 100 $t.expected /tmp/errors.txt
done

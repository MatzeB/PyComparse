#!/usr/bin/env bash

set -u

PARSER_TEST="${PARSER_TEST:-build/parser_test}"
CPYTHON_LIB="${CPYTHON_LIB:-$HOME/cpython/Lib}"

if [[ ! -x "${PARSER_TEST}" ]]; then
  echo "error: parser executable not found or not executable: ${PARSER_TEST}" >&2
  exit 2
fi

if [[ ! -d "${CPYTHON_LIB}" ]]; then
  echo "error: CPython Lib directory not found: ${CPYTHON_LIB}" >&2
  exit 2
fi

find "${CPYTHON_LIB}" -type f -name '*.py' -print0 \
  | sort -z \
  | while IFS= read -r -d '' file; do
    if "${PARSER_TEST}" "${file}" > /dev/null 2>/dev/null; then
      printf 'OK %s\n' "${file}" 2>/dev/null || break
    else
      printf 'FAIL %s\n' "${file}" 2>/dev/null || break
    fi
  done

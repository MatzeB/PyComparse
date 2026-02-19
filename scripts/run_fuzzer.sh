#!/usr/bin/env bash
set -euo pipefail

BUILD_DIR="build-fuzz"

usage() {
  cat <<'EOF'
Usage:
  scripts/run_fuzzer.sh [options] [-- libfuzzer-args...]

Options:
  --build-dir DIR     Fuzzer build directory (default: build-fuzz)
  -h, --help          Show this help

Environment:
  CLANG               Clang binary to use (default: clang)
  JOBS                Parallel build jobs (default: nproc)

Extra arguments after -- are passed directly to libFuzzer (e.g. -max_total_time=60).

Examples:
  scripts/run_fuzzer.sh
  scripts/run_fuzzer.sh -- -max_total_time=30
  scripts/run_fuzzer.sh -- -max_total_time=60 -jobs=4
  CLANG=clang-18 scripts/run_fuzzer.sh
EOF
}

EXTRA_ARGS=()

while [[ $# -gt 0 ]]; do
  case "$1" in
  --build-dir)
    BUILD_DIR="$2"
    shift 2
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  --)
    shift
    EXTRA_ARGS=("$@")
    break
    ;;
  *)
    echo "Unknown argument: $1" >&2
    usage >&2
    exit 2
    ;;
  esac
done

jobs="${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"

echo "[1/2] Configuring fuzzer build in ${BUILD_DIR}"
cmake -S . -B "${BUILD_DIR}" -G Ninja \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DCMAKE_C_COMPILER="${CLANG:-clang}" \
  -DPYCOMPARSE_ENABLE_FUZZER=ON

echo "[2/2] Building fuzz_compile"
cmake --build "${BUILD_DIR}" --target fuzz_compile -j "${jobs}"

mkdir -p "${BUILD_DIR}/corpus" "${BUILD_DIR}/artifacts"

echo "Running fuzzer (Ctrl-C to stop)..."
exec "${BUILD_DIR}/fuzz_compile" \
  "${BUILD_DIR}/corpus" \
  test/ test/errors/ test/compile_only/ \
  -artifact_prefix="${BUILD_DIR}/artifacts/" \
  "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"

#!/usr/bin/env bash
set -euo pipefail

BUILD_DIR="build-pgo-lto"
GEN_DIR=""
PROFRAW_DIR=""
PROFDATA=""
TRAIN_CMD=""

usage() {
  cat <<'EOF'
Usage:
  scripts/build_pgo_lto.sh [options]

Options:
  --build-dir DIR     Final optimized build directory (default: build-pgo-lto)
  --train-cmd CMD     Command used to execute the training workload
  -h, --help          Show this help

Notes:
  - This helper currently expects Clang/LLVM PGO tooling.
  - If --train-cmd is omitted, a default parser workload is executed.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
  --build-dir)
    BUILD_DIR="$2"
    shift 2
    ;;
  --train-cmd)
    TRAIN_CMD="$2"
    shift 2
    ;;
  -h|--help)
    usage
    exit 0
    ;;
  *)
    echo "Unknown argument: $1" >&2
    usage >&2
    exit 2
    ;;
  esac
done

GEN_DIR="${BUILD_DIR}-gen"
PROFRAW_DIR="${BUILD_DIR}/profraw"
PROFDATA="${BUILD_DIR}/pyparse.profdata"

jobs="${JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"

echo "[1/5] Configuring instrumented PGO build in ${GEN_DIR}"
cmake -S . -B "${GEN_DIR}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DPYPARSE_ENABLE_LTO=ON \
  -DPYPARSE_PGO_MODE=generate

compiler_path="$(awk -F= '/^CMAKE_C_COMPILER:FILEPATH=/{print $2}' "${GEN_DIR}/CMakeCache.txt")"
compiler_name="$(basename "${compiler_path}")"
if [[ "${compiler_name}" != *clang* ]]; then
  echo "This helper currently supports Clang only (compiler: ${compiler_path})." >&2
  exit 1
fi

echo "[2/5] Building instrumented binaries"
cmake --build "${GEN_DIR}" -j "${jobs}"

rm -rf "${PROFRAW_DIR}"
mkdir -p "${PROFRAW_DIR}"

export PARSER_TEST="${GEN_DIR}/parser_test"
export LLVM_PROFILE_FILE="${PROFRAW_DIR}/pyparse-%p.profraw"

echo "[3/5] Running training workload"
if [[ -n "${TRAIN_CMD}" ]]; then
  bash -lc "${TRAIN_CMD}"
else
  if [[ -d "${HOME}/compiling/cpython-3.8/Lib" ]]; then
    files=(
      "${HOME}/compiling/cpython-3.8/Lib/pydoc_data/topics.py"
      "${HOME}/compiling/cpython-3.8/Lib/_pydecimal.py"
      "${HOME}/compiling/cpython-3.8/Lib/_pyio.py"
      "${HOME}/compiling/cpython-3.8/Lib/argparse.py"
      "${HOME}/compiling/cpython-3.8/Lib/datetime.py"
      "${HOME}/compiling/cpython-3.8/Lib/doctest.py"
      "${HOME}/compiling/cpython-3.8/Lib/inspect.py"
      "${HOME}/compiling/cpython-3.8/Lib/tarfile.py"
    )
  else
    files=(test/*.py)
  fi

  for _round in 1 2 3; do
    for f in "${files[@]}"; do
      "${PARSER_TEST}" "${f}" >/dev/null
    done
  done
fi

shopt -s nullglob
profraw_files=("${PROFRAW_DIR}"/*.profraw)
if [[ "${#profraw_files[@]}" -eq 0 ]]; then
  echo "No .profraw files were generated; training workload may not have run." >&2
  exit 1
fi

LLVM_PROFDATA_BIN="${LLVM_PROFDATA:-llvm-profdata}"
echo "[4/5] Merging profile data to ${PROFDATA}"
"${LLVM_PROFDATA_BIN}" merge -output="${PROFDATA}" "${profraw_files[@]}"

echo "[5/5] Configuring and building optimized PGO+LTO binaries in ${BUILD_DIR}"
cmake -S . -B "${BUILD_DIR}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DPYPARSE_ENABLE_LTO=ON \
  -DPYPARSE_PGO_MODE=use \
  -DPYPARSE_PGO_DATA="${PROFDATA}"
cmake --build "${BUILD_DIR}" -j "${jobs}"

echo "Done."
echo "Optimized parser binary: ${BUILD_DIR}/parser_test"

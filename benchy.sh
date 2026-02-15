set -euo pipefail

# PGO+LTO optimized pycomparse build
PYCOMPARSE="$(pwd)/build-pgo-lto/pycomparse"
# cpython-3.8 as shipped by UV tool (but circumventing the wrapper)
CPYTHON_3_8="$(uv run python -c "import sys; print(sys.executable)")"

DEST="$(mktemp -d /tmp/perf-XXXXXXXXX)"
WORKDIR="${DEST}/workdir"
mkdir -p "${WORKDIR}"
RESULTS="${DEST}/results"
mkdir -p "${RESULTS}"
SYNTHETIC_LARGE="${DEST}/synthetic_large"
mkdir -p "${SYNTHETIC_LARGE}"

# All files in cpython-3.8/Lib except known problematic test fixtures.
mapfile -d "" SOURCES < <(
  find cpython-3.8/Lib -type f -name "*.py" \
    ! -name "bad*" \
    ! -path "*/lib2to3/tests/data/*" \
    -print0
)

uv run scripts/benchmark_compile_perf.py \
  --baseline="${CPYTHON_3_8}" \
  --tested="${PYCOMPARSE}" \
  --runner=hyperfine \
  --warmup=1 \
  --repeats=10 \
  --work-dir="${WORKDIR}" \
  --json-out="${RESULTS}/bench-all.json" \
  --tsv-out="${RESULTS}/bench-all.tsv" \
  "${SOURCES[@]}"

uv run scripts/generate_large_python_files.py \
  --output-dir="${SYNTHETIC_LARGE}" \
  --sizes-mb=1,5,10 \
  --pattern=statements

uv run scripts/generate_large_python_files.py \
  --output-dir="${SYNTHETIC_LARGE}" \
  --sizes-mb=1,5,10 \
  --pattern=functions

uv run scripts/benchmark_compile_perf.py \
  --baseline="${CPYTHON_3_8}" \
  --tested="${PYCOMPARSE}" \
  --runner=hyperfine \
  --warmup=1 \
  --repeats=10 \
  --work-dir="${WORKDIR}" \
  --json-out="${RESULTS}/bench-synthetic-large.json" \
  --tsv-out="${RESULTS}/bench-synthetic-large.tsv" \
  "${SYNTHETIC_LARGE}"/*

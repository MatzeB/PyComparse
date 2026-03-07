# PyComparse

PyComparse is a Python front-end and bytecode compiler written in C.
It scans and parses Python source, then emits `.pyc` bytecode executable by
CPython.

The project currently targets Python `3.8` syntax and bytecode.
This project is also an experiment in heavy AI-assisted coding.

Current benchmark suites show `>5x` compile-time speedup vs CPython; see the
[homepage benchmark section](https://matzeb.github.io/PyComparse/#benchmarks).

## Status

- Scanner, parser, and code generator cover Python 3.8 language behavior.
- Handles Python 3.8 standard library and test suite inputs broadly, with
  known gaps listed below.

## Key Implementation Details

- Arena-based memory allocation is used throughout parsing/codegen paths.
- Parsing uses recursive descent with precedence climbing for expressions.
- Parser error recovery with anchor-token sets.
- Output is Python 3.8.20-compatible bytecode (`.pyc`).

### Known Limitations

- Constant folding supports a subset of operations (`+`, `-`, `*` for
  restricted integer/float ranges, string concat/multiply, and some dead-code
  elimination paths).
- Maximum stack computation can overestimate for some control-flow patterns.

## Quick Start

Requirements:

- CMake `3.11+`
- C99 compiler
- Python 3.8 for tests (or `uv` configured to provide Python 3.8)

Configure and build:

```sh
cmake -S . -B build -G Ninja
ninja -C build
```

Compile and run a sample input:

```sh
build/pycomparse --out /tmp/test.pyc test/hello.py
uv run python /tmp/test.pyc
```

## Development

### Common Build Modes

Debug/sanitizer-friendly:

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug \
  -DPYCOMPARSE_ENABLE_SANITIZERS=ON
ninja -C build
```

Release build for local profiling:

```sh
cmake -S . -B build-release -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja -C build-release
```

PGO+LTO benchmark build:

```sh
CMAKE_GENERATOR=Ninja scripts/build_pgo_lto.sh
```

### Notable CMake Options

- `-DPYCOMPARSE_ENABLE_LTO=ON`
- `-DPYCOMPARSE_ENABLE_SANITIZERS=ON` (Clang required)
- `-DPYCOMPARSE_PGO_MODE=off|generate|use`
- `-DPYCOMPARSE_PGO_DATA=/path/to/profile-data` (for `use`)
- `-DPYCOMPARSE_MARCH=auto|native|x86-64-v2|x86-64-v3|...`
- `-DCMAKE_DISABLE_FIND_PACKAGE_Iconv=TRUE`

## Testing

Run all parser + scanner integration tests:

```sh
uv run scripts/test.py
```

Run parser-only:

```sh
uv run scripts/test.py parser
```

Run scanner-only:

```sh
uv run scripts/test.py scan
```

Override compiler binary:

```sh
uv run scripts/test.py parser --compiler build/pycomparse
```

## Benchmarking

Run compile-time benchmarks on a PGO+LTO binary:

```sh
uv run python scripts/benchmark_compile_perf.py \
  --tested-compiler build-pgo-lto/pycomparse
```

For homepage-style benchmark data generation, see `benchy.sh` on the
`homepage-site` branch.

## Project Layout and Compilation Pipeline

- `src/scanner.c`: tokenization and identifier/literal interning.
- `src/parser.c`: AST construction.
- `src/ast_fold_constants.c`: AST constant folding.
- `src/codegen*.c`: scope/binding analysis and bytecode emission.
- `src/writer.c`: `.pyc` serialization.
- `src/diagnostics.c`: diagnostic construction and formatting.
- `include/pycomparse/`: public headers.
- `include/pycomparse/adt/`: core ADT/utility headers (arena, dynarrays,
  hashset/hash, stack, low-level helpers).
- `test/`: runtime, error, and compile-only tests.
- `scripts/`: build, test, and benchmark helper scripts.

## Authors

- Matthias Braun <matze@braunis.de>
- Codex
- Claude

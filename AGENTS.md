# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

## Overview

pycomparse is a Python bytecode compiler implemented in C. It consists of a scanner (lexer), parser, and bytecode generator that compiles Python source code to Python bytecode objects (.pyc files).

## Build System

This project uses CMake and Ninja for building:

```bash
cmake -S . -B build -G Ninja
ninja --quiet -C build
```

The build produces three main executables:
- `adt_test` - C unit tests for ADT components
- `scanner_test` - Tests for the lexical scanner
- `pycomparse` - Main parser/compiler that converts Python source to bytecode

## Development Commands

### Setup

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DPYCOMPARSE_ENABLE_SANITIZERS=On
```

### Building

```bash
ninja --quiet -C build
```

### Performance Builds
For local profiling/debugging (for example `perf`/`valgrind`), use an optimized
`Release` CMake build and do not rely on the default build type:
```bash
cmake -S . -B build-release -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja --quiet -C build-release
```
Use `build-release/pycomparse` (or an equivalent `-O3 -DNDEBUG` binary) for
`perf`/`valgrind` runs.

For benchmark numbers and commit-to-commit performance comparisons, use
**PGO + LTO** binaries (required), not plain `Release`.

Build PGO + LTO with:
```bash
CMAKE_GENERATOR=Ninja scripts/build_pgo_lto.sh
```
The script performs:
1. Instrumented `Release` build (`PYCOMPARSE_PGO_MODE=generate`, `PYCOMPARSE_ENABLE_LTO=ON`)
2. Training run (default workload or `--train-cmd`)
3. Profile merge (`llvm-profdata`)
4. Final optimized `Release` build (`PYCOMPARSE_PGO_MODE=use`, `PYCOMPARSE_PGO_DATA=...`)

Use the PGO+LTO binary with the benchmark script (requires `cpython-3.8/` to
be present locally — not tracked in git):
```bash
uv run python scripts/benchmark_compile_perf.py \
  --tested-compiler build-pgo-lto/pycomparse
```

For pycomparse-vs-pycomparse A/B, set both baseline and tested compiler
options (see `scripts/benchmark_compile_perf.py --help`).

### Testing
```bash
uv run scripts/test.py
```
This runner executes parser integration tests in `test/`, compares output with
reference Python execution, checks expected diagnostics in `test/errors/`, and
includes `test/compile_only/`.  It also runs scanner tokenization checks.
Use `uv run scripts/test.py parser` or `uv run scripts/test.py scan` to run
only one suite.
Use `--compiler <path>` to override the compiler binary.

### Code Quality
```bash
./scripts/format.sh    # Format all C source files with clang-format
```

After each series of code changes, run formatting and then run the full test
suite before finishing:

```bash
./scripts/format.sh
uv run scripts/test.py
```

### Commit Messages
- If an AI assistant was used for substantial implementation work, include `Co-authored-by: ` tag with `Codex <codex@openai.com>` or `Claude <noreply@anthropic.com>` at end of commit message.
- Commit messages may be verbose when that improves clarity.
- User preference: use detailed commit messages with bullet points in the body.
- User preference: do not mention routine/normal testing activity in commit messages.
- User preference: keep commit message line length to 80 characters.
- Do not use multiple `-m` options with `git commit`; write the commit message
  via a single message source (for example, an editor or `-F`). (Codex-prone.)

## Development

- Do NOT use plain `python3` as a reference. It is likely not version 3.8 so
  has different behavior what is expected for pycomparse. Use something like
  `uv run python` to get a python with the right version.
- Every commit adding a new feature or fixing an important bug should have a test
  (unless it is especially hard/impossible to test).

### Running Individual Tests
```bash
build/pycomparse --out /tmp/output.pyc test/simple.py && uv run python /tmp/output.pyc
```

### Compiling and printing bytecode for reference compiler
```bash
uv run scripts/decode.py test/simple.py
```

### Compiling and printing bytecode for PyComparse
- remember to build first
```bash
build/pycomparse --out /tmp/test.pyc test/simple.py && uv run scripts/decode.py /tmp/test.pyc
```

## Architecture

Compilation pipeline:
1. `src/scanner.c` tokenizes source and interns identifiers/literals via
   `symbol_table` and `object_intern`.
2. `src/parser.c` parses tokens into an AST
   (`include/pycomparse/ast*_types.h`).
3. `src/ast_fold_constants.c` performs constant folding on the AST.
4. `src/codegen*.c` does scope/binding analysis and emits Python bytecode/code
   objects.
5. `src/writer.c` serializes code objects to `.pyc`; `src/diagnostics.c`
   manages error reporting.

Core support modules:
- `src/object*.c`, `src/object_intern.c`: object model and intern pools.
- `src/symbol_table.c`: symbol interning and keyword prepopulation.
- `src/diagnostics.c`: diagnostic construction, storage, and formatting.
- `include/pycomparse/adt/`:
  - `.../arena.h`: bump allocator used throughout parsing/codegen.
  - `.../idynarray.h`: small-buffer-friendly growable arrays.
  - `.../hashset.h`: open-addressed hash set.
  - `.../hash.h`: hashing functions.
  - `.../stack.h`: lightweight stack.
  - `.../dynmemory.h`, `.../bitfiddle.h`: low-level utilities.
- `src/unicode_*.c`: identifier/normalization lookup tables used by scanner.

## Key Implementation Details

- Uses arena-based memory allocation for performance
- Uses recursive descent parsing with precedence climbing for expressions
- Uses anchor-token sets for parser error recovery
- Generates Python 3.8.20 compatible bytecode

## Test Structure

- `adt_test` - C unit tests for ADT components (`src/adt_test.c`)
- `test/*.py` - Positive test cases that should compile and run correctly
- `test/errors/*.py` - Error test cases with expected error output in `.expected` files
- `test/compile_only/` - Tests that must compile successfully but are not executed

# CLAUDE.md

This file provides guidance to coding agents when working with code in this repository.

## Overview

pycomparse is a Python bytecode compiler implemented in C. It consists of a scanner (lexer), parser, and bytecode generator that compiles Python source code to Python bytecode objects (.pyc files).

## Development Commands

### Building

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug -DPYCOMPARSE_ENABLE_SANITIZERS=On
ninja --quiet -C build
```

The build produces three main executables:
- `pycomparse` - Main parser/compiler that converts Python source to bytecode
- `adt_test` - C unit tests for ADT components
- `scanner_test` - Tests for the lexical scanner

### Performance Builds

> **IMPORTANT for AI agents**: Always use a PGO+LTO binary (built with
> `scripts/build_pgo_lto.py`) for *all* performance work: hyperfine,
> callgrind, perf, valgrind.  Never use `build/pycomparse` or
> `build-release/pycomparse` — those lack PGO+LTO and produce misleading
> numbers.

Use `scripts/build_pgo_lto.py -o NAME` to build PGO+LTO binaries.

```bash
# Build current working tree (including uncommitted changes)
scripts/build_pgo_lto.py -o pycomparse-current

# Build specific revisions under chosen names
scripts/build_pgo_lto.py main   -o pycomparse-baseline
scripts/build_pgo_lto.py HEAD   -o pycomparse-head

# Benchmark vs reference Python (requires cpython-3.8/ locally)
uv run python scripts/benchmark_compile_perf.py \
  --python-compiler python='uv run python' \
  --compiler current=./pycomparse-current

# pycomparse-vs-pycomparse wall-clock A/B (top 20 largest stdlib files)
uv run python scripts/benchmark_compile_perf.py \
  --compiler baseline=./pycomparse-baseline \
  --compiler current=./pycomparse-current

# Same but instruction counts via callgrind
uv run python scripts/benchmark_compile_perf.py \
  --compiler baseline=./pycomparse-baseline \
  --compiler current=./pycomparse-current \
  --runner callgrind
```

### Testing
```bash
uv run scripts/test.py
```
This runner executes parser integration tests in `test/`, compares output with
reference Python execution, checks expected diagnostics in `test/errors/`, and
includes manifest-driven parser tests in `test/options/`.  It also runs
scanner tokenization checks.
Use `uv run scripts/test.py parser` or `uv run scripts/test.py scan` to run
only one suite.
Use `--compiler <path>` to override the compiler binary.

### Code Quality
```bash
./scripts/format.sh    # Format all C source files with clang-format
./scripts/format_lint_py.sh # Format and lint Python scripts with ruff
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
  has different behavior than what is expected for pycomparse. Use something like
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
- `test/scanner_only/` - Scanner/tokenizer fixtures that are not parser/runtime
  tests
- `test/options/` - Parser tests with sidecars for non-default compiler flags,
  compile-only cases, or code-object assertions

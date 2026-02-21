# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

## Overview

PyParse is a Python bytecode compiler implemented in C. It consists of a scanner (lexer), parser, and bytecode generator that compiles Python source code to Python bytecode objects (.pyc files).

## Build System

This project uses CMake and Ninja for building:

```bash
cmake -S . -B build -G Ninja
ninja --quiet -C build
```

The build produces three main executables:
- `arena_test` - Tests for the arena memory allocator
- `scanner_test` - Tests for the lexical scanner
- `pycomparse` - Main parser/compiler that converts Python source to bytecode

## Development Commands

### Building
```bash
cmake -S . -B build -G Ninja
ninja --quiet -C build
```

### Performance Builds
For performance profiling/benchmarking, use an optimized CMake build and do
not rely on the default build type:
```bash
cmake -S . -B build-release -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja --quiet -C build-release
```
Use `build-release/pycomparse` (or an equivalent `-O3 -DNDEBUG` binary) for
`perf`/`valgrind` runs.

For PGO + LTO builds, use:
```bash
CMAKE_GENERATOR=Ninja scripts/build_pgo_lto.sh
```
The helper performs:
1. Instrumented `Release` build (`PYCOMPARSE_PGO_MODE=generate`, `PYCOMPARSE_ENABLE_LTO=ON`)
2. Training run (default workload or `--train-cmd`)
3. Profile merge (`llvm-profdata`)
4. Final optimized `Release` build (`PYCOMPARSE_PGO_MODE=use`, `PYCOMPARSE_PGO_DATA=...`)

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
Note: scan mode currently has known pre-existing mismatches in this repository.

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
- For Codex: do not use multiple `-m` options with `git commit`; write the
  commit message via a single message source (for example, an editor or
  `-F`).

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

### Compiling and print bytecode for reference compiler
```bash
uv run scripts/decode.py /tmp/test.py
```

### Compile and print bytecode for PyComparse
- remember to build first
```bash
build/pycomparse --out /tmp/test.pyc /tmp/test.py && uv run scripts/decode.py /tmp/test.pyc
```

## Architecture

### Core Components

1. **Scanner** (`src/scanner.c`, `include/pycomparse/scanner.h`) - Lexical analysis
   - Tokenizes Python source code
   - Handles Python-specific tokens (keywords, operators, literals)
   - Manages symbol table for identifiers

2. **Parser** (`src/parser.c`, `include/pycomparse/parser.h`) - Syntax analysis and AST generation
   - Builds Abstract Syntax Tree from tokens
   - Handles Python grammar rules
   - Supports expressions, statements, and control flow

3. **AST** (`src/ast.c`, `include/pycomparse/ast.h`) - Abstract Syntax Tree representation
   - Defines all AST node types in `include/pycomparse/ast_types.h`
   - Handles expression types (binary ops, calls, comprehensions, etc.)
   - Statement types (assignments, control flow, function/class definitions)

4. **Code Generator** (`src/codegen*.c`, `include/pycomparse/codegen*.h`) - Bytecode emission
   - Converts AST to Python bytecode
   - Manages bytecode optimization and stack tracking
   - Handles different code contexts (module, function, class)

5. **Object System** (`src/object*.c`, `include/pycomparse/object*.h`) - Python object representation
   - Implements Python object model in C
   - Handles strings, numbers, code objects, etc.
   - Object interning for performance

6. **Symbol Tables** (`src/symbol_table.c`, `include/pycomparse/symbol_table.h`) - Name resolution
   - Tracks variable scopes and bindings
   - Manages local vs global variable resolution

### Data Structures

- **ADT Library** (`include/pycomparse/adt/`) - Core data structures (arena allocator, dynamic arrays, hash sets, stacks)
- **Arena Memory Management** - Custom allocator for efficient memory management
- **Token System** (`include/pycomparse/tokens.h`, `include/pycomparse/token_kinds.h`) - Token definitions and utilities

## Key Implementation Details

- Uses arena-based memory allocation for performance
- Implements Python's operator precedence and associativity rules
- Generates Python 3.8.20 compatible bytecode
- Supports most Python language constructs (see TODO in README.md for missing features)

## Test Structure

- `test/*.py` - Positive test cases that should compile and run correctly
- `test/errors/*.py` - Error test cases with expected error output in `.expected` files
- `test2/` - Additional test files

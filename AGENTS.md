# AGENTS.md

This file provides guidance to coding agents when working with code in this repository.

## Overview

PyParse is a Python bytecode compiler implemented in C. It consists of a scanner (lexer), parser, and bytecode generator that compiles Python source code to Python bytecode objects (.pyc files).

## Build System

This project uses CMake and Ninja for building:

```bash
cmake -S . -B build -G Ninja
ninja -C build
```

The build produces three main executables:
- `arena_test` - Tests for the arena memory allocator
- `scanner_test` - Tests for the lexical scanner
- `parser_test` - Main parser/compiler that converts Python source to bytecode

## Development Commands

### Building
```bash
cmake -S . -B build -G Ninja
ninja -C build
```

### Performance Builds
For performance profiling/benchmarking, use an optimized CMake build and do
not rely on the default build type:
```bash
cmake -S . -B build-release -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja -C build-release
```
Use `build-release/parser_test` (or an equivalent `-O3 -DNDEBUG` binary) for
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
uv run ./test.sh
```
This script runs the parser against test files in `test/` and compares output with reference Python execution. It also tests error cases in `test/errors/` and verifies expected error messages.

### Code Quality
```bash
./scripts/format.sh    # Format all C source files with clang-format
```

### Commit Messages
- If an AI assistant (for example Codex or Claude) was used for substantial implementation work, include `Co-authored-by: <ai-name>` in the commit message body.
- Commit messages may be verbose when that improves clarity.
- User preference: use detailed commit messages with bullet points in the body.
- User preference: do not mention routine/normal testing activity in commit messages.
- User preference: keep commit message line length to 80 characters.

### Running Individual Tests
```bash
build/parser_test test/simple.py > /tmp/output.pyc && uv run python /tmp/output.pyc
```

### Compiling and print bytecode for reference compiler
```bash
uv run utils/decode.py /tmp/test.py
```

### Compile and print bytecode for PyComparse
- remember to build first
```bash
build/parser_test /tmp/test.py | uv run utils/decode.py
```

## Architecture

### Core Components

1. **Scanner** (`scanner.c/h`) - Lexical analysis
   - Tokenizes Python source code
   - Handles Python-specific tokens (keywords, operators, literals)
   - Manages symbol table for identifiers

2. **Parser** (`parser.c/h`) - Syntax analysis and AST generation
   - Builds Abstract Syntax Tree from tokens
   - Handles Python grammar rules
   - Supports expressions, statements, and control flow

3. **AST** (`ast.c/h`) - Abstract Syntax Tree representation
   - Defines all AST node types in `ast_types.h`
   - Handles expression types (binary ops, calls, comprehensions, etc.)
   - Statement types (assignments, control flow, function/class definitions)

4. **Code Generator** (`codegen*.c/h`) - Bytecode emission
   - Converts AST to Python bytecode
   - Manages bytecode optimization and stack tracking
   - Handles different code contexts (module, function, class)

5. **Object System** (`object*.c/h`) - Python object representation
   - Implements Python object model in C
   - Handles strings, numbers, code objects, etc.
   - Object interning for performance

6. **Symbol Tables** (`symbol_table.c/h`) - Name resolution
   - Tracks variable scopes and bindings
   - Manages local vs global variable resolution

### Data Structures

- **ADT Library** (`adt/`) - Core data structures (arena allocator, dynamic arrays, hash sets, stacks)
- **Arena Memory Management** - Custom allocator for efficient memory management
- **Token System** (`tokens.h`, `token_kinds.h`) - Token definitions and utilities

## Key Implementation Details

- Uses arena-based memory allocation for performance
- Implements Python's operator precedence and associativity rules
- Generates Python 3.8.20 compatible bytecode
- Supports most Python language constructs (see TODO in README.md for missing features)

## Test Structure

- `test/*.py` - Positive test cases that should compile and run correctly
- `test/errors/*.py` - Error test cases with expected error output in `.expected` files
- `test2/` - Additional test files

## Current Limitations

See README.md TODO section for comprehensive list of unimplemented features including:
- F-strings, complex numeric literals
- Type annotations, walrus operator, lambda expressions
- Exception handling (try/except/finally)
- Async/await syntax
- Advanced function parameters (star args, keyword-only)

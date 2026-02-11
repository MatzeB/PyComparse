# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

PyParse is a Python bytecode compiler implemented in C. It consists of a scanner (lexer), parser, and bytecode generator that compiles Python source code to Python bytecode objects (.pyc files).

## Build System

This project uses CMake and Ninja for building:

```bash
ninja -C build
```

The build produces three main executables:
- `arena_test` - Tests for the arena memory allocator
- `scanner_test` - Tests for the lexical scanner
- `parser_test` - Main parser/compiler that converts Python source to bytecode

## Development Commands

### Building
```bash
mkdir -p build && cd build && cmake .. && make
```

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
- If an AI assistant (for example Codex or Claude) was used for substantial implementation work, include a short credit line in the commit message body.

### Running Individual Tests
```bash
build/parser_test test/simple.py > /tmp/output.pyc && uv run python /tmp/output.pyc
```

### Compiling and print bytecode for reference compiler
```bash
uv run utils/decode.py /tmp/test.py
```

### Compile and print bytecode for pyparse
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

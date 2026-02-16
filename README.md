# PyComparse

PyComparse is a Python front-end and bytecode compiler written in C.
It scans and parses Python source, then emits `.pyc` bytecode executable by CPython.
PyComparse currently targets Python 3.8 syntax and bytecode.
The parser uses recursive descent with precedence climbing for expressions, and the implementation style is straightforward and performance-focused.

This project is also an experiment in heavy AI-assisted coding 🤖.

## Status

- Scanner, parser, and code generator for complete Python 3.8 language parity
- Successfully handles files from the Python 3.8 standard library and test
  suite, except tests that depend on exact line-number table details and tests
  that fail when running from precompiled `.pyc` files.

### Known Limitations

- Constant folding currently supports a subset of operations: `+`, `-`, `*`
  for integers below 63 bits and floats, string concatenation/multiplication,
  and dead-code elimination during code generation.
- Line number tables do not always agree with CPython (TODO).
- Maximum stack computation results overestimate in some circumstances (TODO).

## Quick Start

Requirements:

- CMake 3.11+
- A C99 compiler
- Python 3.8 (for running integration tests), or a working `uv` setup that can provision Python 3.8

Configure and build:

```sh
cmake -S . -B build
cmake --build build -j
```

Compile and run a sample input:

```sh
./build/scanner_test
./build/pycomparse --out /tmp/test.pyc test/hello.py
uv python /tmp/test.pyc
```

## Development

### Build Configuration

CMake options currently available:

- `-DPYCOMPARSE_ENABLE_LTO=ON` to enable LTO/IPO (if supported by toolchain)
- `-DPYCOMPARSE_PGO_MODE=off|generate|use`
- `-DPYCOMPARSE_PGO_DATA=/path/to/profile-data` when `PYCOMPARSE_PGO_MODE=use`
- `-DCMAKE_DISABLE_FIND_PACKAGE_Iconv=TRUE` builds without iconv

For a full PGO+LTO build flow:

```sh
scripts/build_pgo_lto.sh
```

### Testing

Run the integration test script:

```sh
uv run scripts/test.py parser
```

By default it uses `build/pycomparse`. Override with:

```sh
uv run scripts/test.py parser --compiler build2/pycomparse
```

Run scanner tokenization tests with:

```sh
uv run scripts/test.py scan
```
### Project Layout

- `scanner.c`, `scanner.h`: tokenization
- `parser.c`, `parser.h`: parsing to AST
- `codegen*.c`, `writer.c`: bytecode emission
- `test/`: runtime/error/compile-only tests
- `scripts/`: helper scripts

## Authors

- Matthias Braun <matze@braunis.de> - design, structure, and initial implementation
- Codex
- Claude

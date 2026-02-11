# PyComparse

PyComparse is a Python front-end and bytecode compiler written in C.
It scans and parses Python source, then emits `.pyc` bytecode executable by CPython.
PyComparse currently targets Python 3.8 syntax and bytecode.
The parser uses recursive descent with precedence climbing for expressions, and the implementation style is straightforward and performance-focused.

## Quick Start

Requirements:

- CMake 3.10+
- A C compiler
- Python 3.8 (for running integration tests), or a working `uv` setup that can provision Python 3.8

Configure and build:

```sh
cmake -S . -B build
cmake --build build -j
```

Compile and run a sample input:

```sh
./build/scanner_test
./build/parser_test test/hello.py > /tmp/test.pyc
python3 /tmp/test.pyc
```

## Testing

Run the integration test script:

```sh
./test.sh
```

By default it uses `build/parser_test`. Override with:

```sh
PARSER_TEST=build2/parser_test ./test.sh
```

## Build Configuration

CMake options currently available:

- `-DPYCOMPARSE_ENABLE_LTO=ON` to enable LTO/IPO (if supported by toolchain)
- `-DPYCOMPARSE_PGO_MODE=off|generate|use`
- `-DPYCOMPARSE_PGO_DATA=/path/to/profile-data` when `PYCOMPARSE_PGO_MODE=use`

For a full PGO+LTO build flow:

```sh
scripts/build_pgo_lto.sh
```

## Project Layout

- `scanner.c`, `scanner.h`: tokenization
- `parser.c`, `parser.h`: parsing to AST
- `codegen*.c`, `writer.c`: bytecode emission
- `test/`: runtime/error/compile-only tests
- `scripts/`: helper scripts

## Authors

- Matthias Braun <matze@braunis.de> - design, structure, and initial implementation
- Codex
- Claude

CFLAGS += -I.
CFLAGS += -O0 -g -std=c11
CFLAGS += -Wall -Wextra -Wmissing-prototypes

.PHONY: all
all: scanner_test parser_test

.PHONY: scanner_test
scanner_test: build/scanner_test
.PHONY: parser_test
parser_test: build/parser_test

adt/arena.h : adt/bitfiddle.h

adt/hashset.h: adt/bitfiddle.h

build/scanner_test: scanner.c scanner_test.c symbol_table.c
	mkdir -p build
	clang $(CFLAGS) $^ -o $@

build/parser_test: parser.c scanner.c symbol_table.c writer.c parser_test.c
	mkdir -p build
	clang $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -rf build/*

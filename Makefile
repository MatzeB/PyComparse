CFLAGS += -I.
CFLAGS += -O0 -g -std=c11
CFLAGS += -Wall -Wextra -Wmissing-prototypes

.PHONY: all
all: scanner_test

.PHONY: scanner_test
scanner_test: build/scanner_test

adt/arena.h : adt/bitfiddle.h

adt/hashset.h: adt/bitfiddle.h

build/scanner_test: scanner.c scanner_test.c symbol_table.c
	mkdir -p build
	clang $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -rf build/*

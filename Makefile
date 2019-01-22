CFLAGS += -I.
CFLAGS += -O0 -g -std=c11
CFLAGS += -Wall -Wextra -Wmissing-prototypes

.PHONY: all
all: scanner

.PHONY: scanner
scanner: build/scanner

adt/arena.h : adt/bitfiddle.h

adt/hashset.h: adt/bitfiddle.h

build/scanner: scanner.c
	mkdir -p build
	clang $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -rf build/*

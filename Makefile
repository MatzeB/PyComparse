CFLAGS += -I.
CFLAGS += -O0 -g -std=c11
CFLAGS += -Wall -Wextra -Wmissing-prototypes

.PHONY: all
all: scanner

.PHONY: scanner
scanner: build/scanner

adt/obst.h: obstack/obstack.h

adt/hashset.h: adt/bitfiddle.h

obstack/obstack.c: obstack/obstack.h

build/scanner: scanner.c obstack/obstack.c
	mkdir -p build
	clang $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -rf build/*

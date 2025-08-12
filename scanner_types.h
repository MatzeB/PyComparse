#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAXINDENT 100

struct arena;
struct symbol_table;
union object;

struct token_string {
  size_t      length;
  const char *chars;
};

struct token {
  uint16_t kind;
  union {
    struct symbol *symbol;
    union object  *object;
  } u;
};

struct scanner_state {
  int   c;
  char *p;

  struct token token;

  const char *filename;
  unsigned    line;
  unsigned    paren_level;

  struct symbol_table  *symbol_table;
  struct object_intern *objects;
  struct arena         *strings;

  bool     at_begin_of_line;
  unsigned pending_dedents;
  unsigned last_line_indent;
  unsigned indentation_stack_top;
  unsigned indentation_stack[MAXINDENT];

  char  *buffer_end;
  FILE  *input;
  char  *read_buffer;
  size_t read_buffer_size;
};

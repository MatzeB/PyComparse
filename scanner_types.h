#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAXINDENT 100

struct arena;
struct symbol_table;

struct token {
  uint16_t kind;
  union {
    struct symbol *symbol;
    const char *string;
  } u;
};

struct scanner_state {
  int   c;
  char *p;

  struct token token;

  unsigned line;
  unsigned paren_level;

  struct symbol_table *symbol_table;
  struct arena        *strings;

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

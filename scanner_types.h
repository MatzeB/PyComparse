#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "adt/arena.h"
#include "nullable.h"

ASSUME_NONNULL_BEGIN

#define MAXINDENT           100
#define MAX_FSTRING_NESTING 149

struct arena;
struct diagnostics_state;
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

enum string_quote {
  QUOTE_APOSTROPHE = 1 << 0,
  QUOTE_QUOTATION_MARK = 1 << 1,
  QUOTE_TRIPLE = 1 << 2,
  QUOTE_RCRULY = 1 << 3,
};

struct fstring_state {
  unsigned paren_level;
  uint8_t  quote;
  bool     format_spec;
};

struct scanner_fstring_debug_capture_state {
  struct arena   arena;
  bool           active;
  uint8_t        depth;
  unsigned       starts[MAX_FSTRING_NESTING];
  char *nullable tail_start;
};

struct scanner_state {
  int            c;
  char *nullable p;

  struct token token;

  char *nullable buffer_end;
  char *nullable read_buffer;
  size_t         read_buffer_size;

  FILE       *input;
  const char *filename;
  unsigned    line;
  unsigned    paren_level;

  struct symbol_table  *symbol_table;
  struct object_intern *objects;
  struct arena         *strings;

  struct fstring_state                       fstring;
  struct scanner_fstring_debug_capture_state fstring_debug;
  bool                                       at_begin_of_line;
  uint8_t                                    fstring_stack_top;
  unsigned                                   pending_dedents;
  unsigned                                   last_line_indent;
  unsigned                                   indentation_stack_top;

  struct diagnostics_state *d;

  unsigned             indentation_stack[MAXINDENT];
  struct fstring_state fstring_stack[MAX_FSTRING_NESTING];
};

ASSUME_NONNULL_END

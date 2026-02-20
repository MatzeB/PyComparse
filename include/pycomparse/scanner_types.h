#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

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
  uint8_t  string_is_fstring;
  union {
    struct symbol *symbol;
    union object  *object;
  } u;
};

enum string_quote {
  QUOTE_APOSTROPHE = 1 << 0,
  QUOTE_QUOTATION_MARK = 1 << 1,
  QUOTE_TRIPLE = 1 << 2,
  QUOTE_RCURLY = 1 << 3,
};

struct fstring_state {
  unsigned paren_level;
  uint8_t  quote;
  bool     format_spec;
};

struct fstring_debug_capture {
  /* Bytes captured from previous read buffers after refill. */
  char *nullable       spilled_prefix;
  unsigned             spilled_size;
  unsigned             spilled_capacity;
  uint8_t              depth;
  unsigned             starts[MAX_FSTRING_NESTING];
  const char *nullable tail_start;
};

struct scanner_state {
  int                  c;
  const char *nullable p;

  struct token token;

  const char *nullable buffer_end;
  char *nullable       read_buffer;
  size_t               read_buffer_size;

  FILE *nullable input;
  const char    *filename;
  unsigned       line;
  unsigned       paren_level;

  struct symbol_table  *symbol_table;
  struct object_intern *objects;
  struct arena         *strings;

  struct fstring_state         fstring;
  struct fstring_debug_capture fstring_debug;
  bool                         at_begin_of_line;
  uint8_t                      fstring_stack_top;
  unsigned                     pending_dedents;
  unsigned                     last_line_indent;
  unsigned                     indentation_stack_top;

  struct diagnostics_state *d;
  bool                      read_error_seen;
  bool                      read_error_token_pending;

  unsigned             indentation_stack[MAXINDENT];
  struct fstring_state fstring_stack[MAX_FSTRING_NESTING];

#ifndef PYCOMPARSE_NO_ICONV
  FILE *nullable transcoded_input;
  char *nullable transcoded_source;
#endif
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

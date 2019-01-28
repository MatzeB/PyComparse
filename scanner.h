#ifndef SCANNER_H
#define SCANNER_H

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#define MAXINDENT   100

struct symbol;
struct symbol_table;
struct arena;

struct token {
  uint16_t       kind;
  union {
    struct symbol *symbol;
    const char *string;
  } u;
};

struct scanner_state {
  int c;

  char  *p;
  char  *buffer_end;
  FILE  *input;
  char  *read_buffer;
  size_t read_buffer_size;

  unsigned line;
  unsigned paren_level;

  struct symbol_table *symbol_table;
  struct arena        *strings;

  bool     at_begin_of_line;
  uint8_t  pending_dedents;
  unsigned last_line_indent;
  unsigned indentation_stack_top;
  unsigned indentation_stack[MAXINDENT];
};

void scanner_init(struct scanner_state *s, FILE* input,
                  struct symbol_table *symbol_table, struct arena *strings);

void scanner_free(struct scanner_state *s);

void next_token(struct scanner_state *s, struct token *result);

#endif

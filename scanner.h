#pragma once

#include <stdint.h>
#include <stdio.h>

struct arena;
struct object_intern;
struct scanner_state;
struct symbol_table;
struct token;

struct location {
  unsigned line;
  /* unsigned column; */
};

void scanner_init(struct scanner_state *s, FILE *input, const char *filename,
                  struct symbol_table  *symbol_table,
                  struct object_intern *objects, struct arena *strings);

void scanner_free(struct scanner_state *s);

void scanner_next_token(struct scanner_state *s);

const char *token_kind_name(uint16_t token_kind);

void print_token(FILE *out, const struct token *token);

struct location scanner_location(struct scanner_state *s);

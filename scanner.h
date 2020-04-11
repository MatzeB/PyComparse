#pragma once

#include <stdio.h>
#include <stdint.h>

struct arena;
struct token;
struct scanner_state;
struct symbol_table;

void scanner_init(struct scanner_state *s, FILE* input,
                  struct symbol_table *symbol_table, struct arena *strings);

void scanner_free(struct scanner_state *s);

void scanner_next_token(struct scanner_state *s);

const char *token_kind_name(uint16_t token_kind);

void print_token(FILE *out, const struct token *token);

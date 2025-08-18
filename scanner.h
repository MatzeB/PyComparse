#pragma once

#include <stdint.h>
#include <stdio.h>

#include "nullable.h"
#include "scanner_location.h"
#include "token_kinds.h"

ASSUME_NONNULL_BEGIN

struct arena;
struct object_intern;
struct scanner_state;
struct symbol_table;
struct token;

void scanner_init(struct scanner_state *s, FILE *input, const char *filename,
                  struct symbol_table  *symbol_table,
                  struct object_intern *objects, struct arena *strings);

void scanner_free(struct scanner_state *s);

void scanner_next_token(struct scanner_state *s);

const char *token_kind_name(enum token_kind token_kind);

void print_token(FILE *out, const struct token *token);

struct location scanner_location(struct scanner_state *s);

ASSUME_NONNULL_END

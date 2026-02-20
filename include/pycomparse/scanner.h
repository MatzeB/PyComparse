#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "pycomparse/nullable.h"
#include "pycomparse/scanner_location.h"
#include "pycomparse/token_kinds.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct arena;
struct diagnostics_state;
union object;
struct object_intern;
struct scanner_state;
struct symbol_table;
struct token;

void scanner_init(struct scanner_state *s, FILE *input, const char *filename,
                  struct symbol_table  *symbol_table,
                  struct object_intern *objects, struct arena *strings,
                  struct diagnostics_state *diagnostics);

void scanner_init_from_string(struct scanner_state *s, const char *buf,
                              const char               *filename,
                              struct symbol_table      *symbol_table,
                              struct object_intern     *objects,
                              struct arena             *strings,
                              struct diagnostics_state *diagnostics,
                              bool                      is_utf8);

void scanner_free(struct scanner_state *s);

void scanner_next_token(struct scanner_state *s);

const char *token_kind_name(enum token_kind token_kind);

void print_token(FILE *out, const struct token *token);

struct location scanner_location(struct scanner_state *s);

void          scanner_fstring_debug_capture_begin(struct scanner_state *s);
void          scanner_fstring_debug_capture_discard(struct scanner_state *s);
union object *scanner_fstring_debug_capture_finish(struct scanner_state *s);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

#include "pycomparse/nullable.h"
#include "pycomparse/scanner_location.h"
#include "pycomparse/token_kinds.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct diagnostics_state;
struct symbol;
struct token;
union ast_expression;

void diag_begin_error(struct diagnostics_state *s, struct location location);
void diag_frag(struct diagnostics_state *s, const char *message);
void diag_frag_copy(struct diagnostics_state *s, const char *str, size_t len);
void diag_token(struct diagnostics_state *s, struct token *token);
void diag_token_kind(struct diagnostics_state *s, enum token_kind kind);
void diag_symbol(struct diagnostics_state *s, struct symbol *symbol);
void diag_quoted_char(struct diagnostics_state *s, char c);
void diag_expression(struct diagnostics_state *s,
                     union ast_expression     *expression);
void diag_end(struct diagnostics_state *s);

void diag_init(struct diagnostics_state *s, const char *default_filename);
void diag_free(struct diagnostics_state *s);

bool diag_had_errors(const struct diagnostics_state *s);

const void *nullable diag_elems_begin(const struct diagnostics_state *s);
const void *nullable diag_elems_end(const struct diagnostics_state *s);

void diag_print_all(const struct diagnostics_state *s, FILE *out);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

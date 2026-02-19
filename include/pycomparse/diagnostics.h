#pragma once

#include <stdio.h>

#include "pycomparse/scanner_location.h"
#include "pycomparse/token_kinds.h"

#include "pycomparse/nullable.h"

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
void diag_token(struct diagnostics_state *s, struct token *token);
void diag_token_kind(struct diagnostics_state *s, enum token_kind kind);
void diag_symbol(struct diagnostics_state *s, struct symbol *symbol);
void diag_quoted_char(struct diagnostics_state *s, char c);
void diag_expression(struct diagnostics_state *s,
                     union ast_expression     *expression);
void diag_end(struct diagnostics_state *s);

void diag_init(struct diagnostics_state *s, FILE *out,
               const char *default_filename);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

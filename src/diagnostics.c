#include "pycomparse/diagnostics.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "pycomparse/ast.h"
#include "pycomparse/diagnostics_types.h"
#include "pycomparse/scanner.h"
#include "pycomparse/symbol_types.h"

void diag_begin_error(struct diagnostics_state *s, struct location location)
{
  assert(!s->in_diagnostic);
  s->in_diagnostic = true;
  s->had_error = true;
  if (location.line == INVALID_LOCATION.line) {
    fprintf(s->out, "%s error: ", s->default_filename);
  } else {
    fprintf(s->out, "%s:%u error: ", s->default_filename, location.line);
  }
}

void diag_frag(struct diagnostics_state *s, const char *message)
{
  assert(s->in_diagnostic);
  fputs(message, s->out);
}

void diag_token(struct diagnostics_state *s, struct token *token)
{
  assert(s->in_diagnostic);
  print_token(s->out, token);
}

void diag_token_kind(struct diagnostics_state *s, enum token_kind kind)
{
  assert(s->in_diagnostic);
  const char *name = token_kind_name(kind);
  fputs(name, s->out);
}

void diag_symbol(struct diagnostics_state *s, struct symbol *symbol)
{
  assert(s->in_diagnostic);
  fputc('`', s->out);
  fputs(symbol->string, s->out);
  fputc('`', s->out);
}

void diag_expression(struct diagnostics_state *s,
                     union ast_expression     *expression)
{
  assert(s->in_diagnostic);
  const char *name;
  switch (ast_expression_type(expression)) {
  case AST_INVALID:
    name = "<invalid>";
    break;
  case AST_CONST:
    name = "literal";
    break;
  case AST_DICT_DISPLAY:
    name = "dict literal";
    break;
  case AST_IDENTIFIER:
    name = "name";
    break;
  case AST_SET_DISPLAY:
    name = "set literal";
    break;
  default:
    name = "expression";
    break;
  }
  diag_frag(s, name);
}

void diag_quoted_char(struct diagnostics_state *s, char c)
{
  assert(s->in_diagnostic);
  fputc('`', s->out);
  fputc(c, s->out);
  fputc('`', s->out);
}

void diag_end(struct diagnostics_state *s)
{
  assert(s->in_diagnostic);
  assert(s->had_error);
  s->in_diagnostic = false;
  fputc('\n', s->out);
  fflush(s->out);
}

void diag_init(struct diagnostics_state *s, FILE *out,
               const char *default_filename)
{
  memset(s, 0, sizeof(*s));
  s->out = out;
  s->default_filename = default_filename;
}

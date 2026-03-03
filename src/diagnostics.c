#include "pycomparse/diagnostics.h"

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pycomparse/adt/dynmemory.h"
#include "pycomparse/ast.h"
#include "pycomparse/diagnostics_types.h"
#include "pycomparse/object.h"
#include "pycomparse/object_types.h"
#include "pycomparse/scanner.h"
#include "pycomparse/scanner_types.h"
#include "pycomparse/symbol_types.h"

static void diag_push(struct diagnostics_state *s, const void *data,
                      unsigned size)
{
  unsigned old_size = s->elems_size;
  unsigned new_size = old_size + size;
  if (new_size > s->elems_capacity) {
    s->elems = dynmemory_grow(s->elems, &s->elems_capacity, new_size, 1);
  }
  memcpy(s->elems + old_size, data, size);
  s->elems_size = new_size;
}

static void diag_push_tag(struct diagnostics_state *s, enum diag_elem_tag tag)
{
  uint8_t t = (uint8_t)tag;
  diag_push(s, &t, 1);
}

void diag_begin_error(struct diagnostics_state *s, struct location location)
{
  assert(!s->in_diagnostic);
  s->in_diagnostic = true;
  s->num_errors++;
  struct diag_elem_begin e = { location.line, s->default_filename };
  diag_push_tag(s, DIAG_ELEM_BEGIN);
  diag_push(s, &e, sizeof(e));
}

void diag_frag(struct diagnostics_state *s, const char *message)
{
  assert(s->in_diagnostic);
  struct diag_elem_cstr e = { message };
  diag_push_tag(s, DIAG_ELEM_CSTR);
  diag_push(s, &e, sizeof(e));
}

void diag_frag_copy(struct diagnostics_state *s, const char *str, size_t len)
{
  assert(s->in_diagnostic);
  while (len > 0) {
    uint8_t chunk_len = len > UINT8_MAX ? UINT8_MAX : (uint8_t)len;
    struct diag_elem_inline_str e = { chunk_len };
    diag_push_tag(s, DIAG_ELEM_INLINE_STR);
    diag_push(s, &e, sizeof(e));
    diag_push(s, str, chunk_len);
    str += chunk_len;
    len -= chunk_len;
  }
}

void diag_token(struct diagnostics_state *s, struct token *token)
{
  assert(s->in_diagnostic);
  enum token_kind kind = (enum token_kind)token->kind;
  switch (kind) {
  case T_IDENTIFIER: {
    struct diag_elem_symbol e = { token->u.symbol };
    diag_push_tag(s, DIAG_ELEM_SYMBOL);
    diag_push(s, &e, sizeof(e));
    break;
  }
  case T_STRING:
  case T_FSTRING_START:
  case T_FSTRING_FRAGMENT:
  case T_FSTRING_END:
  case T_FLOAT:
  case T_INTEGER: {
    struct diag_elem_token e = { token->kind, token->u.object };
    diag_push_tag(s, DIAG_ELEM_TOKEN);
    diag_push(s, &e, sizeof(e));
    break;
  }
  default:
    diag_frag(s, token_kind_name(kind));
    break;
  }
}

void diag_token_kind(struct diagnostics_state *s, enum token_kind kind)
{
  assert(s->in_diagnostic);
  diag_frag(s, token_kind_name(kind));
}

void diag_symbol(struct diagnostics_state *s, struct symbol *symbol)
{
  assert(s->in_diagnostic);
  struct diag_elem_symbol e = { symbol };
  diag_push_tag(s, DIAG_ELEM_SYMBOL);
  diag_push(s, &e, sizeof(e));
}

void diag_quoted_char(struct diagnostics_state *s, char c)
{
  assert(s->in_diagnostic);
  struct diag_elem_quoted_char e = { c };
  diag_push_tag(s, DIAG_ELEM_QUOTED_CHAR);
  diag_push(s, &e, sizeof(e));
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

void diag_end(struct diagnostics_state *s)
{
  assert(s->in_diagnostic);
  diag_push_tag(s, DIAG_ELEM_END);
  s->in_diagnostic = false;
}

void diag_init(struct diagnostics_state *s, const char *default_filename)
{
  memset(s, 0, sizeof(*s));
  s->default_filename = default_filename;
}

void diag_free(struct diagnostics_state *s)
{
  free(s->elems);
}

bool diag_had_errors(const struct diagnostics_state *s)
{
  return s->num_errors > 0;
}

const void *diag_elems_begin(const struct diagnostics_state *s)
{
  return s->elems;
}

const void *diag_elems_end(const struct diagnostics_state *s)
{
  return s->elems + s->elems_size;
}

static void print_token_elem(FILE *out, uint16_t kind, const union object *obj)
{
  switch ((enum token_kind)kind) {
  case T_STRING:
  case T_FSTRING_START:
  case T_FSTRING_FRAGMENT:
  case T_FSTRING_END: {
    if (kind == T_STRING) {
      if (obj->type == OBJECT_BYTES) {
        fputs("b\"", out);
      } else {
        fputc('"', out);
      }
    } else {
      fputs(token_kind_name((enum token_kind)kind), out);
      fputs(" \"", out);
    }
    const struct object_string *str = &obj->string;
    for (const char *c = str->chars, *e = c + str->length; c != e; ++c) {
      unsigned char uc = (unsigned char)*c;
      if (isprint(uc)) {
        fputc(*c, out);
      } else {
        fprintf(out, "\\x%02x", uc);
      }
    }
    fputc('"', out);
    break;
  }
  case T_FLOAT:
    fprintf(out, "%f", obj->float_obj.value);
    break;
  case T_INTEGER:
    if (obj->type == OBJECT_INT) {
      fprintf(out, "%" PRId64, obj->int_obj.value);
    } else {
      fprintf(out, "<bigint:%upydigits>", obj->big_int.num_pydigits);
    }
    break;
  default:
    break;
  }
}

static void print_quoted_char_elem(FILE *out, char c)
{
  unsigned char uc = (unsigned char)c;
  fputc('`', out);
  switch (c) {
  case '\n':
    fputs("\\n", out);
    break;
  case '\r':
    fputs("\\r", out);
    break;
  case '\t':
    fputs("\\t", out);
    break;
  case '\f':
    fputs("\\f", out);
    break;
  case '\v':
    fputs("\\v", out);
    break;
  case '\\':
    fputs("\\\\", out);
    break;
  case '`':
    fputs("\\`", out);
    break;
  default:
    if (isprint(uc)) {
      fputc(c, out);
    } else {
      fprintf(out, "\\x%02x", uc);
    }
    break;
  }
  fputc('`', out);
}

void diag_print_all(const struct diagnostics_state *s, FILE *out)
{
  const char *p = (const char *)diag_elems_begin(s);
  const char *end = (const char *)diag_elems_end(s);

  while (p != end) {
    enum diag_elem_tag tag = (enum diag_elem_tag)(uint8_t)*p;
    p += 1;

    switch (tag) {
    case DIAG_ELEM_BEGIN: {
      struct diag_elem_begin e;
      memcpy(&e, p, sizeof(e));
      p += sizeof(e);
      if (e.line == 0) {
        fprintf(out, "%s error: ", e.filename);
      } else {
        fprintf(out, "%s:%u error: ", e.filename, (unsigned)e.line);
      }
      break;
    }
    case DIAG_ELEM_CSTR: {
      struct diag_elem_cstr e;
      memcpy(&e, p, sizeof(e));
      p += sizeof(e);
      fputs(e.str, out);
      break;
    }
    case DIAG_ELEM_SYMBOL: {
      struct diag_elem_symbol e;
      memcpy(&e, p, sizeof(e));
      p += sizeof(e);
      fputc('`', out);
      fputs(e.symbol->string, out);
      fputc('`', out);
      break;
    }
    case DIAG_ELEM_QUOTED_CHAR: {
      struct diag_elem_quoted_char e;
      memcpy(&e, p, sizeof(e));
      p += sizeof(e);
      print_quoted_char_elem(out, e.c);
      break;
    }
    case DIAG_ELEM_TOKEN: {
      struct diag_elem_token e;
      memcpy(&e, p, sizeof(e));
      p += sizeof(e);
      print_token_elem(out, e.kind, (const union object *)e.data);
      break;
    }
    case DIAG_ELEM_END:
      fputc('\n', out);
      break;
    case DIAG_ELEM_INLINE_STR: {
      struct diag_elem_inline_str e;
      memcpy(&e, p, sizeof(e));
      p += sizeof(e);
      fwrite(p, 1, e.len, out);
      p += e.len;
      break;
    }
    }
  }

  fflush(out);
}

#include "parser.h"

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>

#include "adt/arena.h"
#include "opcodes.h"
#include "scanner.h"
#include "symbol_table.h"
#include "token_kinds.h"
#include "writer.h"

#include "objects.h"

#define MAX(a,b) \
  ({ typeof (a) _a = (a); \
     typeof (b) _b = (b); \
     _a > _b ? _a : _b; })

#define UNLIKELY(x)    __builtin_expect((x), 0)

#define EXPRESSION_START_CASES \
  T_IDENTIFIER:  \
  case T_STRING: \
  case T_RAW_STRING: \
  case T_UNICODE_STRING: \
  case T_FORMAT_STRING: \
  case T_BYTE_STRING

enum precedence {
  PREC_TEST,
  PREC_LOGICAL_OR,  /* OR               left to right */
  PREC_LOGICAL_AND, /* AND              left to right */
  PREC_LOGICAL_NOT, /* prefix NOT */
  PREC_COMPARISON,  /* <, >, ==, >=, <=, <>, !=, in, not in, is, is not */

  PREC_XOR,         /* ^                left to right */
  PREC_AND,         /* &                left to right */
  PREC_SHIFT,       /* <<, >>           left to right */
  PREC_ARITH,       /* +, -             left to right */
  PREC_TERM,        /* *, @, /, %, //   left to right */

  PREC_FACTOR,      /* prefix +, -, ~   */
  PREC_POWER,       /* prefix **        */

  PREC_POSTFIX,     /* postfix ( */
  PREC_PRIMARY,
};

static noreturn void unimplemented(void)
{
  fprintf(stderr, "unimplemented\n");
  abort();
}

static inline bool peek(const struct parser_state *s, uint16_t token_kind)
{
  return s->scanner.token.kind == token_kind;
}

static inline void next_token(struct parser_state *s)
{
  scanner_next_token(&s->scanner);
}

static inline void eat(struct parser_state *s, uint16_t token_kind)
{
  assert(s->scanner.token.kind == token_kind);
  (void)token_kind;
  next_token(s);
}

static inline bool accept(struct parser_state *s, uint16_t token_kind)
{
  if (peek(s, token_kind)) {
    next_token(s);
    return true;
  }
  return false;
}

static void add_anchor(struct parser_state *s, uint16_t token_kind)
{
  assert(token_kind < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]));
  ++s->anchor_set[token_kind];
}

static void remove_anchor(struct parser_state *s, uint16_t token_kind)
{
  assert(token_kind < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]));
  assert(s->anchor_set[token_kind] > 0);
  --s->anchor_set[token_kind];
}

static void eat_until_matching_token(struct parser_state *s,
                                     uint16_t token_kind)
{
  uint16_t end_token_kind;
  switch (token_kind) {
  case '(': end_token_kind = ')'; break;
  case '{': end_token_kind = '}'; break;
  case '[': end_token_kind = ']'; break;
  default: end_token_kind = token_kind; break;
  }

  unsigned parenthesis_count = 0;
  unsigned brace_count       = 0;
  unsigned bracket_count     = 0;
  while (!peek(s, end_token_kind)
         || parenthesis_count != 0
         || brace_count       != 0
         || bracket_count     != 0) {
    switch (s->scanner.token.kind) {
    case T_EOF: return;
    case '(': ++parenthesis_count; break;
    case '{': ++brace_count;       break;
    case '[': ++bracket_count;     break;

    case ')':
      if (parenthesis_count > 0)
        --parenthesis_count;
      goto check_stop;

    case '}':
      if (brace_count > 0)
        --brace_count;
      goto check_stop;

    case ']':
      if (bracket_count > 0)
        --bracket_count;
check_stop:
      if (peek(s, end_token_kind)
          && parenthesis_count == 0
          && brace_count       == 0
          && bracket_count     == 0)
        return;
      break;
    default:
      break;
    }
    next_token(s);
  }
}

static void eat_until_anchor(struct parser_state *s)
{
  while (s->anchor_set[s->scanner.token.kind] == 0) {
    if (peek(s, '(') || peek(s, '{') || peek(s, '['))
      eat_until_matching_token(s, s->scanner.token.kind);
    next_token(s);
  }
}

static bool skip_till(struct parser_state *s, uint16_t expected_token_kind)
{
  if (UNLIKELY(!peek(s, expected_token_kind))) {
    fprintf(stderr, "expected %s, got ", token_kind_name(expected_token_kind));
    print_token(stderr, &s->scanner.token);
    fputc('\n', stderr);
    s->error = true;

    add_anchor(s, expected_token_kind);
    eat_until_anchor(s);
    remove_anchor(s, expected_token_kind);
    if (!peek(s, expected_token_kind))
      return false;
  }
  return true;
}

static void expect(struct parser_state *s, uint16_t expected_token_kind)
{
  if (skip_till(s, expected_token_kind))
    eat(s, expected_token_kind);
}

static union object *parse_subexpression(struct parser_state *s,
                                         enum precedence precedence);

static union object *parse_argument(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case '*':
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  case T_ASTERISK_ASTERISK:
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  default: {
    union object *result = parse_subexpression(s, PREC_TEST);
    if (accept(s, '=')) {
      parse_subexpression(s, PREC_TEST);
      return result;
    } else {
      return result;
    }
  }
  }
}

static union object *parse_call(struct parser_state *s, union object *left)
{
  struct object_ast_call *call = arena_allocate_type(&s->writer.objects,
                                                     struct object_ast_call);
  memset(call, 0, sizeof(*call));
  call->base.type = TYPE_AST_CALL;
  call->callee = left;
  call->arguments = object_new_list(&s->writer.objects);

  eat(s, '(');
  add_anchor(s, ')');
  add_anchor(s, ',');

  if (!peek(s, ')')) {
    do {
      union object *argument = parse_argument(s);
      object_list_append(call->arguments, argument);
    } while(accept(s, ',') && !peek(s, ')'));
  }
  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');

  return (union object*)call;
}

#if 0
static inline void parse_binexpr(struct parser_state *s,
                                 uint16_t token_kind,
                                 enum precedence prec_right)
{
  eat(s, token_kind);
  parse_subexpression(s, prec_right);

  // TODO: Semantic
}
#endif

static union object *parse_identifier(struct parser_state *s)
{
  struct symbol *symbol = s->scanner.token.u.symbol;
  eat(s, T_IDENTIFIER);

  uint16_t index = symbol->name_index;
  if (index == 0) {
    index = writer_register_name(&s->writer, symbol->string) + 1;
    symbol->name_index = index;
  }

  struct object_ast_name *name
    = arena_allocate_type(&s->writer.objects, struct object_ast_name);
  name->base.type = TYPE_AST_NAME;
  name->index = index - 1;
  return (union object*)name;
}

static union object *parse_string(struct parser_state *s)
{
  const char *chars = s->scanner.token.u.string;
  uint32_t length = strlen(chars);
  unsigned index = writer_register_string(&s->writer, chars, length);
  eat(s, T_STRING);

  struct object_ast_const *ast_const
    = arena_allocate_type(&s->writer.objects, struct object_ast_const);
  ast_const->base.type = TYPE_AST_CONST;
  ast_const->index = index;
  return (union object*)ast_const;
}

static union object *parse_integer(struct parser_state *s)
{
  const char *string = s->scanner.token.u.string;
  char *endptr;
  errno = 0;
  unsigned long value = strtoul(string, &endptr, 0);
  assert(endptr != NULL);
  assert(*endptr == '\0');
  if (value == 0) {
    assert(errno == 0);
  }
  assert(value <= INT32_MAX);
  unsigned index = writer_register_int(&s->writer, (int32_t)value);
  eat(s, T_INTEGER);

  struct object_ast_const *ast_const
    = arena_allocate_type(&s->writer.objects, struct object_ast_const);
  ast_const->base.type = TYPE_AST_CONST;
  ast_const->index = index;
  return (union object*)ast_const;
}

static union object *parse_atom(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case T_IDENTIFIER: return parse_identifier(s);
  case T_STRING:     return parse_string(s);
  case T_INTEGER:    return parse_integer(s);
  default:
    unimplemented();
  }
}

typedef union object *(*prefix_parser_func)(struct parser_state *s);
typedef union object *(*infix_parser_func)(struct parser_state *s,
                                           union object *left);
struct expression_parser {
  prefix_parser_func prefix;
  infix_parser_func infix;
  enum precedence precedence;
};

static const struct expression_parser parsers[] = {
  ['('] = {
    .infix      = parse_call,
    .precedence = PREC_POSTFIX,
  },
};

union object *parse_subexpression(struct parser_state *s,
                                  enum precedence precedence)
{
  union object *result;
  uint16_t token_kind = s->scanner.token.kind;
  if (token_kind < sizeof(parsers) / sizeof(parsers[0]) &&
      parsers[token_kind].prefix != NULL) {
    prefix_parser_func prefix_parser = parsers[token_kind].prefix;
    result = prefix_parser(s);
  } else {
    result = parse_atom(s);
  }

  for (;;) {
    uint16_t infix_token_kind = s->scanner.token.kind;
    if (infix_token_kind >= sizeof(parsers) / sizeof(parsers[0]))
      break;
    infix_parser_func infix_parser = parsers[infix_token_kind].infix;
    if (infix_parser == NULL ||
        parsers[infix_token_kind].precedence < precedence)
      break;
    result = infix_parser(s, result);
  }
  return result;
}

static void emit_expression(struct parser_state *s, union object *expression)
{
  switch (expression->type) {
  case TYPE_AST_CONST:
    write_push_op(&s->writer, OPCODE_LOAD_CONST, expression->ast_const.index);
    break;
  case TYPE_AST_NAME:
    write_push_op(&s->writer, OPCODE_LOAD_NAME, expression->ast_name.index);
    break;
  case TYPE_AST_CALL: {
    struct object_ast_call *call = &expression->ast_call;
    emit_expression(s, call->callee);
    unsigned n_arguments = call->arguments->length;
    for (unsigned i = 0; i < n_arguments; ++i) {
      emit_expression(s, call->arguments->items[i]);
    }
    write_op(&s->writer, OPCODE_CALL_FUNCTION, n_arguments);
    writer_pop(&s->writer, n_arguments);
    write_pop_op(&s->writer, OPCODE_POP_TOP, 0);
    break;
  }
  default:
    fprintf(stderr, "unexpected expression");
    abort();
  }
}

static void parse_expression_statement(struct parser_state *s)
{
  assert(s->writer.stacksize == 0);
  do {
    /* TODO: star_expr, etc. */
    union object *expression = parse_subexpression(s, PREC_TEST);
    emit_expression(s, expression);
    assert(s->writer.stacksize == 0);
  } while(accept(s, ';') && !peek(s, T_NEWLINE) && !peek(s, T_EOF));

  if (peek(s, T_EOF))
    return;
  expect(s, T_NEWLINE);
}

void parse(struct parser_state *s)
{
  next_token(s);

  writer_begin(&s->writer, stdout);

  add_anchor(s, T_EOF);
  struct token *t = &s->scanner.token;
  for (;;) {
    switch (t->kind) {
    case T_NEWLINE:
      continue;
    case EXPRESSION_START_CASES:
      parse_expression_statement(s);
      continue;
    case T_EOF:
      break;
    default:
      fprintf(stderr, "Unexpected token:");
      print_token(stderr, t);
      fputc('\n', stderr);
      next_token(s);
      continue;
    }
    break;
  }

#ifndef NDEBUG
  remove_anchor(s, T_EOF);
  for (size_t i = 0; i < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]); ++i) {
    if (s->anchor_set[i] != 0) {
      fprintf(stderr, "Anchor for token %s not removed\n", token_kind_name(i));
      abort();
    }
  }
#endif

  writer_finish(&s->writer);
}

void parser_init(struct parser_state *s)
{
  memset(s, 0, sizeof(*s));
  memset(s->anchor_set, 0, sizeof(s->anchor_set));
}

void parser_free(struct parser_state *s)
{
  (void)s;
}

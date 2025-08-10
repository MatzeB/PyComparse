#include "parser.h"
#include "parser_types.h"

#include <assert.h>
#include <stdalign.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "ast.h"
#include "ast_types.h"
#include "codegen_expression.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "object.h"
#include "scanner.h"
#include "symbol_types.h"
#include "token_kinds.h"
#include "util.h"

#define UNLIKELY(x)    __builtin_expect((x), 0)

/* Keep this in sync with prefix_parsers below */
#define EXPRESSION_START_CASES \
  '(': \
  case '+': \
  case '-': \
  case '[': \
  case '{': \
  case '~': \
  case T_not: \
  case T_IDENTIFIER:  \
  case T_STRING: \
  case T_INTEGER: \
  case T_True: \
  case T_False: \
  case T_None: \
  case T_DOT_DOT_DOT

enum precedence {
  PREC_ASSIGN,      /* = */
  PREC_LIST,        /* , */
  PREC_TEST,        /* postfix 'if' _ 'else' _ */
  PREC_LOGICAL_OR,  /* OR */
  PREC_LOGICAL_AND, /* AND */
  PREC_LOGICAL_NOT, /* prefix NOT */
  PREC_COMPARISON,  /* <, >, ==, >=, <=, <>, !=, in, not in, is, is not */

  PREC_OR,          /* | */
  PREC_XOR,         /* ^ */
  PREC_AND,         /* & */
  PREC_SHIFT,       /* <<, >> */
  PREC_ARITH,       /* +, - */
  PREC_TERM,        /* *, @, /, %, // */

  PREC_FACTOR,      /* prefix +, -, ~ */
  PREC_POWER,       /* prefix ** */

  PREC_PRIMARY,     /* .attr  [subscript]   (call) */
  PREC_ATOM,        /* name, number, string, ..., None, True, False */
};

static void parse_error_expected(struct parser_state *s, const char *what)
{
  fprintf(stderr, "%s:%u error: expected %s, got ",
          s->cg.filename, s->scanner.line, what);
  print_token(stderr, &s->scanner.token);
  fputc('\n', stderr);
  s->error = true;
}

static inline uint16_t peek(const struct parser_state *s)
{
  return s->scanner.token.kind;
}

static inline void next_token(struct parser_state *s)
{
  scanner_next_token(&s->scanner);
}

static inline void eat(struct parser_state *s, uint16_t token_kind)
{
  assert(peek(s) == token_kind);
  (void)token_kind;
  next_token(s);
}

static inline struct symbol *eat_identifier(struct parser_state *s)
{
  assert(peek(s) == T_IDENTIFIER);
  struct symbol *symbol = s->scanner.token.u.symbol;
  next_token(s);
  return symbol;
}

static inline union object *peek_get_object(struct parser_state *s,
                                            uint16_t token_kind)
{
  assert(token_kind == T_STRING || token_kind == T_INTEGER);
  assert(peek(s) == token_kind);
  return s->scanner.token.u.object;
}

static inline bool accept(struct parser_state *s, uint16_t token_kind)
{
  if (peek(s) == token_kind) {
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
  while (peek(s) != end_token_kind
         || parenthesis_count != 0
         || brace_count       != 0
         || bracket_count     != 0) {
    switch (peek(s)) {
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
      if (peek(s) == end_token_kind
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
  while (s->anchor_set[peek(s)] == 0) {
    if (peek(s) == '(' || peek(s) == '{' || peek(s) == '[')
      eat_until_matching_token(s, peek(s));
    next_token(s);
  }
}

static bool skip_till(struct parser_state *s, uint16_t expected_token_kind)
{
  if (UNLIKELY(peek(s) != expected_token_kind)) {
    parse_error_expected(s, token_kind_name(expected_token_kind));

    add_anchor(s, expected_token_kind);
    eat_until_anchor(s);
    remove_anchor(s, expected_token_kind);
    if (peek(s) != expected_token_kind)
      return false;
  }
  return true;
}

static void expect(struct parser_state *s, uint16_t expected_token_kind)
{
  if (skip_till(s, expected_token_kind))
    eat(s, expected_token_kind);
}

static union ast_expression *ast_allocate_expression_(struct parser_state *s,
    size_t size, enum ast_expression_type type)
{
  union ast_expression *expression =
    (union ast_expression*)arena_allocate(&s->ast, size, alignof(union ast_expression));
  memset(expression, 0, size);
  expression->type = type;
  return expression;
}

#define ast_allocate_expression(s, type, type_id) \
    ast_allocate_expression_((s), sizeof(type), (type_id))

static union ast_expression *ast_new_const(struct parser_state *s,
                                           union object *object)
{
  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_const, AST_CONST);
  expression->cnst.object = object;
  return expression;
}

static union ast_expression *ast_new_invalid(struct parser_state *s)
{
  union object *object = object_intern_singleton(&s->cg.objects, OBJECT_NONE);
  return ast_new_const(s, object);
}

static union ast_expression *parse_expression(struct parser_state *s,
                                              enum precedence precedence);

static union ast_expression *parse_generator_expression(struct parser_state *s,
    union ast_expression *left)
{
  union ast_expression *generator_expression
      = ast_allocate_expression(s, struct ast_generator_expression,
                                AST_GENERATOR_EXPRESSION);
  generator_expression->generator_expression.expression = left;

  struct generator_expression_part *last = NULL;

  for (;;) {
    struct generator_expression_part *part;
    if (peek(s) == T_for) {
      eat(s, T_for);
      part = arena_allocate_type(&s->ast, struct generator_expression_part);
      part->type = GENERATOR_EXPRESSION_PART_FOR;
      part->target = parse_expression(s, PREC_OR);
      expect(s, T_in);
      part->expression = parse_expression(s, PREC_OR);
    } else if (peek(s) == T_if) {
      eat(s, T_if);
      part = arena_allocate_type(&s->ast, struct generator_expression_part);
      part->type = GENERATOR_EXPRESSION_PART_IF;
      part->target = NULL;
      part->expression = parse_expression(s, PREC_COMPARISON);
    } else {
      break;
    }
    if (last == NULL) {
      generator_expression->generator_expression.parts = part;
    } else {
      last->next = part;
    }
    last = part;
  }
  return generator_expression;
}

static struct argument *parse_argument(struct parser_state *s)
{
  struct argument *argument = arena_allocate_type(&s->ast, struct argument);
  switch (peek(s)) {
  case '*':
    parse_expression(s, PREC_TEST);
    unimplemented();
  case T_ASTERISK_ASTERISK:
    parse_expression(s, PREC_TEST);
    unimplemented();
  default: {
    union ast_expression *expression = parse_expression(s, PREC_TEST);
    if (accept(s, '=')) {
      expression = parse_expression(s, PREC_TEST);
      unimplemented();
    }

    if (peek(s) == T_for) {
      expression = parse_generator_expression(s, expression);
    }

    argument->expression = expression;
    return argument;
  }
  }
}

static union ast_expression *parse_attr(struct parser_state *s,
                                        union ast_expression *left)
{
  eat(s, '.');
  if (!skip_till(s, T_IDENTIFIER)) return ast_new_invalid(s);
  struct symbol *symbol = eat_identifier(s);

  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_attr, AST_ATTR);
  expression->attr.expression = left;
  expression->attr.attr = symbol;
  return expression;
}

static union ast_expression *parse_call(struct parser_state *s,
                                        union ast_expression *left)
{
  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_call, AST_CALL);
  expression->call.callee = left;

  eat(s, '(');
  add_anchor(s, ')');
  add_anchor(s, ',');

  struct argument *argument = NULL;
  if (peek(s) != ')') {
    do {
      struct argument *new_argument = parse_argument(s);
      if (argument == NULL) {
        expression->call.arguments = new_argument;
      } else {
        argument->next = new_argument;
      }
      argument = new_argument;
    } while(accept(s, ',') && peek(s) != ')');
  }
  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');
  return expression;
}

static inline union ast_expression *parse_unexpr(struct parser_state *s,
                                                 enum precedence prec_op,
                                                 enum ast_expression_type type)
{
  next_token(s);
  union ast_expression *op = parse_expression(s, prec_op);

  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_unexpr, type);
  expression->unexpr.op = op;
  return expression;
}

static union ast_expression *
parse_expression_list_helper(struct parser_state *s,
    enum ast_expression_type type, union ast_expression *first)
{
  union ast_expression *inline_storage[16];
  union ast_expression **expressions = inline_storage;
  expressions[0] = first;
  unsigned capacity = sizeof(inline_storage) / sizeof(inline_storage[0]);
  unsigned num_expressions = 1;

  for (;;) {
    if (!accept(s, ','))
      break;
    switch (peek(s)) {
    case EXPRESSION_START_CASES: {
      union ast_expression *expression = parse_expression(s, PREC_LIST + 1);
      if (num_expressions + 1 >= capacity) {
        unsigned new_capacity = capacity * 2;
        union ast_expression **new_expressions =
            malloc(new_capacity * sizeof(expressions[0]));
        if (new_expressions == NULL) abort();
        memcpy(new_expressions, expressions, num_expressions * sizeof(expressions[0]));
        if (expressions != inline_storage) free(expressions);
        expressions = new_expressions;
        capacity = new_capacity;
      }
      expressions[num_expressions++] = expression;
      continue;
    }
    default:
      break;
    }
    break;
  }

  size_t expressions_size = num_expressions * sizeof(expressions[0]);
  union ast_expression *expression = ast_allocate_expression_(s,
      sizeof(struct ast_expression_list) + expressions_size, type);
  expression->expression_list.num_expressions = num_expressions;
  memcpy(expression->expression_list.expressions, expressions,
         expressions_size);
  return expression;
}

static union ast_expression *parse_l_bracket(struct parser_state *s)
{
  eat(s, '[');

  if (accept(s, ']')) {
    union ast_expression *expression
        = ast_allocate_expression(s, struct ast_expression_list,
                                  AST_LIST_DISPLAY);
    expression->expression_list.num_expressions = 0;
    return expression;
  }

  add_anchor(s, ']');
  add_anchor(s, ',');

  union ast_expression *first = parse_expression(s, PREC_LIST + 1);
  union ast_expression *expression =
      parse_expression_list_helper(s, AST_LIST_DISPLAY, first);

  remove_anchor(s, ',');
  remove_anchor(s, ']');
  expect(s, ']');
  return expression;
}

static union ast_expression *parse_l_curly(struct parser_state *s)
{
  eat(s, '{');
  if (accept(s, '}')) {
    union ast_expression *expression =
        ast_allocate_expression(s, struct ast_dict_item_list, AST_DICT_DISPLAY);
    expression->dict_item_list.num_items = 0;
    return expression;
  }

  add_anchor(s, '}');
  add_anchor(s, ',');

  union ast_expression *first = parse_expression(s, PREC_LIST + 1);
  /* set display */
  if (peek(s) == ',' || peek(s) == '}') {
    union ast_expression *expression
        = parse_expression_list_helper(s, AST_SET_DISPLAY, first);

    remove_anchor(s, ',');
    remove_anchor(s, '}');
    expect(s, '}');
    return expression;
  }

  struct dict_item  inline_storage[16];
  struct dict_item *items = inline_storage;
  unsigned capacity = sizeof(inline_storage) / sizeof(inline_storage[0]);
  unsigned num_items = 0;

  union ast_expression *key = first;
  for (;;) {
    expect(s, ':');
    union ast_expression *value = parse_expression(s, PREC_LIST + 1);
    if (num_items + 1 >= capacity) {
      unsigned new_capacity = capacity * 2;
      struct dict_item *new_items = malloc(new_capacity * sizeof(items[0]));
      if (new_items == NULL) abort();
      memcpy(new_items, items, num_items * sizeof(items[0]));
      if (items != inline_storage) free(items);
      items = new_items;
      capacity = new_capacity;
    }
    struct dict_item *item = &items[num_items++];
    item->key = key;
    item->value = value;

    if (peek(s) == '}' || peek(s) == T_EOF)
      break;
    key = parse_expression(s, PREC_LIST + 1);
  }

  remove_anchor(s, ',');
  remove_anchor(s, '}');
  eat(s, '}');

  size_t items_size = num_items * sizeof(items[0]);
  union ast_expression *expression = ast_allocate_expression_(s,
      sizeof(struct ast_dict_item_list) + items_size, AST_DICT_DISPLAY);
  expression->dict_item_list.num_items = num_items;
  memcpy(expression->dict_item_list.items, items, items_size);
  return expression;
}

static union ast_expression *parse_l_paren(struct parser_state *s)
{
  eat(s, '(');
  if (accept(s, ')')) {
    union ast_expression *expression =
        ast_allocate_expression(s, struct ast_expression_list, AST_EXPRESSION_LIST);
    expression->expression_list.num_expressions = 0;
    expression->expression_list.as_constant =
        ast_tuple_compute_constant(&s->cg.objects, &expression->expression_list);
    return expression;
  }

  add_anchor(s, ')');
  add_anchor(s, ',');
  union ast_expression *expression = parse_expression(s, PREC_LIST);

  if (peek(s) == T_for) {
    expression = parse_generator_expression(s, expression);
  }

  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');
  return expression;
}

static union ast_expression *parse_plus(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_PLUS);
}

static union ast_expression *parse_negative(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_NEGATIVE);
}

static union ast_expression *parse_invert(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_INVERT);
}

static union ast_expression *parse_not(struct parser_state *s)
{
  return parse_unexpr(s, PREC_LOGICAL_NOT, AST_UNEXPR_NOT);
}

static union ast_expression *parse_identifier(struct parser_state *s)
{
  struct symbol *symbol = eat_identifier(s);

  union ast_expression *node =
    ast_allocate_expression(s, struct ast_identifier, AST_IDENTIFIER);
  node->identifier.symbol = symbol;
  return node;
}

static union ast_expression *parse_string(struct parser_state *s)
{
  union object *object = peek_get_object(s, T_STRING);
  eat(s, T_STRING);
  return ast_new_const(s, object);
}

static union ast_expression *parse_integer(struct parser_state *s)
{
  union object *object = peek_get_object(s, T_INTEGER);
  eat(s, T_INTEGER);
  return ast_new_const(s, object);
}

static union ast_expression *parse_singleton(struct parser_state *s,
                                             enum object_type type)
{
  next_token(s);
  union object *object = object_intern_singleton(&s->cg.objects, type);
  return ast_new_const(s, object);
}

static union ast_expression *parse_true(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_TRUE);
}

static union ast_expression *parse_false(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_FALSE);
}

static union ast_expression *parse_none(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_NONE);
}

static union ast_expression *parse_ellipsis(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_ELLIPSIS);
}

typedef union ast_expression *(*prefix_parser_func)(struct parser_state *s);
struct prefix_expression_parser {
  prefix_parser_func func;
  enum precedence precedence;
};

static const struct prefix_expression_parser prefix_parsers[] = {
  ['(']           = { .func = parse_l_paren,    .precedence = PREC_ATOM },
  ['+']           = { .func = parse_plus,       .precedence = PREC_FACTOR },
  ['-']           = { .func = parse_negative,   .precedence = PREC_FACTOR },
  ['[']           = { .func = parse_l_bracket,  .precedence = PREC_ATOM },
  ['{']           = { .func = parse_l_curly,    .precedence = PREC_ATOM },
  ['~']           = { .func = parse_invert,     .precedence = PREC_FACTOR },
  [T_not]         = { .func = parse_not,        .precedence = PREC_LOGICAL_NOT},
  [T_IDENTIFIER]  = { .func = parse_identifier, .precedence = PREC_ATOM },
  [T_STRING]      = { .func = parse_string,     .precedence = PREC_ATOM },
  [T_INTEGER]     = { .func = parse_integer,    .precedence = PREC_ATOM },
  [T_True]        = { .func = parse_true,       .precedence = PREC_ATOM },
  [T_False]       = { .func = parse_false,      .precedence = PREC_ATOM },
  [T_None]        = { .func = parse_none,       .precedence = PREC_ATOM },
  [T_DOT_DOT_DOT] = { .func = parse_ellipsis,   .precedence = PREC_ATOM },
};

static inline union ast_expression *parse_binexpr(struct parser_state *s,
                                                  enum precedence prec_right,
                                                  enum ast_expression_type type,
                                                  union ast_expression *left)
{
  next_token(s);
  union ast_expression *right = parse_expression(s, prec_right);

  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_binexpr, type);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static union ast_expression *parse_matmul(struct parser_state *s,
                                          union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MATMUL, left);
}

static union ast_expression *parse_add(struct parser_state *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_ARITH + 1, AST_BINEXPR_ADD, left);
}

static union ast_expression *parse_and(struct parser_state *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_AND + 1, AST_BINEXPR_AND, left);
}

static union ast_expression *parse_assignment(struct parser_state *s,
                                              union ast_expression *left)
{
  return parse_binexpr(s, PREC_ASSIGN, AST_BINEXPR_ASSIGN, left);
}

static union ast_expression *parse_mod(struct parser_state *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MOD, left);
}

static union ast_expression *parse_mul(struct parser_state *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MUL, left);
}

static union ast_expression *parse_sub(struct parser_state *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_ARITH + 1, AST_BINEXPR_SUB, left);
}

static union ast_expression *parse_subscript(struct parser_state *s,
                                             union ast_expression *left)
{
  eat(s, '[');
  add_anchor(s, ']');
  add_anchor(s, ',');
  union ast_expression *right = parse_expression(s, PREC_LIST);
  remove_anchor(s, ',');
  remove_anchor(s, ']');
  expect(s, ']');

  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_binexpr, AST_BINEXPR_SUBSCRIPT);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static union ast_expression *parse_expr_list(struct parser_state *s,
                                             union ast_expression *left)
{
  union ast_expression *expression =
      parse_expression_list_helper(s, AST_EXPRESSION_LIST, left);
  expression->expression_list.as_constant =
      ast_tuple_compute_constant(&s->cg.objects, &expression->expression_list);
  return expression;
}

static union ast_expression *parse_floor_div(struct parser_state *s,
                                             union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_FLOORDIV, left);
}

static union ast_expression *parse_true_div(struct parser_state *s,
                                            union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_TRUEDIV, left);
}

static union ast_expression *parse_greater(struct parser_state *s,
                                           union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_GREATER, left);
}

static union ast_expression *parse_less(struct parser_state *s,
                                        union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_LESS, left);
}

static union ast_expression *parse_equal(struct parser_state *s,
                                         union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_EQUAL, left);
}

static union ast_expression *parse_greater_equal(struct parser_state *s,
                                                 union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_GREATER_EQUAL, left);
}

static union ast_expression *parse_less_equal(struct parser_state *s,
                                              union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_LESS_EQUAL, left);
}

static union ast_expression *parse_unequal(struct parser_state *s,
                                           union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_UNEQUAL, left);
}

static union ast_expression *parse_in(struct parser_state *s,
                                      union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_IN, left);
}

static union ast_expression *parse_or(struct parser_state *s,
                                      union ast_expression *left)
{
  return parse_binexpr(s, PREC_OR + 1, AST_BINEXPR_OR, left);
}

static union ast_expression *parse_xor(struct parser_state *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_XOR + 1, AST_BINEXPR_XOR, left);
}

static union ast_expression *parse_not_in(struct parser_state *s,
                                          union ast_expression *left)
{
  eat(s, T_not);
  if (!accept(s, T_in)) {
    parse_error_expected(s, "in");
    return ast_new_invalid(s);
  }

  union ast_expression *right = parse_expression(s, PREC_COMPARISON + 1);

  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_binexpr, AST_BINEXPR_NOT_IN);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static union ast_expression *parse_is(struct parser_state *s,
                                      union ast_expression *left)
{
  eat(s, T_is);
  uint8_t ast_node_type = accept(s, T_not) ? AST_BINEXPR_IS_NOT
                                           : AST_BINEXPR_IS;

  union ast_expression *right = parse_expression(s, PREC_COMPARISON + 1);

  union ast_expression *expression =
      ast_allocate_expression(s, struct ast_binexpr, ast_node_type);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

typedef union ast_expression *(*postfix_parser_func)
    (struct parser_state *s, union ast_expression * prefix);

struct postfix_expression_parser {
  postfix_parser_func func;
  enum precedence     precedence;
};

static const struct postfix_expression_parser postfix_parsers[] = {
  ['%']    = { .func = parse_mod,        .precedence = PREC_TERM       },
  ['&']    = { .func = parse_and,        .precedence = PREC_AND        },
  ['(']    = { .func = parse_call,       .precedence = PREC_PRIMARY    },
  ['*']    = { .func = parse_mul,        .precedence = PREC_TERM       },
  ['+']    = { .func = parse_add,        .precedence = PREC_ARITH      },
  [',']    = { .func = parse_expr_list,  .precedence = PREC_LIST       },
  ['-']    = { .func = parse_sub,        .precedence = PREC_ARITH      },
  ['.']    = { .func = parse_attr,       .precedence = PREC_PRIMARY    },
  ['/']    = { .func = parse_true_div,   .precedence = PREC_TERM       },
  ['<']    = { .func = parse_less,       .precedence = PREC_COMPARISON },
  ['=']    = { .func = parse_assignment, .precedence = PREC_ASSIGN     },
  ['>']    = { .func = parse_greater,    .precedence = PREC_COMPARISON },
  ['@']    = { .func = parse_matmul,     .precedence = PREC_TERM       },
  ['[']    = { .func = parse_subscript,  .precedence = PREC_PRIMARY    },
  ['^']    = { .func = parse_xor,        .precedence = PREC_XOR        },
  ['|']    = { .func = parse_or,         .precedence = PREC_OR         },
  [T_not]  = { .func = parse_not_in,     .precedence = PREC_COMPARISON },
  [T_in]   = { .func = parse_in,         .precedence = PREC_COMPARISON },
  [T_is]   = { .func = parse_is,         .precedence = PREC_COMPARISON },
  [T_SLASH_SLASH]
      = { .func = parse_floor_div,     .precedence = PREC_TERM       },
  [T_EQUALS_EQUALS]
      = { .func = parse_equal,         .precedence = PREC_COMPARISON },
  [T_GREATER_THAN_EQUALS]
      = { .func = parse_greater_equal, .precedence = PREC_COMPARISON },
  [T_LESS_THAN_EQUALS]
      = { .func = parse_less_equal,    .precedence = PREC_COMPARISON },
  [T_EXCLAMATIONMARKEQUALS]
      = { .func = parse_unequal,       .precedence = PREC_COMPARISON },
};

union ast_expression *parse_expression(struct parser_state *s,
                                       enum precedence precedence)
{
  uint16_t token_kind = peek(s);
  if (token_kind >= sizeof(prefix_parsers) / sizeof(prefix_parsers[0])) {
    parse_error_expected(s, "expression");
    unimplemented();
  }
  const struct prefix_expression_parser *prefix_parser =
      &prefix_parsers[token_kind];
  if (prefix_parser->func == NULL || prefix_parser->precedence < precedence) {
    parse_error_expected(s, "expression");
    unimplemented();
  }
  union ast_expression *result = prefix_parser->func(s);

  for (;;) {
    uint16_t postifx_token_kind = peek(s);
    if (postifx_token_kind >=
        sizeof(postfix_parsers) / sizeof(postfix_parsers[0]))
      break;
    const struct postfix_expression_parser *postfix_parser =
      &postfix_parsers[postifx_token_kind];
    if (postfix_parser->func == NULL ||
        postfix_parser->precedence < precedence) {
      break;
    }
    result = postfix_parser->func(s, result);
  }
  return result;
}

static void parse_expression_statement(struct parser_state *s)
{
  union ast_expression *expression = parse_expression(s, PREC_ASSIGN);
  emit_expression_statement(&s->cg, expression);
}

static struct dotted_name *parse_dotted_name(struct parser_state *s)
{
  arena_grow_begin(&s->ast, alignof(struct dotted_name));
  arena_grow(&s->ast, sizeof(struct dotted_name));
  unsigned num_symbols = 0;
  do {
    if (!skip_till(s, T_IDENTIFIER)) {
      void *begin = arena_grow_finish(&s->ast);
      arena_free_to(&s->ast, begin);
      return NULL;
    }
    struct symbol *symbol = eat_identifier(s);

    struct symbol **ptr = (struct symbol**)arena_grow(&s->ast, sizeof(*ptr));
    *ptr = symbol;
    ++num_symbols;
  } while(accept(s, T_DOT));

  struct dotted_name *result = arena_grow_finish(&s->ast);
  result->num_symbols = num_symbols;
  return result;
}

static void parse_import_statement(struct parser_state *s)
{
  eat(s, T_import);

  do {
    struct dotted_name *dotted_name = parse_dotted_name(s);
    if (accept(s, T_as)) {
      unimplemented();
    }
    emit_import_statement(&s->cg, dotted_name, NULL);
  } while(accept(s, ','));
}

static void parse_return_statement(struct parser_state *s)
{
  eat(s, T_return);

  union ast_expression *expression;
  if (peek(s) != T_NEWLINE) {
    expression = parse_expression(s, PREC_LIST);
  } else {
    expression = NULL;
  }
  emit_return_statement(&s->cg, expression);
}

static void parse_small_statement(struct parser_state *s)
{
  switch (peek(s)) {
  case EXPRESSION_START_CASES:
    parse_expression_statement(s);
    break;
  case T_import:
    parse_import_statement(s);
    break;
  case T_pass:
    eat(s, T_pass);
    break;
  case T_return:
    parse_return_statement(s);
    break;
  /* TODO: del, break, continue, return, raise, yield,
   * import, global, nonlocal, assert */
  default:
    parse_error_expected(s, "statement");
    unimplemented(); /* recovery */
  }
}

static void parse_simple_statement(struct parser_state *s)
{
  do {
    parse_small_statement(s);
  } while (accept(s, ';') && peek(s) != T_NEWLINE);
  expect(s, T_NEWLINE);
}

static void parse_statement(struct parser_state *s);

static void parse_suite(struct parser_state *s)
{
#ifndef NDEBUG
  unsigned prev_stacksize = s->cg.code.stacksize;
#endif
  if (accept(s, T_NEWLINE)) {
    expect(s, T_INDENT);
    do {
      parse_statement(s);
      assert(s->cg.code.stacksize == prev_stacksize);
    } while(!accept(s, T_DEDENT));
  } else {
    parse_simple_statement(s);
    assert(s->cg.code.stacksize == prev_stacksize);
  }
}

static void parse_if(struct parser_state *s)
{
  eat(s, T_if);
  union ast_expression *expression = parse_expression(s, PREC_TEST);
  expect(s, ':');

  struct if_state state;
  emit_if_begin(&s->cg, &state, expression);

  parse_suite(s);

  if (accept(s, T_else)) {
    expect(s, ':');

    emit_else_begin(&s->cg, &state);
    parse_suite(s);
  }
  emit_if_end(&s->cg, &state);
}

static void parse_for(struct parser_state *s)
{
  eat(s, T_for);
  union ast_expression *target = parse_expression(s, PREC_OR);
  expect(s, T_in);
  union ast_expression *expression = parse_expression(s, PREC_TEST);
  expect(s, ':');

  struct for_state state;
  emit_for_begin(&s->cg, &state, target, expression);

  parse_suite(s);

  emit_for_end(&s->cg, &state);
}

static void parse_while(struct parser_state *s)
{
  eat(s, T_while);
  union ast_expression *expression = parse_expression(s, PREC_TEST);
  expect(s, ':');

  struct while_state state;
  emit_while_begin(&s->cg, &state, expression);

  parse_suite(s);

  emit_while_end(&s->cg, &state);
}

static void parse_parameters(struct parser_state *s)
{
  expect(s, '(');

  unsigned num_parameters = 0;
  while (peek(s) == T_IDENTIFIER) {
    struct symbol *symbol = eat_identifier(s);

    if (!emit_parameter(&s->cg, symbol)) {
      fprintf(stderr, "error: duplicate parameter '%s'\n", symbol->string);
      s->error = true;
    } else {
      num_parameters++;
    }

    if (!accept(s, ',')) {
      break;
    }
  }

  expect(s, ')');
  s->cg.code.argcount = num_parameters;
}

static void parse_class(struct parser_state *s, unsigned num_decorators)
{
  eat(s, T_class);
  if (!skip_till(s, T_IDENTIFIER)) return;
  struct symbol *name = eat_identifier(s);

  emit_class_begin(&s->cg, name);

  /* TODO: parse parameters to class */
  expect(s, ':');
  parse_suite(s);

  emit_class_end(&s->cg, name, num_decorators);
}

static void parse_def(struct parser_state *s, unsigned num_decorators)
{
  eat(s, T_def);
  if (!skip_till(s, T_IDENTIFIER)) return;
  struct symbol *name = eat_identifier(s);

  emit_def_begin(&s->cg);

  parse_parameters(s);
  if (accept(s, T_MINUS_GREATER_THAN)) {
    parse_expression(s, PREC_TEST);
    unimplemented();
  }
  expect(s, ':');

  parse_suite(s);

  emit_def_end(&s->cg, name, num_decorators);
}

static void parse_decorator(struct parser_state *s,
                            unsigned num_decorators)
{
  eat(s, '@');
  union ast_expression *expression = parse_expression(s, PREC_ASSIGN);
  expect(s, T_NEWLINE);
  emit_expression(&s->cg, expression);

  switch (peek(s)) {
  case '@':
    parse_decorator(s, num_decorators + 1);
    return;
  case T_class:
    parse_class(s, num_decorators + 1);
    return;
  case T_def:
    parse_def(s, num_decorators + 1);
    return;
  default:
    parse_error_expected(s, "@, class or def after decorator");
    return;
  }
}

static void parse_statement(struct parser_state *s)
{
  switch (peek(s)) {
  case '@':
    parse_decorator(s, /*num_decorators=*/0);
    return;
  case T_class:
    parse_class(s, /*num_decorators=*/0);
    return;
  case T_def:
    parse_def(s, /*num_decorators=*/0);
    return;
  case T_for:
    parse_for(s);
    return;
  case T_if:
    parse_if(s);
    return;
  case T_while:
    parse_while(s);
    return;
  case T_EOF:
    return;
  default:
    parse_simple_statement(s);
    return;
  }
}

union object *parse(struct parser_state *s, const char *filename)
{
  next_token(s);

  cg_init(&s->cg, s->scanner.symbol_table, filename);
  emit_module_begin(&s->cg);

  add_anchor(s, T_EOF);
  while (peek(s) != T_EOF) {
    if (accept(s, T_NEWLINE))
      continue;
    parse_statement(s);
    assert(s->cg.code.stacksize == 0);
  }

#ifndef NDEBUG
  remove_anchor(s, T_EOF);
  for (uint16_t i = 0; i < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]); ++i) {
    if (s->anchor_set[i] != 0) {
      fprintf(stderr, "Anchor for token %s not removed\n", token_kind_name(i));
      abort();
    }
  }
#endif

  return emit_module_end(&s->cg);
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

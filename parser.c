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
  PREC_ASSIGN,      /* = */
  PREC_LIST,        /* , */
  PREC_TEST,        /* postfix 'if' _ 'else' _ */
  PREC_LOGICAL_OR,  /* OR */
  PREC_LOGICAL_AND, /* AND */
  PREC_LOGICAL_NOT, /* prefix NOT */
  PREC_COMPARISON,  /* <, >, ==, >=, <=, <>, !=, in, not in, is, is not */

  PREC_XOR,         /* ^ */
  PREC_AND,         /* & */
  PREC_SHIFT,       /* <<, >> */
  PREC_ARITH,       /* +, - */
  PREC_TERM,        /* *, @, /, %, // */

  PREC_FACTOR,      /* prefix +, -, ~ */
  PREC_POWER,       /* prefix ** */

  PREC_POSTFIX,     /* postfix ( */
  PREC_PRIMARY,
};

enum ast_node_type {
  AST_CALL,
  AST_BINEXPR_ADD,
  AST_BINEXPR_ASSIGN,
  AST_BINEXPR_FLOORDIV,
  AST_BINEXPR_MATMUL,
  AST_BINEXPR_MUL,
  AST_BINEXPR_SUB,
  AST_BINEXPR_TRUEDIV,
  AST_BINEXPR_GREATER,
  AST_BINEXPR_LESS,
  AST_BINEXPR_EQUAL,
  AST_BINEXPR_GREATER_EQUAL,
  AST_BINEXPR_LESS_EQUAL,
  AST_BINEXPR_UNEQUAL,
  AST_BINEXPR_IN,
  AST_BINEXPR_NOT_IN,
  AST_BINEXPR_IS,
  AST_BINEXPR_IS_NOT,
  AST_CONST,
  AST_NAME,
  AST_UNEXPR_INVERT,
  AST_UNEXPR_NEGATIVE,
  AST_UNEXPR_NOT,
  AST_UNEXPR_PLUS,
};

struct ast_node_base {
  uint8_t type;
};
union ast_node;

struct ast_name {
  struct object_base base;
  uint16_t           index;
};

struct ast_const {
  struct object_base base;
  uint16_t           index;
};

struct argument {
  union ast_node  *expression;
  struct argument *next;
};

struct ast_call {
  struct object_base base;
  union ast_node    *callee;
  struct argument   *arguments;
};

struct ast_binexpr {
  struct object_base base;
  union ast_node    *left;
  union ast_node    *right;
};

struct ast_unexpr {
  struct object_base base;
  union ast_node    *op;
};

union ast_node {
  uint8_t              type;
  struct ast_node_base base;

  struct ast_binexpr binexpr;
  struct ast_call    call;
  struct ast_const   cnst;
  struct ast_name    name;
  struct ast_unexpr  unexpr;
};

static noreturn void unimplemented(void)
{
  fprintf(stderr, "unimplemented\n");
  abort();
}

static void parse_error_expected(struct parser_state *s, const char *what)
{
  fprintf(stderr, "error: expected %s, got ", what);
  print_token(stderr, &s->scanner.token);
  fputc('\n', stderr);
  s->error = true;
}

static inline bool peek(const struct parser_state *s, uint16_t token_kind)
{
  return s->scanner.token.kind == token_kind;
}

static inline void next_token(struct parser_state *s)
{
  scanner_next_token(&s->scanner);
#if 0
  print_token(stderr, &s->scanner.token);
  fputc('\n', stderr);
#endif
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
    parse_error_expected(s, token_kind_name(expected_token_kind));

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

static union ast_node *parse_subexpression(struct parser_state *s,
                                           enum precedence precedence);

static struct argument *parse_argument(struct parser_state *s)
{
  struct argument *argument = arena_allocate_type(&s->ast, struct argument);
  switch (s->scanner.token.kind) {
  case '*':
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  case T_ASTERISK_ASTERISK:
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  default: {
    union ast_node *expression = parse_subexpression(s, PREC_TEST);
    if (accept(s, '=')) {
      expression = parse_subexpression(s, PREC_TEST);
      unimplemented();
    }
    argument->expression = expression;
    return argument;
  }
  }
}

static union ast_node *parse_call(struct parser_state *s, union ast_node *left)
{
  struct ast_call *call = arena_allocate_type(&s->ast, struct ast_call);
  memset(call, 0, sizeof(*call));
  call->base.type = AST_CALL;
  call->callee = left;

  eat(s, '(');
  add_anchor(s, ')');
  add_anchor(s, ',');

  struct argument *argument = NULL;
  if (!peek(s, ')')) {
    do {
      struct argument *new_argument = parse_argument(s);
      if (argument == NULL) {
        call->arguments = new_argument;
      } else {
        argument->next = new_argument;
      }
      argument = new_argument;
    } while(accept(s, ',') && !peek(s, ')'));
  }
  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');

  return (union ast_node*)call;
}

static inline union ast_node *parse_unexpr(struct parser_state *s,
                                           enum precedence prec_op,
                                           enum ast_node_type type)
{
  next_token(s);
  union ast_node *op = parse_subexpression(s, prec_op);

  struct ast_unexpr *result = arena_allocate_type(&s->ast, struct ast_unexpr);
  result->base.type = type;
  result->op = op;
  return (union ast_node*)result;
}

static union ast_node *parse_plus(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_PLUS);
}

static union ast_node *parse_negative(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_NEGATIVE);
}

static union ast_node *parse_invert(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_INVERT);
}

static union ast_node *parse_not(struct parser_state *s)
{
  return parse_unexpr(s, PREC_LOGICAL_NOT, AST_UNEXPR_NOT);
}

static inline union ast_node *parse_binexpr(struct parser_state *s,
                                            enum precedence prec_right,
                                            char ast_object_type,
                                            union ast_node *left)
{
  next_token(s);
  union ast_node *right = parse_subexpression(s, prec_right);

  struct ast_binexpr *result = arena_allocate_type(&s->ast, struct ast_binexpr);
  result->base.type = ast_object_type;
  result->left = left;
  result->right = right;
  return (union ast_node*)result;
}

static union ast_node *parse_matmul(struct parser_state *s, union ast_node *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MATMUL, left);
}

static union ast_node *parse_add(struct parser_state *s, union ast_node *left)
{
  return parse_binexpr(s, PREC_ARITH + 1, AST_BINEXPR_ADD, left);
}

static union ast_node *parse_assignment(struct parser_state *s,
                                        union ast_node *left)
{
  return parse_binexpr(s, PREC_ASSIGN, AST_BINEXPR_ASSIGN, left);
}

static union ast_node *parse_mul(struct parser_state *s, union ast_node *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MUL, left);
}

static union ast_node *parse_sub(struct parser_state *s, union ast_node *left)
{
  return parse_binexpr(s, PREC_ARITH + 1, AST_BINEXPR_SUB, left);
}

static union ast_node *parse_floor_div(struct parser_state *s,
                                       union ast_node *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_FLOORDIV, left);
}

static union ast_node *parse_true_div(struct parser_state *s,
                                      union ast_node *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_TRUEDIV, left);
}

static union ast_node *parse_greater(struct parser_state *s,
                                     union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_GREATER, left);
}

static union ast_node *parse_less(struct parser_state *s, union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_LESS, left);
}

static union ast_node *parse_equal(struct parser_state *s, union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_EQUAL, left);
}

static union ast_node *parse_greater_equal(struct parser_state *s,
                                           union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_GREATER_EQUAL, left);
}

static union ast_node *parse_less_equal(struct parser_state *s,
                                        union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_LESS_EQUAL, left);
}

static union ast_node *parse_unequal(struct parser_state *s,
                                     union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_UNEQUAL, left);
}

static union ast_node *parse_in(struct parser_state *s,
                                union ast_node *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_IN, left);
}

static union ast_node *parse_not_in(struct parser_state *s,
                                    union ast_node *left)
{
  eat(s, T_NOT);
  if (!accept(s, T_IN)) {
    parse_error_expected(s, "in");
  }

  union ast_node *right = parse_subexpression(s, PREC_COMPARISON + 1);

  struct ast_binexpr *result = arena_allocate_type(&s->ast, struct ast_binexpr);
  result->base.type = AST_BINEXPR_NOT_IN;
  result->left = left;
  result->right = right;
  return (union ast_node*)result;
}

static union ast_node *parse_is(struct parser_state *s, union ast_node *left)
{
  eat(s, T_IS);
  enum ast_node_type type = accept(s, T_NOT) ? AST_BINEXPR_IS_NOT
                                             : AST_BINEXPR_IS;

  union ast_node *right = parse_subexpression(s, PREC_COMPARISON + 1);

  struct ast_binexpr *result = arena_allocate_type(&s->ast, struct ast_binexpr);
  result->base.type = type;
  result->left = left;
  result->right = right;
  return (union ast_node*)result;
}

typedef union ast_node *(*prefix_parser_func)(struct parser_state *s);
typedef union ast_node *(*infix_parser_func)(struct parser_state *s,
                                             union ast_node *left);
struct expression_parser {
  prefix_parser_func prefix;
  infix_parser_func infix;
  enum precedence precedence;
};

static const struct expression_parser parsers[] = {
  ['(']    = { .infix = parse_call,       .precedence = PREC_POSTFIX    },
  ['+']    = { .prefix = parse_plus,
               .infix = parse_add,        .precedence = PREC_ARITH      },
  ['*']    = { .infix = parse_mul,        .precedence = PREC_TERM       },
  ['@']    = { .infix = parse_matmul,     .precedence = PREC_TERM       },
  ['-']    = { .prefix = parse_negative,
               .infix = parse_sub,        .precedence = PREC_ARITH      },
  ['/']    = { .infix = parse_true_div,   .precedence = PREC_TERM       },
  [T_NOT]  = { .prefix = parse_not,
               .infix = parse_not_in,     .precedence = PREC_COMPARISON },
  ['~']    = { .prefix = parse_invert, },
  ['=']    = { .infix = parse_assignment, .precedence = PREC_ASSIGN     },
  ['<']    = { .infix = parse_less,       .precedence = PREC_COMPARISON },
  ['>']    = { .infix = parse_greater,    .precedence = PREC_COMPARISON },
  [T_IN]   = { .infix = parse_in,         .precedence = PREC_COMPARISON },
  [T_IS]   = { .infix = parse_is,         .precedence = PREC_COMPARISON },
  [T_SLASH_SLASH]
      = { .infix = parse_floor_div,     .precedence = PREC_TERM       },
  [T_EQUALS_EQUALS]
      = { .infix = parse_equal,         .precedence = PREC_COMPARISON },
  [T_GREATER_THAN_EQUALS]
      = { .infix = parse_greater_equal, .precedence = PREC_COMPARISON },
  [T_LESS_THAN_EQUALS]
      = { .infix = parse_less_equal,    .precedence = PREC_COMPARISON },
  [T_EXCLAMATIONMARKEQUALS]
      = { .infix = parse_unequal,       .precedence = PREC_COMPARISON },
};

static uint16_t name_index_from_symbol(struct parser_state *s,
                                       struct symbol *symbol)
{
  if (s->cg.code.module_level) {
    uint16_t index = symbol->name_index;
    if (index > 0)
      return index - 1;

    index = cg_append_name(&s->cg, symbol->string);
    symbol->name_index = index + 1;
    return index;
  } else {
    return cg_register_name(&s->cg, symbol->string);
  }
}

static union ast_node *parse_identifier(struct parser_state *s)
{
  struct symbol *symbol = s->scanner.token.u.symbol;
  eat(s, T_IDENTIFIER);

  struct ast_name *name = arena_allocate_type(&s->ast, struct ast_name);
  name->base.type = AST_NAME;
  name->index = name_index_from_symbol(s, symbol);
  return (union ast_node*)name;
}

static union ast_node *parse_string(struct parser_state *s)
{
  const char *chars = s->scanner.token.u.string;
  uint32_t length = strlen(chars);
  unsigned index = cg_register_string(&s->cg, chars, length);
  eat(s, T_STRING);

  struct ast_const *cnst = arena_allocate_type(&s->ast, struct ast_const);
  cnst->base.type = AST_CONST;
  cnst->index = index;
  return (union ast_node*)cnst;
}

static union ast_node *parse_integer(struct parser_state *s)
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
  unsigned index = cg_register_int(&s->cg, (int32_t)value);
  eat(s, T_INTEGER);

  struct ast_const *ast_const = arena_allocate_type(&s->ast, struct ast_const);
  ast_const->base.type = AST_CONST;
  ast_const->index = index;
  return (union ast_node*)ast_const;
}

static union ast_node *parse_singleton(struct parser_state *s, char type)
{
  next_token(s);
  unsigned index = cg_register_singleton(&s->cg, type);

  struct ast_const *ast_const = arena_allocate_type(&s->ast, struct ast_const);
  ast_const->base.type = AST_CONST;
  ast_const->index = index;
  return (union ast_node*)ast_const;
}

static union ast_node *parse_atom(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case T_IDENTIFIER:  return parse_identifier(s);
  case T_STRING:      return parse_string(s);
  case T_INTEGER:     return parse_integer(s);
  case T_TRUE:        return parse_singleton(s, TYPE_TRUE);
  case T_FALSE:       return parse_singleton(s, TYPE_FALSE);
  case T_NONE:        return parse_singleton(s, TYPE_NONE);
  case T_DOT_DOT_DOT: return parse_singleton(s, TYPE_ELLIPSIS);
  default:
    unimplemented();
  }
}

union ast_node *parse_subexpression(struct parser_state *s,
                                    enum precedence precedence)
{
  union ast_node *result;
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

static void emit_expression(struct parser_state *s, union ast_node *expression,
                            bool drop);

static void emit_binexpr(struct parser_state *s, struct ast_binexpr *binexpr,
                         uint8_t opcode)
{
  emit_expression(s, binexpr->left, false);
  emit_expression(s, binexpr->right, false);
  cg_pop_op(&s->cg, opcode, 0);
}

static void emit_comparison(struct parser_state *s, struct ast_binexpr *binexpr,
                            enum compare_op_arg arg)
{
  emit_expression(s, binexpr->left, false);
  emit_expression(s, binexpr->right, false);
  cg_pop_op(&s->cg, OPCODE_COMPARE_OP, arg);
}

static void emit_unexpr(struct parser_state *s, struct ast_unexpr *unexpr,
                        uint8_t opcode)
{
  emit_expression(s, unexpr->op, false);
  cg_op(&s->cg, opcode, 0);
}

static void emit_assignment(struct parser_state *s, struct ast_binexpr *binexpr,
                            bool drop)
{
  union ast_node *left = binexpr->left;
  if (left->type == AST_NAME) {
    emit_expression(s, binexpr->right, false);
    if (!drop) {
      cg_push_op(&s->cg, OPCODE_DUP_TOP, 0);
    }
    cg_pop_op(&s->cg, OPCODE_STORE_NAME, left->name.index);
  } else {
    fprintf(stderr, "Unsupported or invalid lvalue\n");
    unimplemented();
  }
}

static void emit_call(struct parser_state *s, struct ast_call *call, bool drop)
{
  emit_expression(s, call->callee, false);
  unsigned n_arguments = 0;
  for (struct argument *argument = call->arguments; argument != NULL;
       argument = argument->next) {
    emit_expression(s, argument->expression, false);
    ++n_arguments;
  }
  cg_op(&s->cg, OPCODE_CALL_FUNCTION, n_arguments);
  cg_pop(&s->cg, n_arguments);
  if (drop) {
    cg_pop_op(&s->cg, OPCODE_POP_TOP, 0);
  }
}

static void emit_expression(struct parser_state *s, union ast_node *expression,
                            bool drop)
{
  switch (expression->type) {
  case AST_CONST:
    cg_push_op(&s->cg, OPCODE_LOAD_CONST, expression->cnst.index);
    break;
  case AST_NAME:
    cg_push_op(&s->cg, OPCODE_LOAD_NAME, expression->name.index);
    break;
  case AST_BINEXPR_ADD:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_ADD);
    break;
  case AST_BINEXPR_ASSIGN:
    emit_assignment(s, &expression->binexpr, drop);
    return;
  case AST_BINEXPR_FLOORDIV:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_FLOOR_DIVIDE);
    break;
  case AST_BINEXPR_TRUEDIV:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_TRUE_DIVIDE);
    break;
  case AST_BINEXPR_MATMUL:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_MATRIX_MULTIPLY);
    break;
  case AST_BINEXPR_SUB:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_SUBTRACT);
    break;
  case AST_BINEXPR_MUL:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_MULTIPLY);
    break;
  case AST_BINEXPR_LESS:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_LT);
    break;
  case AST_BINEXPR_LESS_EQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_LE);
    break;
  case AST_BINEXPR_EQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_EQ);
    break;
  case AST_BINEXPR_UNEQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_NE);
    break;
  case AST_BINEXPR_GREATER:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_GT);
    break;
  case AST_BINEXPR_GREATER_EQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_GE);
    break;
  case AST_BINEXPR_IN:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_IN);
    break;
  case AST_BINEXPR_NOT_IN:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_NOT_IN);
    break;
  case AST_BINEXPR_IS:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_IS);
    break;
  case AST_BINEXPR_IS_NOT:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_IS_NOT);
    break;
  case AST_UNEXPR_PLUS:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_POSITIVE);
    break;
  case AST_UNEXPR_NEGATIVE:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_NEGATIVE);
    break;
  case AST_UNEXPR_NOT:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_NOT);
    break;
  case AST_UNEXPR_INVERT:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_INVERT);
    break;
  case AST_CALL: {
    emit_call(s, &expression->call, drop);
    return;
  }
  default:
    fprintf(stderr, "unexpected expression");
    abort();
  }
  if (drop) {
    cg_pop_op(&s->cg, OPCODE_POP_TOP, 0);
  }
}

static void parse_expression_statement(struct parser_state *s)
{
#ifndef NDEBUG
  unsigned prev_stacksize = s->cg.code.stacksize;
#endif
  union ast_node *expression = parse_subexpression(s, PREC_ASSIGN);
  emit_expression(s, expression, true);
  assert(s->cg.code.stacksize == prev_stacksize);
}

static void parse_small_statement(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case EXPRESSION_START_CASES:
    parse_expression_statement(s);
    break;
  case T_PASS:
    eat(s, T_PASS);
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
  } while (accept(s, ';') && !peek(s, T_NEWLINE));
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

static void emit_condjump(struct parser_state *s, union ast_node *expression,
                          struct basic_block *true_block,
                          struct basic_block *false_block)
{
  emit_expression(s, expression, false);
  struct basic_block *block = cg_end_block(&s->cg);
  cg_pop(&s->cg, 1);
  block->jump_opcode = OPCODE_POP_JUMP_IF_FALSE;
  block->jump_target = false_block;
  block->default_target = true_block;
}

static void emit_jump(struct parser_state *s, struct basic_block *target)
{
  struct basic_block *block = cg_end_block(&s->cg);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->default_target = target;
}

static void parse_if(struct parser_state *s)
{
  eat(s, T_IF);
  union ast_node *expression = parse_subexpression(s, PREC_TEST);
  expect(s, ':');

  struct basic_block *true_block = cg_allocate_block(&s->cg);
  struct basic_block *false_block = cg_allocate_block(&s->cg);
  emit_condjump(s, expression, true_block, false_block);

  cg_begin_block(&s->cg, true_block);
  parse_suite(s);

  struct basic_block *footer;
  if (accept(s, T_ELSE)) {
    expect(s, ':');

    footer = cg_allocate_block(&s->cg);
    emit_jump(s, footer);

    cg_begin_block(&s->cg, false_block);
    parse_suite(s);
  } else {
    footer = false_block;
  }
  emit_jump(s, footer);

  cg_begin_block(&s->cg, footer);
}

static void parse_while(struct parser_state *s)
{
  eat(s, T_WHILE);
  union ast_node *expression = parse_subexpression(s, PREC_TEST);
  expect(s, ':');

  struct basic_block *cond = cg_allocate_block(&s->cg);
  struct basic_block *body   = cg_allocate_block(&s->cg);
  struct basic_block *footer = cg_allocate_block(&s->cg);
  struct basic_block *after = cg_allocate_block(&s->cg);

  struct basic_block *header = cg_end_block(&s->cg);
  header->jump_opcode = OPCODE_SETUP_LOOP;
  header->jump_target = after;
  header->default_target = cond;

  cg_begin_block(&s->cg, cond);
  emit_condjump(s, expression, body, footer);

  cg_begin_block(&s->cg, body);
  parse_suite(s);
  emit_jump(s, cond);

  cg_begin_block(&s->cg, footer);
  cg_op(&s->cg, OPCODE_POP_BLOCK, 0);
  cg_end_block(&s->cg);

  cg_begin_block(&s->cg, after);
}

static void parse_parameters(struct parser_state *s)
{
  expect(s, '(');
  /* TODO */
  expect(s, ')');
}

static void parse_def(struct parser_state *s)
{
  eat(s, T_DEF);
  skip_till(s, T_IDENTIFIER);
  struct symbol *symbol = s->scanner.token.u.symbol;
  next_token(s);

  parse_parameters(s);
  if (accept(s, T_MINUS_GREATER_THAN)) {
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  }
  expect(s, ':');

  struct code_state *saved = cg_push_code(&s->cg);

  parse_suite(s);

  struct object_code *code = cg_pop_code(&s->cg, saved, symbol->string);
  unsigned code_index = cg_register_code(&s->cg, code);
  cg_push_op(&s->cg, OPCODE_LOAD_CONST, code_index);

  const char *chars = symbol->string;
  uint32_t length = strlen(chars);
  unsigned name_const_index = cg_register_string(&s->cg, chars, length);
  cg_push_op(&s->cg, OPCODE_LOAD_CONST, name_const_index);

  cg_op(&s->cg, OPCODE_MAKE_FUNCTION, 0);
  cg_pop(&s->cg, 1);
  uint16_t name_index = name_index_from_symbol(s, symbol);
  cg_pop_op(&s->cg, OPCODE_STORE_NAME, name_index);
}

static void parse_statement(struct parser_state *s) {
  switch (s->scanner.token.kind) {
  case T_IF:
    parse_if(s);
    return;
  case T_WHILE:
    parse_while(s);
    return;
  case T_DEF:
    parse_def(s);
    return;
  case T_EOF:
    return;
  default:
    parse_simple_statement(s);
    return;
  }
}

struct object_code *parse(struct parser_state *s)
{
  next_token(s);

  cg_begin(&s->cg);

  add_anchor(s, T_EOF);
  while (s->scanner.token.kind != T_EOF) {
    if (accept(s, T_NEWLINE))
      continue;
    parse_statement(s);
    assert(s->cg.code.stacksize == 0);
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

  return cg_end(&s->cg);
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

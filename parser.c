#include "parser.h"
#include "parser_types.h"

#include <assert.h>
#include <errno.h>
#include <stdalign.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "ast_types.h"
#include "adt/arena.h"
#include "opcodes.h"
#include "scanner.h"
#include "symbol_table_types.h"
#include "symbol_types.h"
#include "token_kinds.h"
#include "util.h"
#include "codegen.h"
#include "codegen_ast.h"

#include "objects.h"

#define UNLIKELY(x)    __builtin_expect((x), 0)

/* Keep this in sync with prefix_parsers */
#define EXPRESSION_START_CASES \
  '+': \
  case '-': \
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

  PREC_XOR,         /* ^ */
  PREC_AND,         /* & */
  PREC_SHIFT,       /* <<, >> */
  PREC_ARITH,       /* +, - */
  PREC_TERM,        /* *, @, /, %, // */

  PREC_FACTOR,      /* prefix +, -, ~ */
  PREC_POWER,       /* prefix ** */

  PREC_POSTFIX,     /* postfix ( */
  PREC_ATOM,        /* name, number, string, ..., None, True, False */
};

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

static union ast_node *ast_allocate_zero_(struct parser_state *s,
                                          size_t size, uint8_t type) {
  union ast_node *node =
    (union ast_node*)arena_allocate(&s->ast, size, alignof(union ast_node));
  memset(node, 0, size);
  node->type = type;
  return node;
}

#define ast_allocate_zero(s, type, type_id) \
    ast_allocate_zero_((s), sizeof(type), (type_id))

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
  union ast_node *node = ast_allocate_zero(s, struct ast_call, AST_CALL);
  node->call.callee = left;

  eat(s, '(');
  add_anchor(s, ')');
  add_anchor(s, ',');

  struct argument *argument = NULL;
  if (!peek(s, ')')) {
    do {
      struct argument *new_argument = parse_argument(s);
      if (argument == NULL) {
        node->call.arguments = new_argument;
      } else {
        argument->next = new_argument;
      }
      argument = new_argument;
    } while(accept(s, ',') && !peek(s, ')'));
  }
  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');
  return node;
}

static inline union ast_node *parse_unexpr(struct parser_state *s,
                                           enum precedence prec_op,
                                           uint8_t ast_node_type)
{
  next_token(s);
  union ast_node *op = parse_subexpression(s, prec_op);

  union ast_node *node = ast_allocate_zero(s, struct ast_unexpr, ast_node_type);
  node->unexpr.op = op;
  return node;
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

static union ast_node *parse_identifier(struct parser_state *s)
{
  struct symbol *symbol = s->scanner.token.u.symbol;
  eat(s, T_IDENTIFIER);

  union ast_node *node = ast_allocate_zero(s, struct ast_identifier,
                                           AST_IDENTIFIER);
  node->identifier.symbol = symbol;
  return node;
}

static union ast_node *parse_string(struct parser_state *s)
{
  const char *chars = s->scanner.token.u.string;
  uint32_t length = strlen(chars);
  unsigned index = cg_register_string(&s->cg, chars, length);
  if ((uint16_t)index != index) {
    abort();
  }
  eat(s, T_STRING);

  union ast_node *node = ast_allocate_zero(s, struct ast_const, AST_CONST);
  node->cnst.index = (uint16_t)index;
  return node;
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
  if ((uint16_t)index != index) {
    abort();
  }
  eat(s, T_INTEGER);

  union ast_node *node = ast_allocate_zero(s, struct ast_const, AST_CONST);
  node->cnst.index = (uint16_t)index;
  return node;
}

static union ast_node *parse_singleton(struct parser_state *s, char type)
{
  next_token(s);
  unsigned index = cg_register_singleton(&s->cg, type);
  if ((uint16_t)index != index) {
    abort();
  }

  union ast_node *node = ast_allocate_zero(s, struct ast_const, AST_CONST);
  node->cnst.index = (uint16_t)index;
  return node;
}

static union ast_node *parse_true(struct parser_state *s)
{
  return parse_singleton(s, TYPE_TRUE);
}

static union ast_node *parse_false(struct parser_state *s)
{
  return parse_singleton(s, TYPE_FALSE);
}

static union ast_node *parse_none(struct parser_state *s)
{
  return parse_singleton(s, TYPE_NONE);
}

static union ast_node *parse_ellipsis(struct parser_state *s)
{
  return parse_singleton(s, TYPE_ELLIPSIS);
}

typedef union ast_node *(*prefix_parser_func)(struct parser_state *s);
struct prefix_expression_parser {
  prefix_parser_func func;
  enum precedence precedence;
};

static const struct prefix_expression_parser prefix_parsers[] = {
  ['+']           = { .func = parse_plus,       .precedence = PREC_FACTOR },
  ['-']           = { .func = parse_negative,   .precedence = PREC_FACTOR },
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


static inline union ast_node *parse_binexpr(struct parser_state *s,
                                            enum precedence prec_right,
                                            uint8_t ast_node_type,
                                            union ast_node *left)
{
  next_token(s);
  union ast_node *right = parse_subexpression(s, prec_right);

  union ast_node *node =
      ast_allocate_zero(s, struct ast_binexpr, ast_node_type);
  node->binexpr.left = left;
  node->binexpr.right = right;
  return node;
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

static union ast_node *make_invalid_expression(struct parser_state *s)
{
  (void)s;
  // TODO
  unimplemented();
}

static union ast_node *parse_not_in(struct parser_state *s,
                                    union ast_node *left)
{
  eat(s, T_not);
  if (!accept(s, T_in)) {
    parse_error_expected(s, "in");
    return make_invalid_expression(s);
  }

  union ast_node *right = parse_subexpression(s, PREC_COMPARISON + 1);

  union ast_node *node =
      ast_allocate_zero(s, struct ast_binexpr, AST_BINEXPR_NOT_IN);
  node->binexpr.left = left;
  node->binexpr.right = right;
  return node;
}

static union ast_node *parse_is(struct parser_state *s, union ast_node *left)
{
  eat(s, T_is);
  uint8_t ast_node_type = accept(s, T_not) ? AST_BINEXPR_IS_NOT
                                           : AST_BINEXPR_IS;

  union ast_node *right = parse_subexpression(s, PREC_COMPARISON + 1);

  union ast_node *node =
      ast_allocate_zero(s, struct ast_binexpr, ast_node_type);
  node->binexpr.left = left;
  node->binexpr.right = right;
  return node;
}

typedef union ast_node *(*postfix_parser_func)(struct parser_state *s,
                                               union ast_node *prefix);

struct postfix_expression_parser {
  postfix_parser_func func;
  enum precedence precedence;
};

static const struct postfix_expression_parser postfix_parsers[] = {
  ['(']    = { .func = parse_call,       .precedence = PREC_POSTFIX    },
  ['+']    = { .func = parse_add,        .precedence = PREC_ARITH      },
  ['*']    = { .func = parse_mul,        .precedence = PREC_TERM       },
  ['@']    = { .func = parse_matmul,     .precedence = PREC_TERM       },
  ['-']    = { .func = parse_sub,        .precedence = PREC_ARITH      },
  ['/']    = { .func = parse_true_div,   .precedence = PREC_TERM       },
  [T_not]  = { .func = parse_not_in,     .precedence = PREC_COMPARISON },
  ['=']    = { .func = parse_assignment, .precedence = PREC_ASSIGN     },
  ['<']    = { .func = parse_less,       .precedence = PREC_COMPARISON },
  ['>']    = { .func = parse_greater,    .precedence = PREC_COMPARISON },
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

union ast_node *parse_subexpression(struct parser_state *s,
                                    enum precedence precedence)
{
  uint16_t token_kind = s->scanner.token.kind;
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
  union ast_node *result = prefix_parser->func(s);

  for (;;) {
    uint16_t postifx_token_kind = s->scanner.token.kind;
    if (postifx_token_kind >= sizeof(postfix_parsers) / sizeof(postfix_parsers[0]))
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
#ifndef NDEBUG
  unsigned prev_stacksize = s->cg.code.stacksize;
#endif
  union ast_node *expression = parse_subexpression(s, PREC_ASSIGN);
  emit_expression(&s->cg, expression, true);
  assert(s->cg.code.stacksize == prev_stacksize);
}

static void parse_small_statement(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case EXPRESSION_START_CASES:
    parse_expression_statement(s);
    break;
  case T_pass:
    eat(s, T_pass);
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
  emit_expression(&s->cg, expression, false);
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
  eat(s, T_if);
  union ast_node *expression = parse_subexpression(s, PREC_TEST);
  expect(s, ':');

  struct basic_block *true_block = cg_allocate_block(&s->cg);
  struct basic_block *false_block = cg_allocate_block(&s->cg);
  emit_condjump(s, expression, true_block, false_block);

  cg_begin_block(&s->cg, true_block);
  parse_suite(s);

  struct basic_block *footer;
  if (accept(s, T_else)) {
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
  eat(s, T_while);
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

  unsigned num_parameters = 0;
  while (peek(s, T_IDENTIFIER)) {
    struct symbol *symbol = s->scanner.token.u.symbol;
    next_token(s);

    struct symbol_info *info = cg_symbol_info(&s->cg, symbol);
    if (info != NULL) {
      fprintf(stderr, "error: duplicate parameter '%s'\n", symbol->string);
      s->error = true;
    } else {
      info = cg_new_symbol_info(&s->cg, symbol);
      info->type = SYMBOL_LOCAL;
      info->index = cg_append_varname(&s->cg, symbol->string);
      assert(s->error || info->index == num_parameters);
    }
    num_parameters++;
    if (!accept(s, ',')) {
      break;
    }
  }

  expect(s, ')');
  s->cg.code.argcount = num_parameters;
}

static void parse_def(struct parser_state *s)
{
  eat(s, T_def);
  skip_till(s, T_IDENTIFIER);
  struct symbol *symbol = s->scanner.token.u.symbol;
  next_token(s);

  cg_push_code(&s->cg);

  parse_parameters(s);
  if (accept(s, T_MINUS_GREATER_THAN)) {
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  }
  expect(s, ':');

  parse_suite(s);

  union object *code = cg_pop_code(&s->cg, symbol->string);
  unsigned code_index = cg_register_code(&s->cg, code);
  cg_push_op(&s->cg, OPCODE_LOAD_CONST, code_index);

  const char *chars = symbol->string;
  uint32_t length = strlen(chars);
  unsigned name_const_index = cg_register_string(&s->cg, chars, length);
  cg_push_op(&s->cg, OPCODE_LOAD_CONST, name_const_index);

  cg_op(&s->cg, OPCODE_MAKE_FUNCTION, 0);
  cg_pop(&s->cg, 1);
  emit_store(&s->cg, symbol);
}

static void parse_statement(struct parser_state *s) {
  switch (s->scanner.token.kind) {
  case T_if:
    parse_if(s);
    return;
  case T_while:
    parse_while(s);
    return;
  case T_def:
    parse_def(s);
    return;
  case T_EOF:
    return;
  default:
    parse_simple_statement(s);
    return;
  }
}

union object *parse(struct parser_state *s)
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
  for (uint16_t i = 0; i < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]); ++i) {
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

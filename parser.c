#include "parser.h"
#include "parser_types.h"

#include <assert.h>
#include <stdalign.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "adt/idynarray.h"
#include "ast.h"
#include "ast_types.h"
#include "codegen.h"
#include "codegen_expression.h"
#include "codegen_statement.h"
#include "diagnostics.h"
#include "object.h"
#include "object_types.h"
#include "scanner.h"
#include "symbol_table.h"
#include "symbol_types.h"
#include "token_kinds.h"
#include "util.h"

#define UNLIKELY(x) __builtin_expect((x), 0)

/* Keep this in sync with prefix_parsers below */
#define EXPRESSION_START_CASES                                                \
  '(' : case '*':                                                             \
  case '+':                                                                   \
  case '-':                                                                   \
  case '[':                                                                   \
  case '{':                                                                   \
  case '~':                                                                   \
  case T_not:                                                                 \
  case T_IDENTIFIER:                                                          \
  case T_FLOAT:                                                               \
  case T_STRING:                                                              \
  case T_INTEGER:                                                             \
  case T_ASTERISK_ASTERISK:                                                   \
  case T_True:                                                                \
  case T_False:                                                               \
  case T_None:                                                                \
  case T_DOT_DOT_DOT

enum precedence {
  PREC_ASSIGN,      /* = */
  PREC_LIST,        /* , */
  PREC_STAR,        /* *, ** prefix */
  PREC_WALRUS,      /* := */
  PREC_LAMBDA,      /* lambda ... */
  PREC_TEST,        /* postfix 'if' _ 'else' _ */
  PREC_LOGICAL_OR,  /* OR */
  PREC_LOGICAL_AND, /* AND */
  PREC_LOGICAL_NOT, /* prefix NOT */
  PREC_COMPARISON,  /* <, >, ==, >=, <=, <>, !=, in, not in, is, is not */

  PREC_OR,    /* | */
  PREC_XOR,   /* ^ */
  PREC_AND,   /* & */
  PREC_SHIFT, /* <<, >> */
  PREC_ARITH, /* +, - */
  PREC_TERM,  /* *, @, /, %, // */

  PREC_FACTOR, /* prefix +, -, ~ */
  PREC_POWER,  /* ** */

  PREC_PRIMARY, /* .attr  [subscript]   (call) */
  PREC_ATOM,    /* name, number, string, ..., None, True, False */
};

static void error_expected(struct parser_state *s, const char *what)
{
  diag_begin_error(&s->d, scanner_location(&s->scanner));
  diag_frag(&s->d, "expected ");
  diag_frag(&s->d, what);
  diag_frag(&s->d, ", got ");
  diag_token(&s->d, &s->scanner.token);
  diag_end(&s->d);
}

static inline enum token_kind peek(const struct parser_state *s)
{
  return s->scanner.token.kind;
}

static inline void next_token(struct parser_state *s)
{
  scanner_next_token(&s->scanner);
}

static inline void eat(struct parser_state *s, enum token_kind token_kind)
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
                                            enum token_kind      token_kind)
{
  assert(token_kind == T_STRING || token_kind == T_INTEGER
         || token_kind == T_FLOAT);
  assert(peek(s) == token_kind);
  return s->scanner.token.u.object;
}

static inline bool accept(struct parser_state *s, enum token_kind token_kind)
{
  if (peek(s) == token_kind) {
    next_token(s);
    return true;
  }
  return false;
}

static void add_anchor(struct parser_state *s, enum token_kind token_kind)
{
  assert(token_kind < sizeof(s->anchor_set) / sizeof(s->anchor_set[0]));
  ++s->anchor_set[token_kind];
}

static void remove_anchor(struct parser_state *s, enum token_kind token_kind)
{
  assert(token_kind < sizeof(s->anchor_set) / sizeof(s->anchor_set[0]));
  assert(s->anchor_set[token_kind] > 0);
  --s->anchor_set[token_kind];
}

static void eat_until_matching_token(struct parser_state *s,
                                     enum token_kind      token_kind)
{
  enum token_kind end_token_kind;
  switch (token_kind) {
  case '(':
    end_token_kind = ')';
    break;
  case '{':
    end_token_kind = '}';
    break;
  case '[':
    end_token_kind = ']';
    break;
  case T_INDENT:
    end_token_kind = T_DEDENT;
    break;
  default:
    end_token_kind = token_kind;
    break;
  }

  unsigned parenthesis_count = 0;
  unsigned brace_count = 0;
  unsigned bracket_count = 0;
  unsigned indent_count = 0;
  while (peek(s) != end_token_kind || parenthesis_count != 0
         || brace_count != 0 || bracket_count != 0) {
    switch (peek(s)) {
    case T_EOF:
      return;
    case '(':
      ++parenthesis_count;
      break;
    case '{':
      ++brace_count;
      break;
    case '[':
      ++bracket_count;
      break;
    case T_INDENT:
      ++indent_count;
      break;

    case ')':
      if (parenthesis_count > 0) --parenthesis_count;
      goto check_stop;
    case '}':
      if (brace_count > 0) --brace_count;
      goto check_stop;
    case ']':
      if (bracket_count > 0) --bracket_count;
      goto check_stop;
    case T_DEDENT:
      if (indent_count > 0) --indent_count;
      goto check_stop;

    check_stop:
      if (peek(s) == end_token_kind && parenthesis_count == 0
          && brace_count == 0 && bracket_count == 0 && indent_count == 0)
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
    if (peek(s) == ':') {
      next_token(s);
      if (accept(s, T_NEWLINE)) {
        if (peek(s) == T_INDENT) {
          eat_until_matching_token(s, peek(s));
          next_token(s);
          if (s->anchor_set[T_NEWLINE] != 0) {
            break;
          }
        }
      }
      continue;
    }
    next_token(s);
  }
}

static bool skip_till(struct parser_state *s,
                      enum token_kind      expected_token_kind)
{
  if (UNLIKELY(peek(s) != expected_token_kind)) {
    error_expected(s, token_kind_name(expected_token_kind));

    add_anchor(s, expected_token_kind);
    eat_until_anchor(s);
    remove_anchor(s, expected_token_kind);
    if (peek(s) != expected_token_kind) return false;
  }
  return true;
}

static void expect(struct parser_state *s, enum token_kind expected_token_kind)
{
  if (skip_till(s, expected_token_kind)) eat(s, expected_token_kind);
}

static union ast_expression *
ast_allocate_expression_(struct parser_state *s, size_t size,
                         enum ast_expression_type type)
{
  union ast_expression *expression = (union ast_expression *)arena_allocate(
      &s->ast, size, alignof(union ast_expression));
  memset(expression, 0, size);
  expression->type = type;
  return expression;
}

#define ast_allocate_expression(s, type, type_id)                             \
  ast_allocate_expression_((s), sizeof(type), (type_id))

static union ast_expression *ast_const_new(struct parser_state *s,
                                           union object        *object)
{
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_const, AST_CONST);
  expression->cnst.object = object;
  return expression;
}

static union ast_expression *ast_invalid(struct parser_state *s)
{
  assert(s->d.had_error);
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_const, AST_INVALID);
  expression->cnst.object
      = object_intern_singleton(&s->cg.objects, OBJECT_NONE);
  return expression;
}

static struct symbol *symbol_invalid(struct parser_state *s)
{
  assert(s->d.had_error);
  return symbol_table_get_or_insert(s->scanner.symbol_table, "<invalid>");
}

static union ast_expression *parse_expression(struct parser_state *s,
                                              enum precedence      precedence);

static bool is_expression_start(enum token_kind token_kind)
{
  switch (token_kind) {
  case EXPRESSION_START_CASES:
    return true;
  default:
    return false;
  }
}

static union ast_expression *parse_expression_or_slice(struct parser_state *s)
{
  union ast_expression *expression = NULL;
  if (peek(s) != ':') {
    expression = parse_expression(s, PREC_LIST + 1);
    if (peek(s) != ':') {
      return expression;
    }
  }
  eat(s, ':');

  union ast_expression *start = expression;
  union ast_expression *stop = NULL;
  union ast_expression *step = NULL;
  if (is_expression_start(peek(s))) {
    stop = parse_expression(s, PREC_LIST + 1);
  }
  if (accept(s, ':') && is_expression_start(peek(s))) {
    step = parse_expression(s, PREC_LIST + 1);
  }

  expression = ast_allocate_expression(s, struct ast_slice, AST_SLICE);
  expression->slice.start = start;
  expression->slice.stop = stop;
  expression->slice.step = step;
  return expression;
}

static union ast_expression *parse_expression_list_helper(
    struct parser_state *s, enum ast_expression_type type,
    union ast_expression *first, enum precedence precedence, bool allow_slices)
{
  union ast_expression *inline_storage[16];
  struct idynarray      expressions;
  idynarray_init(&expressions, inline_storage, sizeof(inline_storage));

  *(idynarray_append(&expressions, union ast_expression *)) = first;

  bool has_star_expression = ast_expression_type(first) == AST_UNEXPR_STAR;
  for (;;) {
    if (!accept(s, ',')) break;
    if (!is_expression_start(peek(s)) && (!allow_slices || peek(s) != ':')) {
      break;
    }
    union ast_expression *expression;
    if (allow_slices) {
      expression = parse_expression_or_slice(s);
    } else {
      expression = parse_expression(s, precedence);
    }
    if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
      has_star_expression = true;
    }
    *(idynarray_append(&expressions, union ast_expression *)) = expression;
  }

  unsigned num_expressions
      = idynarray_length(&expressions, union ast_expression *);
  size_t expressions_size = num_expressions * sizeof(union ast_expression *);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_expression_list) + expressions_size, type);
  expression->expression_list.has_star_expression = has_star_expression;
  expression->expression_list.num_expressions = num_expressions;
  memcpy(expression->expression_list.expressions, idynarray_data(&expressions),
         expressions_size);
  idynarray_free(&expressions);
  return expression;
}

static union ast_expression *
parse_generator_expression(struct parser_state     *s,
                           enum ast_expression_type type,
                           union ast_expression    *left)
{
  assert(type == AST_GENERATOR_EXPRESSION || type == AST_LIST_COMPREHENSION);
  struct generator_expression_part inline_storage[4];
  struct idynarray                 parts;
  idynarray_init(&parts, inline_storage, sizeof(inline_storage));

  while (peek(s) == T_for || peek(s) == T_if) {
    struct generator_expression_part *part
        = idynarray_append(&parts, struct generator_expression_part);
    if (peek(s) == T_for) {
      eat(s, T_for);
      union ast_expression *target = parse_expression(s, PREC_OR);
      if (peek(s) == ',') {
        target = parse_expression_list_helper(s, AST_EXPRESSION_LIST, target,
                                              PREC_OR, /*allow_slices=*/false);
      }
      expect(s, T_in);
      union ast_expression *expression = parse_expression(s, PREC_OR);

      part->type = GENERATOR_EXPRESSION_PART_FOR;
      part->target = target;
      part->expression = expression;
    } else {
      eat(s, T_if);
      union ast_expression *expression = parse_expression(s, PREC_LOGICAL_OR);

      part->type = GENERATOR_EXPRESSION_PART_IF;
      part->target = NULL;
      part->expression = expression;
    }
  }

  unsigned num_parts
      = idynarray_length(&parts, struct generator_expression_part);
  size_t parts_size = num_parts * sizeof(struct generator_expression_part);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_generator_expression) + parts_size, type);
  expression->generator_expression.expression = left;
  expression->generator_expression.num_parts = num_parts;
  memcpy(&expression->generator_expression.parts, idynarray_data(&parts),
         parts_size);
  idynarray_free(&parts);
  return expression;
}

static union ast_expression *parse_attr(struct parser_state  *s,
                                        union ast_expression *left)
{
  eat(s, '.');
  if (!skip_till(s, T_IDENTIFIER)) return ast_invalid(s);
  struct symbol *symbol = eat_identifier(s);

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_attr, AST_ATTR);
  expression->attr.expression = left;
  expression->attr.symbol = symbol;
  return expression;
}

static struct argument *parse_argument(struct parser_state *s,
                                       struct argument     *argument)
{
  union ast_expression *expression = parse_expression(s, PREC_STAR);
  struct symbol        *name = NULL;
  if (accept(s, '=')) {
    if (expression->type == AST_IDENTIFIER) {
      name = expression->identifier.symbol;
    } else {
      diag_begin_error(&s->d, scanner_location(&s->scanner));
      diag_frag(&s->d, "assignment not allowed, perhaps you meant '=='?");
      diag_end(&s->d);
    }
    expression = parse_expression(s, PREC_STAR);
  }

  if (peek(s) == T_for) {
    expression
        = parse_generator_expression(s, AST_GENERATOR_EXPRESSION, expression);
  }

  argument->name = name;
  argument->expression = expression;
  return argument;
}

static union ast_expression *parse_argument_list(struct parser_state  *s,
                                                 union ast_expression *callee)
{
  eat(s, '(');
  add_anchor(s, ')');
  add_anchor(s, ',');

  struct argument  inline_storage[8];
  struct idynarray arguments;
  idynarray_init(&arguments, inline_storage, sizeof(inline_storage));
  bool has_star_argument = false;
  bool has_kw_argument = false;

  if (peek(s) != ')') {
    do {
      struct argument *argument
          = idynarray_append(&arguments, struct argument);
      parse_argument(s, argument);
      enum ast_expression_type type
          = ast_expression_type(argument->expression);
      if (type == AST_UNEXPR_STAR || type == AST_UNEXPR_STAR_STAR) {
        has_star_argument = true;
      }
      if (argument->name != NULL) {
        has_kw_argument = true;
      }
    } while (accept(s, ',') && peek(s) != ')');
  }
  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');

  unsigned num_arguments = idynarray_length(&arguments, struct argument);
  size_t   arguments_size = num_arguments * sizeof(struct argument);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_call) + arguments_size, AST_CALL);
  expression->call.has_star_argument = has_star_argument;
  expression->call.has_kw_argument = has_kw_argument;
  expression->call.callee = callee;
  expression->call.num_arguments = num_arguments;
  memcpy(expression->call.arguments, idynarray_data(&arguments),
         arguments_size);
  return expression;
}

static union ast_expression *parse_call(struct parser_state  *s,
                                        union ast_expression *left)
{
  return parse_argument_list(s, /*callee=*/left);
}

static inline union ast_expression *parse_unexpr(struct parser_state *s,
                                                 enum precedence      prec_op,
                                                 enum ast_expression_type type)
{
  next_token(s);
  union ast_expression *op = parse_expression(s, prec_op);

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_unexpr, type);
  expression->unexpr.op = op;
  return expression;
}

static union ast_expression *parse_l_bracket(struct parser_state *s)
{
  eat(s, '[');

  if (accept(s, ']')) {
    union ast_expression *expression = ast_allocate_expression(
        s, struct ast_expression_list, AST_LIST_DISPLAY);
    expression->expression_list.num_expressions = 0;
    return expression;
  }

  add_anchor(s, ']');
  add_anchor(s, ',');

  union ast_expression *first = parse_expression(s, PREC_LIST + 1);

  union ast_expression *expression;
  if (peek(s) == T_for) {
    expression = parse_generator_expression(s, AST_LIST_COMPREHENSION, first);
  } else {
    expression = parse_expression_list_helper(
        s, AST_LIST_DISPLAY, first, PREC_LIST + 1, /*allow_slices=*/false);
  }

  remove_anchor(s, ',');
  remove_anchor(s, ']');
  expect(s, ']');
  return expression;
}

static union ast_expression *parse_l_curly(struct parser_state *s)
{
  eat(s, '{');
  if (accept(s, '}')) {
    union ast_expression *expression = ast_allocate_expression(
        s, struct ast_dict_item_list, AST_DICT_DISPLAY);
    expression->dict_item_list.num_items = 0;
    return expression;
  }

  add_anchor(s, '}');
  add_anchor(s, ',');

  union ast_expression *first = parse_expression(s, PREC_LIST + 1);
  /* set display */
  if (peek(s) == ',' || peek(s) == '}') {
    union ast_expression *expression = parse_expression_list_helper(
        s, AST_SET_DISPLAY, first, PREC_LIST + 1, /*allow_slices=*/false);

    remove_anchor(s, ',');
    remove_anchor(s, '}');
    expect(s, '}');
    return expression;
  }

  struct dict_item inline_storage[16];
  struct idynarray items;
  idynarray_init(&items, inline_storage, sizeof(inline_storage));

  union ast_expression *key = first;
  for (;;) {
    expect(s, ':');
    union ast_expression *expression = parse_expression(s, PREC_LIST + 1);

    struct dict_item *item = idynarray_append(&items, struct dict_item);
    item->key = key;
    item->expression = expression;

    if (peek(s) == '}' || peek(s) == T_EOF) break;
    expect(s, ',');
    if (peek(s) == '}' || peek(s) == T_EOF) break;
    key = parse_expression(s, PREC_LIST + 1);
  }

  remove_anchor(s, ',');
  remove_anchor(s, '}');
  eat(s, '}');

  unsigned              num_items = idynarray_length(&items, struct dict_item);
  size_t                items_size = num_items * sizeof(struct dict_item);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_dict_item_list) + items_size, AST_DICT_DISPLAY);
  expression->dict_item_list.num_items = num_items;
  memcpy(expression->dict_item_list.items, idynarray_data(&items), items_size);
  idynarray_free(&items);
  return expression;
}

static union ast_expression *parse_l_paren(struct parser_state *s)
{
  eat(s, '(');
  if (accept(s, ')')) {
    union ast_expression *expression = ast_allocate_expression(
        s, struct ast_expression_list, AST_EXPRESSION_LIST);
    expression->expression_list.num_expressions = 0;
    expression->expression_list.as_constant = ast_tuple_compute_constant(
        &s->cg.objects, &expression->expression_list);
    return expression;
  }

  add_anchor(s, ')');
  add_anchor(s, ',');

  union ast_expression *expression;
  if (peek(s) == T_yield) {
    eat(s, T_yield);
    enum ast_expression_type type
        = accept(s, T_from) ? AST_UNEXPR_YIELD_FROM : AST_UNEXPR_YIELD;
    union ast_expression *yield_expression = parse_expression(s, PREC_LIST);
    expression = ast_allocate_expression(s, struct ast_unexpr, type);
    expression->unexpr.op = yield_expression;
    had_yield(&s->cg);
  } else {
    expression = parse_expression(s, PREC_LIST);
    if (peek(s) == T_for) {
      expression = parse_generator_expression(s, AST_GENERATOR_EXPRESSION,
                                              expression);
    }
  }

  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');
  return expression;
}

static union ast_expression *parse_asterisk(struct parser_state *s)
{
  return parse_unexpr(s, PREC_STAR, AST_UNEXPR_STAR);
}

static union ast_expression *parse_asterisk_asterisk(struct parser_state *s)
{
  return parse_unexpr(s, PREC_STAR, AST_UNEXPR_STAR_STAR);
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

  union ast_expression *node
      = ast_allocate_expression(s, struct ast_identifier, AST_IDENTIFIER);
  node->identifier.symbol = symbol;
  return node;
}

static union ast_expression *parse_float(struct parser_state *s)
{
  union object *object = peek_get_object(s, T_FLOAT);
  eat(s, T_FLOAT);
  return ast_const_new(s, object);
}

static union ast_expression *parse_integer(struct parser_state *s)
{
  union object *object = peek_get_object(s, T_INTEGER);
  eat(s, T_INTEGER);
  return ast_const_new(s, object);
}

static union ast_expression *parse_singleton(struct parser_state *s,
                                             enum object_type     type)
{
  next_token(s);
  union object *object = object_intern_singleton(&s->cg.objects, type);
  return ast_const_new(s, object);
}

static union ast_expression *parse_string(struct parser_state *s)
{
  union object *object = peek_get_object(s, T_STRING);
  eat(s, T_STRING);

  if (peek(s) == T_STRING) {
    enum object_type type = object_type(object);
    union object   **inline_storage[8];
    struct idynarray strings;
    idynarray_init(&strings, inline_storage, sizeof(inline_storage));
    *((union object **)idynarray_append(&strings, union object *)) = object;
    size_t combined_length = object->string.length;

    bool            mixed_types = false;
    struct location location;
    do {
      object = peek_get_object(s, T_STRING);
      if (object_type(object) != type && !mixed_types) {
        location = scanner_location(&s->scanner);
        mixed_types = true;
      }
      eat(s, T_STRING);
      *((union object **)idynarray_append(&strings, union object *)) = object;

      combined_length += object->string.length;
    } while (peek(s) == T_STRING);

    if (mixed_types) {
      diag_begin_error(&s->d, location);
      diag_frag(&s->d, "cannot mix bytes and str literals");
      diag_end(&s->d);
    }

    unsigned num_strings = idynarray_length(&strings, union object *);
    if (combined_length > UINT32_MAX) abort();

    char *combined = arena_allocate(s->scanner.strings, combined_length, 1);
    char *dest = combined;
    union object **strings_arr = idynarray_data(&strings);
    for (unsigned i = 0; i < num_strings; i++) {
      union object *string = strings_arr[i];
      unsigned      string_length = string->string.length;
      memcpy(dest, string->string.chars, string_length);
      dest += string_length;
    }
    assert(dest - combined == (ptrdiff_t)combined_length);

    object = object_intern_string(&s->cg.objects, type, combined_length,
                                  combined);
    if (object->string.chars != combined) {
      arena_free_to(s->scanner.strings, combined);
    }
    idynarray_free(&strings);
  }

  return ast_const_new(s, object);
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
};

static const struct prefix_expression_parser prefix_parsers[] = {
  /* clang-format off */
  ['(']                 = { .func = parse_l_paren           },
  ['*']                 = { .func = parse_asterisk          },
  ['+']                 = { .func = parse_plus              },
  ['-']                 = { .func = parse_negative          },
  ['[']                 = { .func = parse_l_bracket         },
  ['{']                 = { .func = parse_l_curly           },
  ['~']                 = { .func = parse_invert            },
  [T_ASTERISK_ASTERISK] = { .func = parse_asterisk_asterisk },
  [T_DOT_DOT_DOT]       = { .func = parse_ellipsis          },
  [T_FLOAT]             = { .func = parse_float             },
  [T_False]             = { .func = parse_false             },
  [T_IDENTIFIER]        = { .func = parse_identifier        },
  [T_INTEGER]           = { .func = parse_integer           },
  [T_None]              = { .func = parse_none              },
  [T_not]               = { .func = parse_not               },
  [T_STRING]            = { .func = parse_string            },
  [T_True]              = { .func = parse_true              },
  /* clang-format on */
};

static inline union ast_expression *
parse_binexpr(struct parser_state *s, enum precedence prec_right,
              enum ast_expression_type type, union ast_expression *left)
{
  next_token(s);
  union ast_expression *right = parse_expression(s, prec_right);

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_binexpr, type);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static bool check_binexpr_assign_target(struct parser_state  *s,
                                        union ast_expression *target,
                                        struct location       location)
{
  enum ast_expression_type type = ast_expression_type(target);
  if (type == AST_IDENTIFIER || type == AST_BINEXPR_SUBSCRIPT
      || type == AST_ATTR)
    return false;
  diag_begin_error(&s->d, location);
  diag_frag(&s->d, "cannot assign to ");
  diag_expression(&s->d, target);
  if (type == AST_EXPRESSION_LIST) {
    diag_frag(&s->d, " for agumented assignment");
  }
  diag_frag(&s->d, ".");
  diag_end(&s->d);
  return true;
}

static inline union ast_expression *
parse_binexpr_assign(struct parser_state *s, enum ast_expression_type type,
                     union ast_expression *left)
{
  struct location       location = scanner_location(&s->scanner);
  union ast_expression *expression = parse_binexpr(s, PREC_ASSIGN, type, left);
  if (check_binexpr_assign_target(s, left, location)) {
    expression = ast_invalid(s);
  }
  return expression;
}

static union ast_expression *parse_add(struct parser_state  *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_ARITH + 1, AST_BINEXPR_ADD, left);
}

static union ast_expression *parse_add_assign(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_ADD_ASSIGN, left);
}

static union ast_expression *parse_and(struct parser_state  *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_AND + 1, AST_BINEXPR_AND, left);
}

static union ast_expression *parse_and_assign(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_AND_ASSIGN, left);
}

static bool check_assignment_target(struct parser_state  *s,
                                    union ast_expression *expression,
                                    struct location location, bool is_del)
{
  enum ast_expression_type type = ast_expression_type(expression);
  if (type == AST_IDENTIFIER || type == AST_BINEXPR_SUBSCRIPT
      || type == AST_ATTR)
    return false;
  if (type == AST_EXPRESSION_LIST || type == AST_LIST_DISPLAY) {
    unsigned num_expressions = expression->expression_list.num_expressions;
    union ast_expression **expressions
        = expression->expression_list.expressions;
    for (unsigned i = 0; i < num_expressions; i++) {
      if (check_assignment_target(s, expressions[i], location, is_del)) {
        return true;
      }
    }
    return false;
  }
  diag_begin_error(&s->d, location);
  diag_frag(&s->d, is_del ? "cannot delete " : "cannot assign to ");
  diag_expression(&s->d, expression);
  if (!is_del) {
    /* TODO: only show ':=' suggestion where it makes sense like python... */
    diag_frag(&s->d, ". Maybe you meant '==', or ':=' instead of '='?");
  }
  diag_end(&s->d);
  return true;
}

static union ast_expression *parse_assignment(struct parser_state  *s,
                                              union ast_expression *left)
{
  struct location       location = scanner_location(&s->scanner);
  union ast_expression *expression
      = parse_binexpr(s, PREC_ASSIGN, AST_BINEXPR_ASSIGN, left);
  if (check_assignment_target(s, left, location, /*is_del=*/false)) {
    expression = ast_invalid(s);
  }
  return expression;
}

static union ast_expression *parse_equal(struct parser_state  *s,
                                         union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_EQUAL, left);
}

static union ast_expression *parse_floor_div(struct parser_state  *s,
                                             union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_FLOORDIV, left);
}

static union ast_expression *parse_floor_div_assign(struct parser_state  *s,
                                                    union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_FLOORDIV_ASSIGN, left);
}

static union ast_expression *parse_greater(struct parser_state  *s,
                                           union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_GREATER, left);
}

static union ast_expression *parse_greater_equal(struct parser_state  *s,
                                                 union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_GREATER_EQUAL,
                       left);
}

static union ast_expression *parse_in(struct parser_state  *s,
                                      union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_IN, left);
}

static union ast_expression *parse_is(struct parser_state  *s,
                                      union ast_expression *left)
{
  eat(s, T_is);
  uint8_t ast_node_type
      = accept(s, T_not) ? AST_BINEXPR_IS_NOT : AST_BINEXPR_IS;

  union ast_expression *right = parse_expression(s, PREC_COMPARISON + 1);

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_binexpr, ast_node_type);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static union ast_expression *parse_less(struct parser_state  *s,
                                        union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_LESS, left);
}

static union ast_expression *parse_less_equal(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_LESS_EQUAL, left);
}

static union ast_expression *parse_logical_and(struct parser_state  *s,
                                               union ast_expression *left)
{
  return parse_binexpr(s, PREC_LOGICAL_AND + 1, AST_BINEXPR_LOGICAL_AND, left);
}

static union ast_expression *parse_logical_or(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr(s, PREC_LOGICAL_OR + 1, AST_BINEXPR_LOGICAL_OR, left);
}

static union ast_expression *parse_matmul(struct parser_state  *s,
                                          union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MATMUL, left);
}

static union ast_expression *parse_matmul_assign(struct parser_state  *s,
                                                 union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_MATMUL_ASSIGN, left);
}

static union ast_expression *parse_mod(struct parser_state  *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MOD, left);
}

static union ast_expression *parse_mod_assign(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_MOD_ASSIGN, left);
}

static union ast_expression *parse_mul(struct parser_state  *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_MUL, left);
}

static union ast_expression *parse_mul_assign(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_MUL_ASSIGN, left);
}

static union ast_expression *parse_not_in(struct parser_state  *s,
                                          union ast_expression *left)
{
  eat(s, T_not);
  expect(s, T_in);

  union ast_expression *right = parse_expression(s, PREC_COMPARISON + 1);
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_binexpr, AST_BINEXPR_NOT_IN);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static union ast_expression *parse_or(struct parser_state  *s,
                                      union ast_expression *left)
{
  return parse_binexpr(s, PREC_OR + 1, AST_BINEXPR_OR, left);
}

static union ast_expression *parse_or_assign(struct parser_state  *s,
                                             union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_OR_ASSIGN, left);
}

static union ast_expression *parse_power(struct parser_state  *s,
                                         union ast_expression *left)
{
  return parse_binexpr(s, PREC_POWER + 1, AST_BINEXPR_POWER, left);
}

static union ast_expression *parse_power_assign(struct parser_state  *s,
                                                union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_POWER_ASSIGN, left);
}

static union ast_expression *parse_shift_left(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr(s, PREC_SHIFT + 1, AST_BINEXPR_SHIFT_LEFT, left);
}

static union ast_expression *
parse_shift_left_assign(struct parser_state *s, union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_SHIFT_LEFT_ASSIGN, left);
}

static union ast_expression *parse_shift_right(struct parser_state  *s,
                                               union ast_expression *left)
{
  return parse_binexpr(s, PREC_SHIFT + 1, AST_BINEXPR_SHIFT_RIGHT, left);
}

static union ast_expression *
parse_shift_right_assign(struct parser_state *s, union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_SHIFT_RIGHT_ASSIGN, left);
}

static union ast_expression *parse_sub(struct parser_state  *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_ARITH + 1, AST_BINEXPR_SUB, left);
}

static union ast_expression *parse_sub_assign(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_SUB_ASSIGN, left);
}

static union ast_expression *parse_subscript(struct parser_state  *s,
                                             union ast_expression *left)
{
  eat(s, '[');
  add_anchor(s, ']');
  add_anchor(s, ',');

  union ast_expression *right = parse_expression_or_slice(s);
  if (peek(s) == ',') {
    right = parse_expression_list_helper(s, AST_EXPRESSION_LIST, right,
                                         PREC_LIST + 1, /*allow_slices=*/true);
  }

  remove_anchor(s, ',');
  remove_anchor(s, ']');
  expect(s, ']');

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_binexpr, AST_BINEXPR_SUBSCRIPT);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  return expression;
}

static union ast_expression *parse_expr_list(struct parser_state  *s,
                                             union ast_expression *left)
{
  union ast_expression *expression = parse_expression_list_helper(
      s, AST_EXPRESSION_LIST, left, PREC_LIST + 1, /*allow_slices=*/false);
  expression->expression_list.as_constant = ast_tuple_compute_constant(
      &s->cg.objects, &expression->expression_list);
  return expression;
}

static union ast_expression *parse_true_div(struct parser_state  *s,
                                            union ast_expression *left)
{
  return parse_binexpr(s, PREC_TERM + 1, AST_BINEXPR_TRUEDIV, left);
}

static union ast_expression *parse_true_div_assign(struct parser_state  *s,
                                                   union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_TRUEDIV_ASSIGN, left);
}

static union ast_expression *parse_unequal(struct parser_state  *s,
                                           union ast_expression *left)
{
  return parse_binexpr(s, PREC_COMPARISON + 1, AST_BINEXPR_UNEQUAL, left);
}

static union ast_expression *parse_xor(struct parser_state  *s,
                                       union ast_expression *left)
{
  return parse_binexpr(s, PREC_XOR + 1, AST_BINEXPR_XOR, left);
}

static union ast_expression *parse_xor_assign(struct parser_state  *s,
                                              union ast_expression *left)
{
  return parse_binexpr_assign(s, AST_BINEXPR_XOR_ASSIGN, left);
}

typedef union ast_expression *(*postfix_parser_func)(
    struct parser_state *s, union ast_expression *prefix);

struct postfix_expression_parser {
  postfix_parser_func func;
  enum precedence     precedence;
};

static const struct postfix_expression_parser postfix_parsers[] = {
  /* clang-format off */
  ['%']    = { .func = parse_mod,         .precedence = PREC_TERM        },
  ['&']    = { .func = parse_and,         .precedence = PREC_AND         },
  ['(']    = { .func = parse_call,        .precedence = PREC_PRIMARY     },
  ['*']    = { .func = parse_mul,         .precedence = PREC_TERM        },
  ['+']    = { .func = parse_add,         .precedence = PREC_ARITH       },
  [',']    = { .func = parse_expr_list,   .precedence = PREC_LIST        },
  ['-']    = { .func = parse_sub,         .precedence = PREC_ARITH       },
  ['.']    = { .func = parse_attr,        .precedence = PREC_PRIMARY     },
  ['/']    = { .func = parse_true_div,    .precedence = PREC_TERM        },
  ['<']    = { .func = parse_less,        .precedence = PREC_COMPARISON  },
  ['=']    = { .func = parse_assignment,  .precedence = PREC_ASSIGN      },
  ['>']    = { .func = parse_greater,     .precedence = PREC_COMPARISON  },
  ['@']    = { .func = parse_matmul,      .precedence = PREC_TERM        },
  ['[']    = { .func = parse_subscript,   .precedence = PREC_PRIMARY     },
  ['^']    = { .func = parse_xor,         .precedence = PREC_XOR         },
  ['|']    = { .func = parse_or,          .precedence = PREC_OR          },
  [T_and]  = { .func = parse_logical_and, .precedence = PREC_LOGICAL_AND },
  [T_in]   = { .func = parse_in,          .precedence = PREC_COMPARISON  },
  [T_is]   = { .func = parse_is,          .precedence = PREC_COMPARISON  },
  [T_not]  = { .func = parse_not_in,      .precedence = PREC_COMPARISON  },
  [T_or]   = { .func = parse_logical_or,  .precedence = PREC_LOGICAL_OR  },
  [T_AMPERSAND_EQUALS]
    = { .func = parse_and_assign,         .precedence = PREC_ASSIGN     },
  [T_ASTERISK_ASTERISK_EQUALS]
    = { .func = parse_power_assign,       .precedence = PREC_ASSIGN     },
  [T_ASTERISK_ASTERISK]
    = { .func = parse_power,              .precedence = PREC_POWER      },
  [T_ASTERISK_EQUALS]
    = { .func = parse_mul_assign,         .precedence = PREC_ASSIGN     },
  [T_AT_EQUALS]
    = { .func = parse_matmul_assign,      .precedence = PREC_ASSIGN     },
  [T_BAR_EQUALS]
    = { .func = parse_or_assign,          .precedence = PREC_ASSIGN     },
  [T_CARET_EQUALS]
    = { .func = parse_xor_assign,         .precedence = PREC_ASSIGN     },
  [T_EQUALS_EQUALS]
    = { .func = parse_equal,              .precedence = PREC_COMPARISON },
  [T_EXCLAMATIONMARKEQUALS]
    = { .func = parse_unequal,            .precedence = PREC_COMPARISON },
  [T_GREATER_THAN_EQUALS]
    = { .func = parse_greater_equal,      .precedence = PREC_COMPARISON },
  [T_GREATER_THAN_GREATER_THAN_EQUALS]
    = { .func = parse_shift_right_assign, .precedence = PREC_ASSIGN     },
  [ T_GREATER_THAN_GREATER_THAN]
    = { .func = parse_shift_right,        .precedence = PREC_SHIFT      },
  [T_LESS_THAN_EQUALS]
    = { .func = parse_less_equal,         .precedence = PREC_COMPARISON },
  [T_LESS_THAN_LESS_THAN_EQUALS]
    = { .func = parse_shift_left_assign,  .precedence = PREC_ASSIGN     },
  [T_LESS_THAN_LESS_THAN]
    = { .func = parse_shift_left,         .precedence = PREC_ASSIGN     },
  [T_MINUS_EQUALS]
    = { .func = parse_sub_assign,         .precedence = PREC_ASSIGN     },
  [T_PERCENT_EQUALS]
    = { .func = parse_mod_assign,         .precedence = PREC_ASSIGN     },
  [T_PLUS_EQUALS]
    = { .func = parse_add_assign,         .precedence = PREC_ASSIGN     },
  [T_SLASH_EQUALS]
    = { .func = parse_true_div_assign,    .precedence = PREC_ASSIGN     },
  [T_SLASH_SLASH_EQUALS]
    = { .func = parse_floor_div_assign,   .precedence = PREC_ASSIGN     },
  [T_SLASH_SLASH]
    = { .func = parse_floor_div,          .precedence = PREC_TERM       },
  /* clang-format on */
};

union ast_expression *parse_expression(struct parser_state *s,
                                       enum precedence      precedence)
{
  enum token_kind token_kind = peek(s);
  if (token_kind >= sizeof(prefix_parsers) / sizeof(prefix_parsers[0])) {
    error_expected(s, "expression");
    return ast_invalid(s);
  }
  const struct prefix_expression_parser *prefix_parser
      = &prefix_parsers[token_kind];
  if (prefix_parser->func == NULL) {
    error_expected(s, "expression");
    return ast_invalid(s);
  }
  union ast_expression *result = prefix_parser->func(s);

  for (;;) {
    enum token_kind postifx_token_kind = peek(s);
    if (postifx_token_kind
        >= sizeof(postfix_parsers) / sizeof(postfix_parsers[0]))
      break;
    const struct postfix_expression_parser *postfix_parser
        = &postfix_parsers[postifx_token_kind];
    if (postfix_parser->func == NULL
        || postfix_parser->precedence < precedence) {
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

static void parse_assert(struct parser_state *s)
{
  eat(s, T_assert);

  union ast_expression *expression = parse_expression(s, PREC_LIST + 1);
  union ast_expression *message = NULL;
  if (accept(s, ',')) {
    message = parse_expression(s, PREC_LIST + 1);
  }

  emit_assert(&s->cg, expression, message);
}

static void parse_break(struct parser_state *s)
{
  if (!emit_break(&s->cg)) {
    struct location location = scanner_location(&s->scanner);
    diag_begin_error(&s->d, location);
    diag_frag(&s->d, "'break' outside loop");
    diag_end(&s->d);
  }
  eat(s, T_break);
}

static void parse_continue(struct parser_state *s)
{
  if (!emit_continue(&s->cg)) {
    struct location location = scanner_location(&s->scanner);
    diag_begin_error(&s->d, location);
    diag_frag(&s->d, "'continue' outside loop");
    diag_end(&s->d);
  }
  eat(s, T_continue);
}

static void parse_del(struct parser_state *s)
{
  eat(s, T_del);
  struct location       location = scanner_location(&s->scanner);
  union ast_expression *expression = parse_expression(s, PREC_LIST);
  if (check_assignment_target(s, expression, location, /*is_del=*/true)) {
    return;
  }
  emit_del(&s->cg, expression);
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

    struct symbol **ptr = (struct symbol **)arena_grow(&s->ast, sizeof(*ptr));
    *ptr = symbol;
    ++num_symbols;
  } while (accept(s, T_DOT));

  struct dotted_name *result = arena_grow_finish(&s->ast);
  result->num_symbols = num_symbols;
  return result;
}

static void parse_from_import_statement(struct parser_state *s)
{
  eat(s, T_from);

  unsigned num_prefix_dots = 0;
  while (accept(s, '.')) {
    num_prefix_dots++;
  }

  struct dotted_name *module = NULL;
  if (num_prefix_dots == 0 || peek(s) == T_IDENTIFIER) {
    module = parse_dotted_name(s);
  }

  struct from_import_item inline_storage[16];
  struct idynarray        pairs;
  idynarray_init(&pairs, inline_storage, sizeof(inline_storage));

  expect(s, T_import);

  if (accept(s, '*')) {
    emit_from_import_star_statement(&s->cg, num_prefix_dots, module);
    return;
  }

  bool braced = accept(s, '(');
  for (;;) {
    struct symbol *name;
    if (skip_till(s, T_IDENTIFIER)) {
      name = eat_identifier(s);
    } else {
      name = symbol_invalid(s);
    }
    struct symbol *as = NULL;
    if (accept(s, T_as)) {
      if (skip_till(s, T_IDENTIFIER)) {
        as = eat_identifier(s);
      } else {
        as = symbol_invalid(s);
      }
    }
    struct from_import_item *item
        = idynarray_append(&pairs, struct from_import_item);
    item->name = name;
    item->as = as;

    if (!accept(s, ',')) break;
    if (braced && peek(s) == ')') break;
  }

  if (braced) expect(s, ')');

  unsigned num_pairs = idynarray_length(&pairs, struct from_import_item);
  emit_from_import_statement(&s->cg, num_prefix_dots, module, num_pairs,
                             idynarray_data(&pairs));
}

static void parse_import_statement(struct parser_state *s)
{
  eat(s, T_import);

  do {
    struct dotted_name *dotted_name = parse_dotted_name(s);
    struct symbol      *as = NULL;
    if (accept(s, T_as) && skip_till(s, T_IDENTIFIER)) {
      as = eat_identifier(s);
    }
    emit_import_statement(&s->cg, dotted_name, as);
  } while (accept(s, ','));
}

static void parse_pass(struct parser_state *s)
{
  eat(s, T_pass);
}

static void parse_raise(struct parser_state *s)
{
  eat(s, T_raise);

  union ast_expression *expression = parse_expression(s, PREC_LAMBDA);
  union ast_expression *from = NULL;
  if (accept(s, T_from)) {
    from = parse_expression(s, PREC_LAMBDA);
  }
  emit_raise_statement(&s->cg, expression, from);
}

static void parse_return(struct parser_state *s)
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

static void parse_yield(struct parser_state *s)
{
  eat(s, T_yield);

  if (accept(s, T_from)) {
    union ast_expression *expression = parse_expression(s, PREC_LIST);
    emit_yield_from_statement(&s->cg, expression);
  } else {
    union ast_expression *expression = parse_expression(s, PREC_LIST);
    emit_yield_statement(&s->cg, expression);
  }
}

static void parse_small_statement(struct parser_state *s)
{
  switch (peek(s)) {
  case EXPRESSION_START_CASES:
    parse_expression_statement(s);
    break;
  case T_assert:
    parse_assert(s);
    break;
  case T_break:
    parse_break(s);
    break;
  case T_continue:
    parse_continue(s);
    break;
  case T_del:
    parse_del(s);
    break;
  case T_from:
    parse_from_import_statement(s);
    break;
  case T_import:
    parse_import_statement(s);
    break;
  case T_pass:
    parse_pass(s);
    break;
  case T_raise:
    parse_raise(s);
    break;
  case T_return:
    parse_return(s);
    break;
  case T_yield:
    parse_yield(s);
    break;
  default:
    error_expected(s, "statement");
    eat_until_anchor(s);
  }
}

static void parse_simple_statement(struct parser_state *s)
{
  add_anchor(s, T_NEWLINE);
  do {
    parse_small_statement(s);
  } while (accept(s, ';') && peek(s) != T_NEWLINE);
  remove_anchor(s, T_NEWLINE);
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
    } while (!accept(s, T_DEDENT));
  } else {
    parse_simple_statement(s);
    assert(s->cg.code.stacksize == prev_stacksize);
  }
}

static void parse_type_parameters(struct parser_state *s)
{
  if (accept(s, '[')) {
    unimplemented();
  }
}

static void parse_class(struct parser_state *s, unsigned num_decorators)
{
  eat(s, T_class);
  if (!skip_till(s, T_IDENTIFIER)) return;
  struct symbol *name = eat_identifier(s);

  parse_type_parameters(s);

  union ast_expression *call;
  if (peek(s) == '(') {
    call = parse_argument_list(s, /*callee=*/NULL);
  } else {
    call = ast_allocate_expression_(s, sizeof(struct ast_call), AST_CALL);
    call->call.has_star_argument = false;
    call->call.has_kw_argument = false;
    call->call.callee = NULL;
    call->call.num_arguments = 0;
  }

  emit_class_begin(&s->cg, name);

  expect(s, ':');
  parse_suite(s);

  emit_class_end(&s->cg, name, &call->call, num_decorators);
}

static void parse_def_parameters(struct parser_state *s,
                                 struct def_state    *state)
{
  expect(s, '(');
  add_anchor(s, ')');

  struct parameter inline_storage[16];
  struct idynarray parameters;
  idynarray_init(&parameters, inline_storage, sizeof(inline_storage));

  unsigned positional_only_argcount = 0;
  bool     had_default = false;
  bool     had_variable_args = false;
  bool     had_variable_keyword_args = false;

  for (;;) {
    enum parameter_type type = PARAMETER_NORMAL;
    switch (peek(s)) {
    case ')':
      break;
    case '/':
      if (positional_only_argcount != 0) {
        diag_begin_error(&s->d, scanner_location(&s->scanner));
        diag_token_kind(&s->d, '/');
        diag_frag(&s->d, " may appear only once");
        diag_end(&s->d);
      } else if (had_variable_args || had_variable_keyword_args) {
        diag_begin_error(&s->d, scanner_location(&s->scanner));
        diag_token_kind(&s->d, '/');
        diag_frag(&s->d, " must be ahead of ");
        diag_token_kind(&s->d, had_variable_args ? '*' : T_ASTERISK_ASTERISK);
        diag_end(&s->d);
      } else {
        positional_only_argcount
            = idynarray_length(&parameters, struct parameter);
        if (positional_only_argcount == 0) {
          diag_begin_error(&s->d, scanner_location(&s->scanner));
          diag_frag(&s->d, "at least one argument must precede ");
          diag_token_kind(&s->d, '/');
          diag_end(&s->d);
        }
      }
      eat(s, '/');
      break;
    case '*':
      if (had_variable_args) {
        diag_begin_error(&s->d, scanner_location(&s->scanner));
        diag_token_kind(&s->d, '*');
        diag_frag(&s->d, " argument may appear only once");
        diag_end(&s->d);
      } else {
        type = PARAMETER_STAR;
        had_variable_args = true;
      }
      eat(s, '*');
      if (peek(s) != T_IDENTIFIER) {
        /* TODO: report error if there isn't at least one more
         * parameter (that is not **kwargs) following. */
        break;
      }
      goto parameter;
    case T_ASTERISK_ASTERISK:
      if (had_variable_keyword_args) {
        diag_begin_error(&s->d, scanner_location(&s->scanner));
        diag_token_kind(&s->d, T_ASTERISK_ASTERISK);
        diag_frag(&s->d, " argument may appear only once");
        diag_end(&s->d);
      } else {
        type = PARAMETER_STAR_STAR;
        had_variable_keyword_args = true;
      }
      /* TODO: report error when any other parameters follow ** */
      eat(s, T_ASTERISK_ASTERISK);
      goto parameter;
    case T_EOF:
      break;
    default:
    parameter:
      if (!skip_till(s, T_IDENTIFIER)) break;
      struct location location = scanner_location(&s->scanner);
      struct symbol  *name = eat_identifier(s);

      union ast_expression *initializer = NULL;
      if (accept(s, '=')) {
        struct location location = scanner_location(&s->scanner);
        initializer = parse_expression(s, PREC_LIST + 1);
        had_default = true;
        if (type != PARAMETER_NORMAL) {
          diag_begin_error(&s->d, location);
          diag_frag(&s->d,
                    "variable argument parameter cannot have a default value");
          diag_end(&s->d);
          initializer = NULL;
        }
      } else if (had_default && !had_variable_args
                 && type != PARAMETER_STAR_STAR) {
        diag_begin_error(&s->d, location);
        diag_frag(&s->d,
                  "parameter without default follows parameter with default");
        diag_end(&s->d);
      }

      bool              error_duplicate = false;
      struct parameter *parr = idynarray_data(&parameters);
      for (unsigned p = 0, e = idynarray_length(&parameters, struct parameter);
           p < e; p++) {
        if (parr[p].name == name) {
          diag_begin_error(&s->d, location);
          diag_frag(&s->d, "duplicate argument ");
          diag_symbol(&s->d, name);
          diag_end(&s->d);
          error_duplicate = true;
        }
      }

      if (!error_duplicate) {
        struct parameter *parameter
            = idynarray_append(&parameters, struct parameter);
        parameter->name = name;
        parameter->initializer = initializer;
        parameter->type = type;
      }
      break;
    }
    if (peek(s) == ')' || peek(s) == T_EOF) break;
    expect(s, ',');
  }

  remove_anchor(s, ')');
  expect(s, ')');

  unsigned num_parameters = idynarray_length(&parameters, struct parameter);
  emit_def_begin(&s->cg, state, num_parameters, idynarray_data(&parameters),
                 positional_only_argcount);

  idynarray_free(&parameters);
}

static void parse_def(struct parser_state *s, unsigned num_decorators)
{
  eat(s, T_def);
  if (!skip_till(s, T_IDENTIFIER)) return;
  struct symbol *name = eat_identifier(s);

  struct def_state state;

  parse_def_parameters(s, &state);
  if (accept(s, T_MINUS_GREATER_THAN)) {
    parse_expression(s, PREC_TEST);
    unimplemented();
  }
  expect(s, ':');

  parse_suite(s);

  emit_def_end(&s->cg, &state, name, num_decorators);
}

static void parse_decorator(struct parser_state *s, unsigned num_decorators)
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
    error_expected(s, "@, class or def after decorator");
    return;
  }
}

static void parse_for(struct parser_state *s)
{
  eat(s, T_for);
  union ast_expression *target = parse_expression(s, PREC_OR);
  if (peek(s) == ',') {
    target = parse_expression_list_helper(s, AST_EXPRESSION_LIST, target,
                                          PREC_OR, /*allow_slices=*/false);
  }
  expect(s, T_in);
  union ast_expression *expression = parse_expression(s, PREC_TEST);
  expect(s, ':');

  struct for_while_state state;
  emit_for_begin(&s->cg, &state, target, expression);

  parse_suite(s);

  emit_for_else(&s->cg, &state);
  if (accept(s, T_else)) {
    expect(s, ':');
    parse_suite(s);
  }

  emit_for_end(&s->cg, &state);
}

static void parse_if(struct parser_state *s)
{
  eat(s, T_if);
  union ast_expression *expression = parse_expression(s, PREC_TEST);
  expect(s, ':');

  struct if_state state;
  emit_if_begin(&s->cg, &state, expression);

  parse_suite(s);

  while (accept(s, T_elif)) {
    union ast_expression *expression = parse_expression(s, PREC_TEST);
    expect(s, ':');

    emit_if_elif(&s->cg, &state, expression);
    parse_suite(s);
  }

  if (accept(s, T_else)) {
    expect(s, ':');

    emit_if_else(&s->cg, &state);
    parse_suite(s);
  }
  emit_if_end(&s->cg, &state);
}

static void parse_try(struct parser_state *s)
{
  eat(s, T_try);
  expect(s, ':');

  struct try_state state;
  emit_try_body_begin(&s->cg, &state);
  parse_suite(s);
  emit_try_body_end(&s->cg, &state);

  bool had_except = false;
  while (accept(s, T_except)) {
    union ast_expression *match = NULL;
    struct symbol        *as = NULL;
    if (peek(s) != ':') {
      match = parse_expression(s, PREC_TEST);
      if (accept(s, T_as) && skip_till(s, T_IDENTIFIER)) {
        as = eat_identifier(s);
      }
    }
    expect(s, ':');

    emit_try_except_begin(&s->cg, &state, match, as);
    parse_suite(s);
    emit_try_except_end(&s->cg, &state, as);
    had_except = true;
  }

  if (accept(s, T_else)) {
    if (!had_except) {
      diag_begin_error(&s->d, scanner_location(&s->scanner));
      diag_token_kind(&s->d, T_try);
      diag_frag(&s->d, " with ");
      diag_token_kind(&s->d, T_else);
      diag_frag(&s->d, " requires prior ");
      diag_token_kind(&s->d, T_except);
      diag_end(&s->d);
    }
    expect(s, ':');

    emit_try_else_begin(&s->cg, &state);
    parse_suite(s);
    emit_try_else_end(&s->cg, &state);
  }

  if (accept(s, T_finally)) {
    expect(s, ':');

    emit_try_finally_begin(&s->cg, &state);
    parse_suite(s);
    emit_try_finally_end(&s->cg, &state);
  }

  while (peek(s) == T_except || peek(s) == T_else) {
    diag_begin_error(&s->d, scanner_location(&s->scanner));
    diag_token_kind(&s->d, peek(s));
    diag_frag(&s->d, " must come before ");
    diag_token_kind(&s->d, T_finally);
    diag_end(&s->d);
    next_token(s);

    eat_until_anchor(s);
  }

  emit_try_end(&s->cg, &state);
}

static void parse_while(struct parser_state *s)
{
  eat(s, T_while);
  union ast_expression *expression = parse_expression(s, PREC_TEST);
  expect(s, ':');

  struct for_while_state state;
  emit_while_begin(&s->cg, &state, expression);

  parse_suite(s);

  emit_while_else(&s->cg, &state);
  if (accept(s, T_else)) {
    expect(s, ':');
    parse_suite(s);
  }

  emit_while_end(&s->cg, &state);
}

static void parse_with(struct parser_state *s)
{
  eat(s, T_with);

  struct with_state inline_storage[4];
  struct idynarray  cleanup_blocks;
  idynarray_init(&cleanup_blocks, inline_storage, sizeof(inline_storage));

  do {
    union ast_expression *expression = parse_expression(s, PREC_TEST);
    union ast_expression *target = NULL;
    if (accept(s, T_as)) {
      target = parse_expression(s, PREC_LIST + 1);
    }

    struct with_state *state
        = idynarray_append(&cleanup_blocks, struct with_state);
    emit_with_begin(&s->cg, state, expression, target);
  } while (accept(s, ','));
  expect(s, ':');

  unsigned num_cleanup_blocks
      = idynarray_length(&cleanup_blocks, struct with_state);
  struct with_state *cleanup_blocks_arr = idynarray_data(&cleanup_blocks);

  parse_suite(s);

  for (unsigned i = num_cleanup_blocks; i-- > 0;) {
    emit_with_end(&s->cg, &cleanup_blocks_arr[i]);
  }
  idynarray_free(&cleanup_blocks);
}

static void parse_statement(struct parser_state *s)
{
  add_anchor(s, T_NEWLINE);
  switch (peek(s)) {
  case '@':
    parse_decorator(s, /*num_decorators=*/0);
    break;
  case T_class:
    parse_class(s, /*num_decorators=*/0);
    break;
  case T_def:
    parse_def(s, /*num_decorators=*/0);
    break;
  case T_for:
    parse_for(s);
    break;
  case T_if:
    parse_if(s);
    break;
  case T_try:
    parse_try(s);
    break;
  case T_while:
    parse_while(s);
    break;
  case T_with:
    parse_with(s);
    break;
  case T_EOF:
    break;
  default:
    parse_simple_statement(s);
    break;
  }
  remove_anchor(s, T_NEWLINE);
}

union object *parse(struct parser_state *s, const char *filename)
{
  diag_init(&s->d, stderr, filename);
  cg_init(&s->cg, s->scanner.symbol_table, filename);
  emit_module_begin(&s->cg);

  next_token(s);

  add_anchor(s, T_EOF);
  while (peek(s) != T_EOF) {
    if (accept(s, T_NEWLINE)) continue;
    parse_statement(s);
    assert(s->cg.code.stacksize == 0);
  }

#ifndef NDEBUG
  remove_anchor(s, T_EOF);
  for (uint16_t i = 0; i < sizeof(s->anchor_set) / sizeof(s->anchor_set[0]);
       ++i) {
    if (s->anchor_set[i] != 0) {
      fprintf(stderr, "Internal error: Anchor for token %s not removed\n",
              token_kind_name(i));
      abort();
    }
  }
#endif

  return emit_module_end(&s->cg);
}

bool parser_had_errors(struct parser_state *s)
{
  return s->d.had_error;
}

void parser_init(struct parser_state *s)
{
  memset(s, 0, sizeof(*s));
  arena_init(&s->ast);
  memset(s->anchor_set, 0, sizeof(s->anchor_set));
}

void parser_free(struct parser_state *s)
{
  cg_free(&s->cg);
  arena_free(&s->ast);
}

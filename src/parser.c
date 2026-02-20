#include "pycomparse/parser.h"

#include <assert.h>
#include <stdalign.h>
#include <stdlib.h>
#include <string.h>

#include "pycomparse/adt/arena.h"
#include "pycomparse/adt/idynarray.h"
#include "pycomparse/ast.h"
#include "pycomparse/ast_expression_types.h"
#include "pycomparse/ast_statement_types.h"
#include "pycomparse/ast_types.h"
#include "pycomparse/diagnostics.h"
#include "pycomparse/object.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/object_types.h"
#include "pycomparse/opcodes.h"
#include "pycomparse/parser_types.h"
#include "pycomparse/scanner.h"
#include "pycomparse/symbol_table.h"
#include "pycomparse/symbol_types.h"
#include "pycomparse/token_kinds.h"
#include "pycomparse/util.h"

#define UNLIKELY(x) __builtin_expect((x), 0)

enum precedence {
  PREC_INVALID,
  PREC_ASSIGN,      /* +=, -=, ... */
  PREC_NAMED,       /* := */
  PREC_TEST,        /* postfix 'if' _ 'else' _ */
  PREC_LOGICAL_OR,  /* OR */
  PREC_LOGICAL_AND, /* AND */
  PREC_COMPARISON,  /* <, >, ==, >=, <=, <>, !=, in, not in, is, is not */

  PREC_OR,    /* | */
  PREC_XOR,   /* ^ */
  PREC_AND,   /* & */
  PREC_SHIFT, /* <<, >> */
  PREC_ARITH, /* +, - */
  PREC_TERM,  /* *, @, /, %, // */

  PREC_FACTOR, /* prefix +, -, ~ */
  PREC_POWER,  /* ** */
  PREC_AWAIT,  /* await */

  PREC_PRIMARY, /* .attr  [subscript]   (call) */
  PREC_ATOM,    /* name, number, string, ..., None, True, False */

  /* PREC_ASSIGN are only used in some instances, while
   * most contexts should start with this. */
  PREC_EXPRESSION = PREC_TEST,
};

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
  (void)token_kind;
  assert(token_kind == T_STRING || token_kind == T_INTEGER
         || token_kind == T_FLOAT || token_kind == T_FSTRING_START
         || token_kind == T_FSTRING_FRAGMENT || token_kind == T_FSTRING_END);
  assert(peek(s) == token_kind);
  return s->scanner.token.u.object;
}

static inline bool peek_string_is_fstring(const struct parser_state *s)
{
  return peek(s) == T_STRING && s->scanner.token.string_is_fstring;
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
    if (peek(s) == '(' || peek(s) == '{' || peek(s) == '[') {
      eat_until_matching_token(s, peek(s));
    }
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

static void error_expected(struct parser_state *s, const char *what)
{
  if (peek(s) == T_INVALID) return;
  diag_begin_error(s->d, scanner_location(&s->scanner));
  diag_frag(s->d, "expected ");
  diag_frag(s->d, what);
  diag_frag(s->d, ", got ");
  diag_token(s->d, &s->scanner.token);
  diag_end(s->d);
}

static void error_expected_tok1(struct parser_state *s, enum token_kind kind)
{
  if (peek(s) == T_INVALID) return;
  diag_begin_error(s->d, scanner_location(&s->scanner));
  diag_frag(s->d, "expected ");
  diag_token_kind(s->d, kind);
  diag_frag(s->d, ", got ");
  diag_token(s->d, &s->scanner.token);
  diag_end(s->d);
}

static void error_expected_tok2(struct parser_state *s, enum token_kind kind0,
                                enum token_kind kind1)
{
  if (peek(s) == T_INVALID) return;
  diag_begin_error(s->d, scanner_location(&s->scanner));
  diag_frag(s->d, "expected ");
  diag_token_kind(s->d, kind0);
  diag_frag(s->d, " or ");
  diag_token_kind(s->d, kind1);
  diag_frag(s->d, ", got ");
  diag_token(s->d, &s->scanner.token);
  diag_end(s->d);
}

static bool skip_till(struct parser_state *s,
                      enum token_kind      expected_token_kind)
{
  if (UNLIKELY(peek(s) != expected_token_kind)) {
    error_expected_tok1(s, expected_token_kind);

    add_anchor(s, expected_token_kind);
    eat_until_anchor(s);
    remove_anchor(s, expected_token_kind);
    if (peek(s) != expected_token_kind) {
      return false;
    }
  }
  return true;
}

static void expect(struct parser_state *s, enum token_kind expected_token_kind)
{
  if (skip_till(s, expected_token_kind)) {
    eat(s, expected_token_kind);
  }
}

static bool await_tracking_begin(struct parser_state *s)
{
  bool saved = s->parsed_await_expression;
  s->parsed_await_expression = false;
  return saved;
}

static bool await_tracking_end(struct parser_state *s, bool saved)
{
  bool parsed_await_expression = s->parsed_await_expression;
  s->parsed_await_expression = saved || parsed_await_expression;
  return parsed_await_expression;
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

static union ast_statement *
ast_allocate_statement_(struct parser_state *s, size_t size,
                        enum ast_statement_type type, struct location location)
{
  union ast_statement *statement = (union ast_statement *)arena_allocate(
      &s->ast, size, alignof(union ast_statement));
  memset(statement, 0, size);
  statement->type = type;
  statement->base.location = location;
  return statement;
}

#define ast_allocate_statement(s, type, type_id, location)                    \
  ast_allocate_statement_((s), sizeof(type), (type_id), (location))

static struct ast_statement_list *
ast_statement_list_from_array(struct parser_state  *s,
                              union ast_statement **statements,
                              unsigned              num_statements)
{
  size_t statements_size = num_statements * sizeof(union ast_statement *);
  struct ast_statement_list *result
      = (struct ast_statement_list *)arena_allocate(
          &s->ast, sizeof(struct ast_statement_list) + statements_size,
          alignof(struct ast_statement_list));
  result->num_statements = num_statements;
  if (num_statements > 0) {
    memcpy(result->statements, statements, statements_size);
  }
  return result;
}

static union ast_expression **
ast_expression_array_copy(struct parser_state   *s,
                          union ast_expression **expressions,
                          unsigned               num_expressions)
{
  if (num_expressions == 0) return NULL;
  size_t size = num_expressions * sizeof(union ast_expression *);
  union ast_expression **result = (union ast_expression **)arena_allocate(
      &s->ast, size, alignof(union ast_expression *));
  memcpy(result, expressions, size);
  return result;
}

static union ast_expression **
ast_expression_array_copy_dyn(struct parser_state *s, struct idynarray *array)
{
  unsigned num_expressions = idynarray_length(array, union ast_expression *);
  union ast_expression **expressions = idynarray_data(array);
  return ast_expression_array_copy(s, expressions, num_expressions);
}

static union ast_expression *ast_const_new(struct parser_state *s,
                                           union object        *object,
                                           struct location      location)
{
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_const, AST_CONST);
  expression->cnst.base.location = location;
  expression->cnst.object = object;
  return expression;
}

static union ast_expression *invalid_expression(struct parser_state *s)
{
  assert(s->d->had_error);
  struct location       location = scanner_location(&s->scanner);
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_const, AST_INVALID);
  expression->cnst.base.location = location;
  expression->cnst.object = object_intern_singleton(s->objects, OBJECT_NONE);
  return expression;
}

static struct symbol *invalid_symbol(struct parser_state *s)
{
  assert(s->d->had_error);
  return symbol_table_get_or_insert(s->scanner.symbol_table, "<invalid>");
}

static struct symbol *maybe_mangle_private_name(struct parser_state *s,
                                                struct symbol       *symbol)
{
  struct symbol *class_name = s->private_class_name;
  if (class_name == NULL) {
    return symbol;
  }

  const char *identifier = symbol->string;
  if (identifier[0] != '_' || identifier[1] != '_') {
    return symbol;
  }

  size_t identifier_length = strlen(identifier);
  if (identifier_length < 3) {
    return symbol;
  }
  if (identifier[identifier_length - 1] == '_'
      && identifier[identifier_length - 2] == '_') {
    return symbol;
  }

  const char *class_identifier = class_name->string;
  while (*class_identifier == '_') {
    ++class_identifier;
  }
  if (*class_identifier == '\0') {
    return symbol;
  }

  size_t class_identifier_length = strlen(class_identifier);
  size_t mangled_length = 1 + class_identifier_length + identifier_length;
  char  *mangled = arena_allocate(&s->ast, mangled_length + 1, 1);
  mangled[0] = '_';
  memcpy(mangled + 1, class_identifier, class_identifier_length);
  memcpy(mangled + 1 + class_identifier_length, identifier,
         identifier_length + 1);
  return symbol_table_get_or_insert(s->scanner.symbol_table, mangled);
}

static struct symbol *parse_identifier(struct parser_state *s)
{
  if (!skip_till(s, T_IDENTIFIER)) {
    return invalid_symbol(s);
  }
  return maybe_mangle_private_name(s, eat_identifier(s));
}

static union ast_expression *parse_expression(struct parser_state *s,
                                              enum precedence      precedence);
static bool                  is_expression_start(enum token_kind token_kind);

static inline union ast_expression *parse_unexpr(struct parser_state *s,
                                                 enum precedence      prec_op,
                                                 enum ast_expression_type type)
{
  struct location location = scanner_location(&s->scanner);
  next_token(s);
  union ast_expression *op = parse_expression(s, prec_op);

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_unexpr, type);
  expression->unexpr.base.location = location;
  expression->unexpr.op = op;
  return expression;
}

static union ast_expression *parse_expression_or_slice(struct parser_state *s)
{
  union ast_expression *expression = NULL;
  if (peek(s) != ':') {
    expression = parse_expression(s, PREC_NAMED);
    if (peek(s) != ':') {
      return expression;
    }
  }
  eat(s, ':');

  union ast_expression *start = expression;
  union ast_expression *stop = NULL;
  union ast_expression *step = NULL;
  if (is_expression_start(peek(s))) {
    stop = parse_expression(s, PREC_NAMED);
  }
  if (accept(s, ':') && is_expression_start(peek(s))) {
    step = parse_expression(s, PREC_NAMED);
  }

  expression = ast_allocate_expression(s, struct ast_slice, AST_SLICE);
  expression->slice.start = start;
  expression->slice.stop = stop;
  expression->slice.step = step;
  return expression;
}

static union ast_expression *parse_expression_list_helper(
    struct parser_state *s, enum ast_expression_type type,
    union ast_expression *first, enum precedence precedence, bool allow_slices,
    bool allow_starred)
{
  union ast_expression *inline_storage[16];
  struct idynarray      expressions;
  idynarray_init(&expressions, inline_storage, sizeof(inline_storage));

  *(idynarray_append(&expressions, union ast_expression *)) = first;

  bool has_star_expression = ast_expression_type(first) == AST_UNEXPR_STAR;
  for (;;) {
    if (!accept(s, ',')) break;
    if (!is_expression_start(peek(s)) && (peek(s) != ':' || !allow_slices)
        && (peek(s) != '*' || !allow_starred)) {
      break;
    }
    union ast_expression *expression;
    if (allow_slices) {
      expression = parse_expression_or_slice(s);
    } else if (allow_starred && peek(s) == '*') {
      expression = parse_unexpr(s, PREC_OR, AST_UNEXPR_STAR);
      has_star_expression = true;
    } else {
      expression = parse_expression(s, precedence);
    }
    *(idynarray_append(&expressions, union ast_expression *)) = expression;
  }

  unsigned num_expressions
      = idynarray_length(&expressions, union ast_expression *);
  size_t expressions_size = num_expressions * sizeof(union ast_expression *);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_expression_list) + expressions_size, type);
  expression->expression_list.base.location = get_expression_location(first);
  expression->expression_list.has_star_expression = has_star_expression;
  expression->expression_list.num_expressions = num_expressions;
  memcpy(expression->expression_list.expressions, idynarray_data(&expressions),
         expressions_size);
  idynarray_free(&expressions);
  return expression;
}

static union ast_expression *parse_star_expression(struct parser_state *s,
                                                   enum precedence precedence)
{
  if (peek(s) == '*') {
    return parse_unexpr(s, PREC_OR, AST_UNEXPR_STAR);
  }
  return parse_expression(s, precedence);
}

static union ast_expression *parse_star_expressions(struct parser_state *s,
                                                    enum precedence precedence)
{
  union ast_expression *expression = parse_star_expression(s, precedence);
  if (peek(s) == ',') {
    expression = parse_expression_list_helper(
        s, AST_EXPRESSION_LIST, expression, precedence, /*allow_slices=*/false,
        /*allow_starred=*/true);
    assert(ast_expression_type(expression) == AST_EXPRESSION_LIST);
    expression->expression_list.as_constant
        = ast_tuple_compute_constant(s->objects, &expression->expression_list);
  }
  return expression;
}

static union ast_expression *
parse_generator_expression(struct parser_state     *s,
                           enum ast_expression_type type)
{
  struct location location = scanner_location(&s->scanner);
  assert(type == AST_GENERATOR_EXPRESSION || type == AST_LIST_COMPREHENSION
         || type == AST_SET_COMPREHENSION || type == AST_DICT_COMPREHENSION);
  struct generator_expression_part inline_storage[4];
  struct idynarray                 parts;
  idynarray_init(&parts, inline_storage, sizeof(inline_storage));
  bool is_async = false;

  while (peek(s) == T_for || peek(s) == T_if || peek(s) == T_async) {
    struct generator_expression_part *part
        = idynarray_append(&parts, struct generator_expression_part);
    if (peek(s) == T_for) {
      eat(s, T_for);
      union ast_expression *targets = parse_star_expressions(s, PREC_OR);
      expect(s, T_in);
      bool                  await_saved = await_tracking_begin(s);
      union ast_expression *expression = parse_expression(s, PREC_OR);

      part->type = GENERATOR_EXPRESSION_PART_FOR;
      part->async = false;
      part->targets = targets;
      part->expression = expression;
      is_async |= await_tracking_end(s, await_saved);
    } else if (peek(s) == T_async) {
      eat(s, T_async);
      expect(s, T_for);
      union ast_expression *targets = parse_star_expressions(s, PREC_OR);
      expect(s, T_in);
      bool                  await_saved = await_tracking_begin(s);
      union ast_expression *expression = parse_expression(s, PREC_OR);

      part->type = GENERATOR_EXPRESSION_PART_FOR;
      part->async = true;
      part->targets = targets;
      part->expression = expression;
      is_async = true;
      is_async |= await_tracking_end(s, await_saved);
    } else {
      eat(s, T_if);
      bool                  await_saved = await_tracking_begin(s);
      union ast_expression *expression = parse_expression(s, PREC_LOGICAL_OR);

      part->type = GENERATOR_EXPRESSION_PART_IF;
      part->async = false;
      part->targets = NULL;
      part->expression = expression;
      is_async |= await_tracking_end(s, await_saved);
    }
  }

  unsigned num_parts
      = idynarray_length(&parts, struct generator_expression_part);
  size_t parts_size = num_parts * sizeof(struct generator_expression_part);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_generator_expression) + parts_size, type);
  expression->generator_expression.base.location = location;
  expression->generator_expression.num_parts = num_parts;
  expression->generator_expression.is_async = is_async;
  memcpy(expression->generator_expression.parts, idynarray_data(&parts),
         parts_size);
  idynarray_free(&parts);
  return expression;
}

static void
set_generator_expression_item(union ast_expression *expression,
                              union ast_expression *item, bool item_has_await,
                              union ast_expression *nullable item_value,
                              bool item_value_has_await)
{
  struct ast_generator_expression *generator
      = &expression->generator_expression;
  generator->expression = item;
  generator->item_value = item_value;
  if (item_has_await || item_value_has_await) {
    generator->is_async = true;
  }
}

static struct argument *parse_argument(struct parser_state *s,
                                       struct argument     *argument)
{
  bool                  await_saved = await_tracking_begin(s);
  union ast_expression *expression;
  if (peek(s) == '*') {
    expression = parse_unexpr(s, PREC_EXPRESSION, AST_UNEXPR_STAR);
  } else if (peek(s) == T_ASTERISK_ASTERISK) {
    expression = parse_unexpr(s, PREC_EXPRESSION, AST_UNEXPR_STAR_STAR);
  } else {
    expression = parse_expression(s, PREC_NAMED);
  }
  struct symbol *name = NULL;
  if (accept(s, '=')) {
    if (expression->type == AST_IDENTIFIER) {
      name = expression->identifier.symbol;
    } else {
      diag_begin_error(s->d, scanner_location(&s->scanner));
      diag_frag(s->d, "assignment not allowed, perhaps you meant '=='?");
      diag_end(s->d);
    }
    expression = parse_expression(s, PREC_NAMED);
  }
  bool expression_has_await = await_tracking_end(s, await_saved);

  if (peek(s) == T_for || peek(s) == T_async) {
    union ast_expression *item = expression;
    expression = parse_generator_expression(s, AST_GENERATOR_EXPRESSION);
    set_generator_expression_item(expression, item, expression_has_await,
                                  /*item_value=*/NULL,
                                  /*item_value_has_await=*/false);
  }

  argument->name = name;
  argument->expression = expression;
  return argument;
}

static union ast_expression *parse_arguments(struct parser_state  *s,
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
  idynarray_free(&arguments);
  return expression;
}

static void parse_parameters(struct parser_state    *s,
                             struct idynarray       *parameters,
                             struct parameter_shape *parameter_shape,
                             enum token_kind         end)
{
  unsigned        positional_only_argcount = 0;
  unsigned        keyword_only_begin = (unsigned)-1;
  bool            had_default = false;
  bool            had_variable_args = false;
  bool            had_variable_keyword_args = false;
  bool            need_kwonly_after_bare_star = false;
  struct location bare_star_location = { 0 };

  add_anchor(s, end);

  for (;;) {
    enum parameter_variant variant = PARAMETER_NORMAL;
    switch (peek(s)) {
    case '/':
      if (positional_only_argcount != 0) {
        diag_begin_error(s->d, scanner_location(&s->scanner));
        diag_token_kind(s->d, '/');
        diag_frag(s->d, " may appear only once");
        diag_end(s->d);
      } else if (had_variable_args || had_variable_keyword_args) {
        diag_begin_error(s->d, scanner_location(&s->scanner));
        diag_token_kind(s->d, '/');
        diag_frag(s->d, " must be ahead of ");
        diag_token_kind(s->d, had_variable_args ? '*' : T_ASTERISK_ASTERISK);
        diag_frag(s->d, "-parameter");
        diag_end(s->d);
      } else {
        positional_only_argcount
            = idynarray_length(parameters, struct parameter);
        if (positional_only_argcount == 0) {
          diag_begin_error(s->d, scanner_location(&s->scanner));
          diag_frag(s->d, "need at least one argument before ");
          diag_token_kind(s->d, '/');
          diag_end(s->d);
        }
      }
      eat(s, '/');
      break;
    case '*': {
      unsigned num_parameters_before_star
          = idynarray_length(parameters, struct parameter);
      if (had_variable_args) {
        diag_begin_error(s->d, scanner_location(&s->scanner));
        diag_token_kind(s->d, '*');
        diag_frag(s->d, " argument may appear only once");
        diag_end(s->d);
      } else {
        variant = PARAMETER_STAR;
        had_variable_args = true;
        keyword_only_begin = num_parameters_before_star + 1;
      }
      struct location star_location = scanner_location(&s->scanner);
      eat(s, '*');
      if (peek(s) != T_IDENTIFIER) {
        keyword_only_begin = num_parameters_before_star;
        need_kwonly_after_bare_star = true;
        bare_star_location = star_location;
        break;
      }
      goto parameter;
    }
    case T_ASTERISK_ASTERISK:
      if (need_kwonly_after_bare_star) {
        diag_begin_error(s->d, bare_star_location);
        diag_frag(s->d, "named arguments must follow bare *");
        diag_end(s->d);
        need_kwonly_after_bare_star = false;
      }
      if (had_variable_keyword_args) {
        diag_begin_error(s->d, scanner_location(&s->scanner));
        diag_token_kind(s->d, T_ASTERISK_ASTERISK);
        diag_frag(s->d, " argument may appear only once");
        diag_end(s->d);
      } else {
        variant = PARAMETER_STAR_STAR;
        had_variable_keyword_args = true;
      }
      /* TODO: report error when any other parameters follow ** */
      eat(s, T_ASTERISK_ASTERISK);
      goto parameter;
    case T_IDENTIFIER:
    parameter: {
      struct location location = scanner_location(&s->scanner);
      struct symbol  *name = parse_identifier(s);
      if (need_kwonly_after_bare_star && variant == PARAMETER_NORMAL) {
        need_kwonly_after_bare_star = false;
      }

      union ast_expression *type = NULL;
      bool                  allow_type_annotations = (end != ':');
      if (allow_type_annotations && accept(s, ':')) {
        type = parse_expression(s, PREC_EXPRESSION);
      }

      union ast_expression *initializer = NULL;
      if (accept(s, '=')) {
        struct location location = scanner_location(&s->scanner);
        initializer = parse_expression(s, PREC_NAMED);
        had_default = true;
        if (variant != PARAMETER_NORMAL) {
          diag_begin_error(s->d, location);
          diag_frag(s->d,
                    "variable argument parameter cannot have a default value");
          diag_end(s->d);
          initializer = NULL;
        }
      } else if (had_default && !had_variable_args
                 && variant != PARAMETER_STAR_STAR) {
        diag_begin_error(s->d, location);
        diag_frag(s->d,
                  "parameter without default follows parameter with default");
        diag_end(s->d);
      }

      bool              error_duplicate = false;
      struct parameter *parr = idynarray_data(parameters);
      for (unsigned p = 0, e = idynarray_length(parameters, struct parameter);
           p < e; p++) {
        if (parr[p].name == name) {
          diag_begin_error(s->d, location);
          diag_frag(s->d, "duplicate argument ");
          diag_symbol(s->d, name);
          diag_end(s->d);
          error_duplicate = true;
        }
      }

      if (!error_duplicate) {
        struct parameter *parameter
            = idynarray_append(parameters, struct parameter);
        parameter->name = name;
        parameter->type = type;
        parameter->initializer = initializer;
        parameter->variant = variant;
      }
      break;
    }
    default:
      break;
    }
    if (!accept(s, ',')) break;
  }

  if (need_kwonly_after_bare_star) {
    diag_begin_error(s->d, bare_star_location);
    diag_frag(s->d, "named arguments must follow bare *");
    diag_end(s->d);
  }

  if (!accept(s, end)) {
    error_expected_tok2(s, ',', end);
    eat_until_anchor(s);
    accept(s, end);
  }
  remove_anchor(s, end);

  unsigned num_parameters = idynarray_length(parameters, struct parameter);
  if (keyword_only_begin == (unsigned)-1
      || keyword_only_begin > num_parameters) {
    keyword_only_begin = num_parameters;
  }
  parameter_shape->num_parameters = num_parameters;
  parameter_shape->keyword_only_begin = keyword_only_begin;
  parameter_shape->positional_only_argcount = positional_only_argcount;
}

static union ast_expression *parse_singleton(struct parser_state *s,
                                             enum object_type     type)
{
  struct location location = scanner_location(&s->scanner);
  next_token(s);
  union object *object = object_intern_singleton(s->objects, type);
  return ast_const_new(s, object, location);
}

static union ast_expression *parse_await(struct parser_state *s)
{
  s->parsed_await_expression = true;
  return parse_unexpr(s, PREC_AWAIT, AST_UNEXPR_AWAIT);
}

static union ast_expression *parse_l_bracket(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, '[');

  if (accept(s, ']')) {
    union ast_expression *expression = ast_allocate_expression(
        s, struct ast_expression_list, AST_LIST_DISPLAY);
    expression->expression_list.base.location = location;
    expression->expression_list.num_expressions = 0;
    return expression;
  }

  add_anchor(s, ']');
  add_anchor(s, ',');

  bool                  await_saved = await_tracking_begin(s);
  union ast_expression *first = parse_star_expression(s, PREC_NAMED);
  bool                  first_has_await = await_tracking_end(s, await_saved);

  union ast_expression *expression;
  if (peek(s) == T_for || peek(s) == T_async) {
    /* TODO: disallow star-expression */
    expression = parse_generator_expression(s, AST_LIST_COMPREHENSION);
    expression->generator_expression.base.location = location;
    set_generator_expression_item(expression, first, first_has_await,
                                  /*item_value=*/NULL,
                                  /*item_value_has_await=*/false);
  } else {
    expression = parse_expression_list_helper(
        s, AST_LIST_DISPLAY, first, PREC_NAMED, /*allow_slices=*/false,
        /*allow_starred=*/true);
  }

  remove_anchor(s, ',');
  remove_anchor(s, ']');
  expect(s, ']');
  return expression;
}

static union ast_expression *parse_l_curly(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, '{');
  if (accept(s, '}')) {
    union ast_expression *expression = ast_allocate_expression(
        s, struct ast_dict_item_list, AST_DICT_DISPLAY);
    expression->dict_item_list.base.location = location;
    expression->dict_item_list.num_items = 0;
    return expression;
  }

  add_anchor(s, '}');

  union ast_expression *nullable key;
  union ast_expression          *value;
  union ast_expression          *first;
  bool                           first_has_await = false;
  if (peek(s) == T_ASTERISK_ASTERISK) {
    value = parse_unexpr(s, PREC_OR, AST_UNEXPR_STAR_STAR);
    key = NULL;
    goto parse_dict;
  } else {
    bool await_saved = await_tracking_begin(s);
    first = parse_star_expression(s, PREC_NAMED);
    first_has_await = await_tracking_end(s, await_saved);
  }
  union ast_expression *expression;
  /* set display */
  if (peek(s) == ',' || peek(s) == '}') {
    add_anchor(s, ',');
    expression = parse_expression_list_helper(
        s, AST_SET_DISPLAY, first, PREC_NAMED, /*allow_slices=*/false,
        /*allow_starred=*/true);
    /* TODO: check that there's no `**` elements in list */
    remove_anchor(s, ',');
  } else if (peek(s) == T_for || peek(s) == T_async) {
    /* set comprehension */
    expression = parse_generator_expression(s, AST_SET_COMPREHENSION);
    expression->generator_expression.base.location = location;
    set_generator_expression_item(expression, first, first_has_await,
                                  /*item_value=*/NULL,
                                  /*item_value_has_await=*/false);
  } else {
    key = first;
    expect(s, ':'); /* TODO: say that we expected `,`, `:` or `}` on error? */
    bool await_saved = await_tracking_begin(s);
    value = parse_expression(s, PREC_NAMED);
    bool value_has_await = await_tracking_end(s, await_saved);

    /* dict comprehension */
    if (peek(s) == T_for || peek(s) == T_async) {
      expression = parse_generator_expression(s, AST_DICT_COMPREHENSION);
      expression->generator_expression.base.location = location;
      set_generator_expression_item(expression, key, first_has_await, value,
                                    value_has_await);
    } else {
    parse_dict: {
      /* dict display */
      struct dict_item inline_storage[16];
      struct idynarray items;
      idynarray_init(&items, inline_storage, sizeof(inline_storage));

      add_anchor(s, ',');
      for (;;) {
        struct dict_item *item = idynarray_append(&items, struct dict_item);
        item->key = key;
        item->value = value;

        if (!accept(s, ',')) break;
        if (peek(s) == '}') break;

        if (peek(s) == T_ASTERISK_ASTERISK) {
          value = parse_unexpr(s, PREC_OR, AST_UNEXPR_STAR_STAR);
          key = NULL;
        } else {
          key = parse_expression(s, PREC_NAMED);
          expect(s, ':');
          value = parse_expression(s, PREC_NAMED);
        }
      }
      remove_anchor(s, ',');

      unsigned num_items = idynarray_length(&items, struct dict_item);
      size_t   items_size = num_items * sizeof(struct dict_item);
      expression = ast_allocate_expression_(
          s, sizeof(struct ast_dict_item_list) + items_size, AST_DICT_DISPLAY);
      expression->dict_item_list.base.location = location;
      expression->dict_item_list.num_items = num_items;
      memcpy(expression->dict_item_list.items, idynarray_data(&items),
             items_size);
      idynarray_free(&items);
    }
    }
  }

  remove_anchor(s, '}');
  expect(s, '}');
  return expression;
}

static union ast_expression *parse_yield_expression(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_yield);
  bool                     is_yield_from = accept(s, T_from);
  enum ast_expression_type type = is_yield_from ? AST_YIELD_FROM : AST_YIELD;
  union ast_expression    *yield_expression = NULL;
  if (is_yield_from) {
    if (!is_expression_start(peek(s))) {
      error_expected(s, "expression");
      yield_expression = invalid_expression(s);
    } else {
      yield_expression = parse_expression(s, PREC_EXPRESSION);
    }
  } else if (is_expression_start(peek(s)) || peek(s) == '*') {
    yield_expression = parse_star_expressions(s, PREC_EXPRESSION);
  }
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_expression_yield, type);
  expression->yield.base.location = location;
  expression->yield.value = yield_expression;
  if (s->current_function_has_yield != NULL) {
    *s->current_function_has_yield = true;
  }
  return expression;
}

static union ast_expression *parse_l_paren(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, '(');
  if (accept(s, ')')) {
    union ast_expression *expression = ast_allocate_expression(
        s, struct ast_expression_list, AST_EXPRESSION_LIST);
    expression->expression_list.base.location = location;
    expression->expression_list.num_expressions = 0;
    expression->expression_list.as_constant
        = ast_tuple_compute_constant(s->objects, &expression->expression_list);
    return expression;
  }

  add_anchor(s, ')');
  add_anchor(s, ',');

  union ast_expression *expression;
  if (peek(s) == T_yield) {
    expression = parse_yield_expression(s);
  } else {
    bool                  await_saved = await_tracking_begin(s);
    union ast_expression *first = parse_star_expression(s, PREC_NAMED);
    bool                  first_has_await = await_tracking_end(s, await_saved);
    if (peek(s) == T_for || peek(s) == T_async) {
      /* TODO: disallow star-expression */
      expression = parse_generator_expression(s, AST_GENERATOR_EXPRESSION);
      expression->generator_expression.base.location = location;
      set_generator_expression_item(expression, first, first_has_await,
                                    /*item_value=*/NULL,
                                    /*item_value_has_await=*/false);
    } else if (peek(s) == ',') {
      expression = parse_expression_list_helper(
          s, AST_EXPRESSION_LIST, first, PREC_NAMED, /*allow_slices=*/false,
          /*allow_starred=*/true);
    } else {
      expression = first;
    }
  }

  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');
  return expression;
}

static union ast_expression *parse_ellipsis(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_ELLIPSIS);
}

static union ast_expression *parse_false(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_FALSE);
}

static union ast_expression *parse_float(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  union object   *object = peek_get_object(s, T_FLOAT);
  eat(s, T_FLOAT);
  return ast_const_new(s, object, location);
}

static union ast_expression *parse_prefix_identifier(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  struct symbol  *symbol = parse_identifier(s);

  union ast_expression *node
      = ast_allocate_expression(s, struct ast_identifier, AST_IDENTIFIER);
  node->identifier.base.location = location;
  node->identifier.symbol = symbol;
  return node;
}

static union ast_expression *parse_integer(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  union object   *object = peek_get_object(s, T_INTEGER);
  eat(s, T_INTEGER);
  return ast_const_new(s, object, location);
}

static union ast_expression *parse_invert(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_INVERT);
}

static union ast_expression *parse_lambda(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_lambda);

  struct parameter inline_storage[8];
  struct idynarray parameters;
  idynarray_init(&parameters, inline_storage, sizeof(inline_storage));

  struct parameter_shape parameter_shape;
  parse_parameters(s, &parameters, &parameter_shape, /*end=*/':');
  union ast_expression *expression = parse_expression(s, PREC_EXPRESSION);

  unsigned num_parameters = parameter_shape.num_parameters;
  size_t   parameters_size = num_parameters * sizeof(struct parameter);
  union ast_expression *lambda = ast_allocate_expression_(
      s, sizeof(struct ast_lambda) + parameters_size, AST_LAMBDA);
  lambda->lambda.base.location = location;
  lambda->lambda.expression = expression;
  lambda->lambda.parameter_shape = parameter_shape;
  memcpy(lambda->lambda.parameters, idynarray_data(&parameters),
         parameters_size);
  idynarray_free(&parameters);

  return lambda;
}

static union ast_expression *parse_plus(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_PLUS);
}

static union object *concat_strings(struct parser_state *s,
                                    struct idynarray    *strings,
                                    enum object_type     type,
                                    size_t               combined_length)
{
  if (combined_length > UINT32_MAX) {
    internal_error("combined string literal too long");
  }

  unsigned num_strings = idynarray_length(strings, union object *);
  char    *combined = arena_allocate(s->scanner.strings, combined_length, 1);
  char    *dest = combined;
  union object **strings_arr = idynarray_data(strings);
  for (unsigned i = 0; i < num_strings; i++) {
    union object *string = strings_arr[i];
    uint32_t      string_length = object_string_length(string);
    memcpy(dest, string->string.chars, string_length);
    dest += string_length;
  }
  assert(dest - combined == (ptrdiff_t)combined_length);

  union object *object
      = object_intern_string(s->objects, type, combined_length, combined);
  if (object->string.chars != combined) {
    arena_free_to(s->scanner.strings, combined);
  }
  return object;
}

static union ast_expression *parse_string(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  bool saw_fstring = (peek(s) == T_FSTRING_START) || peek_string_is_fstring(s);
  union object *string;
  // fast-path: single string literal
  if (peek(s) == T_STRING) {
    saw_fstring = peek_string_is_fstring(s);
    string = peek_get_object(s, T_STRING);
    eat(s, T_STRING);
    if (!saw_fstring && peek(s) != T_STRING && peek(s) != T_FSTRING_START) {
      return ast_const_new(s, string, location);
    }
  } else {
    string = NULL;
  }

  // prepare to concatenate multiple (f-)string literals.
  union object    *inline_storage[8];
  struct idynarray strings;
  idynarray_init(&strings, inline_storage, sizeof(inline_storage));
  size_t           combined_length = 0;
  struct location  mixed_types = { 0 };
  enum object_type type;
  if (string != NULL) {
    type = object_type(string);
    if (object_string_length(string) > 0) {
      *idynarray_append(&strings, union object *) = string;
      combined_length += object_string_length(string);
    }
  } else {
    assert(peek(s) == T_FSTRING_START);
    type = OBJECT_STRING;
  }

  struct fstring_element inline_storage_elements[8];
  struct idynarray       elements;
  idynarray_init(&elements, inline_storage_elements,
                 sizeof(inline_storage_elements));

  for (;;) {
    if (peek(s) == T_STRING) {
      saw_fstring |= peek_string_is_fstring(s);
      string = peek_get_object(s, T_STRING);
      eat(s, T_STRING);
      if (object_string_length(string) > 0) {
        *idynarray_append(&strings, union object *) = string;
        combined_length += object_string_length(string);
      }
      if (object_type(string) != type) {
        mixed_types = scanner_location(&s->scanner);
      }
      continue;
    }
    if (peek(s) == T_FSTRING_START) {
      saw_fstring = true;
      if (type != OBJECT_STRING) {
        mixed_types = scanner_location(&s->scanner);
      }
      for (;;) {
        if (peek(s) != T_FSTRING_START && peek(s) != T_FSTRING_FRAGMENT
            && peek(s) != T_FSTRING_END) {
          diag_begin_error(s->d, scanner_location(&s->scanner));
          diag_frag(s->d, "invalid token inside f-string: got ");
          diag_token_kind(s->d, peek(s));
          diag_end(s->d);
          idynarray_free(&elements);
          idynarray_free(&strings);
          return invalid_expression(s);
        }
        string = peek_get_object(s, peek(s));

        if (object_string_length(string) > 0) {
          *idynarray_append(&strings, union object *) = string;
          combined_length += object_string_length(string);
        }
        if (peek(s) == T_FSTRING_END) {
          next_token(s);
          break;
        }
        scanner_fstring_debug_capture_begin(&s->scanner);
        next_token(s);

        if (combined_length > 0) {
          union object *combined
              = concat_strings(s, &strings, type, combined_length);
          idynarray_clear(&strings);
          combined_length = 0;

          struct fstring_element *element
              = idynarray_append(&elements, struct fstring_element);
          memset(element, 0, sizeof(*element));
          element->u.string = combined;
          element->is_expression = false;
        }

        union ast_expression *expression
            = parse_star_expressions(s, PREC_EXPRESSION);
        bool          debug_expression = accept(s, '=');
        union object *debug_prefix = NULL;
        if (debug_expression) {
          debug_prefix = scanner_fstring_debug_capture_finish(&s->scanner);
        } else {
          scanner_fstring_debug_capture_discard(&s->scanner);
        }
        uint8_t conversion = FORMAT_VALUE_NONE;
        if (accept(s, '!')) {
          struct location location = scanner_location(&s->scanner);
          struct symbol  *symbol = parse_identifier(s);
          const char     *symbol_str = symbol->string;
          if (strcmp(symbol_str, "r") == 0) {
            conversion = FORMAT_VALUE_REPR;
          } else if (strcmp(symbol_str, "s") == 0) {
            conversion = FORMAT_VALUE_STR;
          } else if (strcmp(symbol_str, "a") == 0) {
            conversion = FORMAT_VALUE_ASCII;
          } else if (strcmp(symbol_str, "<invalid>") != 0) {
            diag_begin_error(s->d, location);
            diag_frag(s->d, "f-string conversion should be `r`, `s` or `a`");
            diag_end(s->d);
          }
        }
        union ast_expression *format_spec = NULL;
        if (accept(s, ':')) {
          format_spec = parse_string(s);
        }
        if (debug_expression && conversion == FORMAT_VALUE_NONE
            && format_spec == NULL) {
          conversion = FORMAT_VALUE_REPR;
        }

        if (debug_expression) {
          struct fstring_element *label_element
              = idynarray_append(&elements, struct fstring_element);
          memset(label_element, 0, sizeof(*label_element));
          label_element->u.string = debug_prefix;
          label_element->is_expression = false;
        }

        struct fstring_element *element
            = idynarray_append(&elements, struct fstring_element);
        memset(element, 0, sizeof(*element));
        element->u.expression = expression;
        element->format_spec = format_spec;
        element->is_expression = true;
        element->conversion = conversion;
      }
      continue;
    }
    break;
  }

  if (combined_length > 0) {
    union object *combined
        = concat_strings(s, &strings, type, combined_length);
    struct fstring_element *element
        = idynarray_append(&elements, struct fstring_element);
    memset(element, 0, sizeof(*element));
    element->u.string = combined;
    element->is_expression = false;
  }
  idynarray_free(&strings);

  if (mixed_types.line > 0) {
    diag_begin_error(s->d, mixed_types);
    diag_frag(s->d, "cannot mix bytes and str literals");
    diag_end(s->d);
  }

  unsigned num_elements = idynarray_length(&elements, struct fstring_element);
  struct fstring_element *element_arr = idynarray_data(&elements);
  if (saw_fstring && num_elements == 0) {
    struct fstring_element *element
        = idynarray_append(&elements, struct fstring_element);
    memset(element, 0, sizeof(*element));
    element->u.string = object_intern_cstring(s->objects, "");
    element->is_expression = false;
    num_elements = 1;
    element_arr = idynarray_data(&elements);
  }
  if (!saw_fstring && num_elements <= 1
      && (num_elements == 0 || element_arr[0].is_expression == false)) {
    union object *object;
    if (num_elements == 1) {
      object = element_arr[0].u.string;
    } else {
      object = object_intern_cstring(s->objects, "");
    }
    idynarray_free(&elements);
    return ast_const_new(s, object, location);
  }

  size_t elements_size = num_elements * sizeof(struct fstring_element);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_fstring) + elements_size, AST_FSTRING);
  expression->fstring.base.location = location;
  expression->fstring.num_elements = num_elements;
  memcpy(expression->fstring.elements, idynarray_data(&elements),
         elements_size);
  idynarray_free(&elements);
  return expression;
}

static union ast_expression *parse_negative(struct parser_state *s)
{
  return parse_unexpr(s, PREC_FACTOR, AST_UNEXPR_NEGATIVE);
}

static union ast_expression *parse_none(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_NONE);
}

static union ast_expression *parse_not(struct parser_state *s)
{
  return parse_unexpr(s, PREC_COMPARISON, AST_UNEXPR_NOT);
}

static union ast_expression *parse_true(struct parser_state *s)
{
  return parse_singleton(s, OBJECT_TRUE);
}

/* Keep this in sync with parse_prefix_expression() below */
#define EXPRESSION_START_CASES                                                \
  '(' : case '*':                                                             \
  case '+':                                                                   \
  case '-':                                                                   \
  case '[':                                                                   \
  case '{':                                                                   \
  case '~':                                                                   \
  case T_ASTERISK_ASTERISK:                                                   \
  case T_DOT_DOT_DOT:                                                         \
  case T_FLOAT:                                                               \
  case T_FSTRING_START:                                                       \
  case T_False:                                                               \
  case T_IDENTIFIER:                                                          \
  case T_INTEGER:                                                             \
  case T_None:                                                                \
  case T_STRING:                                                              \
  case T_True:                                                                \
  case T_await:                                                               \
  case T_lambda:                                                              \
  case T_not

static bool is_expression_start(enum token_kind token_kind)
{
  switch (token_kind) {
  case EXPRESSION_START_CASES:
    return true;
  default:
    return false;
  }
}

static union ast_expression *parse_prefix_expression(struct parser_state *s)
{
  switch (peek(s)) {
  case '(':
    return parse_l_paren(s);
  case '+':
    return parse_plus(s);
  case '-':
    return parse_negative(s);
  case '[':
    return parse_l_bracket(s);
  case '{':
    return parse_l_curly(s);
  case '~':
    return parse_invert(s);
  case T_DOT_DOT_DOT:
    return parse_ellipsis(s);
  case T_FLOAT:
    return parse_float(s);
  case T_False:
    return parse_false(s);
  case T_IDENTIFIER:
    return parse_prefix_identifier(s);
  case T_INTEGER:
    return parse_integer(s);
  case T_None:
    return parse_none(s);
  case T_STRING:
  case T_FSTRING_START:
    return parse_string(s);
  case T_True:
    return parse_true(s);
  case T_await:
    return parse_await(s);
  case T_lambda:
    return parse_lambda(s);
  case T_not:
    return parse_not(s);
  case T_yield:
    return parse_yield_expression(s);
  default:
    if (peek(s) == '*' || peek(s) == T_ASTERISK_ASTERISK) {
      struct location          location = scanner_location(&s->scanner);
      enum ast_expression_type type
          = peek(s) == '*' ? AST_UNEXPR_STAR : AST_UNEXPR_STAR_STAR;
      parse_unexpr(s, PREC_OR, type);
      diag_begin_error(s->d, location);
      diag_frag(s->d, "starred expression not allowed here");
      diag_end(s->d);
      return invalid_expression(s);
    }
    error_expected(s, "expression");
    return invalid_expression(s);
  }
}

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
  diag_begin_error(s->d, location);
  diag_frag(s->d, "cannot assign to ");
  diag_expression(s->d, target);
  if (type == AST_EXPRESSION_LIST) {
    diag_frag(s->d, " for augmented assignment");
  }
  diag_frag(s->d, ".");
  diag_end(s->d);
  return true;
}

static inline union ast_expression *
parse_assignment_rhs(struct parser_state *s)
{
  if (peek(s) == T_yield) {
    return parse_yield_expression(s);
  }
  return parse_star_expressions(s, PREC_EXPRESSION);
}

static inline union ast_expression *
parse_binexpr_assign(struct parser_state *s, enum ast_expression_type type,
                     union ast_expression *left)
{
  struct location location = scanner_location(&s->scanner);
  next_token(s);
  union ast_expression *right = parse_assignment_rhs(s);
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_binexpr, type);
  expression->binexpr.left = left;
  expression->binexpr.right = right;
  if (check_binexpr_assign_target(s, left, location)) {
    expression = invalid_expression(s);
  }
  return expression;
}

static int parse_compare_op(struct parser_state *s)
{
  bool future_barry_as_bdfl = (s->future_flags & CO_FUTURE_BARRY_AS_BDFL) != 0;
  switch (peek(s)) {
  case '<':
    next_token(s);
    return COMPARE_OP_LT;
  case T_LESS_THAN_EQUALS:
    next_token(s);
    return COMPARE_OP_LE;
  case T_LESS_THAN_GREATER_THAN: {
    struct location location = scanner_location(&s->scanner);
    next_token(s);
    if (future_barry_as_bdfl) {
      return COMPARE_OP_NE;
    }
    diag_begin_error(s->d, location);
    diag_frag(s->d, "invalid comparison operator ");
    diag_token_kind(s->d, T_LESS_THAN_GREATER_THAN);
    diag_frag(s->d, "; use ");
    diag_token_kind(s->d, T_EXCLAMATIONMARK_EQUALS);
    diag_end(s->d);
    return -1;
  }
  case T_EQUALS_EQUALS:
    next_token(s);
    return COMPARE_OP_EQ;
  case T_EXCLAMATIONMARK_EQUALS: {
    struct location location = scanner_location(&s->scanner);
    next_token(s);
    if (future_barry_as_bdfl) {
      diag_begin_error(s->d, location);
      diag_frag(s->d, "with Barry as BDFL, use ");
      diag_token_kind(s->d, T_LESS_THAN_GREATER_THAN);
      diag_frag(s->d, " instead of ");
      diag_token_kind(s->d, T_EXCLAMATIONMARK_EQUALS);
      diag_end(s->d);
    }
    return COMPARE_OP_NE;
  }
  case '>':
    next_token(s);
    return COMPARE_OP_GT;
  case T_GREATER_THAN_EQUALS:
    next_token(s);
    return COMPARE_OP_GE;
  case T_in:
    next_token(s);
    return COMPARE_OP_IN;
  case T_not:
    next_token(s);
    expect(s, T_in);
    return COMPARE_OP_NOT_IN;
  case T_is:
    next_token(s);
    if (accept(s, T_not)) return COMPARE_OP_IS_NOT;
    return COMPARE_OP_IS;
  default:
    return -1;
  }
}

static union ast_expression *parse_comparison(struct parser_state  *s,
                                              union ast_expression *left)
{
  int op = parse_compare_op(s);
  if (op < 0) {
    return invalid_expression(s);
  }

  struct comparison_op inline_storage[2];
  struct idynarray     operands;
  idynarray_init(&operands, inline_storage, sizeof(inline_storage));

  do {
    union ast_expression *operand = parse_expression(s, PREC_COMPARISON + 1);

    struct comparison_op *comparison_op
        = idynarray_append(&operands, struct comparison_op);
    comparison_op->op = op;
    comparison_op->operand = operand;

    op = parse_compare_op(s);
  } while (op >= 0);

  unsigned num_operands = idynarray_length(&operands, struct comparison_op);
  size_t   operands_size = num_operands * sizeof(struct comparison_op);
  union ast_expression *expression = ast_allocate_expression_(
      s, sizeof(struct ast_comparison) + operands_size, AST_COMPARISON);
  expression->comparison.num_operands = num_operands;
  expression->comparison.left = left;
  memcpy(expression->comparison.operands, idynarray_data(&operands),
         operands_size);
  idynarray_free(&operands);
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

static union ast_expression *parse_attr(struct parser_state  *s,
                                        union ast_expression *left)
{
  eat(s, '.');
  struct symbol *symbol = parse_identifier(s);

  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_attr, AST_ATTR);
  expression->attr.expression = left;
  expression->attr.symbol = symbol;
  return expression;
}

static union ast_expression *parse_call(struct parser_state  *s,
                                        union ast_expression *left)
{
  return parse_arguments(s, /*callee=*/left);
}

static union ast_expression *
parse_conditional(struct parser_state  *s,
                  union ast_expression *true_expression)
{
  eat(s, T_if);
  union ast_expression *condition = parse_expression(s, PREC_TEST + 1);
  expect(s, T_else);
  union ast_expression *false_expression = parse_expression(s, PREC_TEST);
  union ast_expression *expression
      = ast_allocate_expression(s, struct ast_conditional, AST_CONDITIONAL);
  expression->conditional.condition = condition;
  expression->conditional.true_expression = true_expression;
  expression->conditional.false_expression = false_expression;
  return expression;
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
                                         PREC_NAMED, /*allow_slices=*/true,
                                         /*allow_starred=*/true);
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

static union ast_expression *parse_walrus(struct parser_state  *s,
                                          union ast_expression *left)
{
  struct location location = scanner_location(&s->scanner);
  if (ast_expression_type(left) != AST_IDENTIFIER) {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "cannot use ");
    diag_token_kind(s->d, T_COLON_EQUALS);
    diag_frag(s->d, " expression with ");
    diag_expression(s->d, left);
    diag_end(s->d);
    left = invalid_expression(s);
  }
  return parse_binexpr(s, PREC_NAMED + 1, AST_BINEXPR_ASSIGN, left);
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
  ['-']    = { .func = parse_sub,         .precedence = PREC_ARITH       },
  ['.']    = { .func = parse_attr,        .precedence = PREC_PRIMARY     },
  ['/']    = { .func = parse_true_div,    .precedence = PREC_TERM        },
  ['<']    = { .func = parse_comparison,  .precedence = PREC_COMPARISON  },
  ['>']    = { .func = parse_comparison,  .precedence = PREC_COMPARISON  },
  ['@']    = { .func = parse_matmul,      .precedence = PREC_TERM        },
  ['[']    = { .func = parse_subscript,   .precedence = PREC_PRIMARY     },
  ['^']    = { .func = parse_xor,         .precedence = PREC_XOR         },
  ['|']    = { .func = parse_or,          .precedence = PREC_OR          },
  [T_and]  = { .func = parse_logical_and, .precedence = PREC_LOGICAL_AND },
  [T_if]   = { .func = parse_conditional, .precedence = PREC_TEST        },
  [T_in]   = { .func = parse_comparison,  .precedence = PREC_COMPARISON  },
  [T_is]   = { .func = parse_comparison,  .precedence = PREC_COMPARISON  },
  [T_not]  = { .func = parse_comparison,  .precedence = PREC_COMPARISON  },
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
  [T_COLON_EQUALS]
    = { .func = parse_walrus,             .precedence = PREC_NAMED      },
  [T_EQUALS_EQUALS]
    = { .func = parse_comparison,         .precedence = PREC_COMPARISON },
  [T_EXCLAMATIONMARK_EQUALS]
    = { .func = parse_comparison,         .precedence = PREC_COMPARISON },
  [T_GREATER_THAN_EQUALS]
    = { .func = parse_comparison,         .precedence = PREC_COMPARISON },
  [T_GREATER_THAN_GREATER_THAN_EQUALS]
    = { .func = parse_shift_right_assign, .precedence = PREC_ASSIGN     },
  [ T_GREATER_THAN_GREATER_THAN]
    = { .func = parse_shift_right,        .precedence = PREC_SHIFT      },
  [T_LESS_THAN_EQUALS]
    = { .func = parse_comparison,         .precedence = PREC_COMPARISON },
  [T_LESS_THAN_GREATER_THAN]
    = { .func = parse_comparison,         .precedence = PREC_COMPARISON },
  [T_LESS_THAN_LESS_THAN_EQUALS]
    = { .func = parse_shift_left_assign,  .precedence = PREC_ASSIGN     },
  [T_LESS_THAN_LESS_THAN]
    = { .func = parse_shift_left,         .precedence = PREC_SHIFT      },
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
  union ast_expression *result = parse_prefix_expression(s);

  for (;;) {
    enum token_kind postfix_token_kind = peek(s);
    if (postfix_token_kind
        >= sizeof(postfix_parsers) / sizeof(postfix_parsers[0]))
      break;
    const struct postfix_expression_parser *postfix_parser
        = &postfix_parsers[postfix_token_kind];
    if (postfix_parser->func == NULL
        || postfix_parser->precedence < precedence) {
      break;
    }
    result = postfix_parser->func(s, result);
  }
  return result;
}

static union ast_expression *check_assignment_target(
    struct parser_state *s, union ast_expression *expression,
    struct location location, bool is_del, bool show_equal_hint)
{
  enum ast_expression_type type = ast_expression_type(expression);
  if (type == AST_IDENTIFIER || type == AST_BINEXPR_SUBSCRIPT
      || type == AST_ATTR)
    return expression;
  if (type == AST_EXPRESSION_LIST || type == AST_LIST_DISPLAY) {
    unsigned num_expressions = expression->expression_list.num_expressions;
    union ast_expression **expressions
        = expression->expression_list.expressions;
    for (unsigned i = 0; i < num_expressions; i++) {
      if (ast_expression_type(expressions[i]) == AST_UNEXPR_STAR) {
        continue;
      }
      expressions[i] = check_assignment_target(s, expressions[i], location,
                                               is_del, show_equal_hint);
    }
    return expression;
  }
  if (type == AST_INVALID) {
    return expression;
  }
  diag_begin_error(s->d, location);
  diag_frag(s->d, is_del ? "cannot delete " : "cannot assign to ");
  diag_expression(s->d, expression);
  if (show_equal_hint) {
    /* TODO: only show ':=' suggestion where it makes sense (like cpython)...
     */
    diag_frag(s->d, ". Maybe you meant ");
    diag_token_kind(s->d, T_EQUALS_EQUALS);
    diag_frag(s->d, ", or ");
    diag_token_kind(s->d, T_COLON_EQUALS);
    diag_frag(s->d, " instead of ");
    diag_token_kind(s->d, '=');
    diag_frag(s->d, "?");
  }
  diag_end(s->d);
  return invalid_expression(s);
}

static union ast_statement *parse_assignment(struct parser_state  *s,
                                             union ast_expression *target,
                                             struct location       location)
{
  target = check_assignment_target(s, target, location, /*is_del=*/false,
                                   /*show_equal_hint=*/true);
  eat(s, '=');

  union ast_expression *rhs = parse_assignment_rhs(s);

  if (peek(s) == '=') {
    union ast_expression *inline_storage[8];
    struct idynarray      targets;
    idynarray_init(&targets, inline_storage, sizeof(inline_storage));
    *idynarray_append(&targets, union ast_expression *) = target;

    do {
      struct location assign_location = scanner_location(&s->scanner);
      rhs = check_assignment_target(s, rhs, assign_location,
                                    /*is_del=*/false,
                                    /*show_equal_hint=*/true);
      eat(s, '=');
      *idynarray_append(&targets, union ast_expression *) = rhs;
      rhs = parse_assignment_rhs(s);
    } while (peek(s) == '=');

    unsigned num_targets = idynarray_length(&targets, union ast_expression *);
    union ast_statement *statement = ast_allocate_statement(
        s, struct ast_assignment, AST_STATEMENT_ASSIGN, location);
    statement->assign.num_targets = num_targets;
    statement->assign.targets = ast_expression_array_copy_dyn(s, &targets);
    statement->assign.value = rhs;
    idynarray_free(&targets);
    return statement;
  }

  union ast_expression *targets[] = { target };
  union ast_statement  *statement = ast_allocate_statement(
      s, struct ast_assignment, AST_STATEMENT_ASSIGN, location);
  statement->assign.num_targets = 1;
  statement->assign.targets = ast_expression_array_copy(s, targets, 1);
  statement->assign.value = rhs;
  return statement;
}

static union ast_statement *parse_expression_statement(struct parser_state *s,
                                                       bool print_expr)
{
  struct location       location = scanner_location(&s->scanner);
  bool                  starts_with_lparen = (peek(s) == '(');
  union ast_expression *expression = parse_star_expression(s, PREC_NAMED);
  if (accept(s, ':')) {
    /* TODO Check: check that expression is either:
     *  NAME |  ( single_target ) | single_subscript_attribute_target
     */
    union ast_expression *annotation = parse_expression(s, PREC_EXPRESSION);
    union ast_expression *value = NULL;
    if (accept(s, '=')) {
      /* "annotated_rhs": yield | star_expressions */
      if (peek(s) == T_yield) {
        value = parse_yield_expression(s);
      } else {
        value = parse_star_expressions(s, PREC_EXPRESSION);
      }
    }

    union ast_statement *statement
        = ast_allocate_statement(s, struct ast_statement_annotation,
                                 AST_STATEMENT_ANNOTATION, location);
    statement->annotation.target = expression;
    statement->annotation.simple
        = ast_expression_type(expression) == AST_IDENTIFIER
          && !starts_with_lparen;
    statement->annotation.annotation = annotation;
    statement->annotation.value = value;
    return statement;
  }
  if (peek(s) == ',') {
    expression = parse_expression_list_helper(
        s, AST_EXPRESSION_LIST, expression, PREC_NAMED, /*allow_slices=*/false,
        /*allow_starred=*/true);
  }
  enum token_kind postfix_token_kind = peek(s);
  if (postfix_token_kind
      < sizeof(postfix_parsers) / sizeof(postfix_parsers[0])) {
    const struct postfix_expression_parser *parser = &postfix_parsers[peek(s)];
    if (parser->precedence == PREC_ASSIGN) {
      expression = parser->func(s, expression);
      union ast_statement *statement = ast_allocate_statement(
          s, struct ast_augassign, AST_STATEMENT_AUGASSIGN, location);
      statement->augassign.expression = expression;
      return statement;
    }
  }

  if (peek(s) == '=') {
    return parse_assignment(s, expression, location);
  }

  union ast_statement *statement;
  statement = ast_allocate_statement(s, struct ast_expression_statement,
                                     AST_STATEMENT_EXPRESSION, location);
  statement->expression.expression = expression;
  statement->expression.print = print_expr;
  return statement;
}

static union ast_statement *parse_assert(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_assert);

  union ast_expression *expression = parse_expression(s, PREC_NAMED);
  union ast_expression *message = NULL;
  if (accept(s, ',')) {
    message = parse_expression(s, PREC_NAMED);
  }

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_assert, AST_STATEMENT_ASSERT, location);
  statement->assert.expression = expression;
  statement->assert.message = message;
  return statement;
}

static union ast_statement *parse_break(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_break);
  return ast_allocate_statement(s, struct ast_break, AST_STATEMENT_BREAK,
                                location);
}

static union ast_statement *parse_continue(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_continue);
  return ast_allocate_statement(s, struct ast_continue, AST_STATEMENT_CONTINUE,
                                location);
}

static union ast_statement *parse_del(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_del);
  union ast_expression *targets = parse_expression(s, PREC_OR);
  if (peek(s) == ',') {
    targets = parse_expression_list_helper(s, AST_EXPRESSION_LIST, targets,
                                           PREC_OR, /*allow_slices=*/false,
                                           /*allow_starred=*/false);
  }
  targets = check_assignment_target(s, targets, location, /*is_del=*/true,
                                    /*show_equal_hint=*/false);

  union ast_statement *statement
      = ast_allocate_statement(s, struct ast_del, AST_STATEMENT_DEL, location);
  statement->del.targets = targets;
  return statement;
}

static struct dotted_name *parse_dotted_name(struct parser_state *s)
{
  struct symbol   *inline_storage[8];
  struct idynarray symbols;
  idynarray_init(&symbols, inline_storage, sizeof(inline_storage));
  do {
    struct symbol **slot = idynarray_append(&symbols, struct symbol *);
    *slot = parse_identifier(s);
  } while (accept(s, T_DOT));

  unsigned num_symbols = idynarray_length(&symbols, struct symbol *);
  if ((size_t)num_symbols
      > (SIZE_MAX - sizeof(struct dotted_name)) / sizeof(struct symbol *)) {
    internal_error("dotted_name size overflow");
  }
  size_t size = sizeof(struct dotted_name)
                + (size_t)num_symbols * sizeof(struct symbol *);

  struct dotted_name *result
      = arena_allocate(&s->ast, size, alignof(struct dotted_name));
  result->num_symbols = num_symbols;
  if (num_symbols > 0) {
    memcpy(result->symbols, idynarray_data(&symbols),
           (size_t)num_symbols * sizeof(struct symbol *));
  }
  idynarray_free(&symbols);
  return result;
}

static union ast_statement *parse_from_import_statement(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
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
    union ast_statement *statement = ast_allocate_statement(
        s, struct ast_from_import, AST_STATEMENT_FROM_IMPORT, location);
    statement->from_import.num_prefix_dots = num_prefix_dots;
    statement->from_import.module = module;
    statement->from_import.import_star = true;
    statement->from_import.num_items = 0;
    statement->from_import.items = NULL;
    idynarray_free(&pairs);
    return statement;
  }

  bool braced = accept(s, '(');
  for (;;) {
    struct symbol *name = parse_identifier(s);
    struct symbol *as = NULL;
    if (accept(s, T_as)) {
      as = parse_identifier(s);
    }
    struct from_import_item *item
        = idynarray_append(&pairs, struct from_import_item);
    item->name = name;
    item->as = as;

    if (!accept(s, ',')) break;
    if (braced && peek(s) == ')') break;
  }

  if (braced) expect(s, ')');

  unsigned num_items = idynarray_length(&pairs, struct from_import_item);
  struct from_import_item *items = NULL;
  if (num_items > 0) {
    size_t size = num_items * sizeof(*items);
    items = (struct from_import_item *)arena_allocate(
        &s->ast, size, alignof(struct from_import_item));
    memcpy(items, idynarray_data(&pairs), size);
  }
  idynarray_free(&pairs);

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_from_import, AST_STATEMENT_FROM_IMPORT, location);
  statement->from_import.num_prefix_dots = num_prefix_dots;
  statement->from_import.module = module;
  statement->from_import.import_star = false;
  statement->from_import.num_items = num_items;
  statement->from_import.items = items;
  return statement;
}

static union ast_statement *parse_global_statement(struct parser_state *s)
{
  struct location  location = scanner_location(&s->scanner);
  struct symbol   *inline_storage[8];
  struct idynarray names;
  idynarray_init(&names, inline_storage, sizeof(inline_storage));

  eat(s, T_global);
  do {
    struct symbol *name = parse_identifier(s);
    *idynarray_append(&names, struct symbol *) = name;
  } while (accept(s, ','));

  unsigned        num_names = idynarray_length(&names, struct symbol *);
  size_t          size = num_names * sizeof(struct symbol *);
  struct symbol **name_arr = (struct symbol **)arena_allocate(
      &s->ast, size, alignof(struct symbol *));
  memcpy(name_arr, idynarray_data(&names), size);
  idynarray_free(&names);

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_global, AST_STATEMENT_GLOBAL, location);
  statement->global.num_names = num_names;
  statement->global.names = name_arr;
  return statement;
}

static union ast_statement *parse_import_statement(struct parser_state *s)
{
  struct location        location = scanner_location(&s->scanner);
  struct ast_import_item inline_storage[8];
  struct idynarray       items;
  idynarray_init(&items, inline_storage, sizeof(inline_storage));

  eat(s, T_import);

  do {
    struct dotted_name *dotted_name = parse_dotted_name(s);
    struct symbol      *as = NULL;
    if (accept(s, T_as)) {
      as = parse_identifier(s);
    }
    struct ast_import_item *item
        = idynarray_append(&items, struct ast_import_item);
    item->module = dotted_name;
    item->as = as;
  } while (accept(s, ','));

  unsigned num_items = idynarray_length(&items, struct ast_import_item);
  struct ast_import_item *items_arr = NULL;
  if (num_items > 0) {
    size_t size = num_items * sizeof(struct ast_import_item);
    items_arr = (struct ast_import_item *)arena_allocate(
        &s->ast, size, alignof(struct ast_import_item));
    memcpy(items_arr, idynarray_data(&items), size);
  }
  idynarray_free(&items);

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_import, AST_STATEMENT_IMPORT, location);
  statement->import.num_items = num_items;
  statement->import.items = items_arr;
  return statement;
}

static union ast_statement *parse_nonlocal_statement(struct parser_state *s)
{
  struct location  location = scanner_location(&s->scanner);
  struct symbol   *inline_storage[8];
  struct idynarray names;
  idynarray_init(&names, inline_storage, sizeof(inline_storage));

  eat(s, T_nonlocal);
  do {
    struct symbol *name = parse_identifier(s);
    *idynarray_append(&names, struct symbol *) = name;
  } while (accept(s, ','));

  unsigned        num_names = idynarray_length(&names, struct symbol *);
  size_t          size = num_names * sizeof(struct symbol *);
  struct symbol **name_arr = (struct symbol **)arena_allocate(
      &s->ast, size, alignof(struct symbol *));
  memcpy(name_arr, idynarray_data(&names), size);
  idynarray_free(&names);

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_statement_nonlocal, AST_STATEMENT_NONLOCAL, location);
  statement->nonlocal.num_names = num_names;
  statement->nonlocal.names = name_arr;
  return statement;
}

static union ast_statement *parse_pass(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_pass);
  return ast_allocate_statement(s, struct ast_pass, AST_STATEMENT_PASS,
                                location);
}

static union ast_statement *parse_raise(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_raise);

  union ast_expression *expression = NULL;
  union ast_expression *from = NULL;
  if (is_expression_start(peek(s))) {
    expression = parse_expression(s, PREC_EXPRESSION);
    if (accept(s, T_from)) {
      from = parse_expression(s, PREC_EXPRESSION);
    }
  }

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_raise, AST_STATEMENT_RAISE, location);
  statement->raise.expression = expression;
  statement->raise.from = from;
  return statement;
}

static union ast_statement *parse_return(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_return);

  union ast_expression *expression;
  if (is_expression_start(peek(s))) {
    expression = parse_star_expressions(s, PREC_EXPRESSION);
  } else {
    expression = NULL;
  }

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_return, AST_STATEMENT_RETURN, location);
  statement->return_.expression = expression;
  return statement;
}

static union ast_statement *parse_yield_statement(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_yield);
  if (s->current_function_has_yield != NULL) {
    *s->current_function_has_yield = true;
  }

  if (accept(s, T_from)) {
    union ast_expression *expression = parse_expression(s, PREC_EXPRESSION);
    union ast_statement  *statement = ast_allocate_statement(
        s, struct ast_yield, AST_STATEMENT_YIELD_FROM, location);
    statement->yield.expression = expression;
    return statement;
  }
  union ast_expression *expression = NULL;
  if (is_expression_start(peek(s)) || peek(s) == '*') {
    expression = parse_star_expressions(s, PREC_EXPRESSION);
  }
  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_yield, AST_STATEMENT_YIELD, location);
  statement->yield.expression = expression;
  return statement;
}

static union ast_statement *nullable
parse_simple_statement(struct parser_state *s, bool print_expr)
{
  switch (peek(s)) {
  case EXPRESSION_START_CASES:
    return parse_expression_statement(s, print_expr);
  case T_assert:
    return parse_assert(s);
  case T_break:
    return parse_break(s);
  case T_continue:
    return parse_continue(s);
  case T_del:
    return parse_del(s);
  case T_from:
    return parse_from_import_statement(s);
  case T_global:
    return parse_global_statement(s);
  case T_import:
    return parse_import_statement(s);
  case T_nonlocal:
    return parse_nonlocal_statement(s);
  case T_pass:
    return parse_pass(s);
  case T_raise:
    return parse_raise(s);
  case T_return:
    return parse_return(s);
  case T_yield:
    return parse_yield_statement(s);
  default:
    error_expected(s, "statement");
    eat_until_anchor(s);
    return NULL;
  }
}

static struct ast_statement_list *
parse_simple_statements(struct parser_state *s, bool print_expr)
{
  union ast_statement *inline_storage[8];
  struct idynarray     statements;
  idynarray_init(&statements, inline_storage, sizeof(inline_storage));

  add_anchor(s, T_NEWLINE);
  do {
    union ast_statement *statement = parse_simple_statement(s, print_expr);
    if (statement != NULL) {
      *idynarray_append(&statements, union ast_statement *) = statement;
    }
  } while (accept(s, ';') && peek(s) != T_NEWLINE);
  remove_anchor(s, T_NEWLINE);
  expect(s, T_NEWLINE);

  struct ast_statement_list *result = ast_statement_list_from_array(
      s, idynarray_data(&statements),
      idynarray_length(&statements, union ast_statement *));
  idynarray_free(&statements);
  return result;
}

static struct ast_statement_list *parse_suite(struct parser_state *s);
static void                       parse_statement(struct parser_state *s,
                                                  struct idynarray *statements, bool top_level,
                                                  bool print_expr);

static bool statement_is_future_import(union ast_statement *statement)
{
  if (ast_statement_type(statement) != AST_STATEMENT_FROM_IMPORT) return false;
  struct ast_from_import *from_import = &statement->from_import;
  if (from_import->num_prefix_dots != 0) return false;
  struct dotted_name *module = from_import->module;
  return module != NULL && module->num_symbols == 1
         && strcmp(module->symbols[0]->string, "__future__") == 0;
}

static bool is_known_future_feature(const char *name)
{
  static const char *const features[] = {
    "nested_scopes",  "generators",     "division",         "absolute_import",
    "with_statement", "print_function", "unicode_literals", "barry_as_FLUFL",
    "generator_stop", "annotations",    "braces",
  };
  for (unsigned i = 0; i < sizeof(features) / sizeof(features[0]); ++i) {
    if (strcmp(features[i], name) == 0) return true;
  }
  return false;
}

static void validate_future_import(struct parser_state    *s,
                                   struct location         location,
                                   struct ast_from_import *from_import)
{
  if (from_import->import_star) {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "future feature * is not defined");
    diag_end(s->d);
    return;
  }
  for (unsigned i = 0; i < from_import->num_items; ++i) {
    const char *name = from_import->items[i].name->string;
    if (strcmp(name, "braces") == 0) {
      diag_begin_error(s->d, location);
      diag_frag(s->d, "not a chance");
      diag_end(s->d);
    } else if (!is_known_future_feature(name)) {
      diag_begin_error(s->d, location);
      diag_frag(s->d, "future feature ");
      diag_frag(s->d, name);
      diag_frag(s->d, " is not defined");
      diag_end(s->d);
    }
  }
}

static bool statement_is_docstring(union ast_statement *statement)
{
  if (ast_statement_type(statement) != AST_STATEMENT_EXPRESSION) return false;
  union ast_expression *expression = statement->expression.expression;
  if (ast_expression_type(expression) != AST_CONST) return false;
  return object_type(expression->cnst.object) == OBJECT_STRING;
}

static void
parser_handle_top_level_future_statement(struct parser_state *s,
                                         union ast_statement *statement)
{
  if (statement_is_future_import(statement)) {
    struct ast_from_import *from_import = &statement->from_import;
    if (!s->top_level_future_imports_allowed) {
      diag_begin_error(s->d, statement->base.location);
      diag_frag(
          s->d,
          "from __future__ imports must occur at the beginning of the file");
      diag_end(s->d);
      s->top_level_seen_any_statement = true;
      return;
    }
    validate_future_import(s, statement->base.location, from_import);
    if (!from_import->import_star) {
      for (unsigned i = 0; i < from_import->num_items; ++i) {
        struct from_import_item *item = &from_import->items[i];
        if (strcmp(item->name->string, "barry_as_FLUFL") == 0) {
          s->future_flags |= CO_FUTURE_BARRY_AS_BDFL;
        } else if (strcmp(item->name->string, "annotations") == 0) {
          s->future_flags |= CO_FUTURE_ANNOTATIONS;
        }
      }
    }
    s->top_level_seen_any_statement = true;
    return;
  }

  if (!s->top_level_future_imports_allowed) {
    s->top_level_seen_any_statement = true;
    return;
  }

  if (s->top_level_seen_any_statement || !statement_is_docstring(statement)) {
    s->top_level_future_imports_allowed = false;
  }
  s->top_level_seen_any_statement = true;
}

static void append_statement(struct parser_state *s,
                             struct idynarray    *statements,
                             union ast_statement *statement, bool top_level)
{
  if (top_level) {
    parser_handle_top_level_future_statement(s, statement);
  } else if (statement_is_future_import(statement)) {
    diag_begin_error(s->d, statement->base.location);
    diag_frag(
        s->d,
        "from __future__ imports must occur at the beginning of the file");
    diag_end(s->d);
  }
  *idynarray_append(statements, union ast_statement *) = statement;
}

static void append_statement_list(struct parser_state       *s,
                                  struct idynarray          *statements,
                                  struct ast_statement_list *list,
                                  bool                       top_level)
{
  for (unsigned i = 0; i < list->num_statements; ++i) {
    append_statement(s, statements, list->statements[i], top_level);
  }
}

static struct ast_statement_list *empty_statement_list(struct parser_state *s)
{
  return ast_statement_list_from_array(s, NULL, 0);
}

static void parse_type_parameters(struct parser_state *s)
{
  if (accept(s, '[')) {
    diag_begin_error(s->d, scanner_location(&s->scanner));
    diag_frag(s->d, "type parameters are not supported in Python 3.8 target");
    diag_end(s->d);

    add_anchor(s, ']');
    eat_until_anchor(s);
    accept(s, ']');
    remove_anchor(s, ']');
  }
}

static union ast_statement *parse_class(struct parser_state   *s,
                                        unsigned               num_decorators,
                                        union ast_expression **decorators)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_class);
  struct symbol *name = parse_identifier(s);

  parse_type_parameters(s);

  union ast_expression *call;
  if (peek(s) == '(') {
    call = parse_arguments(s, /*callee=*/NULL);
  } else {
    call = ast_allocate_expression_(s, sizeof(struct ast_call), AST_CALL);
    call->call.has_star_argument = false;
    call->call.has_kw_argument = false;
    call->call.callee = NULL;
    call->call.num_arguments = 0;
  }

  expect(s, ':');
  struct symbol *saved_private_class_name = s->private_class_name;
  s->private_class_name = name;
  struct ast_statement_list *body = parse_suite(s);
  s->private_class_name = saved_private_class_name;

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_class, AST_STATEMENT_CLASS, location);
  statement->class_.name = name;
  statement->class_.call = &call->call;
  statement->class_.body = body;
  statement->class_.num_decorators = num_decorators;
  statement->class_.decorators = decorators;
  return statement;
}

static union ast_statement *parse_def(struct parser_state   *s,
                                      unsigned               num_decorators,
                                      union ast_expression **decorators,
                                      bool async_prefix_consumed)
{
  bool async = async_prefix_consumed;
  if (accept(s, T_async)) {
    assert(!async_prefix_consumed);
    async = true;
  }
  struct location location = scanner_location(&s->scanner);
  expect(s, T_def);
  struct symbol *name = parse_identifier(s);

  struct parameter inline_storage[16];
  struct idynarray parameters;
  idynarray_init(&parameters, inline_storage, sizeof(inline_storage));
  struct parameter_shape parameter_shape;

  expect(s, '(');
  parse_parameters(s, &parameters, &parameter_shape, /*end=*/')');

  union ast_expression *return_type = NULL;
  if (accept(s, T_MINUS_GREATER_THAN)) {
    return_type = parse_expression(s, PREC_EXPRESSION);
  }
  expect(s, ':');

  bool  has_yield = false;
  bool *saved_function_has_yield = s->current_function_has_yield;
  s->current_function_has_yield = &has_yield;
  struct ast_statement_list *body = parse_suite(s);
  s->current_function_has_yield = saved_function_has_yield;

  unsigned num_parameters = parameter_shape.num_parameters;
  size_t   parameters_size = num_parameters * sizeof(struct parameter);
  union ast_statement *statement
      = ast_allocate_statement_(s, sizeof(struct ast_def) + parameters_size,
                                AST_STATEMENT_DEF, location);
  statement->def.name = name;
  statement->def.async = async;
  statement->def.has_yield = has_yield;
  statement->def.parameter_shape = parameter_shape;
  if (num_parameters > 0) {
    memcpy(statement->def.parameters, idynarray_data(&parameters),
           parameters_size);
  }
  idynarray_free(&parameters);
  statement->def.return_type = return_type;
  statement->def.body = body;
  statement->def.num_decorators = num_decorators;
  statement->def.decorators = decorators;
  return statement;
}

static union ast_statement *nullable
parse_decorator_statement(struct parser_state *s)
{
  union ast_expression *inline_storage[4];
  struct idynarray      decorators;
  idynarray_init(&decorators, inline_storage, sizeof(inline_storage));

  do {
    eat(s, '@');
    union ast_expression *expression = parse_expression(s, PREC_NAMED);
    expect(s, T_NEWLINE);
    *idynarray_append(&decorators, union ast_expression *) = expression;
  } while (peek(s) == '@');

  unsigned num_decorators
      = idynarray_length(&decorators, union ast_expression *);
  union ast_expression **decorator_arr
      = ast_expression_array_copy_dyn(s, &decorators);
  idynarray_free(&decorators);

  switch (peek(s)) {
  case T_class:
    return parse_class(s, num_decorators, decorator_arr);
  case T_async:
  case T_def:
    return parse_def(s, num_decorators, decorator_arr,
                     /*async_prefix_consumed=*/false);
  default:
    diag_begin_error(s->d, scanner_location(&s->scanner));
    diag_frag(s->d, "expected ");
    diag_token_kind(s->d, '@');
    diag_frag(s->d, ", ");
    diag_token_kind(s->d, T_class);
    diag_frag(s->d, " or ");
    diag_token_kind(s->d, T_def);
    diag_frag(s->d, " after decorator, got ");
    diag_token(s->d, &s->scanner.token);
    diag_end(s->d);
    return NULL;
  }
}

static union ast_statement *parse_for(struct parser_state *s, bool async)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_for);
  union ast_expression *targets = parse_star_expressions(s, PREC_OR);
  targets = check_assignment_target(s, targets, location, /*is_del=*/false,
                                    /*show_equal_hint=*/false);
  expect(s, T_in);
  union ast_expression *expression
      = parse_star_expressions(s, PREC_EXPRESSION);
  /* Somehow a list with star-expression is allowed, but not a single toplevel
   * star-expression. */
  if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
    diag_begin_error(s->d, scanner_location(&s->scanner));
    diag_frag(s->d, "can't use ");
    diag_token_kind(s->d, '*');
    diag_frag(s->d, " expression here");
    diag_end(s->d);
    expression = invalid_expression(s);
  }
  expect(s, ':');
  struct ast_statement_list *body = parse_suite(s);

  struct ast_statement_list *else_body = NULL;
  if (accept(s, T_else)) {
    expect(s, ':');
    else_body = parse_suite(s);
  }

  union ast_statement *statement
      = ast_allocate_statement(s, struct ast_for, AST_STATEMENT_FOR, location);
  statement->for_.async = async;
  statement->for_.targets = targets;
  statement->for_.expression = expression;
  statement->for_.body = body;
  statement->for_.else_body = else_body;
  return statement;
}

static union ast_statement *parse_if(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_if);
  union ast_expression *condition = parse_expression(s, PREC_NAMED);
  expect(s, ':');
  struct ast_statement_list *body = parse_suite(s);

  struct ast_if_elif inline_storage[4];
  struct idynarray   elifs;
  idynarray_init(&elifs, inline_storage, sizeof(inline_storage));

  while (accept(s, T_elif)) {
    struct ast_if_elif *elif_stmt
        = idynarray_append(&elifs, struct ast_if_elif);
    elif_stmt->condition = parse_expression(s, PREC_NAMED);
    expect(s, ':');
    elif_stmt->body = parse_suite(s);
  }

  struct ast_statement_list *else_body = NULL;
  if (accept(s, T_else)) {
    expect(s, ':');
    else_body = parse_suite(s);
  }

  unsigned            num_elifs = idynarray_length(&elifs, struct ast_if_elif);
  struct ast_if_elif *elif_arr = NULL;
  if (num_elifs > 0) {
    size_t size = num_elifs * sizeof(struct ast_if_elif);
    elif_arr = (struct ast_if_elif *)arena_allocate(
        &s->ast, size, alignof(struct ast_if_elif));
    memcpy(elif_arr, idynarray_data(&elifs), size);
  }
  idynarray_free(&elifs);

  union ast_statement *statement
      = ast_allocate_statement(s, struct ast_if, AST_STATEMENT_IF, location);
  statement->if_.condition = condition;
  statement->if_.body = body;
  statement->if_.num_elifs = num_elifs;
  statement->if_.elifs = elif_arr;
  statement->if_.else_body = else_body;
  return statement;
}

static union ast_statement *parse_try(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_try);
  expect(s, ':');
  struct ast_statement_list *body = parse_suite(s);

  struct ast_try_except inline_storage[4];
  struct idynarray      excepts;
  idynarray_init(&excepts, inline_storage, sizeof(inline_storage));

  while (peek(s) == T_except) {
    struct location except_location = scanner_location(&s->scanner);
    eat(s, T_except);
    struct ast_try_except *except_stmt
        = idynarray_append(&excepts, struct ast_try_except);
    except_stmt->location = except_location;
    except_stmt->match = NULL;
    except_stmt->as = NULL;

    if (peek(s) != ':') {
      except_stmt->match = parse_expression(s, PREC_TEST);
      if (accept(s, T_as)) {
        except_stmt->as = parse_identifier(s);
      }
    }
    expect(s, ':');
    except_stmt->body = parse_suite(s);
  }

  bool had_except = idynarray_length(&excepts, struct ast_try_except) > 0;

  struct ast_statement_list *else_body = NULL;
  if (accept(s, T_else)) {
    if (!had_except) {
      diag_begin_error(s->d, scanner_location(&s->scanner));
      diag_token_kind(s->d, T_try);
      diag_frag(s->d, " with ");
      diag_token_kind(s->d, T_else);
      diag_frag(s->d, " requires prior ");
      diag_token_kind(s->d, T_except);
      diag_end(s->d);
    }
    expect(s, ':');
    else_body = parse_suite(s);
  }

  struct ast_statement_list *finally_body = NULL;
  if (accept(s, T_finally)) {
    expect(s, ':');
    finally_body = parse_suite(s);
  }

  while (peek(s) == T_except || peek(s) == T_else) {
    diag_begin_error(s->d, scanner_location(&s->scanner));
    diag_token_kind(s->d, peek(s));
    diag_frag(s->d, " must come before ");
    diag_token_kind(s->d, T_finally);
    diag_end(s->d);
    next_token(s);
    eat_until_anchor(s);
  }

  if (!had_except && finally_body == NULL && else_body == NULL) {
    diag_begin_error(s->d, scanner_location(&s->scanner));
    diag_token_kind(s->d, T_try);
    diag_frag(s->d, "-block requires an ");
    diag_token_kind(s->d, T_except);
    diag_frag(s->d, "- or ");
    diag_token_kind(s->d, T_finally);
    diag_frag(s->d, "-block");
    diag_end(s->d);
  }

  unsigned num_excepts = idynarray_length(&excepts, struct ast_try_except);
  struct ast_try_except *except_arr = NULL;
  if (num_excepts > 0) {
    size_t size = num_excepts * sizeof(struct ast_try_except);
    except_arr = (struct ast_try_except *)arena_allocate(
        &s->ast, size, alignof(struct ast_try_except));
    memcpy(except_arr, idynarray_data(&excepts), size);
  }
  idynarray_free(&excepts);

  union ast_statement *statement
      = ast_allocate_statement(s, struct ast_try, AST_STATEMENT_TRY, location);
  statement->try_.body = body;
  statement->try_.num_excepts = num_excepts;
  statement->try_.excepts = except_arr;
  statement->try_.else_body = else_body;
  statement->try_.finally_body = finally_body;
  return statement;
}

static union ast_statement *parse_while(struct parser_state *s)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_while);
  union ast_expression *condition = parse_expression(s, PREC_NAMED);
  expect(s, ':');
  struct ast_statement_list *body = parse_suite(s);

  struct ast_statement_list *else_body = NULL;
  if (accept(s, T_else)) {
    expect(s, ':');
    else_body = parse_suite(s);
  }

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_while, AST_STATEMENT_WHILE, location);
  statement->while_.condition = condition;
  statement->while_.body = body;
  statement->while_.else_body = else_body;
  return statement;
}

static union ast_statement *parse_with(struct parser_state *s, bool async)
{
  struct location location = scanner_location(&s->scanner);
  eat(s, T_with);

  struct ast_with_item inline_storage[4];
  struct idynarray     items;
  idynarray_init(&items, inline_storage, sizeof(inline_storage));

  do {
    struct ast_with_item *item
        = idynarray_append(&items, struct ast_with_item);
    item->expression = parse_expression(s, PREC_TEST);
    item->as_location = (struct location){ 0 };
    item->targets = NULL;
    if (peek(s) == T_as) {
      item->as_location = scanner_location(&s->scanner);
      eat(s, T_as);
      item->targets = parse_expression(s, PREC_NAMED);
      /* TODO: check_assignment_target */
    }
  } while (accept(s, ','));
  expect(s, ':');
  struct ast_statement_list *body = parse_suite(s);

  unsigned num_items = idynarray_length(&items, struct ast_with_item);
  struct ast_with_item *items_arr = NULL;
  if (num_items > 0) {
    size_t size = num_items * sizeof(struct ast_with_item);
    items_arr = (struct ast_with_item *)arena_allocate(
        &s->ast, size, alignof(struct ast_with_item));
    memcpy(items_arr, idynarray_data(&items), size);
  }
  idynarray_free(&items);

  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_with, AST_STATEMENT_WITH, location);
  statement->with.async = async;
  statement->with.num_items = num_items;
  statement->with.items = items_arr;
  statement->with.body = body;
  return statement;
}

static struct ast_statement_list *parse_suite(struct parser_state *s)
{
  if (accept(s, T_NEWLINE)) {
    if (peek(s) != T_INDENT) {
      error_expected_tok1(s, T_INDENT);
      /* Abort early because a matching T_DEDENT may never arrive. */
      return empty_statement_list(s);
    }
    eat(s, T_INDENT);

    union ast_statement *inline_storage[16];
    struct idynarray     statements;
    idynarray_init(&statements, inline_storage, sizeof(inline_storage));

    /* Add T_DEDENT as an anchor so that error recovery inside statements
     * cannot consume the T_DEDENT that ends this suite.  Without this,
     * eat_until_anchor() can eat the closing T_DEDENT (e.g. when recovering
     * from an error on a line like "lass C:"), leaving this loop spinning at
     * T_EOF with no T_DEDENT ever to find. */
    add_anchor(s, T_DEDENT);
    do {
      parse_statement(s, &statements, /*top_level=*/false,
                      /*print_expr=*/false);
    } while (!accept(s, T_DEDENT));
    remove_anchor(s, T_DEDENT);

    struct ast_statement_list *result = ast_statement_list_from_array(
        s, idynarray_data(&statements),
        idynarray_length(&statements, union ast_statement *));
    idynarray_free(&statements);
    return result;
  }

  return parse_simple_statements(s, /*print_expr=*/false);
}

static void parse_statement(struct parser_state *s,
                            struct idynarray *statements, bool top_level,
                            bool print_expr)
{
  add_anchor(s, T_NEWLINE);
  switch (peek(s)) {
  case '@': {
    union ast_statement *statement = parse_decorator_statement(s);
    if (statement != NULL) {
      append_statement(s, statements, statement, top_level);
    }
    break;
  }
  case T_class:
    append_statement(s, statements, parse_class(s, /*num_decorators=*/0, NULL),
                     top_level);
    break;
  case T_def:
    append_statement(s, statements,
                     parse_def(s, /*num_decorators=*/0, NULL,
                               /*async_prefix_consumed=*/false),
                     top_level);
    break;
  case T_async: {
    eat(s, T_async);
    if (peek(s) == T_for) {
      append_statement(s, statements, parse_for(s, /*async=*/true), top_level);
    } else if (peek(s) == T_with) {
      append_statement(s, statements, parse_with(s, /*async=*/true),
                       top_level);
    } else if (peek(s) == T_def) {
      append_statement(s, statements,
                       parse_def(s, /*num_decorators=*/0, NULL,
                                 /*async_prefix_consumed=*/true),
                       top_level);
    } else {
      diag_begin_error(s->d, scanner_location(&s->scanner));
      diag_frag(s->d, "expected ");
      diag_token_kind(s->d, T_def);
      diag_frag(s->d, ", ");
      diag_token_kind(s->d, T_for);
      diag_frag(s->d, " or ");
      diag_token_kind(s->d, T_with);
      diag_frag(s->d, " after ");
      diag_token_kind(s->d, T_async);
      diag_end(s->d);
      eat_until_anchor(s);
    }
    break;
  }
  case T_for:
    append_statement(s, statements, parse_for(s, /*async=*/false), top_level);
    break;
  case T_if:
    append_statement(s, statements, parse_if(s), top_level);
    break;
  case T_try:
    append_statement(s, statements, parse_try(s), top_level);
    break;
  case T_while:
    append_statement(s, statements, parse_while(s), top_level);
    break;
  case T_with:
    append_statement(s, statements, parse_with(s, /*async=*/false), top_level);
    break;
  case T_INVALID:
    next_token(s);
    break;
  case T_EOF:
    break;
  default: {
    struct ast_statement_list *simple = parse_simple_statements(s, print_expr);
    append_statement_list(s, statements, simple, top_level);
    break;
  }
  }
  remove_anchor(s, T_NEWLINE);
}

struct ast_module *parse_module(struct parser_state *s)
{
  next_token(s);

  union ast_statement *inline_storage[32];
  struct idynarray     statements;
  idynarray_init(&statements, inline_storage, sizeof(inline_storage));

  add_anchor(s, T_EOF);
  while (peek(s) != T_EOF) {
    if (accept(s, T_NEWLINE)) continue;
    parse_statement(s, &statements, /*top_level=*/true,
                    /*print_expr=*/false);
  }

#ifndef NDEBUG
  remove_anchor(s, T_EOF);
  for (uint16_t i = 0; i < sizeof(s->anchor_set) / sizeof(s->anchor_set[0]);
       ++i) {
    if (s->anchor_set[i] != 0) {
      internal_error("Anchor token not removed");
    }
  }
#endif

  struct ast_statement_list *body = ast_statement_list_from_array(
      s, idynarray_data(&statements),
      idynarray_length(&statements, union ast_statement *));
  idynarray_free(&statements);

  struct ast_module *module = arena_allocate(
      &s->ast, sizeof(struct ast_module), alignof(struct ast_module));
  module->body = body;
  module->future_flags = s->future_flags;

  return module;
}

static struct ast_module *
module_from_statement_array(struct parser_state  *s,
                            union ast_statement **statements,
                            unsigned              num_statements)
{
  struct ast_statement_list *body
      = ast_statement_list_from_array(s, statements, num_statements);
  struct ast_module *module = arena_allocate(
      &s->ast, sizeof(struct ast_module), alignof(struct ast_module));
  module->body = body;
  module->future_flags = s->future_flags;
  return module;
}

static void finish_single_input(struct parser_state *s)
{
  while (accept(s, T_NEWLINE)) {
  }
  if (peek(s) != T_EOF) {
    error_expected_tok1(s, T_EOF);
    eat_until_anchor(s);
  }
  remove_anchor(s, T_EOF);
}

static union ast_statement *
make_return_statement(struct parser_state *s, struct location location,
                      union ast_expression *expression)
{
  union ast_statement *statement = ast_allocate_statement(
      s, struct ast_return, AST_STATEMENT_RETURN, location);
  statement->return_.expression = expression;
  return statement;
}

struct ast_module *parse_single_statement(struct parser_state *s)
{
  next_token(s);
  add_anchor(s, T_EOF);
  while (accept(s, T_NEWLINE)) {
  }

  union ast_statement *inline_storage[8];
  struct idynarray     statements;
  idynarray_init(&statements, inline_storage, sizeof(inline_storage));

  if (peek(s) != T_EOF) {
    parse_statement(s, &statements, /*top_level=*/true,
                    /*print_expr=*/true);
  }
  finish_single_input(s);

  struct location       location = scanner_location(&s->scanner);
  union ast_expression *none_expr = ast_const_new(
      s, object_intern_singleton(s->objects, OBJECT_NONE), location);
  union ast_statement *ret_stmt
      = make_return_statement(s, location, none_expr);
  *idynarray_append(&statements, union ast_statement *) = ret_stmt;
  struct ast_module *module = module_from_statement_array(
      s, idynarray_data(&statements),
      idynarray_length(&statements, union ast_statement *));
  idynarray_free(&statements);
  return module;
}

struct ast_module *parse_single_expression(struct parser_state *s)
{
  next_token(s);
  add_anchor(s, T_EOF);

  union ast_expression *expression = parse_star_expressions(s, PREC_NAMED);
  if (ast_expression_type(expression) == AST_UNEXPR_STAR
      || (ast_expression_type(expression) == AST_EXPRESSION_LIST
          && expression->expression_list.has_star_expression)) {
    struct location result_location = get_expression_location(expression);
    diag_begin_error(s->d, result_location);
    diag_frag(s->d, "starred expression not allowed here");
    diag_end(s->d);
    expression = invalid_expression(s);
  }
  finish_single_input(s);

  struct location      location = get_expression_location(expression);
  union ast_statement *ret_stmt
      = make_return_statement(s, location, expression);
  union ast_statement *module_statements[] = { ret_stmt };
  return module_from_statement_array(s, module_statements, 1);
}

void parser_init(struct parser_state *s, struct object_intern *objects,
                 struct diagnostics_state *diagnostics)
{
  memset(s, 0, sizeof(*s));
  arena_init(&s->ast);
  s->objects = objects;
  s->d = diagnostics;
  s->top_level_future_imports_allowed = true;
  memset(s->anchor_set, 0, sizeof(s->anchor_set));
}

void parser_set_flags(struct parser_state *s, uint32_t flags)
{
  s->future_flags = flags & PyCF_MASK;
}

void parser_free(struct parser_state *s)
{
  arena_free(&s->ast);
}

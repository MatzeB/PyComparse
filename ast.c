#include "ast.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

#include "ast_expression_types.h"
#include "ast_statement_types.h"
#include "ast_types.h"
#include "object_intern.h"
#include "util.h"

enum ast_expression_type ast_expression_type(union ast_expression *expression)
{
  return expression->type;
}

enum ast_statement_type ast_statement_type(union ast_statement *statement)
{
  return statement->type;
}

union object *ast_expression_as_constant(union ast_expression *expression)
{
  switch (ast_expression_type(expression)) {
  case AST_CONST:
    return expression->cnst.object;
  case AST_EXPRESSION_LIST:
    return expression->expression_list.as_constant;
  default:
    return NULL;
  }
}

struct location get_expression_location(union ast_expression *expression)
{
  switch (ast_expression_type(expression)) {
  case AST_ATTR:
    return get_expression_location(expression->attr.expression);
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_ADD_ASSIGN:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_AND_ASSIGN:
  case AST_BINEXPR_ASSIGN:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_FLOORDIV_ASSIGN:
  case AST_BINEXPR_LOGICAL_AND:
  case AST_BINEXPR_LOGICAL_OR:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MATMUL_ASSIGN:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MOD_ASSIGN:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_MUL_ASSIGN:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_OR_ASSIGN:
  case AST_BINEXPR_POWER:
  case AST_BINEXPR_POWER_ASSIGN:
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_LEFT_ASSIGN:
  case AST_BINEXPR_SHIFT_RIGHT:
  case AST_BINEXPR_SHIFT_RIGHT_ASSIGN:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_SUB_ASSIGN:
  case AST_BINEXPR_SUBSCRIPT:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_TRUEDIV_ASSIGN:
  case AST_BINEXPR_XOR:
  case AST_BINEXPR_XOR_ASSIGN:
    return get_expression_location(expression->binexpr.left);
  case AST_CALL:
    assert(expression->call.callee != NULL);
    return get_expression_location(expression->call.callee);
  case AST_COMPARISON:
    return get_expression_location(expression->comparison.left);
  case AST_CONDITIONAL:
    return get_expression_location(expression->conditional.true_expression);
  case AST_CONST:
  case AST_DICT_COMPREHENSION:
  case AST_DICT_DISPLAY:
  case AST_EXPRESSION_LIST:
  case AST_FSTRING:
  case AST_GENERATOR_EXPRESSION:
  case AST_IDENTIFIER:
  case AST_INVALID:
  case AST_LAMBDA:
  case AST_LIST_COMPREHENSION:
  case AST_LIST_DISPLAY:
  case AST_SET_COMPREHENSION:
  case AST_SET_DISPLAY:
  case AST_UNEXPR_AWAIT:
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_NOT:
  case AST_UNEXPR_PLUS:
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
  case AST_YIELD:
  case AST_YIELD_FROM:
    return expression->with_location.location;
  case AST_SLICE: {
    if (expression->slice.start != NULL) {
      struct location location
          = get_expression_location(expression->slice.start);
      if (location.line > 0) {
        return location;
      }
    }
    if (expression->slice.stop != NULL) {
      struct location location
          = get_expression_location(expression->slice.stop);
      if (location.line > 0) {
        return location;
      }
    }
    if (expression->slice.step != NULL) {
      return get_expression_location(expression->slice.step);
    }
    return INVALID_LOCATION;
  }
  }

  internal_error("invalid expression type");
}

union object *ast_tuple_compute_constant(struct object_intern       *intern,
                                         struct ast_expression_list *tuple)
{
  /* Check that all arguments are constant or give up. */
  unsigned num_expressions = tuple->num_expressions;
  for (unsigned i = 0; i < num_expressions; i++) {
    union ast_expression *expression = tuple->expressions[i];
    if (ast_expression_as_constant(expression) == NULL) {
      return NULL;
    }
  }

  struct tuple_prep *constant_prep
      = object_intern_tuple_begin(intern, num_expressions);
  for (unsigned i = 0; i < num_expressions; i++) {
    union ast_expression *expression = tuple->expressions[i];
    union object *expression_constant = ast_expression_as_constant(expression);
    object_new_tuple_set_at(constant_prep, i, expression_constant);
  }
  return object_intern_tuple_end(intern, constant_prep,
                                 /*may_free_arena=*/true);
}

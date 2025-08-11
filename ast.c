#include "ast.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

#include "ast_types.h"
#include "object_intern.h"

enum ast_expression_type ast_expression_type(union ast_expression *expression)
{
  return expression->type;
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

  struct arena *arena = object_intern_arena(intern);
  union object *constant = object_new_tuple_begin(arena, num_expressions);
  for (unsigned i = 0; i < num_expressions; i++) {
    union ast_expression *expression = tuple->expressions[i];
    union object *expression_constant = ast_expression_as_constant(expression);
    object_new_tuple_set_at(constant, i, expression_constant);
  }
  object_new_tuple_end(constant);
  /* TODO: intern? */
  return constant;
}

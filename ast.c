#include "ast.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

#include "ast_types.h"
#include "object_intern.h"

union object *ast_expression_as_constant(union ast_expression *expression)
{
  switch (expression->type) {
  case AST_CONST:
    return expression->cnst.object;
  case AST_TUPLE_FORM:
    return expression->tuple_list_form.as_constant;
  case AST_ATTR:
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_ASSIGN:
  case AST_BINEXPR_EQUAL:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_GREATER:
  case AST_BINEXPR_GREATER_EQUAL:
  case AST_BINEXPR_IN:
  case AST_BINEXPR_IS:
  case AST_BINEXPR_IS_NOT:
  case AST_BINEXPR_LESS:
  case AST_BINEXPR_LESS_EQUAL:
  case AST_BINEXPR_LOGICAL_AND:
  case AST_BINEXPR_LOGICAL_OR:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_NOT_IN:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_UNEQUAL:
  case AST_BINEXPR_XOR:
  case AST_CALL:
  case AST_IDENTIFIER:
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_NOT:
  case AST_UNEXPR_PLUS:
    return NULL;
  }
  abort();
}

union object *ast_tuple_compute_constant(struct object_intern *intern,
                                         struct ast_tuple_list_form *tuple)
{
  /* Check that all arguments are constant or give up. */
  unsigned length = 0;
  for (struct argument *argument = tuple->arguments; argument != NULL;
       argument = argument->next) {
    union ast_expression *expression = argument->expression;
    if (ast_expression_as_constant(expression) == NULL) {
      return NULL;
    }
    length++;
  }

  struct arena *arena = object_intern_arena(intern);
  union object *constant = object_new_tuple_begin(arena, length);
  uint32_t index = 0;
  for (struct argument *argument = tuple->arguments; argument != NULL;
       argument = argument->next) {
    union ast_expression *expression = argument->expression;
    union object *expression_constant = ast_expression_as_constant(expression);
    object_new_tuple_set_at(constant, index, expression_constant);
    index++;
  }
  object_new_tuple_end(constant);
  /* TODO: intern? */
  return constant;
}

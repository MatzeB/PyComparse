#include "codegen_expression.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "ast.h"
#include "ast_types.h"
#include "codegen.h"
#include "codegen_types.h"
#include "objects.h"
#include "objects_types.h"
#include "opcodes.h"
#include "symbol_types.h"
#include "symbol_info_types.h"
#include "util.h"

static void emit_binexpr(struct cg_state *cg, struct ast_binexpr *binexpr,
                         uint8_t opcode)
{
  emit_expression(cg, binexpr->left);
  emit_expression(cg, binexpr->right);
  cg_pop_op(cg, opcode, 0);
}

static void emit_comparison(struct cg_state *cg, struct ast_binexpr *binexpr,
                            enum compare_op_arg arg)
{
  emit_expression(cg, binexpr->left);
  emit_expression(cg, binexpr->right);
  cg_pop_op(cg, OPCODE_COMPARE_OP, arg);
}

static void emit_unexpr(struct cg_state *cg, struct ast_unexpr *unexpr,
                        uint8_t opcode)
{
  emit_expression(cg, unexpr->op);
  cg_op(cg, opcode, 0);
}

void emit_store(struct cg_state *cg, struct symbol *symbol)
{
  struct symbol_info *info = cg_symbol_info(cg, symbol);
  if (info == NULL) {
    info = cg_new_symbol_info(cg, symbol);
    if (cg_use_locals(cg)) {
      info->type = SYMBOL_LOCAL;
      info->index = cg_append_varname(cg, symbol->string);
    } else {
      info->type = SYMBOL_NAME;
      info->index = cg_append_name(cg, symbol->string);
    }
  }

  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_pop_op(cg, OPCODE_STORE_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_pop_op(cg, OPCODE_STORE_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_pop_op(cg, OPCODE_STORE_FAST, info->index);
    return;
  }
  fprintf(stderr, "invalid symbol_info type\n");
  abort();
}

void emit_load(struct cg_state *cg, struct symbol *symbol)
{
  struct symbol_info *info = cg_symbol_info(cg, symbol);
  if (info == NULL) {
    info = cg_new_symbol_info(cg, symbol);
    if (cg_use_locals(cg)) {
      info->type = SYMBOL_GLOBAL;
    } else {
      info->type = SYMBOL_NAME;
    }
    info->index = cg_append_name(cg, symbol->string);
  }

  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_push_op(cg, OPCODE_LOAD_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_push_op(cg, OPCODE_LOAD_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_push_op(cg, OPCODE_LOAD_FAST, info->index);
    return;
  }
  fprintf(stderr, "invalid symbol_info type\n");
  abort();
}

void emit_assignment(struct cg_state *cg, union ast_expression *target)
{
  if (target->type == AST_IDENTIFIER) {
    emit_store(cg, target->identifier.symbol);
  } else if (target->type == AST_ATTR) {
    struct ast_attr *attr = &target->attr;
    emit_expression(cg, attr->expression);
    unsigned index = cg_append_name(cg, attr->attr->string);
    cg_op(cg, OPCODE_STORE_ATTR, index);
    cg_pop(cg, 2);
  } else {
    fprintf(stderr, "Unsupported or invalid lvalue\n");
    unimplemented();
  }
}

static union object *constant_expression(struct cg_state *cg,
                                         union ast_expression *expression)
{
  switch (expression->type) {
  case AST_CONST:
    return object_list_at(cg->code.consts, expression->cnst.index);
  case AST_TUPLE_FORM: {
    struct ast_tuple_form *tuple_form = &expression->tuple_form;
    /* TODO: list "leaks" and stays in arena... */
    union object *list = object_new_list(&cg->objects);
    for (struct argument *argument = tuple_form->arguments;
         argument != NULL; argument = argument->next) {
      union object *const_arg = constant_expression(cg, argument->expression);
      if (const_arg == NULL) return NULL;
      object_list_append(list, const_arg);
    }
    union object *tuple = object_new_tuple(&cg->objects, list->list.length);
    for (unsigned i = 0, e = list->list.length; i < e; i++) {
      tuple->tuple.items[i] = list->list.items[i];
    }
    return tuple;
  }
  case AST_ATTR:
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_ASSIGN:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_GREATER:
  case AST_BINEXPR_LESS:
  case AST_BINEXPR_EQUAL:
  case AST_BINEXPR_GREATER_EQUAL:
  case AST_BINEXPR_LESS_EQUAL:
  case AST_BINEXPR_UNEQUAL:
  case AST_BINEXPR_IN:
  case AST_BINEXPR_NOT_IN:
  case AST_BINEXPR_IS:
  case AST_BINEXPR_IS_NOT:
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

static void emit_nonconst_tuple_form(struct cg_state *cg,
                                     struct ast_tuple_form *tuple_form)
{
  unsigned num_elements = 0;
  for (struct argument *argument = tuple_form->arguments;
       argument != NULL; argument = argument->next) {
    emit_expression(cg, argument->expression);
    num_elements++;
  }
  cg_op(cg, OPCODE_BUILD_TUPLE, num_elements);
  cg_pop(cg, num_elements-1);
}

static void emit_attr(struct cg_state *cg, struct ast_attr *attr)
{
  emit_expression(cg, attr->expression);
  unsigned index = cg_append_name(cg, attr->attr->string);
  cg_op(cg, OPCODE_LOAD_ATTR, index);
}

static void emit_call(struct cg_state *cg, struct ast_call *call, bool drop)
{
  emit_expression(cg, call->callee);
  unsigned n_arguments = 0;
  for (struct argument *argument = call->arguments; argument != NULL;
       argument = argument->next) {
    emit_expression(cg, argument->expression);
    ++n_arguments;
  }
  cg_op(cg, OPCODE_CALL_FUNCTION, n_arguments);
  cg_pop(cg, n_arguments);
  if (drop) {
    cg_pop_op(cg, OPCODE_POP_TOP, 0);
  }
}

static void emit_expression_impl(struct cg_state *cg,
                                 union ast_expression *expression, bool drop)
{
  switch (expression->type) {
  case AST_ATTR:
    emit_attr(cg, &expression->attr);
    break;
  case AST_BINEXPR_ADD:
    emit_binexpr(cg, &expression->binexpr, OPCODE_BINARY_ADD);
    break;
  case AST_BINEXPR_ASSIGN:
    emit_expression(cg, expression->binexpr.right);
    if (!drop) {
      cg_push_op(cg, OPCODE_DUP_TOP, 0);
    }
    emit_assignment(cg, expression->binexpr.left);
    return;
  case AST_BINEXPR_FLOORDIV:
    emit_binexpr(cg, &expression->binexpr, OPCODE_BINARY_FLOOR_DIVIDE);
    break;
  case AST_BINEXPR_TRUEDIV:
    emit_binexpr(cg, &expression->binexpr, OPCODE_BINARY_TRUE_DIVIDE);
    break;
  case AST_BINEXPR_MATMUL:
    emit_binexpr(cg, &expression->binexpr, OPCODE_BINARY_MATRIX_MULTIPLY);
    break;
  case AST_BINEXPR_SUB:
    emit_binexpr(cg, &expression->binexpr, OPCODE_BINARY_SUBTRACT);
    break;
  case AST_BINEXPR_MUL:
    emit_binexpr(cg, &expression->binexpr, OPCODE_BINARY_MULTIPLY);
    break;
  case AST_BINEXPR_LESS:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_LT);
    break;
  case AST_BINEXPR_LESS_EQUAL:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_LE);
    break;
  case AST_BINEXPR_EQUAL:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_EQ);
    break;
  case AST_BINEXPR_UNEQUAL:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_NE);
    break;
  case AST_BINEXPR_GREATER:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_GT);
    break;
  case AST_BINEXPR_GREATER_EQUAL:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_GE);
    break;
  case AST_BINEXPR_IN:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_IN);
    break;
  case AST_BINEXPR_NOT_IN:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_NOT_IN);
    break;
  case AST_BINEXPR_IS:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_IS);
    break;
  case AST_BINEXPR_IS_NOT:
    emit_comparison(cg, &expression->binexpr, COMPARE_OP_IS_NOT);
    break;
  case AST_CALL: {
    emit_call(cg, &expression->call, drop);
    return;
  }
  case AST_CONST:
    cg_push_op(cg, OPCODE_LOAD_CONST, expression->cnst.index);
    break;
  case AST_IDENTIFIER:
    assert(!drop);
    emit_load(cg, expression->identifier.symbol);
    break;
  case AST_TUPLE_FORM: {
    union object *object = constant_expression(cg, expression);
    if (object != NULL) {
      unsigned index = cg_register_object(cg, object);
      cg_push_op(cg, OPCODE_LOAD_CONST, index);
    } else {
      emit_nonconst_tuple_form(cg, &expression->tuple_form);
    }
    break;
  }
  case AST_UNEXPR_PLUS:
    emit_unexpr(cg, &expression->unexpr, OPCODE_UNARY_POSITIVE);
    break;
  case AST_UNEXPR_NEGATIVE:
    emit_unexpr(cg, &expression->unexpr, OPCODE_UNARY_NEGATIVE);
    break;
  case AST_UNEXPR_NOT:
    emit_unexpr(cg, &expression->unexpr, OPCODE_UNARY_NOT);
    break;
  case AST_UNEXPR_INVERT:
    emit_unexpr(cg, &expression->unexpr, OPCODE_UNARY_INVERT);
    break;
  }
  if (drop) {
    cg_pop_op(cg, OPCODE_POP_TOP, 0);
  }
}

void emit_expression(struct cg_state *s, union ast_expression *expression)
{
  emit_expression_impl(s, expression, false);
}

void emit_expression_drop_result(struct cg_state *s,
                                 union ast_expression *expression)
{
  emit_expression_impl(s, expression, true);
}

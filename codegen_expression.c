#include "codegen_expression.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "ast.h"
#include "ast_types.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "codegen_types.h"
#include "object.h"
#include "object_types.h"
#include "opcodes.h"
#include "symbol_info_types.h"
#include "symbol_table.h"
#include "symbol_types.h"
#include "util.h"

static void emit_binexpr(struct cg_state *s, struct ast_binexpr *binexpr,
                         uint8_t opcode)
{
  emit_expression(s, binexpr->left);
  emit_expression(s, binexpr->right);
  cg_pop_op(s, opcode, 0);
}

static void emit_comparison(struct cg_state *s, struct ast_binexpr *binexpr,
                            enum compare_op_arg arg)
{
  emit_expression(s, binexpr->left);
  emit_expression(s, binexpr->right);
  cg_pop_op(s, OPCODE_COMPARE_OP, arg);
}

static void emit_unexpr(struct cg_state *s, struct ast_unexpr *unexpr,
                        uint8_t opcode)
{
  emit_expression(s, unexpr->op);
  cg_op(s, opcode, 0);
}

void emit_store(struct cg_state *s, struct symbol *symbol)
{
  struct symbol_info *info = cg_symbol_info(s, symbol);
  if (info == NULL) {
    info = cg_new_symbol_info(s, symbol);
    if (cg_use_locals(s)) {
      info->type = SYMBOL_LOCAL;
      info->index = cg_append_varname(s, symbol->string);
    } else {
      info->type = SYMBOL_NAME;
      info->index = cg_register_name(s, symbol->string);
    }
  }

  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_pop_op(s, OPCODE_STORE_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_pop_op(s, OPCODE_STORE_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_pop_op(s, OPCODE_STORE_FAST, info->index);
    return;
  }
  fprintf(stderr, "invalid symbol_info type\n");
  abort();
}

void emit_load(struct cg_state *s, struct symbol *symbol)
{
  struct symbol_info *info = cg_symbol_info(s, symbol);
  if (info == NULL) {
    info = cg_new_symbol_info(s, symbol);
    if (cg_use_locals(s)) {
      info->type = SYMBOL_GLOBAL;
    } else {
      info->type = SYMBOL_NAME;
    }
    info->index = cg_append_name(s, symbol->string);
  }

  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_push_op(s, OPCODE_LOAD_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_push_op(s, OPCODE_LOAD_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_push_op(s, OPCODE_LOAD_FAST, info->index);
    return;
  }
  fprintf(stderr, "invalid symbol_info type\n");
  abort();
}

void emit_assignment(struct cg_state *s, union ast_expression *target)
{
  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER:
    emit_store(s, target->identifier.symbol);
    return;
  case AST_ATTR: {
    struct ast_attr *attr = &target->attr;
    emit_expression(s, attr->expression);
    unsigned index = cg_append_name(s, attr->attr->string);
    cg_op(s, OPCODE_STORE_ATTR, index);
    cg_pop(s, 2);
    return;
  }
  case AST_BINEXPR_SUBSCRIPT: {
    struct ast_binexpr *binexpr = &target->binexpr;
    emit_expression(s, binexpr->left);
    emit_expression(s, binexpr->right);
    cg_op(s, OPCODE_STORE_SUBSCR, 0);
    cg_pop(s, 3);
    return;
  }
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY: {
    struct ast_expression_list *list = &target->expression_list;
    unsigned                    num_expressions = list->num_expressions;
    cg_pop_op(s, OPCODE_UNPACK_SEQUENCE, num_expressions);
    cg_push(s, num_expressions);
    for (unsigned i = 0; i < num_expressions; i++) {
      emit_assignment(s, list->expressions[i]);
    }
    return;
  }
  default:
    break;
  }
  fprintf(stderr, "Unsupported or invalid lvalue\n");
  unimplemented();
}

static void emit_dictionary_display(struct cg_state           *s,
                                    struct ast_dict_item_list *list)
{
  unsigned num_items = list->num_items;
  for (unsigned i = 0; i < num_items; i++) {
    struct dict_item *item = &list->items[i];
    emit_expression(s, item->key);
    emit_expression(s, item->value);
  }
  cg_pop(s, num_items * 2);
  cg_push_op(s, OPCODE_BUILD_MAP, num_items);
}

static void
emit_list_comprehension(struct cg_state                 *s,
                        struct ast_generator_expression *generator_expression)
{
  cg_push_code(s);
  cg_code_begin(s, /*use_locals=*/true);
  emit_generator_expression_code(s, generator_expression);
  emit_code_end(s);

  union object *code = cg_pop_code(s, "<listcomp>");
  unsigned      code_index = cg_register_object(s, code);
  cg_push_op(s, OPCODE_LOAD_CONST, code_index);
  cg_load_const(s, object_intern_cstring(&s->objects, "<listcomp>"));
  cg_pop_op(s, OPCODE_MAKE_FUNCTION, 0);

  struct generator_expression_part *part = &generator_expression->parts[0];
  emit_expression(s, part->expression);
  cg_op(s, OPCODE_GET_ITER, 0);
  cg_pop(s, 2);
  cg_push_op(s, OPCODE_CALL_FUNCTION, 1);
}

static void emit_list_display(struct cg_state            *s,
                              struct ast_expression_list *list)
{
  unsigned num_expressions = list->num_expressions;
  for (unsigned i = 0; i < num_expressions; i++) {
    emit_expression(s, list->expressions[i]);
  }
  cg_pop(s, num_expressions);
  cg_push_op(s, OPCODE_BUILD_LIST, num_expressions);
}

static void emit_set_display(struct cg_state            *s,
                             struct ast_expression_list *list)
{
  unsigned num_expressions = list->num_expressions;
  for (unsigned i = 0; i < num_expressions; i++) {
    emit_expression(s, list->expressions[i]);
  }
  cg_pop(s, num_expressions);
  cg_push_op(s, OPCODE_BUILD_SET, num_expressions);
}

static void emit_none(struct cg_state *s)
{
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
}

static void emit_slice(struct cg_state *s, struct ast_slice *slice)
{
  if (slice->start) {
    emit_expression(s, slice->start);
  } else {
    emit_none(s);
  }
  if (slice->stop) {
    emit_expression(s, slice->stop);
  } else {
    emit_none(s);
  }
  unsigned arg;
  if (slice->step) {
    emit_expression(s, slice->step);
    arg = 3;
  } else {
    arg = 2;
  }
  cg_pop(s, arg);
  cg_push_op(s, OPCODE_BUILD_SLICE, arg);
}

static void emit_tuple(struct cg_state *s, struct ast_expression_list *tuple)
{
  union object *object = tuple->as_constant;
  if (object != NULL) {
    cg_load_const(s, object);
    return;
  }

  unsigned num_expressions = tuple->num_expressions;
  for (unsigned i = 0; i < num_expressions; i++) {
    emit_expression(s, tuple->expressions[i]);
  }
  cg_pop(s, num_expressions);
  cg_push_op(s, OPCODE_BUILD_TUPLE, num_expressions);
}

static void emit_attr(struct cg_state *s, struct ast_attr *attr)
{
  emit_expression(s, attr->expression);
  unsigned index = cg_append_name(s, attr->attr->string);
  cg_op(s, OPCODE_LOAD_ATTR, index);
}

static void emit_call(struct cg_state *s, struct ast_call *call)
{
  emit_expression(s, call->callee);
  unsigned n_arguments = 0;
  for (struct argument *argument = call->arguments; argument != NULL;
       argument = argument->next) {
    emit_expression(s, argument->expression);
    ++n_arguments;
  }
  cg_op(s, OPCODE_CALL_FUNCTION, n_arguments);
  cg_pop(s, n_arguments);
}

static void emit_generator_expression(
    struct cg_state *s, struct ast_generator_expression *generator_expression)
{
  cg_push_code(s);
  cg_code_begin(s, /*use_locals=*/true);
  emit_generator_expression_code(s, generator_expression);
  emit_code_end(s);

  union object *code = cg_pop_code(s, "<genexpr>");
  unsigned      code_index = cg_register_object(s, code);
  cg_push_op(s, OPCODE_LOAD_CONST, code_index);
  cg_load_const(s, object_intern_cstring(&s->objects, "<genexpr>"));
  cg_pop_op(s, OPCODE_MAKE_FUNCTION, 0);

  struct generator_expression_part *part = &generator_expression->parts[0];
  emit_expression(s, part->expression);
  cg_op(s, OPCODE_GET_ITER, 0);
  cg_pop(s, 2);
  cg_push_op(s, OPCODE_CALL_FUNCTION, 1);
}

static void emit_expression_impl(struct cg_state      *s,
                                 union ast_expression *expression, bool drop)
{
  switch (ast_expression_type(expression)) {
  case AST_ATTR:
    emit_attr(s, &expression->attr);
    break;
  case AST_BINEXPR_ADD:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_ADD);
    break;
  case AST_BINEXPR_AND:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_AND);
    break;
  case AST_BINEXPR_ASSIGN:
    emit_expression(s, expression->binexpr.right);
    if (!drop) {
      cg_push_op(s, OPCODE_DUP_TOP, 0);
    }
    emit_assignment(s, expression->binexpr.left);
    return;
  case AST_BINEXPR_EQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_EQ);
    break;
  case AST_BINEXPR_FLOORDIV:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_FLOOR_DIVIDE);
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
  case AST_BINEXPR_IS:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_IS);
    break;
  case AST_BINEXPR_IS_NOT:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_IS_NOT);
    break;
  case AST_BINEXPR_LESS:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_LT);
    break;
  case AST_BINEXPR_LESS_EQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_LE);
    break;
  case AST_BINEXPR_LOGICAL_AND:
    unimplemented();
  case AST_BINEXPR_LOGICAL_OR:
    unimplemented();
  case AST_BINEXPR_MATMUL:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_MATRIX_MULTIPLY);
    break;
  case AST_BINEXPR_MOD:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_MODULO);
    break;
  case AST_BINEXPR_MUL:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_MULTIPLY);
    break;
  case AST_BINEXPR_NOT_IN:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_NOT_IN);
    break;
  case AST_BINEXPR_OR:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_OR);
    break;
  case AST_BINEXPR_SUB:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_SUBTRACT);
    break;
  case AST_BINEXPR_SUBSCRIPT:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_SUBSCR);
    break;
  case AST_BINEXPR_TRUEDIV:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_TRUE_DIVIDE);
    break;
  case AST_BINEXPR_UNEQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_NE);
    break;
  case AST_BINEXPR_XOR:
    emit_binexpr(s, &expression->binexpr, OPCODE_BINARY_XOR);
    break;
  case AST_CALL:
    emit_call(s, &expression->call);
    break;
  case AST_CONST:
    cg_load_const(s, expression->cnst.object);
    break;
  case AST_DICT_DISPLAY:
    emit_dictionary_display(s, &expression->dict_item_list);
    break;
  case AST_EXPRESSION_LIST:
    emit_tuple(s, &expression->expression_list);
    break;
  case AST_GENERATOR_EXPRESSION:
    emit_generator_expression(s, &expression->generator_expression);
    break;
  case AST_IDENTIFIER:
    emit_load(s, expression->identifier.symbol);
    break;
  case AST_LIST_COMPREHENSION:
    emit_list_comprehension(s, &expression->generator_expression);
    break;
  case AST_LIST_DISPLAY:
    emit_list_display(s, &expression->expression_list);
    break;
  case AST_SET_DISPLAY:
    emit_set_display(s, &expression->expression_list);
    break;
  case AST_SLICE:
    emit_slice(s, &expression->slice);
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
  }
  if (drop) {
    cg_pop_op(s, OPCODE_POP_TOP, 0);
  }
}

void emit_expression(struct cg_state *s, union ast_expression *expression)
{
  emit_expression_impl(s, expression, false);
}

void emit_expression_drop_result(struct cg_state      *s,
                                 union ast_expression *expression)
{
  emit_expression_impl(s, expression, true);
}

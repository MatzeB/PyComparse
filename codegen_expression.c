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

static enum opcode ast_binexpr_to_opcode(enum ast_expression_type type)
{
  static const enum opcode opcodes[] = {
    [AST_BINEXPR_ADD] = OPCODE_BINARY_ADD,
    [AST_BINEXPR_ADD_ASSIGN] = OPCODE_INPLACE_ADD,
    [AST_BINEXPR_AND] = OPCODE_BINARY_AND,
    [AST_BINEXPR_AND_ASSIGN] = OPCODE_INPLACE_AND,
    [AST_BINEXPR_FLOORDIV] = OPCODE_BINARY_FLOOR_DIVIDE,
    [AST_BINEXPR_FLOORDIV_ASSIGN] = OPCODE_INPLACE_FLOOR_DIVIDE,
    [AST_BINEXPR_MATMUL] = OPCODE_BINARY_MATRIX_MULTIPLY,
    [AST_BINEXPR_MATMUL_ASSIGN] = OPCODE_INPLACE_MATRIX_MULTIPLY,
    [AST_BINEXPR_MOD] = OPCODE_BINARY_MODULO,
    [AST_BINEXPR_MOD_ASSIGN] = OPCODE_INPLACE_MODULO,
    [AST_BINEXPR_MUL] = OPCODE_BINARY_MULTIPLY,
    [AST_BINEXPR_MUL_ASSIGN] = OPCODE_INPLACE_MULTIPLY,
    [AST_BINEXPR_OR] = OPCODE_BINARY_OR,
    [AST_BINEXPR_OR_ASSIGN] = OPCODE_INPLACE_OR,
    [AST_BINEXPR_POWER] = OPCODE_BINARY_POWER,
    [AST_BINEXPR_POWER_ASSIGN] = OPCODE_INPLACE_POWER,
    [AST_BINEXPR_SHIFT_LEFT] = OPCODE_BINARY_LSHIFT,
    [AST_BINEXPR_SHIFT_LEFT_ASSIGN] = OPCODE_INPLACE_LSHIFT,
    [AST_BINEXPR_SHIFT_RIGHT] = OPCODE_INPLACE_RSHIFT,
    [AST_BINEXPR_SHIFT_RIGHT_ASSIGN] = OPCODE_INPLACE_RSHIFT,
    [AST_BINEXPR_SUB] = OPCODE_BINARY_SUBTRACT,
    [AST_BINEXPR_SUB_ASSIGN] = OPCODE_INPLACE_SUBTRACT,
    [AST_BINEXPR_SUBSCRIPT] = OPCODE_BINARY_SUBSCR,
    [AST_BINEXPR_TRUEDIV] = OPCODE_BINARY_TRUE_DIVIDE,
    [AST_BINEXPR_TRUEDIV_ASSIGN] = OPCODE_INPLACE_TRUE_DIVIDE,
    [AST_BINEXPR_XOR] = OPCODE_BINARY_XOR,
    [AST_BINEXPR_XOR_ASSIGN] = OPCODE_INPLACE_XOR,
  };
  assert(type < sizeof(opcodes) / sizeof(opcodes[0]));
  enum opcode result = opcodes[type];
  assert(result > 0);
  return result;
}

static void emit_binexpr(struct cg_state *s, struct ast_binexpr *binexpr)
{
  emit_expression(s, binexpr->left);
  emit_expression(s, binexpr->right);
  enum opcode opcode = ast_binexpr_to_opcode(binexpr->base.type);
  cg_pop_op(s, opcode, 0);
}

static void emit_binexpr_logical(struct cg_state    *s,
                                 struct ast_binexpr *binexpr,
                                 enum opcode         opcode)
{
  emit_expression(s, binexpr->left);
  struct basic_block *target = cg_allocate_block(s);
  struct basic_block *fallthrough = cg_allocate_block(s);
  cg_pop(s, 1);
  assert(opcode == OPCODE_JUMP_IF_TRUE_OR_POP
         || opcode == OPCODE_JUMP_IF_FALSE_OR_POP);
  emit_condjump(s, opcode, /*target=*/target,
                /*fallthrough=*/fallthrough);
  cg_block_begin(s, fallthrough);
  emit_expression(s, binexpr->right);
  emit_jump(s, target);
  cg_block_begin(s, target);
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
    unsigned index = cg_append_name(s, attr->symbol->string);
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
    abort();
  }
}

static void emit_binexpr_assign_inner(struct cg_state    *s,
                                      struct ast_binexpr *binexpr)
{
  emit_expression(s, binexpr->right);
  enum opcode opcode = ast_binexpr_to_opcode(binexpr->base.type);
  cg_pop_op(s, opcode, 0);
}

static void emit_binexpr_assign(struct cg_state    *s,
                                struct ast_binexpr *binexpr)
{
  union ast_expression *target = binexpr->left;
  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER: {
    struct symbol *symbol = target->identifier.symbol;
    emit_load(s, symbol);
    emit_binexpr_assign_inner(s, binexpr);
    emit_store(s, symbol);
    return;
  }
  case AST_ATTR: {
    struct ast_attr *attr = &target->attr;
    emit_expression(s, attr->expression);
    cg_push_op(s, OPCODE_DUP_TOP, 0);
    unsigned index = cg_append_name(s, attr->symbol->string);
    cg_op(s, OPCODE_LOAD_ATTR, index);
    emit_binexpr_assign_inner(s, binexpr);
    cg_op(s, OPCODE_ROT_TWO, 0);
    cg_op(s, OPCODE_STORE_ATTR, index);
    cg_pop(s, 2);
    return;
  }
  case AST_BINEXPR_SUBSCRIPT: {
    struct ast_binexpr *subscript = &target->binexpr;
    emit_expression(s, subscript->left);
    emit_expression(s, subscript->right);
    cg_op(s, OPCODE_DUP_TOP_TWO, 0);
    cg_push(s, 2);
    cg_pop_op(s, OPCODE_BINARY_SUBSCR, 0);
    emit_binexpr_assign_inner(s, binexpr);
    cg_op(s, OPCODE_ROT_THREE, 0);
    cg_op(s, OPCODE_STORE_SUBSCR, 0);
    cg_pop(s, 3);
    return;
  }
  default:
    abort();
  }
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

static void
emit_expression_list_helper_unpack(struct cg_state            *s,
                                   struct ast_expression_list *tuple)
{
  unsigned               num_expressions = tuple->num_expressions;
  unsigned               num_unpack_expressions = 0;
  unsigned               idx = 0;
  union ast_expression **expressions = tuple->expressions;
  while (idx < num_expressions) {
    union ast_expression *expression = expressions[idx];
    if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
      emit_expression(s, expression->unexpr.op);
      ++idx;
      ++num_unpack_expressions;
      continue;
    }
    unsigned tuple_begin_idx = idx;
    unsigned tuple_end_idx = idx + 1;
    bool     tuple_const = true;
    while (tuple_end_idx < num_expressions) {
      union ast_expression *expression = expressions[tuple_end_idx];
      if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
        break;
      }
      if (ast_expression_as_constant(expression) == NULL) {
        tuple_const = false;
      }
      ++tuple_end_idx;
    }

    unsigned tuple_length = tuple_end_idx - tuple_begin_idx;
    if (!tuple_const) {
      for (unsigned i = tuple_begin_idx; i < tuple_end_idx; i++) {
        emit_expression(s, expressions[i]);
      }
      cg_pop(s, tuple_length);
      cg_push_op(s, OPCODE_BUILD_TUPLE, tuple_length);
    } else {
      struct arena *arena = object_intern_arena(&s->objects);
      union object *tuple = object_new_tuple_begin(arena, tuple_length);
      for (unsigned i = 0; i < tuple_length; i++) {
        union ast_expression *expression = expressions[tuple_begin_idx + i];
        union object *as_const = ast_expression_as_constant(expression);
        object_new_tuple_set_at(tuple, i, as_const);
      }
      object_new_tuple_end(tuple);
      cg_load_const(s, tuple);
    }
    ++num_unpack_expressions;
    idx += tuple_length;
  }
  cg_pop(s, num_unpack_expressions);
  cg_push_op(s, OPCODE_BUILD_TUPLE_UNPACK, num_unpack_expressions);
}

static void emit_expression_list_helper(struct cg_state            *s,
                                        struct ast_expression_list *tuple)
{
  unsigned num_expressions = tuple->num_expressions;
  if (tuple->has_star_expression) {
    emit_expression_list_helper_unpack(s, tuple);
  } else {
    for (unsigned i = 0; i < num_expressions; i++) {
      emit_expression(s, tuple->expressions[i]);
    }
    cg_pop(s, num_expressions);
    cg_push_op(s, OPCODE_BUILD_TUPLE, num_expressions);
  }
}

static void emit_tuple(struct cg_state *s, struct ast_expression_list *tuple)
{
  union object *object = tuple->as_constant;
  if (object != NULL) {
    cg_load_const(s, object);
    return;
  }

  emit_expression_list_helper(s, tuple);
}

static void emit_attr(struct cg_state *s, struct ast_attr *attr)
{
  emit_expression(s, attr->expression);
  unsigned index = cg_append_name(s, attr->symbol->string);
  cg_op(s, OPCODE_LOAD_ATTR, index);
}

static void emit_call_ex_helper(struct cg_state *s, struct ast_call *call,
                                unsigned num_extra_args)
{
  unsigned         num_processed_arguments = 0;
  unsigned         num_arguments = call->num_arguments;
  struct argument *arguments = call->arguments;
  unsigned         idx = 0;

  while (idx < num_arguments) {
    struct argument      *argument = &arguments[idx];
    union ast_expression *expression = argument->expression;
    if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
      if (num_extra_args > 0) {
        cg_pop(s, num_extra_args);
        cg_push_op(s, OPCODE_BUILD_TUPLE, num_extra_args);
        ++num_processed_arguments;
        num_extra_args = 0;
        continue;
      }
      emit_expression(s, expression->unexpr.op);
      ++num_processed_arguments;
      ++idx;
      continue;
    }

    bool tuple_const = num_extra_args == 0
                       && (ast_expression_as_constant(expression) != NULL);
    unsigned tuple_begin_idx = idx;
    unsigned tuple_end_idx = idx + 1;
    while (tuple_end_idx < num_arguments) {
      struct argument      *argument = &arguments[tuple_end_idx];
      union ast_expression *expression = argument->expression;
      if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
        break;
      }
      if (tuple_const && ast_expression_as_constant(expression) == NULL) {
        tuple_const = false;
      }
      ++tuple_end_idx;
    }
    idx = tuple_end_idx;

    unsigned tuple_length = tuple_end_idx - tuple_begin_idx;
    if (!tuple_const) {
      for (unsigned i = tuple_begin_idx; i < tuple_end_idx; i++) {
        struct argument      *argument = &arguments[i];
        union ast_expression *expression = argument->expression;
        emit_expression(s, expression);
      }
      cg_pop(s, tuple_length);
      cg_push_op(s, OPCODE_BUILD_TUPLE, tuple_length + num_extra_args);
    } else {
      struct arena *arena = object_intern_arena(&s->objects);
      union object *tuple = object_new_tuple_begin(arena, tuple_length);
      for (unsigned i = 0; i < tuple_length; i++) {
        struct argument      *argument = &arguments[tuple_begin_idx + i];
        union ast_expression *expression = argument->expression;
        union object         *object = ast_expression_as_constant(expression);
        object_new_tuple_set_at(tuple, i, object);
      }
      object_new_tuple_end(tuple);
      cg_load_const(s, tuple);
    }
    ++num_processed_arguments;
  }
  cg_pop(s, num_processed_arguments);
  cg_push_op(s, OPCODE_BUILD_TUPLE_UNPACK_WITH_CALL, num_processed_arguments);
  cg_pop(s, 2);
  cg_push_op(s, OPCODE_CALL_FUNCTION_EX, 0);
}

static void emit_call_kw_helper(struct cg_state *s, struct ast_call *call,
                                unsigned num_extra_args)
{
  unsigned         num_arguments = call->num_arguments;
  struct argument *arguments = call->arguments;
  assert(num_arguments >= 1);
  unsigned kw_idx = num_arguments - 1;
  while (kw_idx > 0) {
    struct argument *argument = &arguments[kw_idx - 1];
    if (argument->name == NULL) break;
    --kw_idx;
  }

  for (unsigned i = 0; i < kw_idx; i++) {
    struct argument *argument = &arguments[i];
    emit_expression(s, argument->expression);
  }

  unsigned      num_kw_arguments = num_arguments - kw_idx;
  struct arena *arena = object_intern_arena(&s->objects);
  union object *names = object_new_tuple_begin(arena, num_kw_arguments);
  for (unsigned i = 0; i < num_kw_arguments; i++) {
    struct argument *argument = &arguments[kw_idx + i];
    emit_expression(s, argument->expression);
    union object *name
        = object_intern_cstring(&s->objects, argument->name->string);
    object_new_tuple_set_at(names, i, name);
  }
  object_new_tuple_end(names);
  cg_load_const(s, names);

  num_arguments += num_extra_args;
  cg_pop(s, num_arguments + 2);
  cg_push_op(s, OPCODE_CALL_FUNCTION_KW, num_arguments);
}

void emit_call_helper(struct cg_state *s, struct ast_call *call,
                      unsigned num_extra_args)
{
  if (call->has_star_argument) {
    emit_call_ex_helper(s, call, num_extra_args);
    return;
  }
  if (call->has_kw_argument) {
    emit_call_kw_helper(s, call, num_extra_args);
    return;
  }

  unsigned num_arguments = call->num_arguments;
  for (unsigned i = 0; i < num_arguments; i++) {
    struct argument *argument = &call->arguments[i];
    emit_expression(s, argument->expression);
  }
  num_arguments += num_extra_args;
  cg_pop(s, num_arguments + 1);
  cg_push_op(s, OPCODE_CALL_FUNCTION, num_arguments);
}

static void emit_call(struct cg_state *s, struct ast_call *call)
{
  emit_expression(s, call->callee);
  emit_call_helper(s, call, /*num_extra_args=*/0);
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
  case AST_BINEXPR_AND:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_POWER:
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_RIGHT:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_SUBSCRIPT:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_XOR:
    emit_binexpr(s, &expression->binexpr);
    break;
  case AST_BINEXPR_ADD_ASSIGN:
  case AST_BINEXPR_AND_ASSIGN:
  case AST_BINEXPR_FLOORDIV_ASSIGN:
  case AST_BINEXPR_MATMUL_ASSIGN:
  case AST_BINEXPR_MOD_ASSIGN:
  case AST_BINEXPR_MUL_ASSIGN:
  case AST_BINEXPR_OR_ASSIGN:
  case AST_BINEXPR_POWER_ASSIGN:
  case AST_BINEXPR_SHIFT_LEFT_ASSIGN:
  case AST_BINEXPR_SHIFT_RIGHT_ASSIGN:
  case AST_BINEXPR_SUB_ASSIGN:
  case AST_BINEXPR_TRUEDIV_ASSIGN:
  case AST_BINEXPR_XOR_ASSIGN:
    assert(drop);
    emit_binexpr_assign(s, &expression->binexpr);
    return;
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
    emit_binexpr_logical(s, &expression->binexpr, OPCODE_JUMP_IF_FALSE_OR_POP);
    break;
  case AST_BINEXPR_LOGICAL_OR:
    emit_binexpr_logical(s, &expression->binexpr, OPCODE_JUMP_IF_TRUE_OR_POP);
    break;
  case AST_BINEXPR_NOT_IN:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_NOT_IN);
    break;
  case AST_BINEXPR_UNEQUAL:
    emit_comparison(s, &expression->binexpr, COMPARE_OP_NE);
    break;
  case AST_CALL:
    emit_call(s, &expression->call);
    break;
  case AST_CONST:
  case AST_INVALID:
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
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
    /* not allow in generic contexts */
    abort();
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

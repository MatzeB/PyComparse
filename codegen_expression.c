#include "codegen_expression.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "ast.h"
#include "ast_expression_types.h"
#include "ast_statement_types.h"
#include "ast_types.h"
#include "codegen.h"
#include "codegen_statement.h"
#include "codegen_types.h"
#include "object_intern.h"
#include "diagnostics.h"
#include "object.h"
#include "object_types.h"
#include "opcodes.h"
#include "symbol_info_types.h"
#include "symbol_table.h"
#include "symbol_types.h"
#include "util.h"

static void
emit_generator_helper(struct cg_state                 *s,
                      struct ast_generator_expression *generator_expression,
                      const char                      *name,
                      bool                             is_generator_expression);

static void
emit_generator_helper(struct cg_state                 *s,
                      struct ast_generator_expression *generator_expression,
                      const char                      *name,
                      bool                             is_generator_expression)
{
  bool async_comprehension = generator_expression->is_async;
  bool first_part_async = false;
  if (generator_expression->num_parts > 0
      && generator_expression->parts[0].type
             == GENERATOR_EXPRESSION_PART_FOR) {
    first_part_async = generator_expression->parts[0].async;
  }
  if (async_comprehension && !is_generator_expression
      && !s->code.in_async_function) {
    struct location location = INVALID_LOCATION;
    diag_begin_error(s->d, location);
    diag_frag(s->d, "asynchronous comprehension outside async function");
    diag_end(s->d);
  }

  const char *qualname = cg_build_qualname(s, name);

  cg_push_code(s);
  cg_code_begin(s, /*in_function=*/true);
  s->code.in_async_function = async_comprehension;

  /* Set child's qualname prefix for nested scopes: qualname + ".<locals>." */
  {
    size_t qlen = strlen(qualname);
    struct arena *arena = object_intern_arena(&s->objects);
    char *prefix = arena_allocate(arena, qlen + 10 + 1, 1);
    memcpy(prefix, qualname, qlen);
    memcpy(prefix + qlen, ".<locals>.", 11);
    s->code.qualname_prefix = prefix;
  }

  emit_generator_expression_code(s, generator_expression);
  if (async_comprehension) {
    if (s->code.flags & CO_GENERATOR) {
      s->code.flags = (s->code.flags & ~CO_GENERATOR) | CO_ASYNC_GENERATOR;
    } else {
      s->code.flags |= CO_COROUTINE;
    }
  }
  emit_code_end(s);

  union object *code = cg_pop_code(s, name);
  cg_load_const(s, code);
  cg_load_const(s, object_intern_cstring(&s->objects, qualname));
  cg_op_pop_push(s, OPCODE_MAKE_FUNCTION, 0, /*pop=*/2, /*push=*/1);

  struct generator_expression_part *part = &generator_expression->parts[0];
  emit_expression(s, part->expression);
  if (first_part_async) {
    cg_push(s, 8);
    cg_pop(s, 8);
    cg_op_pop_push(s, OPCODE_GET_AITER, 0, /*pop=*/1, /*push=*/1);
  } else {
    cg_op_pop_push(s, OPCODE_GET_ITER, 0, /*pop=*/1, /*push=*/1);
  }
  cg_op_pop_push(s, OPCODE_CALL_FUNCTION, 1, /*pop=*/2, /*push=*/1);
  if (async_comprehension && !is_generator_expression) {
    cg_push(s, 4);
    cg_pop(s, 4);
    cg_op_pop_push(s, OPCODE_GET_AWAITABLE, 0, /*pop=*/1, /*push=*/1);
    cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
    cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
  }
}

static void emit_expression_list_helper(
    struct cg_state *s, struct ast_expression_list *expression_list,
    enum opcode opcode_build, enum opcode opcode_build_unpack)
{
  unsigned               num_expressions = expression_list->num_expressions;
  union ast_expression **expressions = expression_list->expressions;
  unsigned               num_unpack_items = 0;
  bool                   need_unpack = false;
  for (unsigned idx = 0; idx < num_expressions;) {
    union ast_expression *expression = expressions[idx];
    if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
      emit_expression(s, expression->unexpr.op);
      ++idx;
      ++num_unpack_items;
      need_unpack = true;
      continue;
    }
    unsigned non_star_begin = idx;
    unsigned non_star_end = idx + 1;
    bool     tuple_const = ast_expression_as_constant(expression) != NULL;
    while (non_star_end < num_expressions) {
      union ast_expression *expression = expressions[non_star_end];
      if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
        break;
      }
      if (ast_expression_as_constant(expression) == NULL) {
        tuple_const = false;
      }
      ++non_star_end;
    }

    bool everything = (non_star_begin == 0 && non_star_end == num_expressions);
    unsigned tuple_length = non_star_end - non_star_begin;
    if (!tuple_const || (everything && opcode_build != OPCODE_BUILD_TUPLE)) {
      for (unsigned i = non_star_begin; i < non_star_end; i++) {
        emit_expression(s, expressions[i]);
      }
      cg_op_pop_push(s, everything ? opcode_build : OPCODE_BUILD_TUPLE,
                     tuple_length,
                     /*pop=*/tuple_length, /*push=*/1);
    } else {
      struct tuple_prep *tuple_prep
          = object_intern_tuple_begin(&s->objects, tuple_length);
      for (unsigned i = 0; i < tuple_length; i++) {
        union ast_expression *expression = expressions[non_star_begin + i];
        union object *as_const = ast_expression_as_constant(expression);
        object_new_tuple_set_at(tuple_prep, i, as_const);
      }
      union object *tuple = object_intern_tuple_end(&s->objects, tuple_prep,
                                                    /*may_free_arena=*/true);
      cg_load_const(s, tuple);
    }
    ++num_unpack_items;
    idx += tuple_length;
  }

  if (need_unpack) {
    cg_op_pop_push(s, opcode_build_unpack, num_unpack_items,
                   /*pop=*/num_unpack_items, /*push=*/1);
  } else if (num_expressions == 0) {
    cg_op_push1(s, opcode_build, 0);
  }
}

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
  enum opcode opcode = ast_binexpr_to_opcode(binexpr->base.base.type);
  cg_op_pop_push(s, opcode, 0, /*pop=*/2, /*push=*/1);
}

static void emit_binexpr_logical(struct cg_state    *s,
                                 struct ast_binexpr *binexpr,
                                 enum opcode         opcode)
{
  emit_expression(s, binexpr->left);
  struct basic_block *target = cg_block_allocate(s);
  struct basic_block *fallthrough = cg_block_allocate(s);
  assert(opcode == OPCODE_JUMP_IF_TRUE_OR_POP
         || opcode == OPCODE_JUMP_IF_FALSE_OR_POP);
  cg_pop(s, 1);
  cg_condjump(s, opcode, /*target=*/target, /*fallthrough=*/fallthrough);
  cg_block_begin(s, fallthrough);
  emit_expression(s, binexpr->right);
  cg_jump(s, target);
  cg_block_begin(s, target);
}

void emit_assignment(struct cg_state *s, union ast_expression *target)
{
  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER:
    cg_store(s, target->identifier.symbol);
    return;
  case AST_ATTR: {
    struct ast_attr *attr = &target->attr;
    emit_expression(s, attr->expression);
    unsigned index = cg_register_name(s, attr->symbol);
    cg_op_pop_push(s, OPCODE_STORE_ATTR, index, /*pop=*/2, /*push=*/0);
    return;
  }
  case AST_BINEXPR_SUBSCRIPT: {
    struct ast_binexpr *binexpr = &target->binexpr;
    emit_expression(s, binexpr->left);
    emit_expression(s, binexpr->right);
    cg_op_pop_push(s, OPCODE_STORE_SUBSCR, 0, /*pop=*/3, /*push=*/0);
    return;
  }
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY: {
    struct ast_expression_list *list = &target->expression_list;
    unsigned                    num_expressions = list->num_expressions;
    if (list->has_star_expression) {
      if (num_expressions > 256) {
        struct location location = INVALID_LOCATION;
        diag_begin_error(s->d, location);
        diag_frag(s->d, "too many expressions in star-unpacking assignment");
        diag_end(s->d);
        /* continue; it will just produce an invalid argument for UNPACK_EX */
      }
      unsigned star_index = ~0u;
      for (unsigned i = 0; i < num_expressions; i++) {
        if (ast_expression_type(list->expressions[i]) == AST_UNEXPR_STAR) {
          star_index = i;
          break;
        }
      }
      assert(star_index != ~0u);
      uint32_t arg = (num_expressions - star_index - 1) << 8 | star_index;
      cg_op_pop_push(s, OPCODE_UNPACK_EX, arg, /*pop=*/1,
                     /*push=*/num_expressions);
    } else {
      cg_op_pop_push(s, OPCODE_UNPACK_SEQUENCE, num_expressions, /*pop=*/1,
                     /*push=*/num_expressions);
    }
    for (unsigned i = 0; i < num_expressions; i++) {
      union ast_expression *expression = list->expressions[i];
      if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
        expression = expression->unexpr.op;
      }
      emit_assignment(s, expression);
    }
    return;
  }
  case AST_INVALID:
    cg_pop(s, 1);
    return;
  default:
    internal_error("invalid assignment target");
  }
}

static void emit_assign_expression(struct cg_state    *s,
                                   struct ast_binexpr *binexpr)
{
  emit_expression(s, binexpr->right);
  cg_op_push1(s, OPCODE_DUP_TOP, 0);
  emit_assignment(s, binexpr->left);
}

static void emit_binexpr_assign_inner(struct cg_state    *s,
                                      struct ast_binexpr *binexpr)
{
  emit_expression(s, binexpr->right);
  enum opcode opcode = ast_binexpr_to_opcode(binexpr->base.base.type);
  cg_op_pop1(s, opcode, 0);
}

static void emit_binexpr_assign(struct cg_state    *s,
                                struct ast_binexpr *binexpr)
{
  union ast_expression *target = binexpr->left;
  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER: {
    struct symbol *symbol = target->identifier.symbol;
    cg_load(s, symbol);
    emit_binexpr_assign_inner(s, binexpr);
    cg_store(s, symbol);
    return;
  }
  case AST_ATTR: {
    struct ast_attr *attr = &target->attr;
    emit_expression(s, attr->expression);
    cg_op_push1(s, OPCODE_DUP_TOP, 0);
    unsigned index = cg_register_name(s, attr->symbol);
    cg_op_pop_push(s, OPCODE_LOAD_ATTR, index, /*pop=*/1, /*push=*/1);
    emit_binexpr_assign_inner(s, binexpr);
    cg_op_pop_push(s, OPCODE_ROT_TWO, 0, /*pop=*/2, /*push=*/2);
    cg_op_pop_push(s, OPCODE_STORE_ATTR, index, /*pop=*/2, /*push=*/0);
    return;
  }
  case AST_BINEXPR_SUBSCRIPT: {
    struct ast_binexpr *subscript = &target->binexpr;
    emit_expression(s, subscript->left);
    emit_expression(s, subscript->right);
    cg_op_pop_push(s, OPCODE_DUP_TOP_TWO, 0, /*pop=*/0, /*push=*/2);
    cg_op_pop1(s, OPCODE_BINARY_SUBSCR, 0);
    emit_binexpr_assign_inner(s, binexpr);
    cg_op_pop_push(s, OPCODE_ROT_THREE, 0, /*pop=*/3, /*push=*/3);
    cg_op_pop_push(s, OPCODE_STORE_SUBSCR, 0, /*pop=*/3, /*push=*/0);
    return;
  }
  default:
    internal_error("invalid assignment target in binexpr-assign");
  }
}

static void emit_unexpr(struct cg_state *s, struct ast_unexpr *unexpr,
                        uint8_t opcode)
{
  emit_expression(s, unexpr->op);
  cg_op_pop_push(s, opcode, 0, /*pop=*/1, /*push=*/1);
}

static void emit_dictionary_comprehension(
    struct cg_state *s, struct ast_generator_expression *generator_expression)
{
  emit_generator_helper(s, generator_expression, "<dictcomp>",
                        /*is_generator_expression=*/false);
}

static void emit_dictionary_display(struct cg_state           *s,
                                    struct ast_dict_item_list *list)
{
  struct dict_item *items = list->items;
  unsigned          num_items = list->num_items;
  unsigned          num_maps = 0;
  bool              need_map_unpack = false;
  for (unsigned idx = 0; idx < num_items;) {
    struct dict_item              *item = &items[idx];
    union ast_expression *nullable key = item->key;
    union ast_expression          *value = item->value;

    if (key == NULL) {
      assert(ast_expression_type(value) == AST_UNEXPR_STAR_STAR);
      emit_expression(s, value->unexpr.op);
      need_map_unpack = true;
      ++num_maps;
      ++idx;
      continue;
    }

    unsigned non_star_star_end = idx + 1;
    while (non_star_star_end < num_items
           && items[non_star_star_end].key != NULL) {
      ++non_star_star_end;
    }
    unsigned num_build_map_items = non_star_star_end - idx;
    bool all_const_keys = num_build_map_items > 1;
    for (unsigned i = idx; i < non_star_star_end && all_const_keys; i++) {
      all_const_keys = ast_expression_as_constant(items[i].key) != NULL;
    }
    if (all_const_keys) {
      struct tuple_prep *keys_prep
          = object_intern_tuple_begin(&s->objects, num_build_map_items);
      for (unsigned i = idx; i < non_star_star_end; i++) {
        item = &items[i];
        union object *key = ast_expression_as_constant(item->key);
        assert(key != NULL);
        object_new_tuple_set_at(keys_prep, i - idx, key);
        emit_expression(s, item->value);
      }
      union object *keys = object_intern_tuple_end(&s->objects, keys_prep,
                                                   /*may_free_arena=*/true);
      cg_load_const(s, keys);
      cg_op_pop_push(s, OPCODE_BUILD_CONST_KEY_MAP, num_build_map_items,
                     /*pop=*/num_build_map_items + 1, /*push=*/1);
    } else {
      for (unsigned i = idx; i < non_star_star_end; i++) {
        item = &items[i];
        emit_expression(s, item->key);
        emit_expression(s, item->value);
      }
      cg_op_pop_push(s, OPCODE_BUILD_MAP, num_build_map_items,
                     /*pop=*/num_build_map_items * 2, /*push=*/1);
    }
    ++num_maps;
    idx = non_star_star_end;
  }
  if (need_map_unpack) {
    cg_op_pop_push(s, OPCODE_BUILD_MAP_UNPACK, num_maps, /*pop=*/num_maps,
                   /*push=*/1);
  } else if (num_items == 0) {
    cg_op_push1(s, OPCODE_BUILD_MAP, 0);
  }
}

static void emit_lambda(struct cg_state *s, struct ast_lambda *lambda)
{
  analyze_lambda_bindings(s, lambda);

  struct make_function_state state;
  emit_make_function_begin(s, &state, &lambda->parameter_shape,
                           lambda->parameters,
                           /*async_function=*/false,
                           /*return_type=*/NULL,
                           "<lambda>");

  /* Apply scope bindings (mirrors apply_function_bindings for defs). */
  for (unsigned i = 0; i < lambda->num_scope_globals; ++i) {
    cg_declare(s, lambda->scope_globals[i], SYMBOL_GLOBAL);
  }
  for (unsigned i = 0; i < lambda->num_scope_freevars; ++i) {
    cg_declare(s, lambda->scope_freevars[i], SYMBOL_NONLOCAL);
  }
  for (unsigned i = 0; i < lambda->num_scope_locals; ++i) {
    cg_declare(s, lambda->scope_locals[i], SYMBOL_LOCAL);
  }
  for (unsigned i = 0; i < lambda->num_scope_cellvars; ++i) {
    cg_promote_to_cell(s, lambda->scope_cellvars[i]);
  }

  emit_expression(s, lambda->expression);
  cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);

  /* Set up closure (mirrors emit_function_closure for defs). */
  if (lambda->num_scope_freevars > 0) {
    state.num_closure_symbols = lambda->num_scope_freevars;
    state.closure_symbols = lambda->scope_freevars;
  }

  struct symbol *symbol
      = symbol_table_get_or_insert(s->symbol_table, "<lambda>");
  emit_make_function_end(s, &state, symbol);
}

static void
emit_list_comprehension(struct cg_state                 *s,
                        struct ast_generator_expression *generator_expression)
{
  emit_generator_helper(s, generator_expression, "<listcomp>",
                        /*is_generator_expression=*/false);
}

static void emit_list_display(struct cg_state            *s,
                              struct ast_expression_list *list)
{
  emit_expression_list_helper(s, list, OPCODE_BUILD_LIST,
                              OPCODE_BUILD_LIST_UNPACK);
}

static void
emit_set_comprehension(struct cg_state                 *s,
                       struct ast_generator_expression *generator_expression)
{
  emit_generator_helper(s, generator_expression, "<setcomp>",
                        /*is_generator_expression=*/false);
}

static void emit_set_display(struct cg_state            *s,
                             struct ast_expression_list *list)
{
  unsigned num_expressions = list->num_expressions;
  for (unsigned i = 0; i < num_expressions; i++) {
    emit_expression(s, list->expressions[i]);
  }
  cg_op_pop_push(s, OPCODE_BUILD_SET, num_expressions,
                 /*pop=*/num_expressions, /*push=*/1);
}

static void emit_none(struct cg_state *s)
{
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
}

static void emit_invalid(struct cg_state *s, struct ast_const *expression)
{
  cg_load_const(s, expression->object);
}

static void emit_slice(struct cg_state *s, struct ast_slice *slice)
{
  union ast_expression *start = slice->start;
  if (start != NULL) {
    emit_expression(s, start);
  } else {
    emit_none(s);
  }
  union ast_expression *stop = slice->stop;
  if (stop != NULL) {
    emit_expression(s, stop);
  } else {
    emit_none(s);
  }
  unsigned              arg;
  union ast_expression *step = slice->step;
  if (step != NULL) {
    emit_expression(s, step);
    arg = 3;
  } else {
    arg = 2;
  }
  cg_op_pop_push(s, OPCODE_BUILD_SLICE, arg, /*pop=*/arg, /*push=*/1);
}

static void emit_tuple(struct cg_state *s, struct ast_expression_list *tuple)
{
  union object *object = tuple->as_constant;
  if (object != NULL) {
    cg_load_const(s, object);
    return;
  }

  emit_expression_list_helper(s, tuple, OPCODE_BUILD_TUPLE,
                              OPCODE_BUILD_TUPLE_UNPACK);
}

static void emit_attr(struct cg_state *s, struct ast_attr *attr)
{
  emit_expression(s, attr->expression);
  unsigned index = cg_register_name(s, attr->symbol);
  cg_op(s, OPCODE_LOAD_ATTR, index);
}

static void emit_await(struct cg_state *s, struct ast_unexpr *unexpr)
{
  if (!cg_in_function(s) || !s->code.in_async_function) {
    struct location location = INVALID_LOCATION;
    diag_begin_error(s->d, location);
    diag_token_kind(s->d, T_await);
    diag_frag(s->d, " outside async function");
    diag_end(s->d);
  }

  emit_expression(s, unexpr->op);
  cg_op_pop_push(s, OPCODE_GET_AWAITABLE, 0, /*pop=*/1, /*push=*/1);
  emit_none(s);
  cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
}

static void emit_call_ex_helper(struct cg_state *s, struct ast_call *call,
                                unsigned num_extra_args)
{
  unsigned         num_tuple_elements = 0;
  unsigned         num_arguments = call->num_arguments;
  struct argument *arguments = call->arguments;

  /* Emit positional arguments */
  for (unsigned idx = 0; idx < num_arguments;) {
    struct argument *argument = &arguments[idx];
    /* Ignore keyword arguments */
    if (argument->name != NULL) {
      ++idx;
      continue;
    }
    union ast_expression    *expression = argument->expression;
    enum ast_expression_type type = ast_expression_type(expression);
    if (type == AST_UNEXPR_STAR_STAR) {
      ++idx;
      continue;
    }
    if (type == AST_UNEXPR_STAR) {
      if (num_extra_args > 0) {
        cg_op_pop_push(s, OPCODE_BUILD_TUPLE, num_extra_args,
                       /*pop=*/num_extra_args, /*push=*/1);
        ++num_tuple_elements;
        num_extra_args = 0;
        continue;
      }
      emit_expression(s, expression->unexpr.op);
      ++num_tuple_elements;
      ++idx;
      continue;
    }

    bool tuple_const = num_extra_args == 0
                       && (ast_expression_as_constant(expression) != NULL);
    unsigned tuple_begin_idx = idx;
    unsigned tuple_end_idx = idx + 1;
    while (tuple_end_idx < num_arguments) {
      struct argument *argument = &arguments[tuple_end_idx];
      if (argument->name != NULL) break;
      union ast_expression    *expression = argument->expression;
      enum ast_expression_type type = ast_expression_type(expression);
      if (type == AST_UNEXPR_STAR || type == AST_UNEXPR_STAR_STAR) break;
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
      cg_op_pop_push(s, OPCODE_BUILD_TUPLE, tuple_length + num_extra_args,
                     /*pop=*/tuple_length, /*push=*/1);
    } else {
      struct tuple_prep *tuple_prep
          = object_intern_tuple_begin(&s->objects, tuple_length);
      for (unsigned i = 0; i < tuple_length; i++) {
        struct argument      *argument = &arguments[tuple_begin_idx + i];
        union ast_expression *expression = argument->expression;
        union object         *object = ast_expression_as_constant(expression);
        object_new_tuple_set_at(tuple_prep, i, object);
      }
      union object *tuple = object_intern_tuple_end(&s->objects, tuple_prep,
                                                    /*may_free_arena=*/true);
      cg_load_const(s, tuple);
    }
    ++num_tuple_elements;
  }
  if (num_tuple_elements == 0) {
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE, 0, /*pop=*/0, /*push=*/1);
  } else {
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE_UNPACK_WITH_CALL, num_tuple_elements,
                   /*pop=*/num_tuple_elements, /*push=*/1);
  }

  /* Emit keyword arguments */
  unsigned num_maps = 0;
  for (unsigned idx = 0; idx < num_arguments;) {
    struct argument *argument = &arguments[idx];
    struct symbol   *name = argument->name;

    if (name == NULL) {
      union ast_expression *expression = argument->expression;
      if (ast_expression_type(expression) == AST_UNEXPR_STAR_STAR) {
        emit_expression(s, expression->unexpr.op);
        ++num_maps;
      }
      ++idx;
      continue;
    }

    unsigned kw_begin_idx = idx;
    unsigned kw_end_idx = idx + 1;
    while (kw_end_idx < num_arguments) {
      struct argument *argument = &arguments[kw_end_idx];
      if (argument->name == NULL) break;
      ++kw_end_idx;
    }
    idx = kw_end_idx;

    unsigned kw_length = kw_end_idx - kw_begin_idx;
    if (kw_length == 1) {
      struct argument *argument = &arguments[kw_begin_idx];
      union object    *name
          = object_intern_cstring(&s->objects, argument->name->string);
      cg_load_const(s, name);
      emit_expression(s, argument->expression);
      cg_op_pop_push(s, OPCODE_BUILD_MAP, 1, /*pop=*/2, /*push=*/1);
    } else {
      struct tuple_prep *names_prep
          = object_intern_tuple_begin(&s->objects, kw_length);
      for (unsigned a = 0; a < kw_length; a++) {
        struct argument *argument = &arguments[kw_begin_idx + a];
        union object    *name
            = object_intern_cstring(&s->objects, argument->name->string);
        object_new_tuple_set_at(names_prep, a, name);
        emit_expression(s, argument->expression);
      }
      union object *names = object_intern_tuple_end(&s->objects, names_prep,
                                                    /*may_free_arena=*/false);
      cg_load_const(s, names);
      cg_op_pop_push(s, OPCODE_BUILD_CONST_KEY_MAP, kw_length,
                     /*pop=*/kw_length + 1, /*push=*/1);
    }
    ++num_maps;
  }

  if (num_maps > 1) {
    cg_op_pop_push(s, OPCODE_BUILD_MAP_UNPACK_WITH_CALL, num_maps,
                   /*pop=*/num_maps, /*push=*/1);
  }

  uint32_t call_ex_arg = num_maps > 0;
  cg_op_pop_push(s, OPCODE_CALL_FUNCTION_EX, call_ex_arg,
                 /*pop=*/2 + call_ex_arg, /*push=*/1);
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

  unsigned           num_kw_arguments = num_arguments - kw_idx;
  struct tuple_prep *names_prep
      = object_intern_tuple_begin(&s->objects, num_kw_arguments);
  for (unsigned i = 0; i < num_kw_arguments; i++) {
    struct argument *argument = &arguments[kw_idx + i];
    emit_expression(s, argument->expression);
    union object *name
        = object_intern_cstring(&s->objects, argument->name->string);
    object_new_tuple_set_at(names_prep, i, name);
  }
  union object *names = object_intern_tuple_end(&s->objects, names_prep,
                                                /*may_free_arena=*/false);
  cg_load_const(s, names);

  num_arguments += num_extra_args;
  cg_op_pop_push(s, OPCODE_CALL_FUNCTION_KW, num_arguments,
                 /*pop=*/num_arguments + 2, /*push=*/1);
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
  cg_op_pop_push(s, OPCODE_CALL_FUNCTION, num_arguments,
                 /*pop=*/num_arguments + 1, /*push=*/1);
}

static void emit_call(struct cg_state *s, struct ast_call *call)
{
  union ast_expression *callee = call->callee;
  assert(callee != NULL);
  if (!call->has_star_argument && !call->has_kw_argument
      && ast_expression_type(callee) == AST_ATTR) {
    struct ast_attr *attr = &callee->attr;
    emit_expression(s, attr->expression);
    unsigned index = cg_register_name(s, attr->symbol);
    cg_op_pop_push(s, OPCODE_LOAD_METHOD, index, /*pop=*/1, /*push=*/2);

    unsigned num_arguments = call->num_arguments;
    for (unsigned i = 0; i < num_arguments; ++i) {
      struct argument *argument = &call->arguments[i];
      emit_expression(s, argument->expression);
    }
    cg_op_pop_push(s, OPCODE_CALL_METHOD, num_arguments,
                   /*pop=*/num_arguments + 2, /*push=*/1);
    return;
  }
  emit_expression(s, callee);
  emit_call_helper(s, call, /*num_extra_args=*/0);
}

static void emit_comparison(struct cg_state       *s,
                            struct ast_comparison *comparison)
{
  unsigned num_operands = comparison->num_operands;
  if (num_operands == 1) {
    struct comparison_op *operand = &comparison->operands[0];
    emit_expression(s, comparison->left);
    emit_expression(s, operand->operand);
    cg_op_pop_push(s, OPCODE_COMPARE_OP, operand->op,
                   /*pop=*/2, /*push=*/1);
  } else {
    emit_comparison_multi_value(s, comparison);
  }
}

static void emit_conditional(struct cg_state        *s,
                             struct ast_conditional *conditional)
{
  struct basic_block *true_block = cg_block_allocate(s);
  struct basic_block *false_block = cg_block_allocate(s);
  struct basic_block *footer = cg_block_allocate(s);

  emit_condjump_expr(s, conditional->condition, /*true_block=*/true_block,
                     /*false_block=*/false_block, /*next=*/true_block);
  cg_block_begin(s, true_block);
  emit_expression(s, conditional->true_expression);
  cg_jump(s, footer);
  cg_pop(s, 1); /* adjust because we don't track stacksize per block... */

  cg_block_begin(s, false_block);
  emit_expression(s, conditional->false_expression);
  cg_jump(s, footer);

  cg_block_begin(s, footer);
}

static void emit_fstring(struct cg_state *s, struct ast_fstring *fstring)
{
  struct fstring_element *elements = fstring->elements;
  unsigned                num_elements = fstring->num_elements;
  for (unsigned i = 0; i < num_elements; i++) {
    struct fstring_element *element = &elements[i];
    if (element->is_expression) {
      emit_expression(s, element->u.expression);
      uint32_t argument = element->conversion;
      unsigned pop = 1;
      if (element->format_spec != NULL) {
        emit_expression(s, element->format_spec);
        argument |= FORMAT_VALUE_FMT_SPEC;
        pop = 2;
      }
      cg_op_pop_push(s, OPCODE_FORMAT_VALUE, argument, /*pop=*/pop,
                     /*push=*/1);
    } else {
      cg_load_const(s, element->u.string);
    }
  }
  if (num_elements > 1) {
    cg_op_pop_push(s, OPCODE_BUILD_STRING, num_elements,
                   /*pop=*/num_elements, /*push=*/1);
  }
}

static void emit_generator_expression(
    struct cg_state *s, struct ast_generator_expression *generator_expression)
{
  emit_generator_helper(s, generator_expression, "<genexpr>",
                        /*is_generator_expression=*/true);
}

void emit_yield(struct cg_state *s, union ast_expression *nullable value,
                struct location location)
{
  s->code.flags |= CO_GENERATOR;
  if (!cg_in_function(s)) {
    diag_begin_error(s->d, location);
    diag_token_kind(s->d, T_yield);
    diag_frag(s->d, " outside function");
    diag_end(s->d);
  }

  if (value != NULL) {
    emit_expression(s, value);
  } else {
    emit_none(s);
  }
  cg_op(s, OPCODE_YIELD_VALUE, 0);
}

void emit_yield_from(struct cg_state *s, union ast_expression *nullable value,
                     struct location location)
{
  s->code.flags |= CO_GENERATOR;
  if (!cg_in_function(s)) {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "`yield from` outside function");
    diag_end(s->d);
  } else if (s->code.in_async_function) {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "`yield from` inside async function");
    diag_end(s->d);
  }

  if (value != NULL) {
    emit_expression(s, value);
  } else {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "expected expression after `yield from`");
    diag_end(s->d);
    emit_none(s);
  }
  cg_op(s, OPCODE_GET_YIELD_FROM_ITER, 0);
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
  cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
}

void emit_expression(struct cg_state *s, union ast_expression *expression)
{
  switch (ast_expression_type(expression)) {
  case AST_ATTR:
    emit_attr(s, &expression->attr);
    return;
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
    return;
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
    emit_binexpr_assign(s, &expression->binexpr);
    return;
  case AST_BINEXPR_ASSIGN:
    emit_assign_expression(s, &expression->binexpr);
    return;
  case AST_BINEXPR_LOGICAL_AND:
    emit_binexpr_logical(s, &expression->binexpr, OPCODE_JUMP_IF_FALSE_OR_POP);
    return;
  case AST_BINEXPR_LOGICAL_OR:
    emit_binexpr_logical(s, &expression->binexpr, OPCODE_JUMP_IF_TRUE_OR_POP);
    return;
  case AST_CALL:
    emit_call(s, &expression->call);
    return;
  case AST_COMPARISON:
    emit_comparison(s, &expression->comparison);
    return;
  case AST_CONDITIONAL:
    emit_conditional(s, &expression->conditional);
    return;
  case AST_CONST:
    cg_load_const(s, expression->cnst.object);
    return;
  case AST_INVALID:
    emit_invalid(s, &expression->cnst);
    return;
  case AST_DICT_COMPREHENSION:
    emit_dictionary_comprehension(s, &expression->generator_expression);
    return;
  case AST_DICT_DISPLAY:
    emit_dictionary_display(s, &expression->dict_item_list);
    return;
  case AST_EXPRESSION_LIST:
    emit_tuple(s, &expression->expression_list);
    return;
  case AST_FSTRING:
    emit_fstring(s, &expression->fstring);
    return;
  case AST_GENERATOR_EXPRESSION:
    emit_generator_expression(s, &expression->generator_expression);
    return;
  case AST_IDENTIFIER:
    cg_load(s, expression->identifier.symbol);
    return;
  case AST_LAMBDA:
    emit_lambda(s, &expression->lambda);
    return;
  case AST_LIST_COMPREHENSION:
    emit_list_comprehension(s, &expression->generator_expression);
    return;
  case AST_LIST_DISPLAY:
    emit_list_display(s, &expression->expression_list);
    return;
  case AST_SET_COMPREHENSION:
    emit_set_comprehension(s, &expression->generator_expression);
    return;
  case AST_SET_DISPLAY:
    emit_set_display(s, &expression->expression_list);
    return;
  case AST_SLICE:
    emit_slice(s, &expression->slice);
    return;
  case AST_UNEXPR_AWAIT:
    emit_await(s, &expression->unexpr);
    return;
  case AST_UNEXPR_PLUS:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_POSITIVE);
    return;
  case AST_UNEXPR_NEGATIVE:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_NEGATIVE);
    return;
  case AST_UNEXPR_NOT:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_NOT);
    return;
  case AST_UNEXPR_INVERT:
    emit_unexpr(s, &expression->unexpr, OPCODE_UNARY_INVERT);
    return;
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
    internal_error("attempted to emit `*`/`**` as value");
  case AST_YIELD:
    emit_yield(s, expression->yield.value, INVALID_LOCATION);
    return;
  case AST_YIELD_FROM:
    emit_yield_from(s, expression->yield.value, INVALID_LOCATION);
    return;
  }
}

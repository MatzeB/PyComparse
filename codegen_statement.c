#include "codegen_statement.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/idynarray.h"
#include "ast.h"
#include "ast_types.h"
#include "codegen.h"
#include "codegen_expression.h"
#include "codegen_types.h"
#include "diagnostics.h"
#include "object.h"
#include "object_intern.h"
#include "opcodes.h"
#include "symbol_info_types.h"
#include "symbol_table.h"
#include "symbol_types.h"
#include "util.h"

static void emit_comparison_multi(struct cg_state             *s,
                                  struct ast_comparison       *comparison,
                                  struct basic_block *nullable true_block,
                                  struct basic_block *nullable false_block)
{
  emit_expression(s, comparison->left);

  unsigned num_operands = comparison->num_operands;
  assert(num_operands > 1);
  unsigned            i = 0;
  struct basic_block *op_false_block = NULL;
  bool                produce_value = (true_block == NULL);
  for (;;) {
    struct comparison_op *comparison_op = &comparison->operands[i];
    ++i;
    bool last = (i == num_operands);

    emit_expression(s, comparison_op->operand);
    if (!last) {
      cg_op_push1(s, OPCODE_DUP_TOP, 0);
      cg_op_pop_push(s, OPCODE_ROT_THREE, 0, /*pop=*/3,
                     /*push=*/3);
    }
    cg_op_pop_push(s, OPCODE_COMPARE_OP, comparison_op->op,
                   /*pop=*/2, /*push=*/1);

    if (last) {
      if (produce_value) {
        assert(op_false_block != NULL);
        struct basic_block *footer = cg_block_allocate(s);
        cg_jump(s, footer);

        cg_block_begin(s, op_false_block);
        cg_push(s, 1); /* adjust for tracking not working along jumps */
        cg_op_pop_push(s, OPCODE_ROT_TWO, 0, /*pop=*/2, /*push=*/2);
        cg_op_pop1(s, OPCODE_POP_TOP, 0);
        cg_jump(s, footer);

        cg_block_begin(s, footer);
        break;
      }

      assert(false_block != NULL);
      assert(true_block != NULL);
      cg_pop(s, 1);
      cg_condjump(s, OPCODE_POP_JUMP_IF_FALSE,
                  /*target=*/false_block, /*fallthrough=*/true_block);
      if (op_false_block != NULL) {
        cg_block_begin(s, op_false_block);
        cg_push(s, 1); /* adjust for tracking not working along jumps */
        cg_op_pop1(s, OPCODE_POP_TOP, 0);
        cg_jump(s, false_block);
      }
      break;
    }
    struct basic_block *op_true_block = cg_block_allocate(s);
    if (op_false_block == NULL) {
      op_false_block = cg_block_allocate(s);
    }
    cg_pop(s, 1);
    cg_condjump(s,
                produce_value ? OPCODE_JUMP_IF_FALSE_OR_POP
                              : OPCODE_POP_JUMP_IF_FALSE,
                /*target=*/op_false_block, /*fallthrough=*/op_true_block);
    cg_block_begin(s, op_true_block);
  }
}

void emit_comparison_multi_value(struct cg_state       *s,
                                 struct ast_comparison *comparison)
{
  emit_comparison_multi(s, comparison, /*true_block=*/NULL,
                        /*false_block=*/NULL);
}

static void emit_condjump_optimize(struct cg_state    *s,
                                   struct basic_block *true_block,
                                   struct basic_block *false_block,
                                   struct basic_block *next)
{
  enum opcode         opcode;
  struct basic_block *target;
  struct basic_block *fallthrough;
  if (next == true_block) {
    opcode = OPCODE_POP_JUMP_IF_FALSE;
    target = false_block;
    fallthrough = true_block;
  } else {
    opcode = OPCODE_POP_JUMP_IF_TRUE;
    target = true_block;
    fallthrough = false_block;
  }
  cg_pop(s, 1);
  cg_condjump(s, opcode, /*target=*/target, /*fallthrough=*/fallthrough);
  return;
}

void emit_condjump_expr(struct cg_state *s, union ast_expression *expression,
                        struct basic_block *true_block,
                        struct basic_block *false_block,
                        struct basic_block *next)
{
  switch (ast_expression_type(expression)) {
  case AST_UNEXPR_NOT:
    emit_condjump_expr(s, expression->unexpr.op,
                       /*true_block=*/false_block, /*false_block=*/true_block,
                       /*next=*/next);
    return;
  case AST_BINEXPR_LOGICAL_AND: {
    struct basic_block *middle_block = cg_block_allocate(s);
    emit_condjump_expr(s, expression->binexpr.left,
                       /*true_block=*/middle_block,
                       /*false_block=*/false_block,
                       /*next=*/middle_block);
    cg_block_begin(s, middle_block);
    emit_condjump_expr(s, expression->binexpr.right,
                       /*true_block=*/true_block, /*false_block=*/false_block,
                       /*next=*/next);
    return;
  }
  case AST_BINEXPR_LOGICAL_OR: {
    struct basic_block *middle_block = cg_block_allocate(s);
    emit_condjump_expr(s, expression->binexpr.left,
                       /*true_block=*/true_block, /*false_block=*/middle_block,
                       /*next=*/middle_block);
    cg_block_begin(s, middle_block);
    emit_condjump_expr(s, expression->binexpr.right,
                       /*true_block=*/true_block, /*false_block=*/false_block,
                       /*next=*/next);
    return;
  }
  case AST_COMPARISON: {
    struct ast_comparison *comparison = &expression->comparison;
    if (comparison->num_operands > 1) {
      emit_comparison_multi(s, comparison, true_block, false_block);
      return;
    }
    emit_expression(s, comparison->left);
    struct comparison_op *comparison_op = &comparison->operands[0];
    emit_expression(s, comparison_op->operand);
    cg_op_pop_push(s, OPCODE_COMPARE_OP, comparison_op->op,
                   /*pop=*/2, /*push=*/1);
    goto emit_condjump;
  }
  default:
    emit_expression(s, expression);
    goto emit_condjump;
  emit_condjump:
    emit_condjump_optimize(s, true_block, false_block, next);
    return;
  }
}

static bool unreachable(struct cg_state *s)
{
  return !cg_in_block(s);
}

void emit_code_end(struct cg_state *s)
{
  if (unreachable(s)) return;
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
  cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);
}

void emit_module_begin(struct cg_state *s)
{
  cg_code_begin(s, /*in_function=*/false);
}

union object *emit_module_end(struct cg_state *s)
{
  emit_code_end(s);
  return cg_code_end(s, "<module>");
}

void emit_annotation(struct cg_state *s, union ast_expression *target,
                     union ast_expression *annotation)
{
  if (!cg_in_function(s)) {
    s->code.setup_annotations = true;
  }
  if (unreachable(s)) return;

  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER:
    if (cg_in_function(s)) return;

    emit_expression(s, annotation);
    unsigned annotations_idx = cg_register_name(
        s, symbol_table_get_or_insert(s->symbol_table, "__annotations__"));
    cg_op_push1(s, OPCODE_LOAD_NAME, annotations_idx);
    union object *name = object_intern_cstring(
        &s->objects, target->identifier.symbol->string);
    cg_load_const(s, name);
    cg_op_pop_push(s, OPCODE_STORE_SUBSCR, 0, /*pop=*/3, /*push=*/0);
    return;
  case AST_ATTR:
  case AST_BINEXPR_SUBSCRIPT:
    if (cg_in_function(s)) return;

    emit_expression(s, annotation);
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
    return;
  default:
    internal_error("invalid annotation target");
  }
}

void emit_assert(struct cg_state *s, union ast_expression *expression,
                 union ast_expression *message)
{
  if (unreachable(s)) return;

  struct basic_block *fail = cg_block_allocate(s);
  struct basic_block *continue_block = cg_block_allocate(s);

  emit_condjump_expr(s, expression, /*true_block=*/continue_block,
                     /*false_block=*/fail, /*next=*/fail);

  cg_block_begin(s, fail);
  struct symbol *assertion_error
      = symbol_table_get_or_insert(s->symbol_table, "AssertionError");
  unsigned index = cg_register_name(s, assertion_error);
  cg_op_push1(s, OPCODE_LOAD_GLOBAL, index);
  if (message != NULL) {
    emit_expression(s, message);
    cg_op_pop_push(s, OPCODE_CALL_FUNCTION, 1, /*pop=*/2, /*push=*/1);
  }
  cg_op_pop1(s, OPCODE_RAISE_VARARGS, 1);
  cg_block_end(s);

  cg_block_begin(s, continue_block);
}

void emit_assign_statement(struct cg_state *s, unsigned num_targets,
                           union ast_expression **targets,
                           union ast_expression  *value)
{
  if (unreachable(s)) return;
  emit_expression(s, value);
  for (unsigned i = 0; i < num_targets; i++) {
    union ast_expression *target = targets[i];
    if (i < num_targets - 1) {
      cg_op_push1(s, OPCODE_DUP_TOP, 0);
    }
    emit_assignment(s, target);
  }
}

void emit_expression_statement(struct cg_state      *s,
                               union ast_expression *expression)
{
  if (unreachable(s)) return;
  emit_expression(s, expression);
  cg_op_pop1(s, OPCODE_POP_TOP, 0);
}

void emit_binexpr_assign_statement(struct cg_state      *s,
                                   union ast_expression *expression)
{
  if (unreachable(s)) return;
  emit_expression(s, expression);
}

static unsigned register_dotted_name(struct cg_state    *s,
                                     struct dotted_name *name)
{
  unsigned num_symbols = name->num_symbols;
  unsigned length = num_symbols;
  for (unsigned i = 0; i < num_symbols; i++) {
    length += strlen(name->symbols[i]->string);
  }
  struct arena *arena = object_intern_arena(&s->objects);
  char         *chars = (char *)arena_allocate(arena, length, 1);
  char         *c = chars;
  for (unsigned i = 0; i < num_symbols; i++) {
    if (i > 0) {
      *c++ = '.';
    }
    const char *symbol_string = name->symbols[i]->string;
    size_t      symbol_length = strlen(symbol_string);
    memcpy(c, symbol_string, symbol_length);
    c += symbol_length;
  }
  *c++ = '\0';
  assert(c - chars == length);
  return cg_register_name_from_cstring(s, chars);
}

void emit_from_import_statement(struct cg_state *s, unsigned num_prefix_dots,
                                struct dotted_name *module, unsigned num_pairs,
                                struct from_import_item *items)
{
  if (unreachable(s)) return;
  unsigned module_name = module != NULL ? register_dotted_name(s, module)
                                        : cg_register_name_from_cstring(s, "");
  bool     import_star = (num_pairs == 0);

  struct tuple_prep *name_tuple_prep;
  if (import_star) {
    union object *name = object_intern_cstring(&s->objects, "*");
    name_tuple_prep = object_intern_tuple_begin(&s->objects, 1);
    object_new_tuple_set_at(name_tuple_prep, 0, name);
  } else {
    name_tuple_prep = object_intern_tuple_begin(&s->objects, num_pairs);
    for (unsigned i = 0; i < num_pairs; i++) {
      struct from_import_item *item = &items[i];
      union object            *name
          = object_intern_cstring(&s->objects, item->name->string);
      object_new_tuple_set_at(name_tuple_prep, i, name);
    }
  }
  union object *name_tuple
      = object_intern_tuple_end(&s->objects, name_tuple_prep,
                                /*may_free_arena=*/false);

  cg_load_const(s, object_intern_int(&s->objects, num_prefix_dots));
  cg_load_const(s, name_tuple);
  cg_op_pop_push(s, OPCODE_IMPORT_NAME, module_name, /*pop=*/2, /*push=*/1);
  if (import_star) {
    cg_op_pop1(s, OPCODE_IMPORT_STAR, 0);
  } else {
    for (unsigned i = 0; i < num_pairs; i++) {
      struct from_import_item *item = &items[i];
      struct symbol           *name = item->name;
      struct symbol           *as = item->as != NULL ? item->as : name;
      cg_op_push1(s, OPCODE_IMPORT_FROM, cg_register_name(s, name));
      cg_store(s, as);
    }
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
  }
}

void emit_from_import_star_statement(struct cg_state    *s,
                                     unsigned            num_prefix_dots,
                                     struct dotted_name *module)
{
  emit_from_import_statement(s, num_prefix_dots, module, 0, NULL);
}

bool emit_global_statement(struct cg_state *s, struct symbol *name)
{
  return cg_declare(s, name, SYMBOL_GLOBAL);
}

void emit_import_statement(struct cg_state *s, struct dotted_name *module,
                           struct symbol *as)
{
  if (unreachable(s)) return;
  unsigned      module_name = register_dotted_name(s, module);
  union object *object = object_intern_int(&s->objects, 0);
  cg_load_const(s, object);
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
  cg_op_pop_push(s, OPCODE_IMPORT_NAME, module_name, /*pop=*/2, /*push=*/1);

  if (as == NULL) {
    cg_store(s, module->symbols[0]);
  } else {
    unsigned num_symbols = module->num_symbols;
    for (unsigned i = 1; i < num_symbols; i++) {
      if (i > 1) {
        cg_op_pop_push(s, OPCODE_ROT_TWO, 0, /*pop=*/2, /*push=*/2);
        cg_op_pop1(s, OPCODE_POP_TOP, 0);
      }
      cg_op_push1(s, OPCODE_IMPORT_FROM,
                  cg_register_name(s, module->symbols[i]));
    }
    cg_store(s, as);
    if (num_symbols > 1) {
      cg_op_pop1(s, OPCODE_POP_TOP, 0);
    }
  }
}

static void emit_parameter_defaults(struct cg_state            *s,
                                    struct make_function_state *state,
                                    unsigned                    num_parameters,
                                    struct parameter           *parameters)
{
  /* create tuple with default values */
  bool     all_positional_const = true;
  unsigned num_positional_defaults = 0;
  unsigned num_keyword_defaults = 0;
  unsigned keyword_parameters_begin = num_parameters;
  for (unsigned i = 0; i < num_parameters; i++) {
    struct parameter     *parameter = &parameters[i];
    union ast_expression *initializer = parameter->initializer;
    if (parameter->variant == PARAMETER_STAR) {
      keyword_parameters_begin = i + 1;
      assert(initializer == NULL);
      continue;
    }
    if (initializer == NULL) continue;

    if (i < keyword_parameters_begin) {
      ++num_positional_defaults;
      if (ast_expression_as_constant(initializer) == NULL) {
        all_positional_const = false;
      }
    } else {
      ++num_keyword_defaults;
    }
  }

  if (num_positional_defaults > 0) {
    if (all_positional_const) {
      struct tuple_prep *tuple_prep
          = object_intern_tuple_begin(&s->objects, num_positional_defaults);
      unsigned tuple_idx = 0;
      for (unsigned i = 0; i < keyword_parameters_begin; i++) {
        struct parameter     *parameter = &parameters[i];
        union ast_expression *initializer = parameter->initializer;
        if (initializer == NULL) continue;
        union object *constant = ast_expression_as_constant(initializer);
        object_new_tuple_set_at(tuple_prep, tuple_idx++, constant);
      }
      union object *tuple = object_intern_tuple_end(&s->objects, tuple_prep,
                                                    /*may_free_arena=*/true);
      cg_load_const(s, tuple);
    } else {
      for (unsigned i = 0; i < keyword_parameters_begin; i++) {
        struct parameter     *parameter = &parameters[i];
        union ast_expression *initializer = parameter->initializer;
        if (initializer == NULL) continue;
        emit_expression(s, initializer);
      }
      cg_op_pop_push(s, OPCODE_BUILD_TUPLE, num_positional_defaults,
                     /*pop=*/num_positional_defaults, /*push=*/1);
    }
    state->defaults = true;
  }
  if (num_keyword_defaults > 0) {
    struct tuple_prep *names_prep
        = object_intern_tuple_begin(&s->objects, num_keyword_defaults);
    unsigned name_idx = 0;
    for (unsigned i = keyword_parameters_begin; i < num_parameters; i++) {
      struct parameter     *parameter = &parameters[i];
      union ast_expression *initializer = parameter->initializer;
      if (initializer == NULL) continue;
      emit_expression(s, initializer);
      union object *name
          = object_intern_cstring(&s->objects, parameter->name->string);
      object_new_tuple_set_at(names_prep, name_idx++, name);
    }
    assert(name_idx == num_keyword_defaults);
    union object *names = object_intern_tuple_end(&s->objects, names_prep,
                                                  /*may_free_arena=*/false);
    cg_load_const(s, names);
    cg_op_pop_push(s, OPCODE_BUILD_CONST_KEY_MAP, num_keyword_defaults,
                   /*pop=*/num_keyword_defaults + 1, /*push=*/1);
    state->keyword_defaults = true;
  }
}

static void emit_function_annotations(
    struct cg_state *s, struct make_function_state *state,
    unsigned num_parameters, struct parameter *parameters,
    union ast_expression *nullable return_type)
{
  /* TODO: parameters */
  unsigned num_annotation_items = 0;
  if (return_type != NULL) num_annotation_items++;
  for (unsigned i = 0; i < num_parameters; ++i) {
    struct parameter *parameter = &parameters[i];
    num_annotation_items += (parameter->type != NULL);
  }

  if (num_annotation_items == 0) return;

  state->annotations = true;
  struct tuple_prep *names_prep
      = object_intern_tuple_begin(&s->objects, num_annotation_items);
  unsigned names_idx = 0;

  for (unsigned i = 0; i < num_parameters; ++i) {
    struct parameter     *parameter = &parameters[i];
    union ast_expression *type = parameter->type;
    if (type == NULL) continue;
    emit_expression(s, type);
    union object *name
        = object_intern_cstring(&s->objects, parameter->name->string);
    object_new_tuple_set_at(names_prep, names_idx++, name);
  }
  if (return_type != NULL) {
    emit_expression(s, return_type);
    union object *name = object_intern_cstring(&s->objects, "return");
    object_new_tuple_set_at(names_prep, names_idx++, name);
  }

  union object *names = object_intern_tuple_end(&s->objects, names_prep,
                                                /*may_free_arena=*/false);
  cg_load_const(s, names);
  cg_op_pop_push(s, OPCODE_BUILD_CONST_KEY_MAP, num_annotation_items,
                 /*pop=*/num_annotation_items + 1, /*push=*/1);
}

void emit_make_function_begin(struct cg_state            *s,
                              struct make_function_state *state,
                              unsigned                    num_parameters,
                              struct parameter           *parameters,
                              unsigned positional_only_argcount,
                              union ast_expression *nullable return_type)
{
  memset(state, 0, sizeof(*state));
  if (unreachable(s)) {
    unimplemented("unreachable def");
  }

  emit_parameter_defaults(s, state, num_parameters, parameters);
  emit_function_annotations(s, state, num_parameters, parameters, return_type);

  cg_push_code(s);
  cg_code_begin(s, /*in_function=*/true);

  unsigned       keyword_only_idx = num_parameters;
  struct symbol *variable_arguments_name = NULL;
  struct symbol *variable_keyword_arguments_name = NULL;
  for (unsigned i = 0; i < num_parameters; i++) {
    struct parameter *parameter = &parameters[i];

    struct symbol *name = parameter->name;
    if (parameter->variant == PARAMETER_STAR) {
      assert(variable_arguments_name == NULL);
      keyword_only_idx = i + 1;
      variable_arguments_name = name;
      continue;
    }
    if (parameter->variant == PARAMETER_STAR_STAR) {
      assert(variable_keyword_arguments_name == NULL);
      s->code.flags |= CO_VARKEYWORDS;
      variable_keyword_arguments_name = name;
      continue;
    }

    cg_declare(s, name, SYMBOL_LOCAL);
    if (i < keyword_only_idx) {
      ++s->code.argcount;
    } else {
      ++s->code.keyword_only_argcount;
    }
  }
  if (variable_arguments_name) {
    cg_declare(s, variable_arguments_name, SYMBOL_LOCAL);
    s->code.flags |= CO_VARARGS;
  }
  if (variable_keyword_arguments_name != NULL) {
    cg_declare(s, variable_keyword_arguments_name, SYMBOL_LOCAL);
    s->code.flags |= CO_VARKEYWORDS;
  }
  s->code.positional_only_argcount = positional_only_argcount;
}

void emit_make_function_end(struct cg_state            *s,
                            struct make_function_state *state,
                            struct symbol              *symbol)
{
  emit_code_end(s);
  union object *code = cg_pop_code(s, symbol->string);

  if (state->num_closure_symbols > 0) {
    for (unsigned i = 0; i < state->num_closure_symbols; ++i) {
      struct symbol *closure_symbol = state->closure_symbols[i];
      cg_op_push1(s, OPCODE_LOAD_CLOSURE,
                  cg_closure_index(s, closure_symbol));
    }
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE, state->num_closure_symbols,
                   /*pop=*/state->num_closure_symbols, /*push=*/1);
    state->closure = true;
  }

  cg_load_const(s, code);
  cg_load_const(s, object_intern_cstring(&s->objects, symbol->string));
  uint32_t flags = 0;
  unsigned operands = 2;
  if (state->annotations) {
    flags |= MAKE_FUNCTION_ANNOTATIONS;
    operands++;
  }
  if (state->defaults) {
    flags |= MAKE_FUNCTION_DEFAULTS;
    operands++;
  }
  if (state->keyword_defaults) {
    flags |= MAKE_FUNCTION_KWDEFAULTS;
    operands++;
  }
  if (state->closure) {
    flags |= MAKE_FUNCTION_CLOSURE;
    operands++;
  }
  cg_op_pop_push(s, OPCODE_MAKE_FUNCTION, flags,
                 /*pop=*/operands, /*push=*/1);
}

bool emit_nonlocal_statement(struct cg_state *s, struct symbol *name)
{
  return cg_declare(s, name, SYMBOL_NONLOCAL);
}

void emit_raise_statement(struct cg_state               *s,
                          union ast_expression *nullable expression,
                          union ast_expression *nullable from)
{
  if (unreachable(s)) return;

  unsigned args = 0;
  if (expression != NULL) {
    emit_expression(s, expression);
    args++;
    if (from != NULL) {
      emit_expression(s, from);
      args++;
    }
  }
  /* TODO: should this be a jump and end the block?
   * cpython compiler does not seem to think so, is this on purpose? */
  cg_op_pop_push(s, OPCODE_RAISE_VARARGS, args, /*pop=*/args, /*push=*/0);
}

void emit_return_statement(struct cg_state               *s,
                           union ast_expression *nullable expression)
{
  if (unreachable(s)) return;
  if (expression != NULL) {
    emit_expression(s, expression);
  } else {
    cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
  }
  cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);
}

void emit_try_body_begin(struct cg_state *s, struct try_state *state)
{
  memset(state, 0, sizeof(*state));
  if (unreachable(s)) return;
  state->try_reachable = true;

  struct basic_block *setup_finally = cg_block_allocate(s);
  struct basic_block *setup_except = cg_block_allocate(s);
  state->setup_finally = setup_finally;
  state->setup_except = setup_except;

  cg_jump(s, setup_finally);
  cg_block_insert_delayed(s, setup_finally);
  cg_block_insert_delayed(s, setup_except);

  struct basic_block *body = cg_block_allocate(s);
  cg_block_begin(s, body);
}

void emit_try_body_end(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;
  if (unreachable(s)) return;

  /* if there was an except, then we need to insert a POP_BLOCK here */
  struct basic_block *body_exit = cg_block_allocate(s);
  state->body_exit = body_exit;

  cg_jump(s, body_exit);
  cg_block_insert_delayed(s, body_exit);
}

void emit_try_except_begin(struct cg_state *s, struct try_state *state,
                           union ast_expression *match, struct symbol *as)
{
  if (!state->try_reachable) return;

  struct basic_block *excepts;
  if (!state->had_except) {
    excepts = cg_block_allocate(s);

    struct basic_block *setup_except = state->setup_except;
    cg_block_begin_delayed(s, setup_except);
    cg_condjump(s, OPCODE_SETUP_FINALLY,
                /*target=*/excepts, /*fallthrough=*/setup_except->next);
    state->setup_except = NULL;

    state->had_except = true;
  } else {
    excepts = state->excepts;
    assert(excepts != NULL);
  }

  cg_block_begin(s, excepts);
  cg_push(s, 3); /* runtime pushes traceback, value, type when entering */

  if (match != NULL) {
    struct basic_block *excepts_next = cg_block_allocate(s);
    struct basic_block *except_match = cg_block_allocate(s);
    cg_op_push1(s, OPCODE_DUP_TOP, 0);
    emit_expression(s, match);
    cg_op_pop_push(s, OPCODE_COMPARE_OP, COMPARE_OP_EXC_MATCH,
                   /*pop=*/2, /*push=*/1);
    cg_pop(s, 1);
    cg_condjump(s, OPCODE_POP_JUMP_IF_FALSE,
                /*target=*/excepts_next, /*fallthrough=*/except_match);
    state->excepts = excepts_next;
    cg_block_begin(s, except_match);
  } else {
    state->excepts = NULL;
  }

  cg_op_pop1(s, OPCODE_POP_TOP, 0);
  if (as != NULL) {
    cg_store(s, as);
  } else {
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
  }
  cg_op_pop1(s, OPCODE_POP_TOP, 0);
  if (as != NULL) {
    struct basic_block *except_unassign_as = cg_block_allocate(s);
    struct basic_block *except_body = cg_block_allocate(s);
    assert(state->except_unassign_as == NULL);
    state->except_unassign_as = except_unassign_as;
    cg_condjump(s, OPCODE_SETUP_FINALLY,
                /*then=*/except_unassign_as,
                /*fallthrough=*/except_body);
    cg_block_begin(s, except_body);
  }
}

void emit_try_except_end(struct cg_state *s, struct try_state *state,
                         struct symbol *as)
{
  if (!state->try_reachable) return;
  struct basic_block *except_unassign_as = state->except_unassign_as;

  if (except_unassign_as != NULL) {
    assert(as != NULL);
    if (!unreachable(s)) {
      cg_op(s, OPCODE_POP_BLOCK, 0);
      cg_op(s, OPCODE_BEGIN_FINALLY, 0);
      cg_jump(s, except_unassign_as);
    }

    cg_block_begin(s, except_unassign_as);
    cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
    cg_store(s, as);
    cg_delete(s, as);
    cg_op(s, OPCODE_END_FINALLY, 0);
    state->except_unassign_as = NULL;
  } else {
    if (unreachable(s)) return;
  }
  cg_op(s, OPCODE_POP_EXCEPT, 0);

  struct basic_block *enter_finally = state->enter_finally;
  if (enter_finally == NULL) {
    enter_finally = cg_block_allocate(s);
    state->enter_finally = enter_finally;
  }
  cg_jump(s, enter_finally);
}

void emit_try_else_begin(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;
  state->had_else = true;

  struct basic_block *else_block = state->else_block;
  if (else_block == NULL) {
    else_block = cg_block_allocate(s);
    state->else_block = else_block;
  }

  struct basic_block *excepts = state->excepts;
  if (excepts != NULL) {
    cg_block_begin(s, excepts);
    cg_op(s, OPCODE_END_FINALLY, 0);
    /* END_FINALLY will jump away; no separate jump needed */
    cg_block_end(s);
    state->excepts = NULL;
  }

  cg_block_begin(s, else_block);
}

void emit_try_else_end(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;

  if (!unreachable(s)) {
    struct basic_block *enter_finally = state->enter_finally;
    if (enter_finally == NULL) {
      enter_finally = cg_block_allocate(s);
      state->enter_finally = enter_finally;
    }
    cg_jump(s, enter_finally);
  }
}

void emit_try_finally_begin(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;
  state->had_finally = true;

  struct basic_block *finally_body = cg_block_allocate(s);

  struct basic_block *setup_finally = state->setup_finally;
  cg_block_begin_delayed(s, setup_finally);
  cg_condjump(s, OPCODE_SETUP_FINALLY, /*target=*/finally_body,
              /*fallthrough=*/setup_finally->next);
  cg_push(s, 1);

  struct basic_block *enter_finally = state->enter_finally;
  struct basic_block *excepts = state->excepts;
  if (enter_finally == NULL && (state->body_exit != NULL || excepts != NULL)) {
    enter_finally = cg_block_allocate(s);
    state->enter_finally = enter_finally;
  }

  if (excepts != NULL) {
    cg_block_begin(s, excepts);
    cg_op(s, OPCODE_END_FINALLY, 0);
    cg_jump(s, enter_finally);
    state->excepts = NULL;
  }

  if (enter_finally != NULL) {
    cg_block_begin(s, enter_finally);
    if (state->had_except) {
      cg_op(s, OPCODE_POP_BLOCK, 0);
    }
    cg_op(s, OPCODE_BEGIN_FINALLY, 0);
    cg_jump(s, finally_body);
  }

  cg_block_begin(s, finally_body);
}

void emit_try_finally_end(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;
  if (unreachable(s)) return;
  cg_op_pop1(s, OPCODE_END_FINALLY, 0);

  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_block_allocate(s);
    state->footer = footer;
  }
  cg_jump(s, footer);
}

void emit_try_end(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;

  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_block_allocate(s);
    state->footer = footer;
  }

  struct basic_block *enter_finally = state->enter_finally;

  if (state->had_except) {
    struct basic_block *excepts = state->excepts;
    if (excepts != NULL) {
      if (footer == NULL) {
        footer = cg_block_allocate(s);
      }
      cg_block_begin(s, excepts);
      cg_op(s, OPCODE_END_FINALLY, 0);
      cg_jump(s, footer);
    }
  } else {
    struct basic_block *setup_except = state->setup_except;
    struct basic_block *setup_except_next = state->setup_except->next;
    assert(setup_except_next != NULL);
    cg_block_begin_delayed(s, setup_except);
    cg_jump(s, setup_except_next);
  }

  if (!state->had_finally) {
    struct basic_block *setup_finally = state->setup_finally;
    struct basic_block *setup_finally_next = state->setup_finally->next;
    assert(setup_finally_next != NULL);
    cg_block_begin_delayed(s, setup_finally);
    cg_jump(s, setup_finally_next);

    if (enter_finally != NULL) {
      cg_block_begin(s, enter_finally);
      cg_jump(s, footer);
    }
  }

  struct basic_block *body_exit = state->body_exit;
  if (body_exit != NULL) {
    struct basic_block *body_exit_target = footer;
    if (state->had_else) {
      body_exit_target = state->else_block;
    } else if (state->had_finally) {
      body_exit_target = enter_finally;
    }
    cg_block_begin_delayed(s, body_exit);
    cg_op(s, OPCODE_POP_BLOCK, 0);
    cg_jump(s, body_exit_target);
  }

  cg_block_begin(s, footer);
}

void emit_if_begin(struct cg_state *s, struct if_state *state,
                   union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }

  struct basic_block *then_block = cg_block_allocate(s);
  struct basic_block *else_or_footer = cg_block_allocate(s);
  emit_condjump_expr(s, expression,
                     /*true_block=*/then_block,
                     /*false_block=*/else_or_footer,
                     /*next=*/then_block);

  cg_block_begin(s, then_block);
  state->else_or_footer = else_or_footer;
  state->footer = NULL;
}

void emit_if_elif(struct cg_state *s, struct if_state *state,
                  union ast_expression *expression)
{
  struct basic_block *else_block = state->else_or_footer;
  if (else_block == NULL) return;
  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_block_allocate(s);
    state->footer = footer;
  }
  if (!unreachable(s)) {
    cg_jump(s, footer);
  }

  struct basic_block *new_else = cg_block_allocate(s);
  struct basic_block *then_block = cg_block_allocate(s);
  state->else_or_footer = new_else;
  cg_block_begin(s, else_block);
  emit_condjump_expr(s, expression, /*true_block=*/then_block,
                     /*false_block=*/new_else, /*next=*/then_block);

  cg_block_begin(s, then_block);
}

void emit_if_else(struct cg_state *s, struct if_state *state)
{
  struct basic_block *else_block = state->else_or_footer;
  if (else_block == NULL) return;
  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_block_allocate(s);
    state->footer = footer;
  }
  state->else_or_footer = NULL;
  if (!unreachable(s)) {
    cg_jump(s, footer);
  }

  cg_block_begin(s, else_block);
}

void emit_if_end(struct cg_state *s, struct if_state *state)
{
  struct basic_block *footer = state->footer;
  struct basic_block *else_or_footer = state->else_or_footer;
  if (footer == NULL) footer = else_or_footer;
  if (footer == NULL) return;
  if (!unreachable(s)) {
    cg_jump(s, footer);
  }

  if (else_or_footer != NULL && else_or_footer != footer) {
    cg_block_begin(s, else_or_footer);
    cg_jump(s, footer);
  }

  cg_block_begin(s, footer);
}

static void emit_for_begin_impl(struct cg_state        *s,
                                struct for_while_state *state,
                                union ast_expression   *targets)
{
  struct basic_block *header = cg_block_allocate(s);
  struct basic_block *footer = cg_block_allocate(s);
  struct basic_block *else_block = cg_block_allocate(s);
  struct basic_block *body = cg_block_allocate(s);

  cg_jump(s, header);

  cg_block_begin(s, header);
  cg_condjump(s, OPCODE_FOR_ITER, else_block, body);
  cg_push(s, 1);

  cg_block_begin(s, body);
  emit_assignment(s, targets);

  state->else_or_footer = else_block;
  state->saved = s->code.loop_state;
  s->code.loop_state = (struct loop_state){
    .continue_block = header,
    .break_block = footer,
    .pop_on_break = true,
  };
}

void emit_for_begin(struct cg_state *s, struct for_while_state *state,
                    union ast_expression *targets,
                    union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }
  emit_expression(s, expression);
  cg_op(s, OPCODE_GET_ITER, 0);
  emit_for_begin_impl(s, state, targets);
}

static void emit_loop_else(struct cg_state *s, struct for_while_state *state)
{
  struct basic_block *else_block = state->else_or_footer;
  if (else_block == NULL) return;

  struct basic_block *footer = s->code.loop_state.break_block;
  struct basic_block *header = s->code.loop_state.continue_block;
  s->code.loop_state = state->saved;
  if (!unreachable(s)) {
    cg_jump(s, header);
  }

  cg_block_begin(s, else_block);
  state->else_or_footer = footer;
}

static void emit_loop_end(struct cg_state *s, struct for_while_state *state)
{
  struct basic_block *footer = state->else_or_footer;
  if (footer == NULL) return;

  if (!unreachable(s)) {
    cg_jump(s, footer);
  }
  cg_block_begin(s, footer);
}

void emit_for_else(struct cg_state *s, struct for_while_state *state)
{
  emit_loop_else(s, state);
  cg_pop(s, 1);
}

void emit_for_end(struct cg_state *s, struct for_while_state *state)
{
  emit_loop_end(s, state);
}

bool emit_continue(struct cg_state *s)
{
  struct basic_block *target = s->code.loop_state.continue_block;
  if (target == NULL) return false;
  if (!unreachable(s)) {
    cg_jump(s, target);
  }
  return true;
}

bool emit_break(struct cg_state *s)
{
  struct basic_block *target = s->code.loop_state.break_block;
  if (target == NULL) return false;
  if (!unreachable(s)) {
    if (s->code.loop_state.pop_on_break) {
      cg_op(s, OPCODE_POP_TOP, 0);
    }
    cg_jump(s, target);
  }
  return true;
}

void emit_while_begin(struct cg_state *s, struct for_while_state *state,
                      union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }
  struct basic_block *header = cg_block_allocate(s);
  struct basic_block *body = cg_block_allocate(s);
  struct basic_block *else_block = cg_block_allocate(s);
  struct basic_block *footer = cg_block_allocate(s);

  cg_jump(s, header);
  cg_block_begin(s, header);

  emit_condjump_expr(s, expression, /*true_block=*/body,
                     /*false_block=*/footer, /*next=*/body);

  cg_block_begin(s, body);

  state->else_or_footer = else_block;
  state->saved = s->code.loop_state;
  s->code.loop_state = (struct loop_state){
    .continue_block = header,
    .break_block = footer,
    .pop_on_break = false,
  };
}

void emit_while_else(struct cg_state *s, struct for_while_state *state)
{
  emit_loop_else(s, state);
}

void emit_while_end(struct cg_state *s, struct for_while_state *state)
{
  emit_loop_end(s, state);
}

void emit_class_begin(struct cg_state *s, struct symbol *symbol)
{
  cg_op_push1(s, OPCODE_LOAD_BUILD_CLASS, 0);

  cg_push_code(s);
  cg_code_begin(s, /*in_function=*/false);
  s->code.in_class_body = true;

  cg_load(s, symbol_table_get_or_insert(s->symbol_table, "__name__"));
  cg_store(s, symbol_table_get_or_insert(s->symbol_table, "__module__"));
  cg_load_const(s, object_intern_cstring(&s->objects, symbol->string));
  cg_store(s, symbol_table_get_or_insert(s->symbol_table, "__qualname__"));
}

static void emit_decorator_calls(struct cg_state *s, unsigned num_decorators)
{
  for (unsigned i = 0; i < num_decorators; i++) {
    cg_op_pop1(s, OPCODE_CALL_FUNCTION, 1);
  }
}

void emit_class_end(struct cg_state *s, struct symbol *symbol,
                    struct ast_call *call, unsigned num_decorators,
                    unsigned num_closure_symbols,
                    struct symbol **nullable closure_symbols)
{
  emit_code_end(s);
  union object *code = cg_pop_code(s, symbol->string);

  uint32_t flags = 0;
  unsigned operands = 2;
  if (num_closure_symbols > 0) {
    for (unsigned i = 0; i < num_closure_symbols; ++i) {
      cg_op_push1(s, OPCODE_LOAD_CLOSURE,
                  cg_closure_index(s, closure_symbols[i]));
    }
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE, num_closure_symbols,
                   /*pop=*/num_closure_symbols, /*push=*/1);
    flags |= MAKE_FUNCTION_CLOSURE;
    ++operands;
  }

  cg_load_const(s, code);

  union object *string = object_intern_cstring(&s->objects, symbol->string);
  cg_load_const(s, string);
  cg_op_pop_push(s, OPCODE_MAKE_FUNCTION, flags, /*pop=*/operands,
                 /*push=*/1);
  cg_load_const(s, string);
  emit_call_helper(s, call, /*num_extra_args=*/2);
  emit_decorator_calls(s, num_decorators);
  cg_store(s, symbol);
}

void emit_def_begin(struct cg_state *s, struct make_function_state *state,
                    unsigned num_parameters, struct parameter *parameters,
                    unsigned                       positional_only_argcount,
                    union ast_expression *nullable return_type)
{
  emit_make_function_begin(s, state, num_parameters, parameters,
                           positional_only_argcount, return_type);
}

void emit_def_end(struct cg_state *s, struct make_function_state *state,
                  struct symbol *symbol, unsigned num_decorators, bool async)
{
  if (async) {
    if (s->code.flags & CO_GENERATOR) {
      s->code.flags = (s->code.flags & ~CO_GENERATOR) | CO_ASYNC_GENERATOR;
    } else {
      s->code.flags |= CO_COROUTINE;
    }
  }
  emit_make_function_end(s, state, symbol);
  emit_decorator_calls(s, num_decorators);
  cg_store(s, symbol);
}

void emit_del(struct cg_state *s, union ast_expression *targets)
{
  switch (ast_expression_type(targets)) {
  case AST_IDENTIFIER:
    cg_delete(s, targets->identifier.symbol);
    return;
  case AST_ATTR: {
    struct ast_attr *attr = &targets->attr;
    emit_expression(s, attr->expression);
    unsigned index = cg_register_name(s, attr->symbol);
    cg_op_pop1(s, OPCODE_DELETE_ATTR, index);
    return;
  }
  case AST_BINEXPR_SUBSCRIPT: {
    struct ast_binexpr *binexpr = &targets->binexpr;
    emit_expression(s, binexpr->left);
    emit_expression(s, binexpr->right);
    cg_op_pop_push(s, OPCODE_DELETE_SUBSCR, 0, /*pop=*/2, /*push=*/0);
    return;
  }
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY: {
    struct ast_expression_list *list = &targets->expression_list;
    unsigned                    num_expressions = list->num_expressions;
    for (unsigned i = 0; i < num_expressions; i++) {
      emit_del(s, list->expressions[i]);
    }
    return;
  }
  case AST_INVALID:
    return;
  default:
    internal_error("invalid del target");
  }
}

static void emit_generator_expression_part(
    struct cg_state *s, struct ast_generator_expression *generator_expression,
    unsigned part_index)
{
  if (part_index >= generator_expression->num_parts) {
    emit_expression(s, generator_expression->expression);
    enum ast_expression_type type = generator_expression->base.type;
    if (type == AST_LIST_COMPREHENSION || type == AST_SET_COMPREHENSION
        || type == AST_DICT_COMPREHENSION) {
      /* Need to compute position of list/set in stack...
       * using "number of for" + 1 for now but this feels sketchy... */
      unsigned num_for = 0;
      unsigned num_parts = generator_expression->num_parts;
      for (unsigned i = 0; i < num_parts; i++) {
        if (generator_expression->parts[i].type
            == GENERATOR_EXPRESSION_PART_FOR) {
          num_for++;
        }
      }
      if (type == AST_LIST_COMPREHENSION) {
        cg_op_pop1(s, OPCODE_LIST_APPEND, num_for + 1);
      } else if (type == AST_SET_COMPREHENSION) {
        cg_op_pop1(s, OPCODE_SET_ADD, num_for + 1);
      } else {
        assert(type == AST_DICT_COMPREHENSION);
        emit_expression(s, generator_expression->item_value);
        cg_op_pop_push(s, OPCODE_MAP_ADD, num_for + 1, /*pop=*/2, /*push=*/0);
      }
    } else {
      assert(type == AST_GENERATOR_EXPRESSION);
      s->code.flags |= CO_GENERATOR;
      cg_op(s, OPCODE_YIELD_VALUE, 0);
      cg_op_pop1(s, OPCODE_POP_TOP, 0);
    }
    return;
  }
  struct generator_expression_part *part
      = &generator_expression->parts[part_index];
  if (part->type == GENERATOR_EXPRESSION_PART_FOR) {
    struct for_while_state state;
    union ast_expression  *targets = part->targets;
    assert(targets != NULL);
    emit_for_begin(s, &state, targets, part->expression);

    emit_generator_expression_part(s, generator_expression, part_index + 1);

    emit_for_else(s, &state);
    emit_for_end(s, &state);
    return;
  }
  assert(part->type == GENERATOR_EXPRESSION_PART_IF);
  struct basic_block *false_block = cg_block_allocate(s);

  emit_expression(s, part->expression);
  struct basic_block *loop_header = s->code.loop_state.continue_block;
  cg_pop(s, 1);
  cg_condjump(s, OPCODE_POP_JUMP_IF_FALSE, loop_header, false_block);
  cg_block_begin(s, false_block);

  emit_generator_expression_part(s, generator_expression, part_index + 1);
}

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression)
{
  struct symbol *dot_zero = symbol_table_get_or_insert(s->symbol_table, ".0");
  cg_declare(s, dot_zero, SYMBOL_LOCAL);
  s->code.argcount = 1;

  enum ast_expression_type type = generator_expression->base.type;
  bool                     return_value;
  if (type == AST_LIST_COMPREHENSION) {
    cg_op_push1(s, OPCODE_BUILD_LIST, 0);
    return_value = true;
  } else if (type == AST_SET_COMPREHENSION) {
    cg_op_push1(s, OPCODE_BUILD_SET, 0);
    return_value = true;
  } else if (type == AST_DICT_COMPREHENSION) {
    cg_op_push1(s, OPCODE_BUILD_MAP, 0);
    return_value = true;
  } else {
    assert(type == AST_GENERATOR_EXPRESSION);
    return_value = false;
  }

  struct generator_expression_part *part = generator_expression->parts;
  cg_op_push1(s, OPCODE_LOAD_FAST, 0);
  struct for_while_state state;
  emit_for_begin_impl(s, &state, part->targets);

  emit_generator_expression_part(s, generator_expression, /*part_index=*/1);

  emit_for_else(s, &state);
  emit_for_end(s, &state);

  if (return_value) {
    cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
    cg_block_end(s);
  }
}

void emit_with_begin(struct cg_state *s, struct with_state *state,
                     union ast_expression *expression,
                     union ast_expression *targets)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }

  struct basic_block *cleanup = cg_block_allocate(s);
  struct basic_block *body = cg_block_allocate(s);
  state->cleanup = cleanup;

  emit_expression(s, expression);
  cg_condjump(s, OPCODE_SETUP_WITH, cleanup, body);

  cg_block_begin(s, body);
  if (targets != NULL) {
    emit_assignment(s, targets);
  } else {
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
  }
}

void emit_with_end(struct cg_state *s, struct with_state *state)
{
  if (state->cleanup == NULL) return;
  if (!unreachable(s)) {
    cg_op(s, OPCODE_POP_BLOCK, 0);
    cg_op(s, OPCODE_BEGIN_FINALLY, 0);
    cg_jump(s, state->cleanup);
  }

  cg_block_begin(s, state->cleanup);
  cg_op_push1(s, OPCODE_WITH_CLEANUP_START, 0);
  cg_op_pop1(s, OPCODE_WITH_CLEANUP_FINISH, 0);
  cg_op(s, OPCODE_END_FINALLY, 0);
}

void emit_yield_statement(struct cg_state *s, union ast_expression *expression)
{
  s->code.flags |= CO_GENERATOR;
  if (unreachable(s)) return;
  emit_yield(s, expression);
  cg_op_pop1(s, OPCODE_POP_TOP, 0);
}

void emit_yield_from_statement(struct cg_state      *s,
                               union ast_expression *expression)
{
  s->code.flags |= CO_GENERATOR;
  if (unreachable(s)) return;
  emit_yield_from(s, expression);
  cg_op_pop1(s, OPCODE_POP_TOP, 0);
}

struct binding_scope {
  struct binding_scope   *nullable parent;
  struct ast_statement_def *nullable def;
  struct cg_state        *cg;
  bool                    is_class;

  struct symbol *locals_inline[16];
  struct symbol *bound_before_decl_inline[16];
  struct symbol *globals_inline[8];
  struct symbol *nonlocals_inline[8];
  struct symbol *freevars_inline[8];
  struct symbol *cellvars_inline[8];
  struct symbol *uses_inline[16];
  struct ast_statement_def *children_inline[8];
  struct ast_statement_class *class_children_inline[8];

  struct idynarray locals;
  struct idynarray bound_before_decl;
  struct idynarray globals;
  struct idynarray nonlocals;
  struct idynarray freevars;
  struct idynarray cellvars;
  struct idynarray uses;
  struct idynarray children;
  struct idynarray class_children;
};

static void emit_statement_list_with_function(
    struct cg_state *s, struct ast_statement_list *statement_list,
    struct ast_statement_def *nullable current_function);
static void emit_statement(struct cg_state *s, union ast_statement *statement,
                           struct ast_statement_def *nullable current_function);

static bool symbol_array_contains(struct idynarray *array, struct symbol *symbol)
{
  struct symbol **symbols = idynarray_data(array);
  unsigned        num_symbols = idynarray_length(array, struct symbol *);
  for (unsigned i = 0; i < num_symbols; ++i) {
    if (symbols[i] == symbol) {
      return true;
    }
  }
  return false;
}

static bool symbol_array_append_unique(struct idynarray *array,
                                       struct symbol    *symbol)
{
  if (symbol_array_contains(array, symbol)) {
    return false;
  }
  *idynarray_append(array, struct symbol *) = symbol;
  return true;
}

static struct symbol **nullable symbol_array_copy(struct cg_state *s,
                                                  struct idynarray *array)
{
  unsigned num_symbols = idynarray_length(array, struct symbol *);
  if (num_symbols == 0) return NULL;
  size_t size = num_symbols * sizeof(struct symbol *);
  struct symbol **result = arena_allocate(object_intern_arena(&s->objects), size,
                                          alignof(struct symbol *));
  memcpy(result, idynarray_data(array), size);
  return result;
}

static void binding_scope_init(struct binding_scope        *scope,
                               struct cg_state             *cg,
                               struct binding_scope *nullable parent,
                               struct ast_statement_def *nullable def,
                               bool                       is_class)
{
  memset(scope, 0, sizeof(*scope));
  scope->cg = cg;
  scope->parent = parent;
  scope->def = def;
  scope->is_class = is_class;
  idynarray_init(&scope->locals, scope->locals_inline, sizeof(scope->locals_inline));
  idynarray_init(&scope->bound_before_decl, scope->bound_before_decl_inline,
                 sizeof(scope->bound_before_decl_inline));
  idynarray_init(&scope->globals, scope->globals_inline, sizeof(scope->globals_inline));
  idynarray_init(&scope->nonlocals, scope->nonlocals_inline,
                 sizeof(scope->nonlocals_inline));
  idynarray_init(&scope->freevars, scope->freevars_inline,
                 sizeof(scope->freevars_inline));
  idynarray_init(&scope->cellvars, scope->cellvars_inline,
                 sizeof(scope->cellvars_inline));
  idynarray_init(&scope->uses, scope->uses_inline, sizeof(scope->uses_inline));
  idynarray_init(&scope->children, scope->children_inline,
                 sizeof(scope->children_inline));
  idynarray_init(&scope->class_children, scope->class_children_inline,
                 sizeof(scope->class_children_inline));
}

static void binding_scope_free(struct binding_scope *scope)
{
  idynarray_clear(&scope->class_children);
  idynarray_free(&scope->class_children);
  idynarray_clear(&scope->children);
  idynarray_free(&scope->children);
  idynarray_free(&scope->uses);
  idynarray_free(&scope->cellvars);
  idynarray_free(&scope->freevars);
  idynarray_free(&scope->nonlocals);
  idynarray_free(&scope->globals);
  idynarray_free(&scope->bound_before_decl);
  idynarray_free(&scope->locals);
}

static bool resolve_from_parent_scope(struct binding_scope *nullable scope,
                                      struct symbol                 *name)
{
  if (scope == NULL) {
    return false;
  }

  if (scope->is_class) {
    if (symbol_array_contains(&scope->freevars, name)) {
      return true;
    }
    if (resolve_from_parent_scope(scope->parent, name)) {
      symbol_array_append_unique(&scope->freevars, name);
      return true;
    }
    return false;
  }

  if (symbol_array_contains(&scope->globals, name)) {
    return false;
  }
  if (symbol_array_contains(&scope->locals, name)) {
    symbol_array_append_unique(&scope->cellvars, name);
    return true;
  }
  if (symbol_array_contains(&scope->freevars, name)
      || symbol_array_contains(&scope->nonlocals, name)) {
    symbol_array_append_unique(&scope->freevars, name);
    return true;
  }
  if (resolve_from_parent_scope(scope->parent, name)) {
    symbol_array_append_unique(&scope->freevars, name);
    return true;
  }
  return false;
}

static void scope_mark_local(struct binding_scope *scope, struct symbol *symbol)
{
  if (symbol_array_contains(&scope->globals, symbol)
      || symbol_array_contains(&scope->nonlocals, symbol)) {
    return;
  }
  symbol_array_append_unique(&scope->locals, symbol);
  symbol_array_append_unique(&scope->bound_before_decl, symbol);
}

static void scope_note_use(struct binding_scope *scope, struct symbol *symbol)
{
  symbol_array_append_unique(&scope->uses, symbol);
}

static void analyze_expression(struct binding_scope *scope,
                               union ast_expression *expression);
static void analyze_statement_collect(struct binding_scope *scope,
                                      union ast_statement *statement);

static void analyze_assignment_target(struct binding_scope *scope,
                                      union ast_expression *target, bool bind)
{
  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER:
    if (bind) {
      scope_mark_local(scope, target->identifier.symbol);
    }
    return;
  case AST_ATTR:
    analyze_expression(scope, target->attr.expression);
    return;
  case AST_BINEXPR_SUBSCRIPT:
    analyze_expression(scope, target->binexpr.left);
    analyze_expression(scope, target->binexpr.right);
    return;
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY: {
    struct ast_expression_list *list = &target->expression_list;
    for (unsigned i = 0; i < list->num_expressions; ++i) {
      union ast_expression *item = list->expressions[i];
      if (ast_expression_type(item) == AST_UNEXPR_STAR) {
        item = item->unexpr.op;
      }
      analyze_assignment_target(scope, item, bind);
    }
    return;
  }
  case AST_INVALID:
    return;
  default:
    analyze_expression(scope, target);
    return;
  }
}

static void analyze_augassign_expression(struct binding_scope *scope,
                                         union ast_expression *expression)
{
  assert(expression->type >= AST_BINEXPR_ADD_ASSIGN
         && expression->type <= AST_BINEXPR_XOR_ASSIGN);
  union ast_expression *target = expression->binexpr.left;
  analyze_expression(scope, target);
  if (ast_expression_type(target) == AST_IDENTIFIER) {
    scope_mark_local(scope, target->identifier.symbol);
  }
  analyze_expression(scope, expression->binexpr.right);
}

static void analyze_expression(struct binding_scope *scope,
                               union ast_expression *expression)
{
  switch (ast_expression_type(expression)) {
  case AST_IDENTIFIER:
    scope_note_use(scope, expression->identifier.symbol);
    return;
  case AST_ATTR:
    analyze_expression(scope, expression->attr.expression);
    return;
  case AST_BINEXPR_ASSIGN:
    if (ast_expression_type(expression->binexpr.left) == AST_IDENTIFIER) {
      scope_mark_local(scope, expression->binexpr.left->identifier.symbol);
    } else {
      analyze_expression(scope, expression->binexpr.left);
    }
    analyze_expression(scope, expression->binexpr.right);
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
    analyze_augassign_expression(scope, expression);
    return;
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_LOGICAL_AND:
  case AST_BINEXPR_LOGICAL_OR:
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
    analyze_expression(scope, expression->binexpr.left);
    analyze_expression(scope, expression->binexpr.right);
    return;
  case AST_CALL: {
    struct ast_call *call = &expression->call;
    if (call->callee != NULL) {
      analyze_expression(scope, call->callee);
    }
    for (unsigned i = 0; i < call->num_arguments; ++i) {
      analyze_expression(scope, call->arguments[i].expression);
    }
    return;
  }
  case AST_COMPARISON: {
    struct ast_comparison *comparison = &expression->comparison;
    analyze_expression(scope, comparison->left);
    for (unsigned i = 0; i < comparison->num_operands; ++i) {
      analyze_expression(scope, comparison->operands[i].operand);
    }
    return;
  }
  case AST_CONDITIONAL:
    analyze_expression(scope, expression->conditional.condition);
    analyze_expression(scope, expression->conditional.true_expression);
    analyze_expression(scope, expression->conditional.false_expression);
    return;
  case AST_CONST:
  case AST_INVALID:
    return;
  case AST_DICT_DISPLAY: {
    struct ast_dict_item_list *items = &expression->dict_item_list;
    for (unsigned i = 0; i < items->num_items; ++i) {
      if (items->items[i].key != NULL) {
        analyze_expression(scope, items->items[i].key);
      }
      analyze_expression(scope, items->items[i].value);
    }
    return;
  }
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY:
  case AST_SET_DISPLAY: {
    struct ast_expression_list *list = &expression->expression_list;
    for (unsigned i = 0; i < list->num_expressions; ++i) {
      analyze_expression(scope, list->expressions[i]);
    }
    return;
  }
  case AST_FSTRING: {
    struct ast_fstring *fstring = &expression->fstring;
    for (unsigned i = 0; i < fstring->num_elements; ++i) {
      struct fstring_element *element = &fstring->elements[i];
      if (element->is_expression) {
        analyze_expression(scope, element->u.expression);
        if (element->format_spec != NULL) {
          analyze_expression(scope, element->format_spec);
        }
      }
    }
    return;
  }
  case AST_GENERATOR_EXPRESSION:
  case AST_LIST_COMPREHENSION:
  case AST_SET_COMPREHENSION:
  case AST_DICT_COMPREHENSION: {
    /* Comprehensions have a nested scope; only the first iterable is
     * evaluated in the outer scope. */
    struct ast_generator_expression *generator = &expression->generator_expression;
    if (generator->num_parts > 0
        && generator->parts[0].type == GENERATOR_EXPRESSION_PART_FOR) {
      analyze_expression(scope, generator->parts[0].expression);
    }
    return;
  }
  case AST_LAMBDA:
    /* Lambda body has its own scope. */
    return;
  case AST_SLICE:
    if (expression->slice.start != NULL) analyze_expression(scope, expression->slice.start);
    if (expression->slice.stop != NULL) analyze_expression(scope, expression->slice.stop);
    if (expression->slice.step != NULL) analyze_expression(scope, expression->slice.step);
    return;
  case AST_UNEXPR_AWAIT:
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_NOT:
  case AST_UNEXPR_PLUS:
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
    analyze_expression(scope, expression->unexpr.op);
    return;
  case AST_YIELD:
  case AST_YIELD_FROM:
    if (expression->yield.value != NULL) {
      analyze_expression(scope, expression->yield.value);
    }
    return;
  }
}

static void analyze_statement_list_collect(struct binding_scope      *scope,
                                           struct ast_statement_list *statement_list)
{
  for (unsigned i = 0; i < statement_list->num_statements; ++i) {
    analyze_statement_collect(scope, statement_list->statements[i]);
  }
}

static void analyze_global_or_nonlocal(struct binding_scope *scope,
                                       struct location       location,
                                       struct symbol        *name,
                                       bool                  nonlocal)
{
  (void)location;
  if (symbol_array_contains(&scope->bound_before_decl, name)) {
    return;
  }
  if (nonlocal) {
    symbol_array_append_unique(&scope->nonlocals, name);
  } else {
    symbol_array_append_unique(&scope->globals, name);
  }
}

static void analyze_statement_collect(struct binding_scope *scope,
                                      union ast_statement *statement)
{
  switch (ast_statement_type(statement)) {
  case AST_STATEMENT_ANNOTATION:
    analyze_expression(scope, statement->annotation.annotation);
    analyze_assignment_target(scope, statement->annotation.target,
                              /*bind=*/true);
    if (statement->annotation.value != NULL) {
      analyze_expression(scope, statement->annotation.value);
    }
    return;
  case AST_STATEMENT_ASSERT:
    analyze_expression(scope, statement->assertion.expression);
    if (statement->assertion.message != NULL) {
      analyze_expression(scope, statement->assertion.message);
    }
    return;
  case AST_STATEMENT_ASSIGN:
    analyze_expression(scope, statement->assign.value);
    for (unsigned i = 0; i < statement->assign.num_targets; ++i) {
      analyze_assignment_target(scope, statement->assign.targets[i],
                                /*bind=*/true);
    }
    return;
  case AST_STATEMENT_AUGASSIGN:
    analyze_augassign_expression(scope, statement->augassign.expression);
    return;
  case AST_STATEMENT_BREAK:
  case AST_STATEMENT_CONTINUE:
  case AST_STATEMENT_PASS:
    return;
  case AST_STATEMENT_CLASS: {
    struct ast_statement_class *class_stmt = &statement->class_stmt;
    scope_mark_local(scope, class_stmt->name);
    for (unsigned i = 0; i < class_stmt->num_decorators; ++i) {
      analyze_expression(scope, class_stmt->decorators[i]);
    }
    for (unsigned i = 0; i < class_stmt->call->num_arguments; ++i) {
      analyze_expression(scope, class_stmt->call->arguments[i].expression);
    }
    *idynarray_append(&scope->class_children, struct ast_statement_class *)
        = class_stmt;
    return;
  }
  case AST_STATEMENT_DEF: {
    struct ast_statement_def *def_stmt = &statement->def_stmt;
    scope_mark_local(scope, def_stmt->name);
    for (unsigned i = 0; i < def_stmt->num_decorators; ++i) {
      analyze_expression(scope, def_stmt->decorators[i]);
    }
    for (unsigned i = 0; i < def_stmt->num_parameters; ++i) {
      struct parameter *parameter = &def_stmt->parameters[i];
      if (parameter->type != NULL) {
        analyze_expression(scope, parameter->type);
      }
      if (parameter->initializer != NULL) {
        analyze_expression(scope, parameter->initializer);
      }
    }
    if (def_stmt->return_type != NULL) {
      analyze_expression(scope, def_stmt->return_type);
    }
    *idynarray_append(&scope->children, struct ast_statement_def *) = def_stmt;
    return;
  }
  case AST_STATEMENT_DEL:
    analyze_assignment_target(scope, statement->del_stmt.targets, /*bind=*/false);
    return;
  case AST_STATEMENT_EXPRESSION:
    analyze_expression(scope, statement->expression_stmt.expression);
    return;
  case AST_STATEMENT_FOR:
    analyze_assignment_target(scope, statement->for_stmt.targets, /*bind=*/true);
    analyze_expression(scope, statement->for_stmt.expression);
    analyze_statement_list_collect(scope, statement->for_stmt.body);
    if (statement->for_stmt.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->for_stmt.else_body);
    }
    return;
  case AST_STATEMENT_FROM_IMPORT:
    if (!statement->from_import_stmt.import_star) {
      for (unsigned i = 0; i < statement->from_import_stmt.num_items; ++i) {
        struct from_import_item *item = &statement->from_import_stmt.items[i];
        scope_mark_local(scope, item->as != NULL ? item->as : item->name);
      }
    }
    return;
  case AST_STATEMENT_GLOBAL:
    for (unsigned i = 0; i < statement->global_stmt.num_names; ++i) {
      analyze_global_or_nonlocal(scope, statement->base.location,
                                 statement->global_stmt.names[i],
                                 /*nonlocal=*/false);
    }
    return;
  case AST_STATEMENT_IF:
    analyze_expression(scope, statement->if_stmt.condition);
    analyze_statement_list_collect(scope, statement->if_stmt.body);
    for (unsigned i = 0; i < statement->if_stmt.num_elifs; ++i) {
      analyze_expression(scope, statement->if_stmt.elifs[i].condition);
      analyze_statement_list_collect(scope, statement->if_stmt.elifs[i].body);
    }
    if (statement->if_stmt.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->if_stmt.else_body);
    }
    return;
  case AST_STATEMENT_IMPORT:
    for (unsigned i = 0; i < statement->import_stmt.num_items; ++i) {
      struct ast_import_item *item = &statement->import_stmt.items[i];
      struct symbol          *name = item->as;
      if (name == NULL) {
        name = item->module->symbols[0];
      }
      scope_mark_local(scope, name);
    }
    return;
  case AST_STATEMENT_NONLOCAL:
    for (unsigned i = 0; i < statement->nonlocal_stmt.num_names; ++i) {
      analyze_global_or_nonlocal(scope, statement->base.location,
                                 statement->nonlocal_stmt.names[i],
                                 /*nonlocal=*/true);
    }
    return;
  case AST_STATEMENT_RAISE:
    if (statement->raise_stmt.expression != NULL) {
      analyze_expression(scope, statement->raise_stmt.expression);
    }
    if (statement->raise_stmt.from != NULL) {
      analyze_expression(scope, statement->raise_stmt.from);
    }
    return;
  case AST_STATEMENT_RETURN:
    if (statement->return_stmt.expression != NULL) {
      analyze_expression(scope, statement->return_stmt.expression);
    }
    return;
  case AST_STATEMENT_TRY:
    analyze_statement_list_collect(scope, statement->try_stmt.body);
    for (unsigned i = 0; i < statement->try_stmt.num_excepts; ++i) {
      struct ast_try_except *except_stmt = &statement->try_stmt.excepts[i];
      if (except_stmt->match != NULL) {
        analyze_expression(scope, except_stmt->match);
      }
      if (except_stmt->as != NULL) {
        scope_mark_local(scope, except_stmt->as);
      }
      analyze_statement_list_collect(scope, except_stmt->body);
    }
    if (statement->try_stmt.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->try_stmt.else_body);
    }
    if (statement->try_stmt.finally_body != NULL) {
      analyze_statement_list_collect(scope, statement->try_stmt.finally_body);
    }
    return;
  case AST_STATEMENT_WHILE:
    analyze_expression(scope, statement->while_stmt.condition);
    analyze_statement_list_collect(scope, statement->while_stmt.body);
    if (statement->while_stmt.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->while_stmt.else_body);
    }
    return;
  case AST_STATEMENT_WITH:
    for (unsigned i = 0; i < statement->with_stmt.num_items; ++i) {
      analyze_expression(scope, statement->with_stmt.items[i].expression);
      if (statement->with_stmt.items[i].targets != NULL) {
        analyze_assignment_target(scope, statement->with_stmt.items[i].targets,
                                  /*bind=*/true);
      }
    }
    analyze_statement_list_collect(scope, statement->with_stmt.body);
    return;
  case AST_STATEMENT_YIELD:
  case AST_STATEMENT_YIELD_FROM:
    if (statement->yield_stmt.expression != NULL) {
      analyze_expression(scope, statement->yield_stmt.expression);
    }
    return;
  }
}

static void analyze_function_bindings(struct cg_state *s,
                                      struct ast_statement_def *def_stmt,
                                      struct binding_scope *nullable parent);
static void analyze_class_bindings(struct cg_state *s,
                                   struct ast_statement_class *class_stmt,
                                   struct binding_scope *nullable parent);

static void analyze_class_bindings(struct cg_state *s,
                                   struct ast_statement_class *class_stmt,
                                   struct binding_scope *nullable parent)
{
  if (class_stmt->scope_bindings_ready) {
    return;
  }

  struct binding_scope scope;
  binding_scope_init(&scope, s, parent, /*def=*/NULL, /*is_class=*/true);
  analyze_statement_list_collect(&scope, class_stmt->body);

  struct ast_statement_class **class_children
      = idynarray_data(&scope.class_children);
  unsigned num_class_children
      = idynarray_length(&scope.class_children, struct ast_statement_class *);
  for (unsigned i = 0; i < num_class_children; ++i) {
    analyze_class_bindings(s, class_children[i], &scope);
  }

  struct ast_statement_def **children = idynarray_data(&scope.children);
  unsigned                  num_children
      = idynarray_length(&scope.children, struct ast_statement_def *);
  for (unsigned i = 0; i < num_children; ++i) {
    analyze_function_bindings(s, children[i], &scope);
  }

  struct symbol **nonlocals = idynarray_data(&scope.nonlocals);
  unsigned        num_nonlocals = idynarray_length(&scope.nonlocals, struct symbol *);
  for (unsigned i = 0; i < num_nonlocals; ++i) {
    struct symbol *name = nonlocals[i];
    if (!resolve_from_parent_scope(scope.parent, name)) {
      diag_begin_error(s->d, class_stmt->base.location);
      diag_frag(s->d, "no binding for nonlocal ");
      diag_symbol(s->d, name);
      diag_frag(s->d, " found");
      diag_end(s->d);
    } else {
      symbol_array_append_unique(&scope.freevars, name);
    }
  }

  struct symbol **uses = idynarray_data(&scope.uses);
  unsigned        num_uses = idynarray_length(&scope.uses, struct symbol *);
  for (unsigned i = 0; i < num_uses; ++i) {
    struct symbol *name = uses[i];
    if (symbol_array_contains(&scope.locals, name)
        || symbol_array_contains(&scope.globals, name)
        || symbol_array_contains(&scope.nonlocals, name)
        || symbol_array_contains(&scope.freevars, name)) {
      continue;
    }
    if (resolve_from_parent_scope(scope.parent, name)) {
      symbol_array_append_unique(&scope.freevars, name);
    }
  }

  class_stmt->num_scope_globals = idynarray_length(&scope.globals, struct symbol *);
  class_stmt->scope_globals = symbol_array_copy(s, &scope.globals);
  class_stmt->num_scope_locals = idynarray_length(&scope.locals, struct symbol *);
  class_stmt->scope_locals = symbol_array_copy(s, &scope.locals);
  class_stmt->num_scope_freevars = idynarray_length(&scope.freevars, struct symbol *);
  class_stmt->scope_freevars = symbol_array_copy(s, &scope.freevars);
  class_stmt->scope_bindings_ready = true;

  binding_scope_free(&scope);
}

static void analyze_function_bindings(struct cg_state *s,
                                      struct ast_statement_def *def_stmt,
                                      struct binding_scope *nullable parent)
{
  if (def_stmt->scope_bindings_ready) {
    return;
  }

  struct binding_scope scope;
  binding_scope_init(&scope, s, parent, def_stmt, /*is_class=*/false);

  for (unsigned i = 0; i < def_stmt->num_parameters; ++i) {
    scope_mark_local(&scope, def_stmt->parameters[i].name);
  }

  analyze_statement_list_collect(&scope, def_stmt->body);

  struct ast_statement_class **class_children
      = idynarray_data(&scope.class_children);
  unsigned num_class_children
      = idynarray_length(&scope.class_children, struct ast_statement_class *);
  for (unsigned i = 0; i < num_class_children; ++i) {
    analyze_class_bindings(s, class_children[i], &scope);
  }

  struct ast_statement_def **children = idynarray_data(&scope.children);
  unsigned                  num_children
      = idynarray_length(&scope.children, struct ast_statement_def *);
  for (unsigned i = 0; i < num_children; ++i) {
    analyze_function_bindings(s, children[i], &scope);
  }

  struct symbol **nonlocals = idynarray_data(&scope.nonlocals);
  unsigned        num_nonlocals = idynarray_length(&scope.nonlocals, struct symbol *);
  for (unsigned i = 0; i < num_nonlocals; ++i) {
    struct symbol *name = nonlocals[i];
    if (!resolve_from_parent_scope(scope.parent, name)) {
      diag_begin_error(s->d, def_stmt->base.location);
      diag_frag(s->d, "no binding for nonlocal ");
      diag_symbol(s->d, name);
      diag_frag(s->d, " found");
      diag_end(s->d);
    } else {
      symbol_array_append_unique(&scope.freevars, name);
    }
  }

  struct symbol **uses = idynarray_data(&scope.uses);
  unsigned        num_uses = idynarray_length(&scope.uses, struct symbol *);
  for (unsigned i = 0; i < num_uses; ++i) {
    struct symbol *name = uses[i];
    if (symbol_array_contains(&scope.locals, name)
        || symbol_array_contains(&scope.globals, name)
        || symbol_array_contains(&scope.nonlocals, name)
        || symbol_array_contains(&scope.freevars, name)) {
      continue;
    }
    if (resolve_from_parent_scope(scope.parent, name)) {
      symbol_array_append_unique(&scope.freevars, name);
    }
  }

  def_stmt->num_scope_globals = idynarray_length(&scope.globals, struct symbol *);
  def_stmt->scope_globals = symbol_array_copy(s, &scope.globals);
  def_stmt->num_scope_locals = idynarray_length(&scope.locals, struct symbol *);
  def_stmt->scope_locals = symbol_array_copy(s, &scope.locals);
  def_stmt->num_scope_cellvars = idynarray_length(&scope.cellvars, struct symbol *);
  def_stmt->scope_cellvars = symbol_array_copy(s, &scope.cellvars);
  def_stmt->num_scope_freevars = idynarray_length(&scope.freevars, struct symbol *);
  def_stmt->scope_freevars = symbol_array_copy(s, &scope.freevars);
  def_stmt->scope_bindings_ready = true;

  binding_scope_free(&scope);
}

static void apply_class_bindings(struct cg_state *s,
                                 struct ast_statement_class *class_stmt)
{
  for (unsigned i = 0; i < class_stmt->num_scope_freevars; ++i) {
    cg_register_freevar(s, class_stmt->scope_freevars[i]);
  }
  for (unsigned i = 0; i < class_stmt->num_scope_globals; ++i) {
    cg_declare(s, class_stmt->scope_globals[i], SYMBOL_GLOBAL);
  }
  for (unsigned i = 0; i < class_stmt->num_scope_freevars; ++i) {
    struct symbol *name = class_stmt->scope_freevars[i];
    bool           is_global = false;
    for (unsigned j = 0; j < class_stmt->num_scope_globals; ++j) {
      if (class_stmt->scope_globals[j] == name) {
        is_global = true;
        break;
      }
    }
    if (is_global) continue;

    bool is_local = false;
    for (unsigned j = 0; j < class_stmt->num_scope_locals; ++j) {
      if (class_stmt->scope_locals[j] == name) {
        is_local = true;
        break;
      }
    }
    if (is_local) continue;

    cg_declare(s, class_stmt->scope_freevars[i], SYMBOL_NONLOCAL);
  }
}

static void apply_function_bindings(struct cg_state *s,
                                    struct ast_statement_def *def_stmt)
{
  for (unsigned i = 0; i < def_stmt->num_scope_globals; ++i) {
    cg_declare(s, def_stmt->scope_globals[i], SYMBOL_GLOBAL);
  }
  for (unsigned i = 0; i < def_stmt->num_scope_freevars; ++i) {
    cg_declare(s, def_stmt->scope_freevars[i], SYMBOL_NONLOCAL);
  }
  for (unsigned i = 0; i < def_stmt->num_scope_locals; ++i) {
    cg_declare(s, def_stmt->scope_locals[i], SYMBOL_LOCAL);
  }
  for (unsigned i = 0; i < def_stmt->num_scope_cellvars; ++i) {
    cg_promote_to_cell(s, def_stmt->scope_cellvars[i]);
  }
}

static void emit_function_closure(struct cg_state            *s,
                                  struct make_function_state *state,
                                  struct ast_statement_def   *def_stmt)
{
  (void)s;
  unsigned num_freevars = def_stmt->num_scope_freevars;
  if (num_freevars == 0) {
    return;
  }
  state->num_closure_symbols = num_freevars;
  state->closure_symbols = def_stmt->scope_freevars;
}

static void emit_break_statement(struct cg_state *s, struct location location)
{
  if (!emit_break(s)) {
    diag_begin_error(s->d, location);
    diag_token_kind(s->d, T_break);
    diag_frag(s->d, " outside loop");
    diag_end(s->d);
  }
}

static void emit_continue_statement(struct cg_state *s, struct location location)
{
  if (!emit_continue(s)) {
    diag_begin_error(s->d, location);
    diag_token_kind(s->d, T_continue);
    diag_frag(s->d, " outside loop");
    diag_end(s->d);
  }
}

static void emit_global_statement_checked(struct cg_state *s,
                                          struct location  location,
                                          struct symbol   *name)
{
  if (!emit_global_statement(s, name)) {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "name ");
    diag_symbol(s->d, name);
    diag_frag(s->d, " is assigned to before global declaration");
    diag_end(s->d);
  }
}

static void emit_nonlocal_statement_checked(struct cg_state *s,
                                            struct location  location,
                                            struct symbol   *name)
{
  if (!emit_nonlocal_statement(s, name)) {
    diag_begin_error(s->d, location);
    diag_frag(s->d, "name ");
    diag_symbol(s->d, name);
    diag_frag(s->d, " is assigned to before nonlocal declaration");
    diag_end(s->d);
  }
}

static void emit_return_statement_checked(struct cg_state               *s,
                                          struct location                location,
                                          union ast_expression *nullable expression)
{
  if (!cg_in_function(s)) {
    diag_begin_error(s->d, location);
    diag_token_kind(s->d, T_return);
    diag_frag(s->d, " outside function");
    diag_end(s->d);
  }
  emit_return_statement(s, expression);
}

void emit_statement_list(struct cg_state *s,
                         struct ast_statement_list *statement_list)
{
  emit_statement_list_with_function(s, statement_list,
                                    /*current_function=*/NULL);
}

static void emit_statement_list_with_function(
    struct cg_state *s, struct ast_statement_list *statement_list,
    struct ast_statement_def *nullable current_function)
{
  for (unsigned i = 0; i < statement_list->num_statements; ++i) {
    emit_statement(s, statement_list->statements[i], current_function);
  }
}

static void emit_statement(struct cg_state *s, union ast_statement *statement,
                           struct ast_statement_def *nullable current_function)
{
  switch (ast_statement_type(statement)) {
  case AST_STATEMENT_ANNOTATION: {
    struct ast_statement_annotation *annotation = &statement->annotation;
    if (annotation->value != NULL) {
      union ast_expression *targets[] = { annotation->target };
      emit_assign_statement(s, 1, targets, annotation->value);
    }
    emit_annotation(s, annotation->target, annotation->annotation);
    return;
  }
  case AST_STATEMENT_ASSERT: {
    struct ast_statement_assert *assertion = &statement->assertion;
    emit_assert(s, assertion->expression, assertion->message);
    return;
  }
  case AST_STATEMENT_ASSIGN: {
    struct ast_statement_assign *assign = &statement->assign;
    emit_assign_statement(s, assign->num_targets, assign->targets,
                          assign->value);
    return;
  }
  case AST_STATEMENT_AUGASSIGN:
    emit_binexpr_assign_statement(s, statement->augassign.expression);
    return;
  case AST_STATEMENT_BREAK:
    emit_break_statement(s, statement->base.location);
    return;
  case AST_STATEMENT_CLASS: {
    struct ast_statement_class *class_stmt = &statement->class_stmt;
    analyze_class_bindings(s, class_stmt, /*parent=*/NULL);
    for (unsigned i = 0; i < class_stmt->num_decorators; ++i) {
      emit_expression(s, class_stmt->decorators[i]);
    }
    emit_class_begin(s, class_stmt->name);
    apply_class_bindings(s, class_stmt);
    emit_statement_list_with_function(s, class_stmt->body,
                                      /*current_function=*/NULL);
    emit_class_end(s, class_stmt->name, class_stmt->call,
                   class_stmt->num_decorators,
                   class_stmt->num_scope_freevars,
                   class_stmt->scope_freevars);
    return;
  }
  case AST_STATEMENT_CONTINUE:
    emit_continue_statement(s, statement->base.location);
    return;
  case AST_STATEMENT_DEF: {
    struct ast_statement_def *def_stmt = &statement->def_stmt;
    analyze_function_bindings(s, def_stmt, /*parent=*/NULL);
    for (unsigned i = 0; i < def_stmt->num_decorators; ++i) {
      emit_expression(s, def_stmt->decorators[i]);
    }
    struct make_function_state state;
    emit_def_begin(s, &state, def_stmt->num_parameters, def_stmt->parameters,
                   def_stmt->positional_only_argcount, def_stmt->return_type);
    apply_function_bindings(s, def_stmt);
    emit_statement_list_with_function(s, def_stmt->body, def_stmt);
    if (def_stmt->has_yield) {
      s->code.flags |= CO_GENERATOR;
    }
    emit_function_closure(s, &state, def_stmt);
    emit_def_end(s, &state, def_stmt->name, def_stmt->num_decorators,
                 def_stmt->async);
    return;
  }
  case AST_STATEMENT_DEL:
    emit_del(s, statement->del_stmt.targets);
    return;
  case AST_STATEMENT_EXPRESSION:
    emit_expression_statement(s, statement->expression_stmt.expression);
    return;
  case AST_STATEMENT_FOR: {
    struct ast_statement_for *for_stmt = &statement->for_stmt;
    struct for_while_state    state;
    emit_for_begin(s, &state, for_stmt->targets, for_stmt->expression);
    emit_statement_list_with_function(s, for_stmt->body, current_function);
    emit_for_else(s, &state);
    if (for_stmt->else_body != NULL) {
      emit_statement_list_with_function(s, for_stmt->else_body,
                                        current_function);
    }
    emit_for_end(s, &state);
    return;
  }
  case AST_STATEMENT_FROM_IMPORT: {
    struct ast_statement_from_import *from_import = &statement->from_import_stmt;
    if (from_import->import_star) {
      emit_from_import_star_statement(s, from_import->num_prefix_dots,
                                      from_import->module);
    } else {
      emit_from_import_statement(s, from_import->num_prefix_dots,
                                 from_import->module, from_import->num_items,
                                 from_import->items);
    }
    return;
  }
  case AST_STATEMENT_GLOBAL: {
    struct ast_statement_global *global = &statement->global_stmt;
    for (unsigned i = 0; i < global->num_names; ++i) {
      emit_global_statement_checked(s, statement->base.location,
                                    global->names[i]);
    }
    return;
  }
  case AST_STATEMENT_IF: {
    struct ast_statement_if *if_stmt = &statement->if_stmt;
    struct if_state          state;
    emit_if_begin(s, &state, if_stmt->condition);
    emit_statement_list_with_function(s, if_stmt->body, current_function);
    for (unsigned i = 0; i < if_stmt->num_elifs; ++i) {
      struct ast_if_elif *elif_stmt = &if_stmt->elifs[i];
      emit_if_elif(s, &state, elif_stmt->condition);
      emit_statement_list_with_function(s, elif_stmt->body, current_function);
    }
    if (if_stmt->else_body != NULL) {
      emit_if_else(s, &state);
      emit_statement_list_with_function(s, if_stmt->else_body,
                                        current_function);
    }
    emit_if_end(s, &state);
    return;
  }
  case AST_STATEMENT_IMPORT: {
    struct ast_statement_import *import_stmt = &statement->import_stmt;
    for (unsigned i = 0; i < import_stmt->num_items; ++i) {
      struct ast_import_item *item = &import_stmt->items[i];
      emit_import_statement(s, item->module, item->as);
    }
    return;
  }
  case AST_STATEMENT_NONLOCAL: {
    struct ast_statement_nonlocal *nonlocal = &statement->nonlocal_stmt;
    for (unsigned i = 0; i < nonlocal->num_names; ++i) {
      emit_nonlocal_statement_checked(s, statement->base.location,
                                      nonlocal->names[i]);
    }
    return;
  }
  case AST_STATEMENT_PASS:
    return;
  case AST_STATEMENT_RAISE: {
    struct ast_statement_raise *raise = &statement->raise_stmt;
    emit_raise_statement(s, raise->expression, raise->from);
    return;
  }
  case AST_STATEMENT_RETURN:
    emit_return_statement_checked(s, statement->base.location,
                                  statement->return_stmt.expression);
    return;
  case AST_STATEMENT_TRY: {
    struct ast_statement_try *try_stmt = &statement->try_stmt;
    struct try_state          state;
    emit_try_body_begin(s, &state);
    emit_statement_list_with_function(s, try_stmt->body, current_function);
    emit_try_body_end(s, &state);

    for (unsigned i = 0; i < try_stmt->num_excepts; ++i) {
      struct ast_try_except *except_stmt = &try_stmt->excepts[i];
      emit_try_except_begin(s, &state, except_stmt->match, except_stmt->as);
      emit_statement_list_with_function(s, except_stmt->body,
                                        current_function);
      emit_try_except_end(s, &state, except_stmt->as);
    }

    if (try_stmt->else_body != NULL) {
      emit_try_else_begin(s, &state);
      emit_statement_list_with_function(s, try_stmt->else_body,
                                        current_function);
      emit_try_else_end(s, &state);
    }

    if (try_stmt->finally_body != NULL) {
      emit_try_finally_begin(s, &state);
      emit_statement_list_with_function(s, try_stmt->finally_body,
                                        current_function);
      emit_try_finally_end(s, &state);
    }

    emit_try_end(s, &state);
    return;
  }
  case AST_STATEMENT_WHILE: {
    struct ast_statement_while *while_stmt = &statement->while_stmt;
    struct for_while_state      state;
    emit_while_begin(s, &state, while_stmt->condition);
    emit_statement_list_with_function(s, while_stmt->body, current_function);
    emit_while_else(s, &state);
    if (while_stmt->else_body != NULL) {
      emit_statement_list_with_function(s, while_stmt->else_body,
                                        current_function);
    }
    emit_while_end(s, &state);
    return;
  }
  case AST_STATEMENT_WITH: {
    struct ast_statement_with *with_stmt = &statement->with_stmt;
    unsigned                  num_items = with_stmt->num_items;
    struct with_state        *states = NULL;
    if (num_items > 0) {
      states = (struct with_state *)calloc(num_items, sizeof(*states));
      if (states == NULL) {
        internal_error("out of memory");
      }
    }

    for (unsigned i = 0; i < num_items; ++i) {
      struct ast_with_item *item = &with_stmt->items[i];
      emit_with_begin(s, &states[i], item->expression, item->targets);
    }
    emit_statement_list_with_function(s, with_stmt->body, current_function);
    for (unsigned i = num_items; i-- > 0;) {
      emit_with_end(s, &states[i]);
    }
    free(states);
    return;
  }
  case AST_STATEMENT_YIELD:
    emit_yield_statement(s, statement->yield_stmt.expression);
    return;
  case AST_STATEMENT_YIELD_FROM:
    emit_yield_from_statement(s, statement->yield_stmt.expression);
    return;
  default:
    internal_error("invalid statement");
  }
}

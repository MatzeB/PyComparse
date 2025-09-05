#include "codegen_statement.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

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
        struct basic_block *footer = cg_allocate_block(s);
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
    struct basic_block *op_true_block = cg_allocate_block(s);
    if (op_false_block == NULL) {
      op_false_block = cg_allocate_block(s);
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
    struct basic_block *middle_block = cg_allocate_block(s);
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
    struct basic_block *middle_block = cg_allocate_block(s);
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
    /* expression type not allowed for annotation */
    abort();
  }
}

void emit_assert(struct cg_state *s, union ast_expression *expression,
                 union ast_expression *message)
{
  if (unreachable(s)) return;

  struct basic_block *fail = cg_allocate_block(s);
  struct basic_block *continue_block = cg_allocate_block(s);

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

static void emit_parameter_variable(struct cg_state *s, struct symbol *name)
{
  assert(cg_symbol_info(s, name) == NULL);
  struct symbol_info *info = cg_new_symbol_info(s, name);
  info->type = SYMBOL_LOCAL;
  info->index = cg_append_varname(s, name);
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
    unimplemented(); /* TODO: unreachable def-statement... */
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

    emit_parameter_variable(s, name);
    if (i < keyword_only_idx) {
      ++s->code.argcount;
    } else {
      ++s->code.keyword_only_argcount;
    }
  }
  if (variable_arguments_name) {
    emit_parameter_variable(s, variable_arguments_name);
    s->code.flags |= CO_VARARGS;
  }
  if (variable_keyword_arguments_name != NULL) {
    emit_parameter_variable(s, variable_keyword_arguments_name);
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
  cg_op_pop_push(s, OPCODE_MAKE_FUNCTION, flags,
                 /*pop=*/operands, /*push=*/1);
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

  struct basic_block *setup_finally = cg_allocate_block(s);
  struct basic_block *setup_except = cg_allocate_block(s);
  state->setup_finally = setup_finally;
  state->setup_except = setup_except;

  cg_jump(s, setup_finally);
  cg_block_insert_delayed(s, setup_finally);
  cg_block_insert_delayed(s, setup_except);

  struct basic_block *body = cg_allocate_block(s);
  cg_block_begin(s, body);
}

void emit_try_body_end(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;
  if (unreachable(s)) return;

  /* if there was an except, then we need to insert a POP_BLOCK here */
  struct basic_block *body_exit = cg_allocate_block(s);
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
    excepts = cg_allocate_block(s);

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
    struct basic_block *excepts_next = cg_allocate_block(s);
    struct basic_block *except_match = cg_allocate_block(s);
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
    struct basic_block *except_unassign_as = cg_allocate_block(s);
    struct basic_block *except_body = cg_allocate_block(s);
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
    enter_finally = cg_allocate_block(s);
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
    else_block = cg_allocate_block(s);
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
      enter_finally = cg_allocate_block(s);
      state->enter_finally = enter_finally;
    }
    cg_jump(s, enter_finally);
  }
}

void emit_try_finally_begin(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;
  state->had_finally = true;

  struct basic_block *finally_body = cg_allocate_block(s);

  struct basic_block *setup_finally = state->setup_finally;
  cg_block_begin_delayed(s, setup_finally);
  cg_condjump(s, OPCODE_SETUP_FINALLY, /*target=*/finally_body,
              /*fallthrough=*/setup_finally->next);
  cg_push(s, 1);

  struct basic_block *enter_finally = state->enter_finally;
  struct basic_block *excepts = state->excepts;
  if (enter_finally == NULL && (state->body_exit != NULL || excepts != NULL)) {
    enter_finally = cg_allocate_block(s);
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
    footer = cg_allocate_block(s);
    state->footer = footer;
  }
  cg_jump(s, footer);
}

void emit_try_end(struct cg_state *s, struct try_state *state)
{
  if (!state->try_reachable) return;

  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_allocate_block(s);
    state->footer = footer;
  }

  struct basic_block *enter_finally = state->enter_finally;

  if (state->had_except) {
    struct basic_block *excepts = state->excepts;
    if (excepts != NULL) {
      if (footer == NULL) {
        footer = cg_allocate_block(s);
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

  struct basic_block *then_block = cg_allocate_block(s);
  struct basic_block *else_or_footer = cg_allocate_block(s);
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
    footer = cg_allocate_block(s);
    state->footer = footer;
  }
  if (!unreachable(s)) {
    cg_jump(s, footer);
  }

  struct basic_block *new_else = cg_allocate_block(s);
  struct basic_block *then_block = cg_allocate_block(s);
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
    footer = cg_allocate_block(s);
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
  struct basic_block *header = cg_allocate_block(s);
  struct basic_block *footer = cg_allocate_block(s);
  struct basic_block *else_block = cg_allocate_block(s);
  struct basic_block *body = cg_allocate_block(s);

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
  struct basic_block *header = cg_allocate_block(s);
  struct basic_block *body = cg_allocate_block(s);
  struct basic_block *else_block = cg_allocate_block(s);
  struct basic_block *footer = cg_allocate_block(s);

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
                    struct ast_call *call, unsigned num_decorators)
{
  emit_code_end(s);
  union object *code = cg_pop_code(s, symbol->string);
  cg_load_const(s, code);

  union object *string = object_intern_cstring(&s->objects, symbol->string);
  cg_load_const(s, string);
  cg_op_pop1(s, OPCODE_MAKE_FUNCTION, 0);
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
                  struct symbol *symbol, unsigned num_decorators)
{
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
    abort();
  }
}

static void emit_generator_expression_part(
    struct cg_state *s, struct ast_generator_expression *generator_expression,
    unsigned part_index)
{
  if (part_index >= generator_expression->num_parts) {
    emit_expression(s, generator_expression->expression);
    if (generator_expression->base.type == AST_LIST_COMPREHENSION) {
      /* Need to compute position of list in stack...
       * using "number of for" + 1 for now but this feels sketchy... */
      unsigned num_for = 0;
      unsigned num_parts = generator_expression->num_parts;
      for (unsigned i = 0; i < num_parts; i++) {
        if (generator_expression->parts[i].type
            == GENERATOR_EXPRESSION_PART_FOR) {
          num_for++;
        }
      }
      cg_op_pop1(s, OPCODE_LIST_APPEND, num_for + 1);
    } else {
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
  struct basic_block *false_block = cg_allocate_block(s);

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
  emit_parameter_variable(s,
                          symbol_table_get_or_insert(s->symbol_table, ".0"));
  s->code.argcount = 1;

  enum ast_expression_type type = generator_expression->base.type;
  assert(type == AST_GENERATOR_EXPRESSION || type == AST_LIST_COMPREHENSION);
  if (type == AST_LIST_COMPREHENSION) {
    cg_op_pop_push(s, OPCODE_BUILD_LIST, 0, /*pop=*/0, /*push=*/1);
  }

  struct generator_expression_part *part = generator_expression->parts;
  cg_op_push1(s, OPCODE_LOAD_FAST, 0);
  struct for_while_state state;
  emit_for_begin_impl(s, &state, part->targets);

  emit_generator_expression_part(s, generator_expression, /*part_index=*/1);

  emit_for_else(s, &state);
  emit_for_end(s, &state);

  if (type == AST_LIST_COMPREHENSION) {
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

  struct basic_block *cleanup = cg_allocate_block(s);
  struct basic_block *body = cg_allocate_block(s);
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

#include "pycomparse/codegen_statement.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pycomparse/adt/idynarray.h"
#include "pycomparse/ast.h"
#include "pycomparse/ast_expression_types.h"
#include "pycomparse/ast_statement_types.h"
#include "pycomparse/ast_types.h"
#include "pycomparse/ast_unparse.h"
#include "pycomparse/codegen.h"
#include "pycomparse/codegen_expression.h"
#include "pycomparse/codegen_types.h"
#include "pycomparse/diagnostics.h"
#include "pycomparse/object.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/opcodes.h"
#include "pycomparse/symbol_info_types.h"
#include "pycomparse/symbol_table.h"
#include "pycomparse/symbol_types.h"
#include "pycomparse/util.h"

struct try_state {
  struct basic_block *nullable excepts;
  struct basic_block *nullable except_next;
  struct basic_block *nullable except_unassign_as;
  struct basic_block *nullable try_else_block;
  struct basic_block *nullable enter_finally;
  struct basic_block *nullable finally_block;
  struct basic_block *nullable footer;
};

struct for_while_state {
  struct basic_block           *else_or_footer;
  struct loop_state             saved;
  union scope_cleanup *nullable loop_iter_cleanup;
  bool                          has_loop_iter_cleanup;
  bool                          async_for;
};

struct if_state {
  struct basic_block *nullable else_or_footer;
  struct basic_block *nullable footer;
};

enum scope_cleanup_kind {
  SCOPE_CLEANUP_LOOP_ITER,
  SCOPE_CLEANUP_POP_BLOCK,
  SCOPE_CLEANUP_POP_EXCEPT,
  SCOPE_CLEANUP_FINALLY,
  SCOPE_CLEANUP_WITHIN_FINALLY,
  SCOPE_CLEANUP_EXCEPT_AS,
  SCOPE_CLEANUP_WITH,
};

struct scope_cleanup_base {
  enum scope_cleanup_kind       kind;
  union scope_cleanup *nullable prev;
};

struct except_as_scope_cleanup {
  struct scope_cleanup_base base;
  struct symbol *nullable   as_symbol;
};

struct finally_scope_cleanup {
  struct scope_cleanup_base    base;
  bool                         finally_needs_placeholder;
  struct basic_block *nullable finally_block;
};

struct with_scope_cleanup {
  struct scope_cleanup_base base;
  bool                      async;
  struct basic_block       *block;
};

union scope_cleanup {
  enum scope_cleanup_kind   kind;
  struct scope_cleanup_base base;

  struct except_as_scope_cleanup except_as;
  struct finally_scope_cleanup   finally;
  struct with_scope_cleanup      with;
};

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

static void emit_scope_cleanup(struct cg_state              *s,
                               union scope_cleanup *nullable stop,
                               bool value_passthrough);

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

void emit_code_end(struct cg_state *s)
{
  if (cg_unreachable(s)) return;
  cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
  cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);
}

static union object *nullable
statement_leading_docstring(union ast_statement *statement)
{
  if (ast_statement_type(statement) != AST_STATEMENT_EXPRESSION) return NULL;
  union ast_expression *expression = statement->expression.expression;
  if (ast_expression_type(expression) != AST_CONST) return NULL;
  union object *doc = expression->cnst.object;
  if (object_type(doc) != OBJECT_STRING) return NULL;
  return doc;
}

static union object *nullable
statement_list_leading_docstring(struct ast_statement_list *statement_list)
{
  if (statement_list->num_statements == 0) return NULL;
  return statement_leading_docstring(statement_list->statements[0]);
}

static bool statement_list_has_scope_annotation(
    const struct ast_statement_list *nullable statements);
static void emit_statement_list(struct cg_state           *s,
                                struct ast_statement_list *statement_list);
static void
emit_statement_list_skip_first(struct cg_state           *s,
                               struct ast_statement_list *statement_list);
static void
analyze_and_apply_toplevel_function_bindings(struct cg_state   *s,
                                             struct ast_module *module);

static void
emit_nonfunction_statement_list(struct cg_state           *s,
                                struct ast_statement_list *statement_list)
{
  if (statement_list_has_scope_annotation(statement_list)) {
    cg_op(s, OPCODE_SETUP_ANNOTATIONS, 0);
  }

  union object *doc = statement_list_leading_docstring(statement_list);
  if (doc != NULL) {
    if (!s->optimize_no_docstrings) {
      union ast_statement *first = statement_list->statements[0];
      cg_set_lineno(s, first->base.location.line);
      cg_load_const(s, doc);
      cg_store(s, symbol_table_get_or_insert(s->symbol_table, "__doc__"));
    }
    emit_statement_list_skip_first(s, statement_list);
  } else {
    emit_statement_list(s, statement_list);
  }
}

union object *emit_module(struct cg_state *s, struct ast_module *module)
{
  bool toplevel_function = s->toplevel_function;
  cg_code_begin(s, toplevel_function);
  s->code.flags |= module->future_flags;
  if (toplevel_function) {
    analyze_and_apply_toplevel_function_bindings(s, module);
    union object *doc = statement_list_leading_docstring(module->body);
    cg_set_function_docstring(s, s->optimize_no_docstrings ? NULL : doc);
    if (doc != NULL) {
      emit_statement_list_skip_first(s, module->body);
    } else {
      emit_statement_list(s, module->body);
    }
  } else {
    emit_nonfunction_statement_list(s, module->body);
  }

  emit_code_end(s);
  return cg_code_end(s, "<module>");
}

static void emit_annotation(struct cg_state *s, union ast_expression *target,
                            union ast_expression *annotation, bool simple)
{
  if (cg_unreachable(s)) return;

  switch (ast_expression_type(target)) {
  case AST_IDENTIFIER:
    if (cg_in_function(s)) return;
    if (!simple) {
      emit_expression(s, annotation);
      cg_op_pop1(s, OPCODE_POP_TOP, 0);
      return;
    }
    bool future_annotations = (s->code.flags & CO_FUTURE_ANNOTATIONS) != 0;
    if (future_annotations) {
      union object *annotation_string
          = ast_unparse_expression(s->objects, annotation);
      cg_load_const(s, annotation_string);
    } else {
      emit_expression(s, annotation);
    }
    unsigned annotations_idx = cg_register_name(
        s, symbol_table_get_or_insert(s->symbol_table, "__annotations__"));
    cg_op_push1(s, OPCODE_LOAD_NAME, annotations_idx);
    union object *name
        = object_intern_cstring(s->objects, target->identifier.symbol->string);
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

static void emit_assert(struct cg_state *s, union ast_expression *expression,
                        union ast_expression *message)
{
  if (s->optimize_no_assertions) return;
  if (cg_unreachable(s)) return;

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

static void emit_assign(struct cg_state *s, unsigned num_targets,
                        union ast_expression **targets,
                        union ast_expression  *value)
{
  if (cg_unreachable(s)) return;
  emit_expression(s, value);
  for (unsigned i = 0; i < num_targets; i++) {
    union ast_expression *target = targets[i];
    if (i < num_targets - 1) {
      cg_op_push1(s, OPCODE_DUP_TOP, 0);
    }
    emit_assignment(s, target);
  }
}

static unsigned register_dotted_name(struct cg_state    *s,
                                     struct dotted_name *name)
{
  unsigned num_symbols = name->num_symbols;
  unsigned length = num_symbols;
  for (unsigned i = 0; i < num_symbols; i++) {
    length += strlen(name->symbols[i]->string);
  }
  struct arena *arena = object_intern_arena(s->objects);
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

static void emit_from_import(struct cg_state *s, unsigned num_prefix_dots,
                             struct dotted_name *module, unsigned num_pairs,
                             struct from_import_item *items)
{
  if (cg_unreachable(s)) return;
  unsigned module_name = module != NULL ? register_dotted_name(s, module)
                                        : cg_register_name_from_cstring(s, "");
  bool     import_star = (num_pairs == 0);

  struct tuple_prep *name_tuple_prep;
  if (import_star) {
    union object *name = object_intern_cstring(s->objects, "*");
    name_tuple_prep = object_intern_tuple_begin(s->objects, 1);
    object_new_tuple_set_at(name_tuple_prep, 0, name);
  } else {
    name_tuple_prep = object_intern_tuple_begin(s->objects, num_pairs);
    for (unsigned i = 0; i < num_pairs; i++) {
      struct from_import_item *item = &items[i];
      union object            *name
          = object_intern_cstring(s->objects, item->name->string);
      object_new_tuple_set_at(name_tuple_prep, i, name);
    }
  }
  union object *name_tuple
      = object_intern_tuple_end(s->objects, name_tuple_prep,
                                /*may_free_arena=*/false);

  cg_load_const(s, object_intern_int(s->objects, num_prefix_dots));
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

static void emit_import(struct cg_state *s, struct dotted_name *module,
                        struct symbol *as)
{
  if (cg_unreachable(s)) return;
  unsigned      module_name = register_dotted_name(s, module);
  union object *object = object_intern_int(s->objects, 0);
  cg_load_const(s, object);
  cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
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

static void
emit_parameter_defaults(struct cg_state *s, struct make_function_state *state,
                        const struct parameter_shape *parameter_shape,
                        struct parameter             *parameters)
{
  /* create tuple with default values */
  unsigned num_parameters = parameter_shape->num_parameters;
  unsigned num_positional_defaults = 0;
  unsigned num_keyword_defaults = 0;
  unsigned keyword_parameters_begin = parameter_shape->keyword_only_begin;
  if (keyword_parameters_begin > num_parameters) {
    keyword_parameters_begin = num_parameters;
  }
  for (unsigned i = 0; i < num_parameters; i++) {
    struct parameter     *parameter = &parameters[i];
    union ast_expression *initializer = parameter->initializer;
    if (initializer == NULL) continue;

    if (i < keyword_parameters_begin) {
      ++num_positional_defaults;
    } else {
      ++num_keyword_defaults;
    }
  }

  if (num_positional_defaults > 0) {
    for (unsigned i = 0; i < keyword_parameters_begin; i++) {
      struct parameter     *parameter = &parameters[i];
      union ast_expression *initializer = parameter->initializer;
      if (initializer == NULL) continue;
      emit_expression(s, initializer);
    }
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE, num_positional_defaults,
                   /*pop=*/num_positional_defaults, /*push=*/1);
    state->defaults = true;
  }
  if (num_keyword_defaults > 0) {
    struct tuple_prep *names_prep
        = object_intern_tuple_begin(s->objects, num_keyword_defaults);
    unsigned name_idx = 0;
    for (unsigned i = keyword_parameters_begin; i < num_parameters; i++) {
      struct parameter     *parameter = &parameters[i];
      union ast_expression *initializer = parameter->initializer;
      if (initializer == NULL) continue;
      emit_expression(s, initializer);
      union object *name
          = object_intern_cstring(s->objects, parameter->name->string);
      object_new_tuple_set_at(names_prep, name_idx++, name);
    }
    assert(name_idx == num_keyword_defaults);
    union object *names = object_intern_tuple_end(s->objects, names_prep,
                                                  /*may_free_arena=*/false);
    cg_load_const(s, names);
    cg_op_pop_push(s, OPCODE_BUILD_CONST_KEY_MAP, num_keyword_defaults,
                   /*pop=*/num_keyword_defaults + 1, /*push=*/1);
    state->keyword_defaults = true;
  }
}

static void emit_function_annotations(
    struct cg_state *s, struct make_function_state *state,
    const struct parameter_shape *parameter_shape,
    struct parameter *parameters, union ast_expression *nullable return_type)
{
  /* TODO: parameters */
  unsigned num_parameters = parameter_shape->num_parameters;
  unsigned num_annotation_items = 0;
  if (return_type != NULL) num_annotation_items++;
  for (unsigned i = 0; i < num_parameters; ++i) {
    struct parameter *parameter = &parameters[i];
    num_annotation_items += (parameter->type != NULL);
  }

  if (num_annotation_items == 0) return;

  state->annotations = true;
  bool future_annotations = (s->code.flags & CO_FUTURE_ANNOTATIONS) != 0;
  struct tuple_prep *names_prep
      = object_intern_tuple_begin(s->objects, num_annotation_items);
  unsigned names_idx = 0;

  for (unsigned i = 0; i < num_parameters; ++i) {
    struct parameter     *parameter = &parameters[i];
    union ast_expression *type = parameter->type;
    if (type == NULL) continue;
    if (future_annotations) {
      cg_load_const(s, ast_unparse_expression(s->objects, type));
    } else {
      emit_expression(s, type);
    }
    union object *name
        = object_intern_cstring(s->objects, parameter->name->string);
    object_new_tuple_set_at(names_prep, names_idx++, name);
  }
  if (return_type != NULL) {
    if (future_annotations) {
      cg_load_const(s, ast_unparse_expression(s->objects, return_type));
    } else {
      emit_expression(s, return_type);
    }
    union object *name = object_intern_cstring(s->objects, "return");
    object_new_tuple_set_at(names_prep, names_idx++, name);
  }

  union object *names = object_intern_tuple_end(s->objects, names_prep,
                                                /*may_free_arena=*/false);
  cg_load_const(s, names);
  cg_op_pop_push(s, OPCODE_BUILD_CONST_KEY_MAP, num_annotation_items,
                 /*pop=*/num_annotation_items + 1, /*push=*/1);
}

void emit_make_function_begin(struct cg_state               *s,
                              struct make_function_state    *state,
                              const struct parameter_shape  *parameter_shape,
                              struct parameter              *parameters,
                              bool                           async_function,
                              union ast_expression *nullable return_type,
                              const char *name, bool global_binding)
{
  unsigned num_parameters = parameter_shape->num_parameters;
  memset(state, 0, sizeof(*state));

  emit_parameter_defaults(s, state, parameter_shape, parameters);
  emit_function_annotations(s, state, parameter_shape, parameters,
                            return_type);

  const char *qualname = global_binding ? name : cg_build_qualname(s, name);
  state->qualname = qualname;

  cg_push_code(s);
  cg_code_begin(s, /*in_function=*/true);
  s->code.in_async_function = async_function;

  /* Set child's qualname prefix for nested scopes: qualname + ".<locals>." */
  {
    size_t        qlen = strlen(qualname);
    struct arena *arena = object_intern_arena(s->objects);
    char         *prefix = arena_allocate(arena, qlen + 10 + 1, 1);
    memcpy(prefix, qualname, qlen);
    memcpy(prefix + qlen, ".<locals>.", 11);
    s->code.qualname_prefix = prefix;
  }

  unsigned keyword_only_idx = parameter_shape->keyword_only_begin;
  if (keyword_only_idx > num_parameters) {
    keyword_only_idx = num_parameters;
  }
  struct symbol *variable_arguments_name = NULL;
  struct symbol *variable_keyword_arguments_name = NULL;
  for (unsigned i = 0; i < num_parameters; i++) {
    struct parameter *parameter = &parameters[i];

    struct symbol *name = parameter->name;
    if (parameter->variant == PARAMETER_STAR) {
      assert(variable_arguments_name == NULL);
      variable_arguments_name = name;
      continue;
    }
    if (parameter->variant == PARAMETER_STAR_STAR) {
      assert(variable_keyword_arguments_name == NULL);
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
  s->code.positional_only_argcount = parameter_shape->positional_only_argcount;
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
      cg_op_push1(s, OPCODE_LOAD_CLOSURE, cg_closure_index(s, closure_symbol));
    }
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE, state->num_closure_symbols,
                   /*pop=*/state->num_closure_symbols, /*push=*/1);
    state->closure = true;
  }

  cg_load_const(s, code);
  cg_load_const(s, object_intern_cstring(s->objects, state->qualname));
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

static void emit_except_begin(struct cg_state *s, struct try_state *state,
                              struct location       location,
                              union ast_expression *match, struct symbol *as)
{
  cg_set_lineno(s, location.line);
  cg_push(s, 3); /* runtime pushes traceback, value, type when entering */

  struct basic_block *except_next = NULL;
  if (match != NULL) {
    except_next = cg_block_allocate(s);
    struct basic_block *except_match = cg_block_allocate(s);
    cg_op_push1(s, OPCODE_DUP_TOP, 0);
    emit_expression(s, match);
    cg_op_pop_push(s, OPCODE_COMPARE_OP, COMPARE_OP_EXC_MATCH,
                   /*pop=*/2, /*push=*/1);
    cg_pop(s, 1);
    cg_condjump(s, OPCODE_POP_JUMP_IF_FALSE,
                /*target=*/except_next, /*fallthrough=*/except_match);
    cg_block_begin(s, except_match);
  }
  state->except_next = except_next;

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

static void emit_except_end(struct cg_state *s, struct try_state *state,
                            struct symbol *as)
{
  struct basic_block *except_unassign_as = state->except_unassign_as;

  if (except_unassign_as != NULL) {
    assert(as != NULL);
    if (!cg_unreachable(s)) {
      cg_op(s, OPCODE_POP_BLOCK, 0);
      cg_op_pop_push(s, OPCODE_BEGIN_FINALLY, 0, /*pop=*/0, /*push=*/6);
      cg_jump(s, except_unassign_as);
    } else {
      cg_push(s, 6);
    }

    cg_block_begin(s, except_unassign_as);
    cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
    cg_store(s, as);
    cg_delete(s, as);
    cg_op_pop_push(s, OPCODE_END_FINALLY, 0, /*pop=*/6, /*push=*/0);
    state->except_unassign_as = NULL;
  } else {
    if (cg_unreachable(s)) {
      struct basic_block *except_next = state->except_next;
      if (except_next != NULL) {
        cg_block_begin(s, except_next);
      }
      return;
    }
  }
  cg_op(s, OPCODE_POP_EXCEPT, 0);

  if (state->finally_block != NULL) {
    struct basic_block *enter_finally = state->enter_finally;
    if (enter_finally == NULL) {
      enter_finally = cg_block_allocate(s);
      state->enter_finally = enter_finally;
    }
    cg_jump(s, enter_finally);
  } else {
    struct basic_block *footer = state->footer;
    if (footer == NULL) {
      footer = cg_block_allocate(s);
      state->footer = footer;
    }
    cg_jump(s, footer);
  }

  struct basic_block *except_next = state->except_next;
  if (except_next != NULL) {
    cg_block_begin(s, except_next);
  }
}

static void emit_if_begin(struct cg_state *s, struct if_state *state,
                          union ast_expression *expression)
{
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

static void emit_if_elif(struct cg_state *s, struct if_state *state,
                         union ast_expression *expression)
{
  struct basic_block *else_block = state->else_or_footer;
  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_block_allocate(s);
    state->footer = footer;
  }
  if (!cg_unreachable(s)) {
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

static void emit_if_else(struct cg_state *s, struct if_state *state)
{
  struct basic_block *else_block = state->else_or_footer;
  struct basic_block *footer = state->footer;
  if (footer == NULL) {
    footer = cg_block_allocate(s);
    state->footer = footer;
  }
  state->else_or_footer = NULL;
  if (!cg_unreachable(s)) {
    cg_jump(s, footer);
  }

  cg_block_begin(s, else_block);
}

static void emit_if_end(struct cg_state *s, struct if_state *state)
{
  struct basic_block *footer = state->footer;
  struct basic_block *else_or_footer = state->else_or_footer;
  if (footer == NULL) footer = else_or_footer;
  if (!cg_unreachable(s)) {
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
  state->has_loop_iter_cleanup = true;
  state->async_for = false;
  s->code.loop_state = (struct loop_state){
    .continue_block = header,
    .break_block = footer,
    .scope_cleanup_at_loop = s->code.scope_cleanup,
    .pop_on_break = true,
  };

  struct scope_cleanup_base *loop_cleanup = arena_allocate_type(
      object_intern_arena(s->objects), struct scope_cleanup_base);
  *loop_cleanup = (struct scope_cleanup_base){
    .kind = SCOPE_CLEANUP_LOOP_ITER,
    .prev = s->code.scope_cleanup,
  };
  state->loop_iter_cleanup = (union scope_cleanup *)loop_cleanup;
  s->code.scope_cleanup = (union scope_cleanup *)loop_cleanup;
  s->code.loop_state.scope_cleanup_at_loop
      = (union scope_cleanup *)loop_cleanup;
}

static void emit_for_begin_async(struct cg_state        *s,
                                 struct for_while_state *state,
                                 union ast_expression   *targets,
                                 union ast_expression   *expression)
{
  emit_expression(s, expression);
  cg_op(s, OPCODE_GET_AITER, 0);

  struct basic_block *header = cg_block_allocate(s);
  struct basic_block *footer = cg_block_allocate(s);
  struct basic_block *else_block = cg_block_allocate(s);
  struct basic_block *body = cg_block_allocate(s);

  cg_jump(s, header);
  cg_block_begin(s, header);
  cg_condjump(s, OPCODE_SETUP_FINALLY, else_block, body);

  cg_block_begin(s, body);
  cg_op(s, OPCODE_GET_ANEXT, 0);
  cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
  cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
  cg_op(s, OPCODE_POP_BLOCK, 0);
  emit_assignment(s, targets);

  state->else_or_footer = else_block;
  state->saved = s->code.loop_state;
  state->has_loop_iter_cleanup = false;
  state->loop_iter_cleanup = NULL;
  state->async_for = true;
  s->code.loop_state = (struct loop_state){
    .continue_block = header,
    .break_block = footer,
    .scope_cleanup_at_loop = s->code.scope_cleanup,
    .pop_on_break = true,
  };
}

static void emit_for_begin(struct cg_state *s, struct for_while_state *state,
                           union ast_expression *targets,
                           union ast_expression *expression, bool async)
{
  if (async) {
    emit_for_begin_async(s, state, targets, expression);
  } else {
    emit_expression(s, expression);
    cg_op(s, OPCODE_GET_ITER, 0);
    emit_for_begin_impl(s, state, targets);
  }
}

static void emit_loop_else(struct cg_state *s, struct for_while_state *state)
{
  struct basic_block *else_block = state->else_or_footer;
  if (else_block == NULL) return;

  if (state->has_loop_iter_cleanup) {
    union scope_cleanup *loop_cleanup = state->loop_iter_cleanup;
    if (loop_cleanup != NULL && s->code.scope_cleanup == loop_cleanup) {
      s->code.scope_cleanup = loop_cleanup->base.prev;
    } else {
      /* Unwinding should have restored the loop marker as the current head. */
      s->code.scope_cleanup = state->saved.scope_cleanup_at_loop;
    }
  }

  struct basic_block *footer = s->code.loop_state.break_block;
  struct basic_block *header = s->code.loop_state.continue_block;
  s->code.loop_state = state->saved;
  if (!cg_unreachable(s)) {
    cg_jump(s, header);
  }

  cg_block_begin(s, else_block);
  state->else_or_footer = footer;
}

static void emit_loop_end(struct cg_state *s, struct for_while_state *state)
{
  struct basic_block *footer = state->else_or_footer;
  if (footer == NULL) return;

  if (!cg_unreachable(s)) {
    cg_jump(s, footer);
  }
  cg_block_begin(s, footer);
}

static void emit_for_else(struct cg_state *s, struct for_while_state *state)
{
  emit_loop_else(s, state);
  if (state->async_for) {
    cg_op(s, OPCODE_END_ASYNC_FOR, 0);
  } else {
    cg_pop(s, 1);
  }
}

static void emit_for_end(struct cg_state *s, struct for_while_state *state)
{
  emit_loop_end(s, state);
}

static void emit_continue(struct cg_state *s)
{
  struct basic_block *target = s->code.loop_state.continue_block;
  assert(target != NULL);
  unsigned saved_stacksize = s->code.stacksize;
  emit_scope_cleanup(s, s->code.loop_state.scope_cleanup_at_loop,
                     /*value_passthrough=*/false);
  if (!cg_unreachable(s)) {
    cg_jump(s, target);
  }
  s->code.stacksize = saved_stacksize;
}

static void emit_break(struct cg_state *s)
{
  struct basic_block *target = s->code.loop_state.break_block;
  assert(target != NULL);
  unsigned saved_stacksize = s->code.stacksize;
  emit_scope_cleanup(s, s->code.loop_state.scope_cleanup_at_loop,
                     /*value_passthrough=*/false);
  if (!cg_unreachable(s)) {
    if (s->code.loop_state.pop_on_break) {
      cg_op(s, OPCODE_POP_TOP, 0);
    }
    cg_jump(s, target);
  }
  s->code.stacksize = saved_stacksize;
}

static void emit_decorator_calls(struct cg_state *s, unsigned num_decorators)
{
  for (unsigned i = 0; i < num_decorators; i++) {
    cg_op_pop1(s, OPCODE_CALL_FUNCTION, 1);
  }
}

static void emit_del(struct cg_state *s, union ast_expression *targets)
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
    enum ast_expression_type type
        = ast_expression_type((union ast_expression *)generator_expression);
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
    emit_for_begin(s, &state, targets, part->expression, part->async);

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
  enum ast_expression_type type
      = ast_expression_type((union ast_expression *)generator_expression);
  bool return_value;
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
  if (part->async) {
    state.has_loop_iter_cleanup = false;
    state.loop_iter_cleanup = NULL;
    state.async_for = true;
    struct basic_block *header = cg_block_allocate(s);
    struct basic_block *footer = cg_block_allocate(s);
    struct basic_block *else_block = cg_block_allocate(s);
    struct basic_block *body = cg_block_allocate(s);

    cg_jump(s, header);
    cg_block_begin(s, header);
    cg_condjump(s, OPCODE_SETUP_FINALLY, else_block, body);

    cg_block_begin(s, body);
    cg_op(s, OPCODE_GET_ANEXT, 0);
    cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
    cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
    cg_op(s, OPCODE_POP_BLOCK, 0);
    emit_assignment(s, part->targets);

    state.else_or_footer = else_block;
    state.saved = s->code.loop_state;
    s->code.loop_state = (struct loop_state){
      .continue_block = header,
      .break_block = footer,
      .scope_cleanup_at_loop = s->code.scope_cleanup,
      .pop_on_break = true,
    };
  } else {
    emit_for_begin_impl(s, &state, part->targets);
  }

  emit_generator_expression_part(s, generator_expression, /*part_index=*/1);

  emit_for_else(s, &state);
  emit_for_end(s, &state);

  if (return_value) {
    cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
    cg_block_end(s);
  }
}

struct binding_scope {
  struct binding_scope *nullable parent;
  struct ast_def *nullable       def;
  struct cg_state               *cg;
  struct symbol                 *class_symbol;
  bool                           is_class;
  bool                           is_comprehension;
  bool                           class_needs_class_cell;

  struct symbol                   *locals_inline[16];
  struct symbol                   *bound_before_decl_inline[16];
  struct symbol                   *globals_inline[8];
  struct symbol                   *nonlocals_inline[8];
  struct symbol                   *freevars_inline[8];
  struct symbol                   *cellvars_inline[8];
  struct symbol                   *uses_inline[16];
  struct ast_def                  *children_inline[8];
  struct ast_class                *class_children_inline[8];
  struct ast_lambda               *lambda_children_inline[8];
  struct ast_generator_expression *generator_children_inline[8];

  struct idynarray locals;
  struct idynarray bound_before_decl;
  struct idynarray globals;
  struct idynarray nonlocals;
  struct idynarray freevars;
  struct idynarray cellvars;
  struct idynarray uses;
  struct idynarray children;
  struct idynarray class_children;
  struct idynarray lambda_children;
  struct idynarray generator_children;
};

static void emit_statement(struct cg_state *s, union ast_statement *statement);

static bool symbol_array_contains(struct idynarray *array,
                                  struct symbol    *symbol)
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

static void symbol_array_remove_at_swap(struct idynarray *array,
                                        unsigned          index)
{
  unsigned        num_symbols = idynarray_length(array, struct symbol *);
  struct symbol **symbols = idynarray_data(array);
  assert(index < num_symbols);
  symbols[index] = symbols[num_symbols - 1];
  array->size -= sizeof(struct symbol *);
  idynarray_refresh_poisoning_(array);
}

static bool is_dunder_class(struct symbol *symbol)
{
  return strcmp(symbol->string, "__class__") == 0;
}

static bool class_explicitly_binds_class_symbol(struct binding_scope *scope)
{
  return symbol_array_contains(&scope->globals, scope->class_symbol)
         || symbol_array_contains(&scope->nonlocals, scope->class_symbol);
}

static void emit_scope_cleanup(struct cg_state              *s,
                               union scope_cleanup *nullable stop,
                               bool                          value_passthrough)
{
  union scope_cleanup *saved_cleanup = s->code.scope_cleanup;
  for (union scope_cleanup *cleanup = saved_cleanup;
       cleanup != NULL && cleanup != stop; cleanup = cleanup->base.prev) {
    s->code.scope_cleanup = cleanup->base.prev;
    switch (cleanup->kind) {
    case SCOPE_CLEANUP_LOOP_ITER:
      if (value_passthrough) {
        cg_op(s, OPCODE_ROT_TWO, 0);
      }
      cg_op_pop1(s, OPCODE_POP_TOP, 0);
      break;
    case SCOPE_CLEANUP_POP_BLOCK:
      cg_op(s, OPCODE_POP_BLOCK, 0);
      break;
    case SCOPE_CLEANUP_POP_EXCEPT:
      if (value_passthrough) cg_op(s, OPCODE_ROT_FOUR, 0);
      cg_op_pop_push(s, OPCODE_POP_EXCEPT, 0, /*pop=*/3, /*push=*/0);
      break;
    case SCOPE_CLEANUP_FINALLY:
      cg_op(s, OPCODE_POP_BLOCK, 0);
      struct basic_block *finally_block = cleanup->finally.finally_block;
      assert(finally_block != NULL);
      if (value_passthrough && cleanup->finally.finally_needs_placeholder) {
        /* Preserve the return value across the finally call by removing the
         * pre-SETUP_FINALLY placeholder before CALL_FINALLY runs. */
        cg_op(s, OPCODE_ROT_TWO, 0);
        cg_op_pop1(s, OPCODE_POP_TOP, 0);
      }
      struct basic_block *after_call_finally = cg_block_allocate(s);
      cg_condjump(s, OPCODE_CALL_FINALLY, finally_block,
                  /*fallthrough=*/after_call_finally);
      cg_block_begin(s, after_call_finally);
      if (value_passthrough && cleanup->finally.finally_needs_placeholder) {
        /* CALL_FINALLY returns with an extra token above/below the preserved
         * value; rotate and discard it before continuing the return path. */
        cg_op(s, OPCODE_ROT_TWO, 0);
        cg_op_pop1(s, OPCODE_POP_TOP, 0);
      } else if (cleanup->finally.finally_needs_placeholder) {
        cg_op_pop1(s, OPCODE_POP_TOP, 0);
      }
      break;
    case SCOPE_CLEANUP_WITHIN_FINALLY:
      cg_op_pop_push(s, OPCODE_POP_FINALLY, value_passthrough ? 1 : 0,
                     /*pop=*/6, /*push=*/0);
      if (value_passthrough) cg_op(s, OPCODE_ROT_TWO, 0);
      cg_op_pop1(s, OPCODE_POP_TOP, 0);
      break;
    case SCOPE_CLEANUP_EXCEPT_AS:
      cg_op(s, OPCODE_POP_BLOCK, 0);
      cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
      cg_store(s, cleanup->except_as.as_symbol);
      cg_delete(s, cleanup->except_as.as_symbol);
      break;
    case SCOPE_CLEANUP_WITH:
      cg_op(s, OPCODE_POP_BLOCK, 0);
      if (value_passthrough) cg_op(s, OPCODE_ROT_TWO, 0);
      cg_op_pop_push(s, OPCODE_BEGIN_FINALLY, 0, /*pop=*/0, /*push=*/6);
      cg_op_pop_push(s, OPCODE_WITH_CLEANUP_START, 0, /*pop=*/0, /*push=*/2);
      if (cleanup->with.async) {
        cg_op(s, OPCODE_GET_AWAITABLE, 0);
        cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
        cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
      }
      cg_op_pop_push(s, OPCODE_WITH_CLEANUP_FINISH, 0, /*pop=*/3, /*push=*/0);
      cg_op_pop_push(s, OPCODE_POP_FINALLY, 0, /*pop=*/6, /*push=*/0);
      break;
    }
    if (cg_unreachable(s)) {
      break;
    }
  }
  s->code.scope_cleanup = saved_cleanup;
}

static void emit_if_constant_branch(struct cg_state *s, struct ast_if *if_stmt,
                                    bool condition_truth)
{
  if (condition_truth) {
    emit_statement_list(s, if_stmt->body);
    return;
  }
  if (if_stmt->else_body != NULL) {
    emit_statement_list(s, if_stmt->else_body);
  }
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

static struct symbol **nullable symbol_array_copy(struct cg_state  *s,
                                                  struct idynarray *array)
{
  unsigned num_symbols = idynarray_length(array, struct symbol *);
  if (num_symbols == 0) return NULL;
  size_t          size = num_symbols * sizeof(struct symbol *);
  struct symbol **result = arena_allocate(object_intern_arena(s->objects),
                                          size, alignof(struct symbol *));
  memcpy(result, idynarray_data(array), size);
  return result;
}

static struct ast_scope_bindings *scope_bindings_alloc(struct cg_state *s)
{
  struct ast_scope_bindings *bindings
      = arena_allocate(object_intern_arena(s->objects), sizeof(*bindings),
                       alignof(struct ast_scope_bindings));
  memset(bindings, 0, sizeof(*bindings));
  return bindings;
}

static struct ast_scope_bindings *
scope_bindings_from_scope(struct cg_state *s, struct binding_scope *scope)
{
  struct ast_scope_bindings *bindings = scope_bindings_alloc(s);
  bindings->num_globals = idynarray_length(&scope->globals, struct symbol *);
  bindings->globals = symbol_array_copy(s, &scope->globals);
  bindings->num_locals = idynarray_length(&scope->locals, struct symbol *);
  bindings->locals = symbol_array_copy(s, &scope->locals);
  bindings->num_cellvars = idynarray_length(&scope->cellvars, struct symbol *);
  bindings->cellvars = symbol_array_copy(s, &scope->cellvars);
  bindings->num_freevars = idynarray_length(&scope->freevars, struct symbol *);
  bindings->freevars = symbol_array_copy(s, &scope->freevars);
  return bindings;
}

static void binding_scope_init(struct binding_scope          *scope,
                               struct cg_state               *cg,
                               struct binding_scope *nullable parent,
                               struct ast_def *nullable def, bool is_class,
                               bool is_comprehension)
{
  memset(scope, 0, sizeof(*scope));
  scope->cg = cg;
  scope->parent = parent;
  scope->def = def;
  scope->class_symbol
      = symbol_table_get_or_insert(cg->symbol_table, "__class__");
  scope->is_class = is_class;
  scope->is_comprehension = is_comprehension;
  idynarray_init(&scope->locals, scope->locals_inline,
                 sizeof(scope->locals_inline));
  idynarray_init(&scope->bound_before_decl, scope->bound_before_decl_inline,
                 sizeof(scope->bound_before_decl_inline));
  idynarray_init(&scope->globals, scope->globals_inline,
                 sizeof(scope->globals_inline));
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
  idynarray_init(&scope->lambda_children, scope->lambda_children_inline,
                 sizeof(scope->lambda_children_inline));
  idynarray_init(&scope->generator_children, scope->generator_children_inline,
                 sizeof(scope->generator_children_inline));
}

static void binding_scope_free(struct binding_scope *scope)
{
  idynarray_clear(&scope->generator_children);
  idynarray_free(&scope->generator_children);
  idynarray_clear(&scope->lambda_children);
  idynarray_free(&scope->lambda_children);
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
    if (is_dunder_class(name)) {
      if (symbol_array_contains(&scope->globals, name)) {
        return false;
      }
      if (symbol_array_contains(&scope->nonlocals, name)) {
        if (resolve_from_parent_scope(scope->parent, name)) {
          symbol_array_append_unique(&scope->freevars, name);
          return true;
        }
        return false;
      }
      /* Methods nested in this class should capture this class via the
       * implicit __class__ cell, even if an outer scope also has __class__. */
      scope->class_needs_class_cell = true;
      return true;
    }
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

static void scope_mark_local(struct binding_scope *scope,
                             struct symbol        *symbol)
{
  if (symbol_array_contains(&scope->globals, symbol)
      || symbol_array_contains(&scope->nonlocals, symbol)) {
    return;
  }
  symbol_array_append_unique(&scope->locals, symbol);
  symbol_array_append_unique(&scope->bound_before_decl, symbol);
}

static struct binding_scope *namedexpr_owner_scope(struct binding_scope *scope)
{
  if (!scope->is_comprehension) {
    return scope;
  }
  struct binding_scope *owner = scope->parent;
  while (owner != NULL && owner->is_comprehension) {
    owner = owner->parent;
  }
  return owner != NULL ? owner : scope;
}

static void scope_mark_namedexpr_target(struct binding_scope *scope,
                                        struct symbol        *symbol)
{
  struct binding_scope *owner = namedexpr_owner_scope(scope);
  if (owner == scope) {
    scope_mark_local(scope, symbol);
    return;
  }

  if (symbol_array_contains(&owner->globals, symbol)) {
    symbol_array_append_unique(&scope->globals, symbol);
    return;
  }

  scope_mark_local(owner, symbol);
  symbol_array_append_unique(&scope->uses, symbol);
}

static void scope_note_use(struct binding_scope *scope, struct symbol *symbol)
{
  symbol_array_append_unique(&scope->uses, symbol);
}

static void analyze_expression(struct binding_scope *scope,
                               union ast_expression *expression);
static void analyze_statement_collect(struct binding_scope *scope,
                                      union ast_statement  *statement);

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
    /* When 'super' is referenced inside a function, CPython implicitly adds
     * __class__ as a free variable so that zero-argument super() can find the
     * enclosing class at runtime.  Mirror that behaviour here: add __class__
     * as a use which the normal binding resolution will propagate to the
     * enclosing class scope's cell variables. */
    if (!scope->is_class
        && strcmp(expression->identifier.symbol->string, "super") == 0) {
      struct symbol *class_sym
          = symbol_table_get_or_insert(scope->cg->symbol_table, "__class__");
      scope_note_use(scope, class_sym);
    }
    return;
  case AST_ATTR:
    analyze_expression(scope, expression->attr.expression);
    return;
  case AST_BINEXPR_ASSIGN:
    if (ast_expression_type(expression->binexpr.left) == AST_IDENTIFIER) {
      scope_mark_namedexpr_target(scope,
                                  expression->binexpr.left->identifier.symbol);
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
    struct ast_generator_expression *generator
        = &expression->generator_expression;
    if (generator->num_parts > 0
        && generator->parts[0].type == GENERATOR_EXPRESSION_PART_FOR) {
      analyze_expression(scope, generator->parts[0].expression);
    }
    *idynarray_append(&scope->generator_children,
                      struct ast_generator_expression *)
        = generator;
    return;
  }
  case AST_LAMBDA: {
    /* Lambda body has its own scope; analyse parameter defaults/annotations
     * in the *outer* scope and register the lambda for later analysis. */
    struct ast_lambda *lambda = &expression->lambda;
    for (unsigned i = 0; i < lambda->parameter_shape.num_parameters; ++i) {
      struct parameter *parameter = &lambda->parameters[i];
      if (parameter->type != NULL) {
        analyze_expression(scope, parameter->type);
      }
      if (parameter->initializer != NULL) {
        analyze_expression(scope, parameter->initializer);
      }
    }
    *idynarray_append(&scope->lambda_children, struct ast_lambda *) = lambda;
    return;
  }
  case AST_SLICE:
    if (expression->slice.start != NULL)
      analyze_expression(scope, expression->slice.start);
    if (expression->slice.stop != NULL)
      analyze_expression(scope, expression->slice.stop);
    if (expression->slice.step != NULL)
      analyze_expression(scope, expression->slice.step);
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

static void
analyze_statement_list_collect(struct binding_scope      *scope,
                               struct ast_statement_list *statement_list)
{
  for (unsigned i = 0; i < statement_list->num_statements; ++i) {
    analyze_statement_collect(scope, statement_list->statements[i]);
  }
}

static void analyze_global_or_nonlocal(struct binding_scope *scope,
                                       struct location       location,
                                       struct symbol *name, bool nonlocal)
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
                                      union ast_statement  *statement)
{
  switch (ast_statement_type(statement)) {
  case AST_STATEMENT_ANNOTATION:
    analyze_expression(scope, statement->annotation.annotation);
    analyze_assignment_target(scope, statement->annotation.target,
                              /*bind=*/statement->annotation.value != NULL
                                  || (statement->annotation.base.flags
                                      & STATEMENT_ANNOTATION_SIMPLE));
    if (statement->annotation.value != NULL) {
      analyze_expression(scope, statement->annotation.value);
    }
    return;
  case AST_STATEMENT_ASSERT:
    analyze_expression(scope, statement->assert.expression);
    if (statement->assert.message != NULL) {
      analyze_expression(scope, statement->assert.message);
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
    struct ast_class *class_stmt = &statement->class_;
    scope_mark_local(scope, class_stmt->name);
    for (unsigned i = 0; i < class_stmt->num_decorators; ++i) {
      analyze_expression(scope, class_stmt->decorators[i]);
    }
    for (unsigned i = 0; i < class_stmt->call->num_arguments; ++i) {
      analyze_expression(scope, class_stmt->call->arguments[i].expression);
    }
    *idynarray_append(&scope->class_children, struct ast_class *) = class_stmt;
    return;
  }
  case AST_STATEMENT_DEF: {
    struct ast_def *def = &statement->def;
    scope_mark_local(scope, def->name);
    for (unsigned i = 0; i < def->num_decorators; ++i) {
      analyze_expression(scope, def->decorators[i]);
    }
    for (unsigned i = 0; i < def->parameter_shape.num_parameters; ++i) {
      struct parameter *parameter = &def->parameters[i];
      if (parameter->type != NULL) {
        analyze_expression(scope, parameter->type);
      }
      if (parameter->initializer != NULL) {
        analyze_expression(scope, parameter->initializer);
      }
    }
    if (def->return_type != NULL) {
      analyze_expression(scope, def->return_type);
    }
    *idynarray_append(&scope->children, struct ast_def *) = def;
    return;
  }
  case AST_STATEMENT_DEL:
    analyze_assignment_target(scope, statement->del.targets,
                              /*bind=*/true);
    return;
  case AST_STATEMENT_EXPRESSION:
    analyze_expression(scope, statement->expression.expression);
    return;
  case AST_STATEMENT_FOR:
    analyze_assignment_target(scope, statement->for_.targets,
                              /*bind=*/true);
    analyze_expression(scope, statement->for_.expression);
    analyze_statement_list_collect(scope, statement->for_.body);
    if (statement->for_.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->for_.else_body);
    }
    return;
  case AST_STATEMENT_FROM_IMPORT:
    if (!statement->from_import.import_star) {
      for (unsigned i = 0; i < statement->from_import.num_items; ++i) {
        struct from_import_item *item = &statement->from_import.items[i];
        scope_mark_local(scope, item->as != NULL ? item->as : item->name);
      }
    }
    return;
  case AST_STATEMENT_GLOBAL:
    for (unsigned i = 0; i < statement->global.num_names; ++i) {
      analyze_global_or_nonlocal(scope, statement->base.location,
                                 statement->global.names[i],
                                 /*nonlocal=*/false);
    }
    return;
  case AST_STATEMENT_IF:
    analyze_expression(scope, statement->if_.condition);
    analyze_statement_list_collect(scope, statement->if_.body);
    for (unsigned i = 0; i < statement->if_.num_elifs; ++i) {
      analyze_expression(scope, statement->if_.elifs[i].condition);
      analyze_statement_list_collect(scope, statement->if_.elifs[i].body);
    }
    if (statement->if_.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->if_.else_body);
    }
    return;
  case AST_STATEMENT_IMPORT:
    for (unsigned i = 0; i < statement->import.num_items; ++i) {
      struct ast_import_item *item = &statement->import.items[i];
      struct symbol          *name = item->as;
      if (name == NULL) {
        name = item->module->symbols[0];
      }
      scope_mark_local(scope, name);
    }
    return;
  case AST_STATEMENT_NONLOCAL:
    for (unsigned i = 0; i < statement->nonlocal.num_names; ++i) {
      analyze_global_or_nonlocal(scope, statement->base.location,
                                 statement->nonlocal.names[i],
                                 /*nonlocal=*/true);
    }
    return;
  case AST_STATEMENT_RAISE:
    if (statement->raise.expression != NULL) {
      analyze_expression(scope, statement->raise.expression);
    }
    if (statement->raise.from != NULL) {
      analyze_expression(scope, statement->raise.from);
    }
    return;
  case AST_STATEMENT_RETURN:
    if (statement->return_.expression != NULL) {
      analyze_expression(scope, statement->return_.expression);
    }
    return;
  case AST_STATEMENT_TRY:
    analyze_statement_list_collect(scope, statement->try_.body);
    for (unsigned i = 0; i < statement->try_.num_excepts; ++i) {
      struct ast_try_except *except_stmt = &statement->try_.excepts[i];
      if (except_stmt->match != NULL) {
        analyze_expression(scope, except_stmt->match);
      }
      if (except_stmt->as != NULL) {
        scope_mark_local(scope, except_stmt->as);
      }
      analyze_statement_list_collect(scope, except_stmt->body);
    }
    if (statement->try_.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->try_.else_body);
    }
    if (statement->try_.finally_body != NULL) {
      analyze_statement_list_collect(scope, statement->try_.finally_body);
    }
    return;
  case AST_STATEMENT_WHILE:
    analyze_expression(scope, statement->while_.condition);
    analyze_statement_list_collect(scope, statement->while_.body);
    if (statement->while_.else_body != NULL) {
      analyze_statement_list_collect(scope, statement->while_.else_body);
    }
    return;
  case AST_STATEMENT_WITH:
    for (unsigned i = 0; i < statement->with.num_items; ++i) {
      struct ast_with_item *item = &statement->with.items[i];
      analyze_expression(scope, item->expression);
      union ast_expression *target = item->target;
      if (target != NULL) {
        analyze_assignment_target(scope, target, /*bind=*/true);
      }
    }
    analyze_statement_list_collect(scope, statement->with.body);
    return;
  case AST_STATEMENT_YIELD:
  case AST_STATEMENT_YIELD_FROM:
    if (statement->yield.expression != NULL) {
      analyze_expression(scope, statement->yield.expression);
    }
    return;
  }
}

static void analyze_function_bindings(struct cg_state *s, struct ast_def *def,
                                      struct binding_scope *nullable parent);
static void analyze_class_bindings(struct cg_state               *s,
                                   struct ast_class              *class_stmt,
                                   struct binding_scope *nullable parent);
static void
analyze_lambda_bindings_inner(struct cg_state *s, struct ast_lambda *lambda,
                              struct binding_scope *nullable parent);
static void
analyze_generator_bindings_inner(struct cg_state                 *s,
                                 struct ast_generator_expression *generator,
                                 struct binding_scope *nullable   parent);

static void analyze_children_bindings(struct cg_state      *s,
                                      struct binding_scope *scope)
{
  struct ast_class **class_children = idynarray_data(&scope->class_children);
  unsigned           num_class_children
      = idynarray_length(&scope->class_children, struct ast_class *);
  for (unsigned i = 0; i < num_class_children; ++i) {
    analyze_class_bindings(s, class_children[i], scope);
  }

  struct ast_def **children = idynarray_data(&scope->children);
  unsigned num_children = idynarray_length(&scope->children, struct ast_def *);
  for (unsigned i = 0; i < num_children; ++i) {
    analyze_function_bindings(s, children[i], scope);
  }

  struct ast_lambda **lambda_children
      = idynarray_data(&scope->lambda_children);
  unsigned num_lambda_children
      = idynarray_length(&scope->lambda_children, struct ast_lambda *);
  for (unsigned i = 0; i < num_lambda_children; ++i) {
    analyze_lambda_bindings_inner(s, lambda_children[i], scope);
  }

  struct ast_generator_expression **generator_children
      = idynarray_data(&scope->generator_children);
  unsigned num_generator_children = idynarray_length(
      &scope->generator_children, struct ast_generator_expression *);
  for (unsigned i = 0; i < num_generator_children; ++i) {
    analyze_generator_bindings_inner(s, generator_children[i], scope);
  }
}

static void resolve_nonlocals(struct cg_state *s, struct binding_scope *scope,
                              struct location *nullable error_location)
{
  unsigned i = 0;
  while (i < idynarray_length(&scope->nonlocals, struct symbol *)) {
    struct symbol **nonlocals = idynarray_data(&scope->nonlocals);
    struct symbol  *name = nonlocals[i];
    if (!resolve_from_parent_scope(scope->parent, name)) {
      if (is_dunder_class(name) && scope->parent != NULL
          && scope->parent->is_class) {
        symbol_array_append_unique(&scope->freevars, name);
        scope->parent->class_needs_class_cell = true;
        ++i;
        continue;
      }
      if (error_location != NULL) {
        diag_begin_error(s->d, *error_location);
        diag_frag(s->d, "no binding for nonlocal ");
        diag_symbol(s->d, name);
        diag_frag(s->d, " found");
        diag_end(s->d);
      }
      /* Keep unresolved names out of nonlocal scope metadata so nested
       * closures don't try to capture missing cells. */
      symbol_array_remove_at_swap(&scope->nonlocals, i);
    } else {
      symbol_array_append_unique(&scope->freevars, name);
      ++i;
    }
  }
}

static void resolve_uses(struct binding_scope *scope, bool check_class_binds)
{
  struct symbol **uses = idynarray_data(&scope->uses);
  unsigned        num_uses = idynarray_length(&scope->uses, struct symbol *);
  for (unsigned i = 0; i < num_uses; ++i) {
    struct symbol *name = uses[i];
    if (symbol_array_contains(&scope->locals, name)
        || symbol_array_contains(&scope->globals, name)
        || symbol_array_contains(&scope->nonlocals, name)
        || symbol_array_contains(&scope->freevars, name)) {
      continue;
    }
    if (resolve_from_parent_scope(scope->parent, name)) {
      symbol_array_append_unique(&scope->freevars, name);
    } else if (check_class_binds && is_dunder_class(name)
               && scope->parent != NULL && scope->parent->is_class
               && !class_explicitly_binds_class_symbol(scope->parent)) {
      symbol_array_append_unique(&scope->freevars, name);
      scope->parent->class_needs_class_cell = true;
    }
  }
}

static void analyze_class_bindings(struct cg_state               *s,
                                   struct ast_class              *class_stmt,
                                   struct binding_scope *nullable parent)
{
  if (class_stmt->scope != NULL) {
    return;
  }

  struct binding_scope scope;
  binding_scope_init(&scope, s, parent, /*def=*/NULL, /*is_class=*/true,
                     /*is_comprehension=*/false);
  analyze_statement_list_collect(&scope, class_stmt->body);

  analyze_children_bindings(s, &scope);
  resolve_nonlocals(s, &scope, &class_stmt->base.location);
  resolve_uses(&scope, /*check_class_binds=*/false);

  class_stmt->scope = scope_bindings_from_scope(s, &scope);
  if (scope.class_needs_class_cell)
    class_stmt->base.flags |= STATEMENT_CLASS_NEEDS_CLASS_CELL;

  binding_scope_free(&scope);
}

static void analyze_function_bindings(struct cg_state *s, struct ast_def *def,
                                      struct binding_scope *nullable parent)
{
  if (def->scope != NULL) {
    return;
  }

  struct binding_scope scope;
  binding_scope_init(&scope, s, parent, def, /*is_class=*/false,
                     /*is_comprehension=*/false);

  for (unsigned i = 0; i < def->parameter_shape.num_parameters; ++i) {
    scope_mark_local(&scope, def->parameters[i].name);
  }

  analyze_statement_list_collect(&scope, def->body);

  analyze_children_bindings(s, &scope);
  resolve_nonlocals(s, &scope, &def->base.location);
  resolve_uses(&scope, /*check_class_binds=*/true);

  def->scope = scope_bindings_from_scope(s, &scope);

  binding_scope_free(&scope);
}

static void
analyze_lambda_bindings_inner(struct cg_state *s, struct ast_lambda *lambda,
                              struct binding_scope *nullable parent)
{
  if (lambda->scope != NULL) {
    return;
  }

  struct binding_scope scope;
  binding_scope_init(&scope, s, parent, /*def=*/NULL, /*is_class=*/false,
                     /*is_comprehension=*/false);

  for (unsigned i = 0; i < lambda->parameter_shape.num_parameters; ++i) {
    scope_mark_local(&scope, lambda->parameters[i].name);
  }

  analyze_expression(&scope, lambda->expression);

  analyze_children_bindings(s, &scope);
  resolve_nonlocals(s, &scope, /*error_location=*/NULL);
  resolve_uses(&scope, /*check_class_binds=*/true);

  lambda->scope = scope_bindings_from_scope(s, &scope);

  binding_scope_free(&scope);
}

static void
analyze_generator_bindings_inner(struct cg_state                 *s,
                                 struct ast_generator_expression *generator,
                                 struct binding_scope *nullable   parent)
{
  if (generator->scope != NULL) {
    return;
  }

  struct binding_scope scope;
  binding_scope_init(&scope, s, parent, /*def=*/NULL, /*is_class=*/false,
                     /*is_comprehension=*/true);

  for (unsigned i = 0; i < generator->num_parts; ++i) {
    struct generator_expression_part *part = &generator->parts[i];
    if (part->type == GENERATOR_EXPRESSION_PART_FOR) {
      if (i > 0) {
        analyze_expression(&scope, part->expression);
      }
      assert(part->targets != NULL);
      analyze_assignment_target(&scope, part->targets, /*bind=*/true);
    } else {
      assert(part->type == GENERATOR_EXPRESSION_PART_IF);
      analyze_expression(&scope, part->expression);
    }
  }

  analyze_expression(&scope, generator->expression);
  if (generator->item_value != NULL) {
    analyze_expression(&scope, generator->item_value);
  }

  analyze_children_bindings(s, &scope);
  resolve_nonlocals(s, &scope, /*error_location=*/NULL);
  resolve_uses(&scope, /*check_class_binds=*/true);

  generator->scope = scope_bindings_from_scope(s, &scope);

  binding_scope_free(&scope);
}

void analyze_lambda_bindings(struct cg_state *s, struct ast_lambda *lambda)
{
  analyze_lambda_bindings_inner(s, lambda, /*parent=*/NULL);
}

void analyze_generator_bindings(struct cg_state                 *s,
                                struct ast_generator_expression *generator)
{
  analyze_generator_bindings_inner(s, generator, /*parent=*/NULL);
}

static void apply_class_bindings(struct cg_state  *s,
                                 struct ast_class *class_stmt)
{
  const struct ast_scope_bindings *scope
      = scope_bindings_or_empty(class_stmt->scope);
  if (class_stmt->base.flags & STATEMENT_CLASS_NEEDS_CLASS_CELL) {
    struct symbol *class_symbol
        = symbol_table_get_or_insert(s->symbol_table, "__class__");
    cg_declare(s, class_symbol, SYMBOL_CELL);
  }
  for (unsigned i = 0; i < scope->num_freevars; ++i) {
    cg_register_freevar(s, scope->freevars[i]);
  }
  for (unsigned i = 0; i < scope->num_globals; ++i) {
    cg_declare(s, scope->globals[i], SYMBOL_GLOBAL);
  }
  for (unsigned i = 0; i < scope->num_freevars; ++i) {
    struct symbol *name = scope->freevars[i];
    bool           is_global = false;
    for (unsigned j = 0; j < scope->num_globals; ++j) {
      if (scope->globals[j] == name) {
        is_global = true;
        break;
      }
    }
    if (is_global) continue;

    bool is_local = false;
    for (unsigned j = 0; j < scope->num_locals; ++j) {
      if (scope->locals[j] == name) {
        is_local = true;
        break;
      }
    }
    if (is_local) continue;

    cg_declare(s, scope->freevars[i], SYMBOL_NONLOCAL);
  }
}

static void apply_function_bindings(struct cg_state *s, struct ast_def *def)
{
  const struct ast_scope_bindings *scope = scope_bindings_or_empty(def->scope);
  for (unsigned i = 0; i < scope->num_globals; ++i) {
    cg_declare(s, scope->globals[i], SYMBOL_GLOBAL);
  }
  for (unsigned i = 0; i < scope->num_freevars; ++i) {
    cg_declare(s, scope->freevars[i], SYMBOL_NONLOCAL);
  }
  for (unsigned i = 0; i < scope->num_locals; ++i) {
    cg_declare(s, scope->locals[i], SYMBOL_LOCAL);
  }
  for (unsigned i = 0; i < scope->num_cellvars; ++i) {
    cg_promote_to_cell(s, scope->cellvars[i]);
  }
}

static void
analyze_and_apply_toplevel_function_bindings(struct cg_state   *s,
                                             struct ast_module *module)
{
  struct binding_scope scope;
  binding_scope_init(&scope, s, /*parent=*/NULL, /*def=*/NULL,
                     /*is_class=*/false, /*is_comprehension=*/false);
  analyze_statement_list_collect(&scope, module->body);
  analyze_children_bindings(s, &scope);
  resolve_nonlocals(s, &scope, /*error_location=*/NULL);
  resolve_uses(&scope, /*check_class_binds=*/true);

  for (unsigned i = 0; i < idynarray_length(&scope.globals, struct symbol *);
       ++i) {
    struct symbol **globals = idynarray_data(&scope.globals);
    cg_declare(s, globals[i], SYMBOL_GLOBAL);
  }
  for (unsigned i = 0; i < idynarray_length(&scope.locals, struct symbol *);
       ++i) {
    struct symbol **locals = idynarray_data(&scope.locals);
    cg_declare(s, locals[i], SYMBOL_LOCAL);
  }
  for (unsigned i = 0; i < idynarray_length(&scope.cellvars, struct symbol *);
       ++i) {
    struct symbol **cellvars = idynarray_data(&scope.cellvars);
    cg_promote_to_cell(s, cellvars[i]);
  }

  binding_scope_free(&scope);
}

static void emit_statement_list(struct cg_state           *s,
                                struct ast_statement_list *statement_list)
{
  for (unsigned i = 0; i < statement_list->num_statements; ++i) {
    emit_statement(s, statement_list->statements[i]);
  }
}

static void
emit_statement_list_skip_first(struct cg_state           *s,
                               struct ast_statement_list *statement_list)
{
  for (unsigned i = 1; i < statement_list->num_statements; ++i) {
    emit_statement(s, statement_list->statements[i]);
  }
}

static void emit_def(struct cg_state *s, struct ast_def *def)
{
  if (cg_unreachable(s)) {
    return;
  }
  analyze_function_bindings(s, def, /*parent=*/NULL);
  for (unsigned i = 0; i < def->num_decorators; ++i) {
    struct location location = get_expression_location(def->decorators[i]);
    if (location.line > 0) {
      cg_set_lineno(s, location.line);
    }
    emit_expression(s, def->decorators[i]);
  }
  cg_set_lineno(s, def->base.location.line);

  struct make_function_state state;
  bool global_binding = cg_symbol_is_global(s, def->name);
  emit_make_function_begin(s, &state, &def->parameter_shape, def->parameters,
                           (def->base.flags & STATEMENT_DEF_ASYNC) != 0,
                           def->return_type, def->name->string,
                           global_binding);
  union object *doc = statement_list_leading_docstring(def->body);
  cg_set_function_docstring(s, s->optimize_no_docstrings ? NULL : doc);
  apply_function_bindings(s, def);
  cg_set_lineno(s, def->base.location.line);
  if (doc != NULL) {
    emit_statement_list_skip_first(s, def->body);
  } else {
    emit_statement_list(s, def->body);
  }
  if (def->base.flags & STATEMENT_DEF_HAS_YIELD) {
    s->code.flags |= CO_GENERATOR;
  }
  const struct ast_scope_bindings *scope = scope_bindings_or_empty(def->scope);
  unsigned                         num_freevars = scope->num_freevars;
  if (num_freevars > 0) {
    state.num_closure_symbols = num_freevars;
    state.closure_symbols = scope->freevars;
  }

  if (def->base.flags & STATEMENT_DEF_ASYNC) {
    if (s->code.flags & CO_GENERATOR) {
      s->code.flags = (s->code.flags & ~CO_GENERATOR) | CO_ASYNC_GENERATOR;
    } else {
      s->code.flags |= CO_COROUTINE;
    }
  }
  emit_make_function_end(s, &state, def->name);
  emit_decorator_calls(s, def->num_decorators);
  cg_store(s, def->name);
}

static void emit_class(struct cg_state *s, struct ast_class *class_stmt)
{
  analyze_class_bindings(s, class_stmt, /*parent=*/NULL);
  for (unsigned i = 0; i < class_stmt->num_decorators; ++i) {
    struct location location
        = get_expression_location(class_stmt->decorators[i]);
    if (location.line > 0) {
      cg_set_lineno(s, location.line);
    }
    emit_expression(s, class_stmt->decorators[i]);
  }
  cg_set_lineno(s, class_stmt->base.location.line);

  cg_op_push1(s, OPCODE_LOAD_BUILD_CLASS, 0);
  bool        global_binding = cg_symbol_is_global(s, class_stmt->name);
  const char *class_qualname
      = global_binding ? class_stmt->name->string
                       : cg_build_qualname(s, class_stmt->name->string);
  cg_push_code(s);
  cg_code_begin(s, /*in_function=*/false);
  s->code.in_class_body = true;

  /* Set class body's qualname prefix: class_qualname + "." */
  {
    size_t        qlen = strlen(class_qualname);
    struct arena *arena = object_intern_arena(s->objects);
    char         *prefix = arena_allocate(arena, qlen + 2, 1);
    memcpy(prefix, class_qualname, qlen);
    prefix[qlen] = '.';
    prefix[qlen + 1] = '\0';
    s->code.qualname_prefix = prefix;
  }

  cg_load(s, symbol_table_get_or_insert(s->symbol_table, "__name__"));
  cg_store(s, symbol_table_get_or_insert(s->symbol_table, "__module__"));
  cg_load_const(s, object_intern_cstring(s->objects, class_qualname));
  cg_store(s, symbol_table_get_or_insert(s->symbol_table, "__qualname__"));

  apply_class_bindings(s, class_stmt);
  cg_set_lineno(s, class_stmt->base.location.line);
  emit_nonfunction_statement_list(s, class_stmt->body);
  if (class_stmt->base.flags & STATEMENT_CLASS_NEEDS_CLASS_CELL) {
    if (!cg_unreachable(s)) {
      struct symbol *class_symbol
          = symbol_table_get_or_insert(s->symbol_table, "__class__");
      struct symbol *classcell_symbol
          = symbol_table_get_or_insert(s->symbol_table, "__classcell__");
      cg_op_push1(s, OPCODE_LOAD_CLOSURE, cg_closure_index(s, class_symbol));
      cg_op_push1(s, OPCODE_DUP_TOP, 0);
      cg_store(s, classcell_symbol);
      cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
      cg_block_end(s);
    }
  } else {
    emit_code_end(s);
  }
  union object *code = cg_pop_code(s, class_stmt->name->string);

  uint32_t                         flags = 0;
  unsigned                         operands = 2;
  const struct ast_scope_bindings *scope
      = scope_bindings_or_empty(class_stmt->scope);
  if (scope->num_freevars > 0) {
    for (unsigned i = 0; i < scope->num_freevars; ++i) {
      cg_op_push1(s, OPCODE_LOAD_CLOSURE,
                  cg_closure_index(s, scope->freevars[i]));
    }
    cg_op_pop_push(s, OPCODE_BUILD_TUPLE, scope->num_freevars,
                   /*pop=*/scope->num_freevars, /*push=*/1);
    flags |= MAKE_FUNCTION_CLOSURE;
    ++operands;
  }

  cg_load_const(s, code);
  union object *name_const
      = object_intern_cstring(s->objects, class_stmt->name->string);
  cg_load_const(s, name_const);
  cg_op_pop_push(s, OPCODE_MAKE_FUNCTION, flags, /*pop=*/operands,
                 /*push=*/1);
  cg_load_const(s, name_const);
  emit_call_helper(s, class_stmt->call, /*num_extra_args=*/2);
  emit_decorator_calls(s, class_stmt->num_decorators);
  cg_store(s, class_stmt->name);
}

static void emit_for(struct cg_state *s, struct ast_for *for_stmt)
{
  if (cg_unreachable(s)) {
    return;
  }
  struct for_while_state state;
  emit_for_begin(s, &state, for_stmt->targets, for_stmt->expression,
                 (for_stmt->base.flags & STATEMENT_FOR_ASYNC) != 0);
  emit_statement_list(s, for_stmt->body);
  emit_for_else(s, &state);
  if (for_stmt->else_body != NULL) {
    emit_statement_list(s, for_stmt->else_body);
  }
  emit_for_end(s, &state);
}

static void emit_from_import_node(struct cg_state        *s,
                                  struct ast_from_import *from_import)
{
  if (from_import->import_star) {
    emit_from_import(s, from_import->num_prefix_dots, from_import->module, 0,
                     NULL);
  } else {
    emit_from_import(s, from_import->num_prefix_dots, from_import->module,
                     from_import->num_items, from_import->items);
  }
}

static void emit_global_statement_node(struct cg_state   *s,
                                       struct location    location,
                                       struct ast_global *global)
{
  for (unsigned i = 0; i < global->num_names; ++i) {
    struct symbol *name = global->names[i];
    if (!cg_declare(s, name, SYMBOL_GLOBAL)) {
      diag_begin_error(s->d, location);
      diag_frag(s->d, "name ");
      diag_symbol(s->d, name);
      diag_frag(s->d, " is assigned to before global declaration");
      diag_end(s->d);
    }
  }
}

static bool
statement_has_scope_annotation(const union ast_statement *statement)
{
  switch (ast_statement_type((union ast_statement *)statement)) {
  case AST_STATEMENT_ANNOTATION:
    return true;
  case AST_STATEMENT_IF: {
    const struct ast_if *if_stmt = &statement->if_;
    if (statement_list_has_scope_annotation(if_stmt->body)) return true;
    for (unsigned i = 0; i < if_stmt->num_elifs; ++i) {
      if (statement_list_has_scope_annotation(if_stmt->elifs[i].body)) {
        return true;
      }
    }
    return statement_list_has_scope_annotation(if_stmt->else_body);
  }
  case AST_STATEMENT_FOR:
    return statement_list_has_scope_annotation(statement->for_.body)
           || statement_list_has_scope_annotation(statement->for_.else_body);
  case AST_STATEMENT_WHILE:
    return statement_list_has_scope_annotation(statement->while_.body)
           || statement_list_has_scope_annotation(statement->while_.else_body);
  case AST_STATEMENT_WITH:
    return statement_list_has_scope_annotation(statement->with.body);
  case AST_STATEMENT_TRY:
    if (statement_list_has_scope_annotation(statement->try_.body)) return true;
    for (unsigned i = 0; i < statement->try_.num_excepts; ++i) {
      if (statement_list_has_scope_annotation(
              statement->try_.excepts[i].body)) {
        return true;
      }
    }
    return statement_list_has_scope_annotation(statement->try_.else_body)
           || statement_list_has_scope_annotation(
               statement->try_.finally_body);
  default:
    return false;
  }
}

static bool statement_list_has_scope_annotation(
    const struct ast_statement_list *nullable statements)
{
  if (statements == NULL) return false;
  for (unsigned i = 0; i < statements->num_statements; ++i) {
    if (statement_has_scope_annotation(statements->statements[i])) {
      return true;
    }
  }
  return false;
}

static bool finally_body_needs_placeholder_statement_list(
    const struct ast_statement_list *nullable statements, unsigned loop_depth);

static bool
finally_body_needs_placeholder_statement(const union ast_statement *statement,
                                         unsigned                   loop_depth)
{
  switch (ast_statement_type((union ast_statement *)statement)) {
  case AST_STATEMENT_RETURN:
    return true;
  case AST_STATEMENT_BREAK:
  case AST_STATEMENT_CONTINUE:
    return loop_depth == 0;
  case AST_STATEMENT_IF: {
    const struct ast_if *if_stmt = &statement->if_;
    if (finally_body_needs_placeholder_statement_list(if_stmt->body,
                                                      loop_depth)) {
      return true;
    }
    for (unsigned i = 0; i < if_stmt->num_elifs; ++i) {
      if (finally_body_needs_placeholder_statement_list(if_stmt->elifs[i].body,
                                                        loop_depth)) {
        return true;
      }
    }
    return finally_body_needs_placeholder_statement_list(if_stmt->else_body,
                                                         loop_depth);
  }
  case AST_STATEMENT_FOR:
    if (finally_body_needs_placeholder_statement_list(statement->for_.body,
                                                      loop_depth + 1)) {
      return true;
    }
    return finally_body_needs_placeholder_statement_list(
        statement->for_.else_body, loop_depth);
  case AST_STATEMENT_WHILE:
    if (finally_body_needs_placeholder_statement_list(statement->while_.body,
                                                      loop_depth + 1)) {
      return true;
    }
    return finally_body_needs_placeholder_statement_list(
        statement->while_.else_body, loop_depth);
  case AST_STATEMENT_WITH:
    return finally_body_needs_placeholder_statement_list(statement->with.body,
                                                         loop_depth);
  case AST_STATEMENT_TRY:
    if (finally_body_needs_placeholder_statement_list(statement->try_.body,
                                                      loop_depth)) {
      return true;
    }
    for (unsigned i = 0; i < statement->try_.num_excepts; ++i) {
      if (finally_body_needs_placeholder_statement_list(
              statement->try_.excepts[i].body, loop_depth)) {
        return true;
      }
    }
    if (finally_body_needs_placeholder_statement_list(
            statement->try_.else_body, loop_depth)) {
      return true;
    }
    return finally_body_needs_placeholder_statement_list(
        statement->try_.finally_body, loop_depth);
  case AST_STATEMENT_CLASS:
  case AST_STATEMENT_DEF:
    return false;
  default:
    return false;
  }
}

static bool finally_body_needs_placeholder_statement_list(
    const struct ast_statement_list *nullable statements, unsigned loop_depth)
{
  if (statements == NULL) return false;
  for (unsigned i = 0; i < statements->num_statements; ++i) {
    if (finally_body_needs_placeholder_statement(statements->statements[i],
                                                 loop_depth)) {
      return true;
    }
  }
  return false;
}

static void emit_if(struct cg_state *s, struct ast_if *if_stmt)
{
  if (cg_unreachable(s)) {
    return;
  }

  /* Keep symbol analysis unchanged but avoid emitting dead branches for
   * if-statements with compile-time boolean conditions and no elifs. */
  if (if_stmt->num_elifs == 0) {
    union object *condition_constant
        = ast_expression_as_constant(if_stmt->condition);
    if (condition_constant != NULL) {
      enum object_type condition_type = object_type(condition_constant);
      if (condition_type == OBJECT_TRUE || condition_type == OBJECT_FALSE) {
        emit_if_constant_branch(s, if_stmt, condition_type == OBJECT_TRUE);
        return;
      }
    }
  }

  struct if_state state;
  emit_if_begin(s, &state, if_stmt->condition);
  emit_statement_list(s, if_stmt->body);
  for (unsigned i = 0; i < if_stmt->num_elifs; ++i) {
    struct ast_if_elif *elif_stmt = &if_stmt->elifs[i];
    emit_if_elif(s, &state, elif_stmt->condition);
    emit_statement_list(s, elif_stmt->body);
  }
  if (if_stmt->else_body != NULL) {
    emit_if_else(s, &state);
    emit_statement_list(s, if_stmt->else_body);
  }
  emit_if_end(s, &state);
}

static void emit_import_node(struct cg_state   *s,
                             struct ast_import *import_stmt)
{
  for (unsigned i = 0; i < import_stmt->num_items; ++i) {
    struct ast_import_item *item = &import_stmt->items[i];
    emit_import(s, item->module, item->as);
  }
}

static void
emit_nonlocal_statement_node(struct cg_state *s, struct location location,
                             struct ast_statement_nonlocal *nonlocal)
{
  for (unsigned i = 0; i < nonlocal->num_names; ++i) {
    struct symbol *name = nonlocal->names[i];
    if (!cg_declare(s, name, SYMBOL_NONLOCAL)) {
      diag_begin_error(s->d, location);
      diag_frag(s->d, "name ");
      diag_symbol(s->d, name);
      diag_frag(s->d, " is assigned to before nonlocal declaration");
      diag_end(s->d);
    }
  }
}

static void emit_try(struct cg_state *s, struct ast_try *try_stmt)
{
  if (cg_unreachable(s)) return;

  union scope_cleanup *saved_scope_cleanup = s->code.scope_cleanup;
  bool                 finally_needs_placeholder
      = try_stmt->finally_body != NULL
        && finally_body_needs_placeholder_statement_list(
            try_stmt->finally_body, /*loop_depth=*/0);
  unsigned num_excepts = try_stmt->num_excepts;

  struct try_state state;
  memset(&state, 0, sizeof(state));

  struct finally_scope_cleanup finally_cleanup;
  if (try_stmt->finally_body) {
    struct basic_block *finally_block = cg_block_allocate(s);
    state.finally_block = finally_block;
    if (finally_needs_placeholder) {
      /* Reserve a placeholder so abrupt exits from the finally body
       * (`break`/`continue`/`return`) can balance the END/POP_FINALLY path. */
      cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
    }
    struct basic_block *setup_finally_next = cg_block_allocate(s);
    cg_condjump(s, OPCODE_SETUP_FINALLY, /*target=*/finally_block,
                /*fallthrough=*/setup_finally_next);
    cg_block_begin(s, setup_finally_next);

    finally_cleanup = (struct finally_scope_cleanup){
      .base = {
        .kind = SCOPE_CLEANUP_FINALLY,
        .prev = saved_scope_cleanup,
      },
      .finally_needs_placeholder = finally_needs_placeholder,
      .finally_block = state.finally_block,
    };
    s->code.scope_cleanup = (union scope_cleanup *)&finally_cleanup;
  }

  union scope_cleanup      *pre_body_pending = s->code.scope_cleanup;
  struct scope_cleanup_base except_cleanup;
  if (num_excepts > 0) {
    struct basic_block *setup_finally_next = cg_block_allocate(s);
    struct basic_block *excepts = cg_block_allocate(s);
    state.excepts = excepts;
    cg_condjump(s, OPCODE_SETUP_FINALLY,
                /*target=*/excepts, /*fallthrough=*/setup_finally_next);
    cg_block_begin(s, setup_finally_next);

    except_cleanup = (struct scope_cleanup_base){
      .kind = SCOPE_CLEANUP_POP_BLOCK,
      .prev = pre_body_pending,
    };
    s->code.scope_cleanup = (union scope_cleanup *)&except_cleanup;
  }

  emit_statement_list(s, try_stmt->body);

  /* Pop the except SETUP_FINALLY cleanup before entering except handlers. */
  if (num_excepts > 0) {
    s->code.scope_cleanup = pre_body_pending;
  }

  if (!cg_unreachable(s)) {
    if (num_excepts > 0) {
      cg_op(s, OPCODE_POP_BLOCK, 0); /* pop setup_finally for excepts */
    }
    if (try_stmt->else_body != NULL) {
      assert(state.try_else_block == NULL);
      struct basic_block *try_else_block = cg_block_allocate(s);
      state.try_else_block = try_else_block;
      cg_jump(s, state.try_else_block);
    } else if (try_stmt->finally_body) {
      struct basic_block *enter_finally = state.enter_finally;
      if (enter_finally == NULL) {
        enter_finally = cg_block_allocate(s);
        state.enter_finally = enter_finally;
      }
      cg_jump(s, enter_finally);
    } else {
      struct basic_block *footer = state.footer;
      if (footer == NULL) {
        footer = cg_block_allocate(s);
        state.footer = footer;
      }
      cg_jump(s, footer);
    }
  }

  if (num_excepts > 0) {
    cg_block_begin(s, state.excepts);
    cg_push(s, 6);
    for (unsigned i = 0; i < num_excepts; ++i) {
      struct ast_try_except *except_stmt = &try_stmt->excepts[i];
      emit_except_begin(s, &state, except_stmt->location, except_stmt->match,
                        except_stmt->as);

      /* Push cleanup entries for break/continue/return inside the except
       * handler body.  Innermost first: as-cleanup, then POP_EXCEPT. */
      union scope_cleanup *pre_handler_pending = s->code.scope_cleanup;
      union scope_cleanup pop_except_cleanup = {
        .base = {
          .kind = SCOPE_CLEANUP_POP_EXCEPT,
          .prev = s->code.scope_cleanup,
        },
      };
      union scope_cleanup as_cleanup;
      if (except_stmt->as != NULL) {
        as_cleanup = (union scope_cleanup){
          .except_as = {
            .base = {
              .kind = SCOPE_CLEANUP_EXCEPT_AS,
              .prev = &pop_except_cleanup,
            },
            .as_symbol = except_stmt->as,
          },
        };
        s->code.scope_cleanup = &as_cleanup;
      } else {
        s->code.scope_cleanup = &pop_except_cleanup;
      }

      emit_statement_list(s, except_stmt->body);

      /* Restore before emit_except_end emits its own cleanup. */
      s->code.scope_cleanup = pre_handler_pending;
      emit_except_end(s, &state, except_stmt->as);
    }

    if (!cg_unreachable(s)) {
      cg_op_pop_push(s, OPCODE_END_FINALLY, 0, /*pop=*/6, /*push=*/0);
    } else {
      cg_pop(s, 6);
    }
  }

  if (try_stmt->else_body != NULL) {
    struct basic_block *try_else_block = state.try_else_block;
    if (try_else_block != NULL) {
      if (!cg_unreachable(s)) {
        cg_jump(s, try_else_block);
      }
      cg_block_begin(s, try_else_block);
    }

    emit_statement_list(s, try_stmt->else_body);
  }

  if (try_stmt->finally_body != NULL) {
    struct basic_block *enter_finally = state.enter_finally;
    if (enter_finally != NULL) {
      if (!cg_unreachable(s)) {
        cg_jump(s, enter_finally);
      }
      cg_block_begin(s, enter_finally);
    }

    struct basic_block *finally_block = state.finally_block;
    if (!cg_unreachable(s)) {
      cg_op(s, OPCODE_POP_BLOCK, 0);
      cg_op_pop_push(s, OPCODE_BEGIN_FINALLY, 0, /*pop=*/0, /*push=*/6);
      cg_jump(s, finally_block);
    } else {
      /* Placeholder (if any) was pushed before SETUP_FINALLY and is already
       * reflected in the tracked stack size; only model BEGIN_FINALLY here. */
      cg_push(s, 6);
    }
    cg_block_begin(s, finally_block);

    s->code.scope_cleanup = saved_scope_cleanup;
    struct scope_cleanup_base cleanup = {
      .kind = SCOPE_CLEANUP_WITHIN_FINALLY,
      .prev = s->code.scope_cleanup,
    };
    s->code.scope_cleanup = (union scope_cleanup *)&cleanup;

    emit_statement_list(s, try_stmt->finally_body);

    /* Keep the trailing END_FINALLY (and placeholder POP_TOP when present)
     * even when unreachable so line/block metadata stays consistent. */
    bool dead_tail = cg_unreachable(s);
    if (dead_tail) {
      struct basic_block *dead_finally_tail = cg_block_allocate(s);
      cg_block_begin(s, dead_finally_tail);
    }

    cg_op_pop_push(s, OPCODE_END_FINALLY, 0, /*pop=*/6, /*push=*/0);
    if (finally_needs_placeholder) {
      cg_op_pop1(s, OPCODE_POP_TOP, 0);
    }

    if (dead_tail) {
      cg_block_end(s);
    }
    s->code.scope_cleanup = saved_scope_cleanup;
  }

  if (!cg_unreachable(s)) {
    struct basic_block *footer = state.footer;
    if (footer == NULL) {
      footer = cg_block_allocate(s);
      state.footer = footer;
    }
    cg_jump(s, footer);
  }

  s->code.scope_cleanup = saved_scope_cleanup;

  struct basic_block *footer = state.footer;
  if (footer != NULL) {
    cg_block_begin(s, footer);
  }
}

static void emit_while(struct cg_state *s, struct ast_while *while_stmt)
{
  if (cg_unreachable(s)) {
    return;
  }

  union object *condition_constant
      = ast_expression_as_constant(while_stmt->condition);
  if (condition_constant != NULL) {
    enum object_type condition_type = object_type(condition_constant);
    if (condition_type == OBJECT_FALSE) {
      if (while_stmt->else_body != NULL) {
        emit_statement_list(s, while_stmt->else_body);
      }
      return;
    }

    if (condition_type == OBJECT_TRUE && while_stmt->body != NULL
        && while_stmt->body->num_statements == 1
        && ast_statement_type(while_stmt->body->statements[0])
               == AST_STATEMENT_RETURN) {
      /* Match CPython dead-block behavior for `while True: return ...`,
       * including cases with an unreachable `else` block. */
      emit_statement_list(s, while_stmt->body);
      return;
    }

    if (condition_type == OBJECT_TRUE) {
      if (cg_unreachable(s)) {
        return;
      }

      struct basic_block *header = cg_block_allocate(s);
      struct basic_block *footer = cg_block_allocate(s);

      cg_jump(s, header);
      cg_block_begin(s, header);

      struct loop_state saved = s->code.loop_state;
      s->code.loop_state = (struct loop_state){
        .continue_block = header,
        .break_block = footer,
        .scope_cleanup_at_loop = s->code.scope_cleanup,
        .pop_on_break = false,
      };

      emit_statement_list(s, while_stmt->body);

      s->code.loop_state = saved;
      if (!cg_unreachable(s)) {
        cg_jump(s, header);
      }
      cg_block_begin(s, footer);
      return;
    }
  }

  struct basic_block *header = cg_block_allocate(s);
  struct basic_block *body = cg_block_allocate(s);
  struct basic_block *else_block = cg_block_allocate(s);
  struct basic_block *footer = cg_block_allocate(s);

  cg_jump(s, header);
  cg_block_begin(s, header);

  emit_condjump_expr(s, while_stmt->condition, /*true_block=*/body,
                     /*false_block=*/else_block, /*next=*/body);

  cg_block_begin(s, body);

  struct for_while_state state = {
    .else_or_footer = else_block,
    .saved = s->code.loop_state,
  };
  s->code.loop_state = (struct loop_state){
    .continue_block = header,
    .break_block = footer,
    .scope_cleanup_at_loop = s->code.scope_cleanup,
    .pop_on_break = false,
  };

  emit_statement_list(s, while_stmt->body);
  emit_loop_else(s, &state);
  if (while_stmt->else_body != NULL) {
    emit_statement_list(s, while_stmt->else_body);
  }
  emit_loop_end(s, &state);
}

static void emit_with_setup(struct cg_state           *s,
                            struct with_scope_cleanup *cleanup,
                            union ast_expression      *expression,
                            struct location            as_location,
                            union ast_expression *target, bool async)
{
  struct basic_block *cleanup_block = cg_block_allocate(s);
  struct basic_block *body = cg_block_allocate(s);

  emit_expression(s, expression);
  if (async) {
    cg_op(s, OPCODE_BEFORE_ASYNC_WITH, 0);
    cg_op(s, OPCODE_GET_AWAITABLE, 0);
    cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
    cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
    cg_condjump(s, OPCODE_SETUP_ASYNC_WITH, cleanup_block, body);
    cg_push(s, 1);
  } else {
    cg_condjump(s, OPCODE_SETUP_WITH, cleanup_block, body);
    cg_push(s, 1);
  }
  cg_block_begin(s, body);

  if (target != NULL) {
    if (as_location.line != 0) {
      cg_set_lineno(s, as_location.line);
    }
    emit_assignment(s, target);
  } else {
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
  }

  *cleanup = (struct with_scope_cleanup){
    .base = {
      .kind = SCOPE_CLEANUP_WITH,
      .prev = s->code.scope_cleanup,
    },
    .async = async,
    .block = cleanup_block,
  };
  s->code.scope_cleanup = (union scope_cleanup *)cleanup;
}

static void emit_with_cleanup(struct cg_state                 *s,
                              const struct with_scope_cleanup *cleanup)
{
  struct basic_block *block = cleanup->block;
  if (!cg_unreachable(s)) {
    cg_op(s, OPCODE_POP_BLOCK, 0);
    cg_op_pop_push(s, OPCODE_BEGIN_FINALLY, 0, /*pop=*/0, /*push=*/6);
    cg_jump(s, block);
  } else {
    cg_push(s, 6);
  }

  cg_block_begin(s, block);
  cg_op_pop_push(s, OPCODE_WITH_CLEANUP_START, 0, /*pop=*/0, /*push=*/2);
  if (cleanup->async) {
    cg_op(s, OPCODE_GET_AWAITABLE, 0);
    cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
    cg_op_pop1(s, OPCODE_YIELD_FROM, 0);
  }
  cg_op_pop_push(s, OPCODE_WITH_CLEANUP_FINISH, 0, /*pop=*/3, /*push=*/0);
  cg_op_pop_push(s, OPCODE_END_FINALLY, 0, /*pop=*/6, /*push=*/0);
}

static void emit_with(struct cg_state *s, struct ast_with *with)
{
  if (cg_unreachable(s)) return;

  unsigned                   num_items = with->num_items;
  union scope_cleanup       *saved_scope_cleanup = s->code.scope_cleanup;
  struct with_scope_cleanup *cleanups = NULL;
  if (num_items > 0) {
    cleanups
        = (struct with_scope_cleanup *)calloc(num_items, sizeof(*cleanups));
    if (cleanups == NULL) internal_error("out of memory");

    for (unsigned i = 0; i < num_items; ++i) {
      struct ast_with_item *item = &with->items[i];
      emit_with_setup(s, &cleanups[i], item->expression, item->as_location,
                      item->target,
                      (with->base.flags & STATEMENT_WITH_ASYNC) != 0);
    }
  }

  emit_statement_list(s, with->body);
  s->code.scope_cleanup = saved_scope_cleanup;

  for (unsigned i = num_items; i-- > 0;) {
    emit_with_cleanup(s, &cleanups[i]);
  }
  free(cleanups);
}

static void emit_annotation_stmt(struct cg_state                 *s,
                                 struct ast_statement_annotation *annotation)
{
  if (annotation->value != NULL) {
    union ast_expression *targets[] = { annotation->target };
    emit_assign(s, 1, targets, annotation->value);
  }
  emit_annotation(s, annotation->target, annotation->annotation,
                  (annotation->base.flags & STATEMENT_ANNOTATION_SIMPLE) != 0);
}

static void emit_augassign(struct cg_state *s, struct ast_augassign *augassign)
{
  if (!cg_unreachable(s)) {
    emit_expression(s, augassign->expression);
  }
}

static void emit_expression_statement(struct cg_state                 *s,
                                      struct ast_expression_statement *expr)
{
  if (!cg_unreachable(s)) {
    emit_expression(s, expr->expression);
    cg_op_pop1(s,
               (expr->base.flags & STATEMENT_EXPRESSION_PRINT)
                   ? OPCODE_PRINT_EXPR
                   : OPCODE_POP_TOP,
               0);
  }
}

static void emit_raise(struct cg_state *s, struct ast_raise *raise)
{
  if (!cg_unreachable(s)) {
    unsigned args = 0;
    if (raise->expression != NULL) {
      emit_expression(s, raise->expression);
      args++;
      if (raise->from != NULL) {
        emit_expression(s, raise->from);
        args++;
      }
    }
    /* TODO: should this be a jump and end the block?
     * cpython compiler does not seem to think so, is this on purpose? */
    cg_op_pop_push(s, OPCODE_RAISE_VARARGS, args, /*pop=*/args, /*push=*/0);
  }
}

static bool is_side_effect_free(union ast_expression *expression)
{
  /* TODO: Any more cases here? */
  return ast_expression_type(expression) == AST_CONST;
}

static void emit_return(struct cg_state *s, struct ast_return *return_stmt)
{
  if (cg_unreachable(s)) return;

  unsigned saved_stacksize = s->code.stacksize;

  union ast_expression *expression = return_stmt->expression;
  bool                  side_effect_free
      = expression == NULL || is_side_effect_free(expression);
  bool has_scope_cleanup = (s->code.scope_cleanup != NULL);
  bool in_loop = (s->code.loop_state.break_block != NULL);
  bool use_value_passthrough
      = !side_effect_free && (!in_loop || !has_scope_cleanup);
  struct symbol *nullable tmp = NULL;
  if (!side_effect_free) {
    emit_expression(s, return_stmt->expression);
    if (!use_value_passthrough && has_scope_cleanup) {
      /* Loop iterator cleanup is not represented in scope_cleanup; stash the
       * return value so cleanup can freely unwind the runtime stack shape. */
      tmp = symbol_table_get_or_insert(s->symbol_table, "<pycomparse-return>");
      if (cg_in_function(s)) {
        cg_declare(s, tmp, SYMBOL_LOCAL);
      }
      cg_store(s, tmp);
    }
  }

  emit_scope_cleanup(s, /*stop=*/NULL,
                     /*value_passthrough=*/use_value_passthrough);

  if (tmp != NULL) {
    cg_load(s, tmp);
  }

  if (side_effect_free) {
    if (expression == NULL) {
      cg_load_const(s, object_intern_singleton(s->objects, OBJECT_NONE));
    } else {
      emit_expression(s, expression);
    }
  }
  cg_op_pop1(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);

  s->code.stacksize = saved_stacksize;
}

static void emit_yield_statement(struct cg_state *s, struct ast_yield *yield)
{
  if (!cg_unreachable(s)) {
    emit_yield(s, yield->expression);
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
  }
}

static void emit_yield_from_statement(struct cg_state  *s,
                                      struct ast_yield *yield_from)
{
  if (!cg_unreachable(s)) {
    emit_yield_from(s, yield_from->expression);
    cg_op_pop1(s, OPCODE_POP_TOP, 0);
  }
}

static void emit_statement(struct cg_state *s, union ast_statement *statement)
{
  cg_set_lineno(s, statement->base.location.line);
  switch (ast_statement_type(statement)) {
  case AST_STATEMENT_ANNOTATION:
    emit_annotation_stmt(s, &statement->annotation);
    return;
  case AST_STATEMENT_ASSERT:
    emit_assert(s, statement->assert.expression, statement->assert.message);
    return;
  case AST_STATEMENT_ASSIGN:
    emit_assign(s, statement->assign.num_targets, statement->assign.targets,
                statement->assign.value);
    return;
  case AST_STATEMENT_AUGASSIGN:
    emit_augassign(s, &statement->augassign);
    return;
  case AST_STATEMENT_BREAK:
    emit_break(s);
    return;
  case AST_STATEMENT_CLASS:
    emit_class(s, &statement->class_);
    return;
  case AST_STATEMENT_CONTINUE:
    emit_continue(s);
    return;
  case AST_STATEMENT_DEF:
    emit_def(s, &statement->def);
    return;
  case AST_STATEMENT_DEL:
    emit_del(s, statement->del.targets);
    return;
  case AST_STATEMENT_EXPRESSION:
    emit_expression_statement(s, &statement->expression);
    return;
  case AST_STATEMENT_FOR:
    emit_for(s, &statement->for_);
    return;
  case AST_STATEMENT_FROM_IMPORT:
    emit_from_import_node(s, &statement->from_import);
    return;
  case AST_STATEMENT_GLOBAL:
    emit_global_statement_node(s, statement->base.location,
                               &statement->global);
    return;
  case AST_STATEMENT_IF:
    emit_if(s, &statement->if_);
    return;
  case AST_STATEMENT_IMPORT:
    emit_import_node(s, &statement->import);
    return;
  case AST_STATEMENT_NONLOCAL:
    emit_nonlocal_statement_node(s, statement->base.location,
                                 &statement->nonlocal);
    return;
  case AST_STATEMENT_PASS:
    return;
  case AST_STATEMENT_RAISE:
    emit_raise(s, &statement->raise);
    return;
  case AST_STATEMENT_RETURN:
    emit_return(s, &statement->return_);
    return;
  case AST_STATEMENT_TRY:
    emit_try(s, &statement->try_);
    return;
  case AST_STATEMENT_WHILE:
    emit_while(s, &statement->while_);
    return;
  case AST_STATEMENT_WITH:
    emit_with(s, &statement->with);
    return;
  case AST_STATEMENT_YIELD:
    emit_yield_statement(s, &statement->yield);
    return;
  case AST_STATEMENT_YIELD_FROM:
    emit_yield_from_statement(s, &statement->yield);
    return;
  default:
    internal_error("invalid statement");
  }
}

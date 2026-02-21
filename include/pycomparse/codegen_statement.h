#pragma once

#include <stdbool.h>

#include "pycomparse/codegen_types.h"
#include "pycomparse/nullable.h"
#include "pycomparse/opcodes.h"
#include "pycomparse/scanner_location.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct ast_call;
struct ast_comparison;
struct ast_generator_expression;
struct ast_lambda;
struct ast_module;
struct ast_statement_list;
struct basic_block;
struct cg_state;
struct dotted_name;
struct from_import_item;
struct parameter;
struct parameter_shape;
struct symbol;
union ast_expression;
union ast_statement;

struct make_function_state {
  bool                               annotations;
  bool                               closure;
  bool                               defaults;
  bool                               keyword_defaults;
  unsigned                           num_closure_symbols;
  struct symbol * nonnull * nullable closure_symbols;
  const char                        *qualname;
};

struct for_while_state {
  struct basic_block *else_or_footer;
  struct loop_state   saved;
  bool                async_for;
};

struct if_state {
  struct basic_block *nullable else_or_footer;
  struct basic_block *nullable footer;
};

struct try_state {
  struct basic_block *nullable setup_finally;
  struct basic_block *nullable setup_except;
  struct basic_block *nullable body_exit;
  struct basic_block *nullable excepts;
  struct basic_block *nullable except_unassign_as;
  struct basic_block *nullable else_block;
  struct basic_block *nullable enter_finally;
  struct basic_block *nullable finally_body;
  struct basic_block *nullable footer;
  bool                         try_reachable;
  bool                         had_except;
  bool                         had_else;
  bool                         had_finally;
  bool                         finally_needs_placeholder;
};

struct with_state {
  struct basic_block *cleanup;
  bool                async_with;
};

union object *emit_module(struct cg_state *s, struct ast_module *module);

void emit_code_end(struct cg_state *s);

void emit_comparison_multi_value(struct cg_state       *s,
                                 struct ast_comparison *comparison);

void emit_condjump_expr(struct cg_state *s, union ast_expression *expression,
                        struct basic_block *true_block,
                        struct basic_block *false_block,
                        struct basic_block *next);

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression);

void emit_make_function_begin(struct cg_state               *s,
                              struct make_function_state    *state,
                              const struct parameter_shape  *parameter_shape,
                              struct parameter              *parameters,
                              bool                           async_function,
                              union ast_expression *nullable return_type,
                              const char *name, bool global_binding);
void emit_make_function_end(struct cg_state            *s,
                            struct make_function_state *state,
                            struct symbol              *symbol);

void analyze_lambda_bindings(struct cg_state *s, struct ast_lambda *lambda);
void analyze_generator_bindings(struct cg_state                 *s,
                                struct ast_generator_expression *generator);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

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

struct ast_comparison;
struct ast_generator_expression;
struct ast_lambda;
struct ast_module;
struct basic_block;
struct cg_state;
struct parameter;
struct parameter_shape;
struct symbol;
union ast_expression;

struct make_function_state {
  bool                               annotations;
  bool                               closure;
  bool                               defaults;
  bool                               keyword_defaults;
  unsigned                           num_closure_symbols;
  struct symbol * nonnull * nullable closure_symbols;
  const char                        *qualname;
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

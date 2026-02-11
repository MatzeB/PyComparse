#pragma once

#include <stdbool.h>

#include "codegen_types.h"
#include "nullable.h"
#include "opcodes.h"

ASSUME_NONNULL_BEGIN

struct ast_call;
struct ast_comparison;
struct ast_generator_expression;
struct ast_statement_list;
struct basic_block;
struct cg_state;
struct dotted_name;
struct from_import_item;
struct parameter;
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
};

struct for_while_state {
  struct basic_block *else_or_footer;
  struct loop_state   saved;
};

struct if_state {
  struct basic_block *nullable else_or_footer;
  struct basic_block *nullable footer;
};

struct try_state {
  struct basic_block *setup_finally;
  struct basic_block *setup_except;
  struct basic_block *body_exit;
  struct basic_block *excepts;
  struct basic_block *except_unassign_as;
  struct basic_block *else_block;
  struct basic_block *enter_finally;
  struct basic_block *footer;
  bool                try_reachable;
  bool                had_except;
  bool                had_else;
  bool                had_finally;
};

struct with_state {
  struct basic_block *cleanup;
};

void          emit_module_begin(struct cg_state *s);
union object *emit_module_end(struct cg_state *s);

void emit_code_end(struct cg_state *s);

void emit_comparison_multi_value(struct cg_state       *s,
                                 struct ast_comparison *comparison);

void emit_condjump_expr(struct cg_state *s, union ast_expression *expression,
                        struct basic_block *true_block,
                        struct basic_block *false_block,
                        struct basic_block *next);

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression);

void emit_make_function_begin(struct cg_state            *s,
                              struct make_function_state *state,
                              unsigned                    num_parameters,
                              struct parameter           *parameters,
                              unsigned positional_only_argcount,
                              union ast_expression *nullable return_type);
void emit_make_function_end(struct cg_state            *s,
                            struct make_function_state *state,
                            struct symbol              *symbol);

void emit_statement_list(struct cg_state           *s,
                         struct ast_statement_list *statement_list);

ASSUME_NONNULL_END

#pragma once

#include <stdbool.h>

#include "codegen_types.h"
#include "nullable.h"
#include "opcodes.h"

ASSUME_NONNULL_BEGIN

struct ast_call;
struct ast_generator_expression;
struct basic_block;
struct cg_state;
struct dotted_name;
struct symbol;
union ast_expression;

struct def_state {
  bool defaults;
  bool keyword_defaults;
};

struct dotted_name {
  unsigned               num_symbols;
  struct symbol *nonnull symbols[];
};

struct for_while_state {
  struct basic_block *else_or_footer;
  struct loop_state   saved;
};

struct from_import_item {
  struct symbol          *name;
  struct symbol *nullable as;
};

struct if_state {
  struct basic_block *nullable else_or_footer;
  struct basic_block *nullable footer;
};

enum parameter_type {
  PARAMETER_NORMAL,
  PARAMETER_STAR,
  PARAMETER_STAR_STAR,
};

struct parameter {
  struct symbol                 *name;
  union ast_expression *nullable initializer;
  enum parameter_type            type;
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

void emit_assert(struct cg_state *s, union ast_expression *expression,
                 union ast_expression *message);

bool emit_break(struct cg_state *s);

void emit_class_begin(struct cg_state *s, struct symbol *name);
void emit_class_end(struct cg_state *s, struct symbol *name,
                    struct ast_call *call, unsigned num_decorators);

void emit_code_end(struct cg_state *s);

bool emit_continue(struct cg_state *s);

void emit_def_begin(struct cg_state *s, struct def_state *state,
                    unsigned num_parameters, struct parameter *parameters,
                    unsigned positional_only_argcount);
void emit_def_end(struct cg_state *s, struct def_state *state,
                  struct symbol *symbol, unsigned num_decorators);

void emit_del(struct cg_state *s, union ast_expression *target);

void emit_expression_statement(struct cg_state      *s,
                               union ast_expression *expression);

void emit_for_begin(struct cg_state *s, struct for_while_state *state,
                    union ast_expression *target,
                    union ast_expression *expression);
void emit_for_else(struct cg_state *s, struct for_while_state *state);
void emit_for_end(struct cg_state *s, struct for_while_state *state);

void emit_from_import_statement(struct cg_state *s, unsigned num_prefix_dots,
                                struct dotted_name *module, unsigned num_pairs,
                                struct from_import_item *nullable items);
void emit_from_import_star_statement(struct cg_state    *s,
                                     unsigned            num_prefix_dots,
                                     struct dotted_name *module);

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression);

void emit_if_begin(struct cg_state *s, struct if_state *state,
                   union ast_expression *expression);
void emit_if_elif(struct cg_state *s, struct if_state *state,
                  union ast_expression *expression);
void emit_if_else(struct cg_state *s, struct if_state *state);
void emit_if_end(struct cg_state *s, struct if_state *state);

void emit_import_statement(struct cg_state *s, struct dotted_name *module,
                           struct symbol *as);

void emit_raise_statement(struct cg_state *s, union ast_expression *expression,
                          union ast_expression *nullable from);

void emit_return_statement(struct cg_state               *s,
                           union ast_expression *nullable expression);

void emit_try_body_begin(struct cg_state *s, struct try_state *state);
void emit_try_body_end(struct cg_state *s, struct try_state *state);
void emit_try_else_begin(struct cg_state *s, struct try_state *state);
void emit_try_else_end(struct cg_state *s, struct try_state *state);
void emit_try_except_begin(struct cg_state *s, struct try_state *state,
                           union ast_expression   *match,
                           struct symbol *nullable as);
void emit_try_except_end(struct cg_state *s, struct try_state *state,
                         struct symbol *nullable as);
void emit_try_finally_begin(struct cg_state *s, struct try_state *state);
void emit_try_finally_end(struct cg_state *s, struct try_state *state);
void emit_try_end(struct cg_state *s, struct try_state *state);

void emit_with_begin(struct cg_state *s, struct with_state *state,
                     union ast_expression *expression,
                     union ast_expression *target);
void emit_with_end(struct cg_state *s, struct with_state *state);

void emit_while_begin(struct cg_state *s, struct for_while_state *state,
                      union ast_expression *expression);
void emit_while_else(struct cg_state *s, struct for_while_state *state);
void emit_while_end(struct cg_state *s, struct for_while_state *state);

void emit_yield_statement(struct cg_state *s, union ast_expression *value);
void emit_yield_from_statement(struct cg_state      *s,
                               union ast_expression *value);

ASSUME_NONNULL_END

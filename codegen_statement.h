#pragma once

#include <stdbool.h>

#include "opcodes.h"

#include "codegen_types.h"

struct argument;
struct ast_call;
struct ast_generator_expression;
struct basic_block;
struct cg_state;
struct dotted_name;
struct from_import_pair;
struct symbol;
union ast_expression;

struct if_state {
  struct basic_block *else_or_footer;
};

struct for_while_state {
  struct basic_block *else_or_footer;
  struct loop_state   saved;
};

struct with_state {
  struct basic_block *cleanup;
};

enum parameter_type {
  PARAMETER_NORMAL,
  PARAMETER_STAR,
  PARAMETER_STAR_STAR,
};

struct parameter {
  struct symbol        *name;
  union ast_expression *initializer;
  enum parameter_type   type;
};

struct def_state {
  bool defaults;
  bool keyword_defaults;
};

void          emit_module_begin(struct cg_state *s);
union object *emit_module_end(struct cg_state *s);

void emit_assert(struct cg_state *s, union ast_expression *expression,
                 union ast_expression *message);

void emit_expression_statement(struct cg_state      *s,
                               union ast_expression *expression);
void emit_import_statement(struct cg_state *s, struct dotted_name *module,
                           struct symbol *as);
void emit_from_import_statement(struct cg_state *s, unsigned num_prefix_dots,
                                struct dotted_name *module, unsigned num_pairs,
                                struct from_import_pair *pairs);
void emit_from_import_star_statement(struct cg_state    *s,
                                     unsigned            num_prefix_dots,
                                     struct dotted_name *module);
void emit_return_statement(struct cg_state      *s,
                           union ast_expression *expression);

void emit_class_begin(struct cg_state *s, struct symbol *name);
void emit_class_end(struct cg_state *s, struct symbol *name,
                    struct ast_call *call, unsigned num_decorators);

void emit_if_begin(struct cg_state *s, struct if_state *state,
                   union ast_expression *expression);
void emit_else_begin(struct cg_state *s, struct if_state *state);
void emit_if_end(struct cg_state *s, struct if_state *state);

void emit_for_begin(struct cg_state *s, struct for_while_state *state,
                    union ast_expression *target,
                    union ast_expression *expression);
void emit_for_else(struct cg_state *s, struct for_while_state *state);
void emit_for_end(struct cg_state *s, struct for_while_state *state);

bool emit_continue(struct cg_state *s);
bool emit_break(struct cg_state *s);

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression);

void emit_while_begin(struct cg_state *s, struct for_while_state *state,
                      union ast_expression *expression);
void emit_while_else(struct cg_state *s, struct for_while_state *state);
void emit_while_end(struct cg_state *s, struct for_while_state *state);

void emit_with_begin(struct cg_state *s, struct with_state *state,
                     union ast_expression *expression,
                     union ast_expression *target);
void emit_with_end(struct cg_state *s, struct with_state *state);

void emit_def_begin(struct cg_state *s, struct def_state *state,
                    unsigned num_parameters, struct parameter *parameters,
                    unsigned positional_only_argcount);
void emit_def_end(struct cg_state *s, struct def_state *state,
                  struct symbol *symbol, unsigned num_decorators);

void emit_condjump(struct cg_state *s, enum opcode opcode,
                   struct basic_block *target,
                   struct basic_block *fallthrough);
void emit_jump(struct cg_state *s, struct basic_block *target);

void emit_code_end(struct cg_state *s);

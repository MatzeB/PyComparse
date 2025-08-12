#pragma once

#include <stdbool.h>

struct argument;
struct ast_generator_expression;
struct basic_block;
struct cg_state;
struct dotted_name;
struct from_import_pair;
struct symbol;
union ast_expression;

struct if_state {
  struct basic_block *false_or_footer;
};

struct for_state {
  struct basic_block *header;
  struct basic_block *body;
  struct basic_block *after;
};

struct while_state {
  struct basic_block *header;
  struct basic_block *body;
  struct basic_block *after;
};

void          emit_module_begin(struct cg_state *s);
union object *emit_module_end(struct cg_state *s);

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
                    struct argument *arguments, unsigned num_decorators);

void emit_if_begin(struct cg_state *s, struct if_state *state,
                   union ast_expression *expression);
void emit_else_begin(struct cg_state *s, struct if_state *state);
void emit_if_end(struct cg_state *s, struct if_state *state);

void emit_for_begin(struct cg_state *s, struct for_state *state,
                    union ast_expression *target,
                    union ast_expression *expression);
void emit_for_end(struct cg_state *s, struct for_state *state);
void emit_continue(struct cg_state *s, struct for_state *state);

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression);

void emit_while_begin(struct cg_state *s, struct while_state *state,
                      union ast_expression *expression);
void emit_while_end(struct cg_state *s, struct while_state *state);

void emit_def_begin(struct cg_state *s);
bool emit_parameter(struct cg_state *s, struct symbol *symbol);
void emit_def_end(struct cg_state *s, struct symbol *symbol,
                  unsigned num_decorators);

void emit_code_end(struct cg_state *s);

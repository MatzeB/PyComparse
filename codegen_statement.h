#pragma once

#include <stdbool.h>

struct basic_block;
struct cg_state;
struct dotted_name;
struct symbol;
union ast_node;

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

void emit_module_begin(struct cg_state *s);
union object *emit_module_end(struct cg_state *s);

void emit_expression_statement(struct cg_state *s, union ast_node *expression);
void emit_import_statement(struct cg_state *s, struct dotted_name *name,
                           struct symbol *as);
void emit_return_statement(struct cg_state *s, union ast_node *expression);

void emit_if_begin(struct cg_state *s, struct if_state *state,
                   union ast_node *expression);
void emit_else_begin(struct cg_state *s, struct if_state *state);
void emit_if_end(struct cg_state *s, struct if_state *state);

void emit_for_begin(struct cg_state *s, struct for_state *state,
                    union ast_node *target, union ast_node *expression);
void emit_for_end(struct cg_state *s, struct for_state *state);

void emit_while_begin(struct cg_state *s, struct while_state *state,
                      union ast_node *expression);
void emit_while_end(struct cg_state *s, struct while_state *state);

void emit_def_begin(struct cg_state *s);
bool emit_parameter(struct cg_state *s, struct symbol *symbol);
void emit_def_end(struct cg_state *s, struct symbol *symbol);

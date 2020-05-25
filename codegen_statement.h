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
  struct basic_block *footer;
  struct basic_block *after;
};

struct while_state {
  struct basic_block *header;
  struct basic_block *body;
  struct basic_block *footer;
  struct basic_block *after;
};

void emit_expression_statement(struct cg_state *s, union ast_node *expression);
void emit_import_statement(struct cg_state *s, struct dotted_name *name,
                           struct symbol *as);
void emit_return_statement(struct cg_state *s, union ast_node *expression);

void emit_begin_if(struct cg_state *s, struct if_state *state,
                   union ast_node *expression);
void emit_begin_else(struct cg_state *s, struct if_state *state);
void emit_end_if(struct cg_state *s, struct if_state *state);

void emit_begin_for(struct cg_state *s, struct for_state *state,
                    union ast_node *target, union ast_node *expression);
void emit_end_for(struct cg_state *s, struct for_state *state);

void emit_begin_while(struct cg_state *s, struct while_state *state,
                      union ast_node *expression);
void emit_end_while(struct cg_state *s, struct while_state *state);

void emit_begin_def(struct cg_state *s);
bool emit_parameter(struct cg_state *s, struct symbol *symbol);
void emit_end_def(struct cg_state *s, struct symbol *symbol);

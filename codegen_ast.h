#pragma once

#include <stdbool.h>

struct cg_state;
struct symbol;
union ast_node;

void emit_assignment(struct cg_state *cg, union ast_node *target);

void emit_expression(struct cg_state *cg, union ast_node *expression);

void emit_expression_drop_result(struct cg_state *cg,
                                 union ast_node *expression);

void emit_store(struct cg_state *cg, struct symbol *symbol);

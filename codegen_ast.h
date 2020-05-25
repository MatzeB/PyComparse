#pragma once

#include <stdbool.h>

struct cg_state;
struct symbol;
union ast_node;

void emit_expression(struct cg_state *cg, union ast_node *expression,
                     bool drop);

void emit_store(struct cg_state *cg, struct symbol *symbol);

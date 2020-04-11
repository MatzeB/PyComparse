#pragma once

#include <stdbool.h>

struct cg_state;
union ast_node;

void emit_expression(struct cg_state *cg, union ast_node *expression,
                     bool drop);

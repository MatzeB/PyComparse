#pragma once

#include <stdbool.h>

struct argument;
struct cg_state;
struct symbol;
union ast_expression;

void emit_assignment(struct cg_state *cg, union ast_expression *target);

void emit_expression(struct cg_state *cg, union ast_expression *expression);

void emit_expression_drop_result(struct cg_state      *cg,
                                 union ast_expression *expression);

unsigned emit_arguments(struct cg_state *cg, struct argument *arguments);

void emit_load(struct cg_state *cg, struct symbol *symbol);
void emit_store(struct cg_state *cg, struct symbol *symbol);

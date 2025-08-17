#pragma once

struct ast_call;
struct cg_state;
union ast_expression;

void emit_assignment(struct cg_state *cg, union ast_expression *target);

void emit_expression(struct cg_state *cg, union ast_expression *expression);

void emit_expression_drop_result(struct cg_state      *cg,
                                 union ast_expression *expression);

void emit_call_helper(struct cg_state *cg, struct ast_call *call,
                      unsigned num_extra_args);

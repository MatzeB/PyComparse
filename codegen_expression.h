#pragma once

#include "nullable.h"
#include "scanner_location.h"

ASSUME_NONNULL_BEGIN

struct ast_call;
struct cg_state;
union ast_expression;

void emit_assignment(struct cg_state *cg, union ast_expression *target);

void emit_expression(struct cg_state *cg, union ast_expression *expression);

void emit_call_helper(struct cg_state *cg, struct ast_call *call,
                      unsigned num_extra_args);

void emit_yield(struct cg_state *cg, union ast_expression *nullable value,
                struct location location);
void emit_yield_from(struct cg_state *cg, union ast_expression *nullable value,
                     struct location location);

ASSUME_NONNULL_END

#pragma once

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct ast_statement_list;
struct arena;
struct object_intern;

struct ast_statement_list *ast_fold_constants(struct object_intern      *intern,
                                              struct arena              *ast_arena,
                                              struct ast_statement_list *module);

ASSUME_NONNULL_END

#pragma once

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct ast_module;
struct arena;
struct object_intern;

void ast_fold_constants(struct object_intern *intern, struct arena *ast_arena,
                        struct ast_module *module);

ASSUME_NONNULL_END

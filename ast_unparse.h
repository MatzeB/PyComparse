#pragma once

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct object_intern;
union ast_expression;
union object;

union object *ast_unparse_expression(struct object_intern *intern,
                                     union ast_expression *expression);

ASSUME_NONNULL_END

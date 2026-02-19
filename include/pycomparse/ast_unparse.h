#pragma once

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct object_intern;
union ast_expression;
union object;

union object *ast_unparse_expression(struct object_intern *intern,
                                     union ast_expression *expression);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

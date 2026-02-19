#pragma once

#include <stdio.h>

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

union object;

void write_single_object(FILE *out, const union object *object);
void write_module(FILE *out, const union object *object);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

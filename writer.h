#pragma once

#include <stdio.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

union object;

void write_single_object(FILE *out, const union object *object);
void write_module(FILE *out, const union object *object);

ASSUME_NONNULL_END

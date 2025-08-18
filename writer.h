#pragma once

#include <stdio.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

union object;

void write_module(FILE *out, const union object *object);

ASSUME_NONNULL_END

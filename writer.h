#pragma once

#include <stdio.h>

union object;

void write(FILE *out, const union object *object);

#pragma once

#include <stdio.h>

union object;

void write_module(FILE *out, const union object *object);

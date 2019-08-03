#ifndef WRITER_H
#define WRITER_H

#include <stdio.h>

union object;

void write(FILE *out, const union object *object);

#endif

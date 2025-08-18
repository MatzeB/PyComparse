#pragma once

#include <stdint.h>

#include "object.h"

struct arena;
struct object_intern;
union object;

void object_intern_init(struct object_intern *s);
void object_intern_free(struct object_intern *s);

union object *object_intern_cstring(struct object_intern *s,
                                    const char           *cstring);
union object *object_intern_int(struct object_intern *s, int64_t value);
union object *object_intern_float(struct object_intern *s, double value);
union object *object_intern_singleton(struct object_intern *s,
                                      enum object_type      type);
union object *object_intern_string(struct object_intern *s,
                                   enum object_type type, uint32_t length,
                                   const char *chars);
union object *object_intern_empty_tuple(struct object_intern *s);

struct arena *object_intern_arena(struct object_intern *s);

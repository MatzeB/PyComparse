#pragma once

#include <stdint.h>
#include <stdbool.h>

struct arena;

enum object_type {
  TYPE_NULL     = '0',
  TYPE_NONE     = 'N',
  TYPE_TRUE     = 'T',
  TYPE_FALSE    = 'F',
  TYPE_STRING   = 's',
  TYPE_UNICODE  = 'u',
  TYPE_CODE     = 'c',
  TYPE_TUPLE    = '(',
  TYPE_LIST     = '[',
  TYPE_ELLIPSIS = '.',

  TYPE_ASCII   = 'a',
  TYPE_INT     = 'i',

  TYPE_SHORT_ASCII = 'z',
  TYPE_SMALL_TUPLE = ')',
};

union object;

union object *object_new_singleton(struct arena *arena, char type);
union object *object_new_code(struct arena *arena);
union object *object_new_list(struct arena *arena);
union object *object_new_tuple(struct arena *arena, uint32_t length);
union object *make_string(struct arena *arena, char type, uint32_t length,
                          const char *chars);
union object *make_int(struct arena *arena, int32_t value);
union object *make_bytes(struct arena *arena, uint32_t length,
                         const char *chars);
union object *make_ascii(struct arena *arena, const char *str);

void object_list_append(union object *list, union object *object);

bool object_type_is_singleton(char type);

bool objects_equal(const union object *object0, const union object *object1);

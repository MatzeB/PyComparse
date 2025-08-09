#include "object_intern.h"

#include <assert.h>
#include <string.h>

#include "object_intern_types.h"

void object_intern_init(struct object_intern *s)
{
  memset(s, 0, sizeof(*s));
  s->objects = object_new_list(&s->arena);
  s->singleton_none = object_new_singleton(&s->arena, OBJECT_NONE);
  s->singleton_true = object_new_singleton(&s->arena, OBJECT_TRUE);
  s->singleton_false = object_new_singleton(&s->arena, OBJECT_FALSE);
  s->singleton_ellipsis = object_new_singleton(&s->arena, OBJECT_ELLIPSIS);

  union object *empty_tuple = object_new_tuple_begin(&s->arena, 0);
  object_new_tuple_end(empty_tuple);
  s->empty_tuple = empty_tuple;
}

void object_intern_free(struct object_intern *s)
{
  arena_free_all(&s->arena);
}

struct arena *object_intern_arena(struct object_intern *s)
{
  return &s->arena;
}

union object *object_intern_singleton(struct object_intern *s,
                                      enum object_type type)
{
  switch (type) {
  case OBJECT_NONE: return s->singleton_none;
  case OBJECT_TRUE: return s->singleton_true;
  case OBJECT_FALSE: return s->singleton_false;
  case OBJECT_ELLIPSIS: return s->singleton_ellipsis;
  default:
    abort();
  }
}

union object *object_intern_empty_tuple(struct object_intern *s)
{
  return s->empty_tuple;
}

union object *object_intern_string(struct object_intern *s, enum object_type type,
                                   uint32_t length, const char *chars)
{
  assert(type == OBJECT_STRING || type == OBJECT_ASCII);
  // TODO: hashmap
  for (uint32_t i = 0, l = object_list_length(s->objects); i < l; i++) {
    union object *object = object_list_at(s->objects, i);
    if (object_type(object) == type &&
        object_string_equals(object, length, chars)) {
      return object;
    }
  }

  union object *string = object_new_string(&s->arena, type, length, chars);
  object_list_append(s->objects, string);
  return string;
}

union object *object_intern_cstring(struct object_intern *s,
                                    const char *cstring)
{
  size_t length = strlen(cstring);
  assert(length == (uint32_t)length);
  return object_intern_string(s, OBJECT_ASCII, (uint32_t)length, cstring);
}

union object *object_intern_int(struct object_intern *s, int64_t value)
{
  // TODO: hashmap
  for (uint32_t i = 0, l = object_list_length(s->objects); i < l; i++) {
    union object *object = object_list_at(s->objects, i);
    if (object_type(object) == OBJECT_INT &&
        object_int_value(object) == value) {
      return object;
    }
  }

  union object *result = object_new_int(&s->arena, value);
  object_list_append(s->objects, result);
  return result;
}

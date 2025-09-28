#include "object_intern.h"

#include <assert.h>
#include <string.h>

#include "object_intern_types.h"
#include "util.h"

void object_intern_init(struct object_intern *s)
{
  memset(s, 0, sizeof(*s));
  s->objects = object_new_list(&s->arena);
  s->singleton_none = object_new_singleton(&s->arena, OBJECT_NONE);
  s->singleton_true = object_new_singleton(&s->arena, OBJECT_TRUE);
  s->singleton_false = object_new_singleton(&s->arena, OBJECT_FALSE);
  s->singleton_ellipsis = object_new_singleton(&s->arena, OBJECT_ELLIPSIS);

  struct tuple_prep *empty_tuple_prep = object_new_tuple_begin(&s->arena, 0);
  s->empty_tuple = object_new_tuple_end(empty_tuple_prep);
}

void object_intern_free(struct object_intern *s)
{
  arena_free(&s->arena);
}

struct arena *object_intern_arena(struct object_intern *s)
{
  return &s->arena;
}

union object *object_intern_singleton(struct object_intern *s,
                                      enum object_type      type)
{
  switch (type) {
  case OBJECT_NONE:
    return s->singleton_none;
  case OBJECT_TRUE:
    return s->singleton_true;
  case OBJECT_FALSE:
    return s->singleton_false;
  case OBJECT_ELLIPSIS:
    return s->singleton_ellipsis;
  default:
    internal_error("object_intern_singleton called with non-singleton");
  }
}

union object *object_intern_empty_tuple(struct object_intern *s)
{
  return s->empty_tuple;
}

union object *object_intern_string(struct object_intern *s,
                                   enum object_type type, uint32_t length,
                                   const char *chars)
{
  assert(type == OBJECT_STRING || type == OBJECT_BYTES);
  // TODO: hashmap
  for (uint32_t i = 0, l = object_list_length(s->objects); i < l; i++) {
    union object *object = object_list_at(s->objects, i);
    if (object_type(object) == type
        && object_string_equals(object, length, chars)) {
      return object;
    }
  }

  union object *string = object_new_string(&s->arena, type, length, chars);
  object_list_append(s->objects, string);
  return string;
}

union object *object_intern_cstring(struct object_intern *s,
                                    const char           *cstring)
{
  size_t length = strlen(cstring);
  assert(length == (uint32_t)length);
  return object_intern_string(s, OBJECT_STRING, (uint32_t)length, cstring);
}

union object *object_intern_float(struct object_intern *s, double value)
{
  // TODO: hashmap
  for (uint32_t i = 0, l = object_list_length(s->objects); i < l; i++) {
    union object *object = object_list_at(s->objects, i);
    if (object_type(object) != OBJECT_FLOAT) continue;
    double object_value = object_float_value(object);
    if (memcmp(&object_value, &value, sizeof(value)) == 0) {
      return object;
    }
  }

  union object *result = object_new_float(&s->arena, value);
  object_list_append(s->objects, result);
  return result;
}

union object *object_intern_int(struct object_intern *s, uint64_t value)
{
  // TODO: hashmap
  for (uint32_t i = 0, l = object_list_length(s->objects); i < l; i++) {
    union object *object = object_list_at(s->objects, i);
    if (object_type(object) == OBJECT_INT
        && object_int_value(object) == value) {
      return object;
    }
  }

  union object *result = object_new_int(&s->arena, value);
  object_list_append(s->objects, result);
  return result;
}

struct tuple_prep *object_intern_tuple_begin(struct object_intern *s,
                                             uint32_t              length)
{
  return object_new_tuple_begin(&s->arena, length);
}

union object *object_intern_tuple_end(struct object_intern *s,
                                      struct tuple_prep    *tuple,
                                      bool                  may_free_arena)
{
  union object *tuple_obj = object_new_tuple_end(tuple);

  // TODO: hashmap
  for (uint32_t i = 0, l = object_list_length(s->objects); i < l; i++) {
    union object *object = object_list_at(s->objects, i);
    if (object_type(object) != OBJECT_TUPLE) continue;
    uint32_t length = object_tuple_length(tuple_obj);
    if (object_tuple_length(object) != length) continue;
    bool equal = true;
    for (uint32_t i = 0; i < length; i++) {
      if (object_tuple_at(object, i) != object_tuple_at(tuple_obj, i)) {
        equal = false;
        break;
      }
    }
    if (equal) {
      if (may_free_arena) {
        arena_free_to(&s->arena, tuple_obj);
      }
      return object;
    }
  }

  object_list_append(s->objects, tuple_obj);
  return tuple_obj;
}

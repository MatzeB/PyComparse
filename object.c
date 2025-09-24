#include "object.h"
#include "object_types.h"

#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "adt/dynmemory.h"
#include "util.h"

#define OBJECT_TUPLE_CONSTRUCTION '\1'

static union object *object_allocate_zero_(struct arena *arena, size_t size,
                                           char type)
{
  union object *object
      = (union object *)arena_allocate(arena, size, alignof(union object));
  memset(object, 0, size);
  object->type = type;
  return object;
}

#define object_allocate_zero(arena, type, type_id)                            \
  object_allocate_zero_((arena), sizeof(type), type_id)

union object *object_new_list(struct arena *arena)
{
  return object_allocate_zero(arena, struct object_list, OBJECT_LIST);
}

static void object_list_grow(struct object_list *olist, unsigned size)
{
  olist->items = (union object **)dynmemory_grow(
      olist->items, &olist->capacity, size, sizeof(olist->items[0]));
  if (olist->items == NULL) {
    internal_error("out of memory");
  }
}

void object_list_append(union object *list, union object *object)
{
  assert(list->type == OBJECT_LIST);
  struct object_list *olist = &list->list;
  unsigned            length = olist->length;
  unsigned            new_length = length + 1;
  if (UNLIKELY(new_length >= olist->capacity)) {
    object_list_grow(olist, new_length);
  }
  olist->items[length] = object;
  olist->length = new_length;
}

union object *object_list_at(union object *list, uint32_t index)
{
  assert(list->type == OBJECT_LIST);
  struct object_list *olist = &list->list;
  assert(index < olist->length);
  return olist->items[index];
}

uint32_t object_list_length(union object *list)
{
  return list->list.length;
}

union object *object_new_code(struct arena *arena)
{
  return object_allocate_zero(arena, struct object_code, OBJECT_CODE);
}

struct tuple_prep *object_new_tuple_begin(struct arena *arena, uint32_t length)
{
  assert(length < UINT32_MAX);
  union object *object;
  size_t        size
      = sizeof(struct object_tuple) + length * sizeof(object->tuple.items[0]);
  object = object_allocate_zero_(arena, size, OBJECT_TUPLE_CONSTRUCTION);
  object->tuple.length = length;
#ifndef NDEBUG
  for (uint32_t i = 0; i < length; i++) {
    object->tuple.items[i] = (union object *)(-1);
  }
#endif
  return (struct tuple_prep *)object;
}

void object_new_tuple_set_at(struct tuple_prep *tuple, uint32_t index,
                             union object *object)
{
  union object *tuple_obj = (union object *)tuple;
  assert(tuple_obj->type == OBJECT_TUPLE_CONSTRUCTION);
  assert(index < tuple_obj->tuple.length);
  tuple_obj->tuple.items[index] = object;
}

union object *object_new_tuple_end(struct tuple_prep *tuple)
{
  union object *object = (union object *)tuple;
  assert(object->type == OBJECT_TUPLE_CONSTRUCTION);
#ifndef NDEBUG
  for (uint32_t i = 0; i < object->tuple.length; i++) {
    assert(object->tuple.items[i] != (union object *)(-1));
  }
#endif
  object->type = OBJECT_TUPLE;
  return object;
}

static bool object_type_is_singleton(enum object_type type)
{
  switch (type) {
  case OBJECT_ELLIPSIS:
  case OBJECT_FALSE:
  case OBJECT_NONE:
  case OBJECT_TRUE:
    return true;
  default:
    return false;
  }
}

static bool object_type_is_string(enum object_type type)
{
  return type == OBJECT_STRING || type == OBJECT_BYTES;
}

enum object_type object_type(const union object *object)
{
  return object->type;
}

union object *object_new_singleton(struct arena *arena, enum object_type type)
{
  assert(object_type_is_singleton(type));
  return object_allocate_zero(arena, struct object_base, type);
}

union object *object_new_string(struct arena *arena, enum object_type type,
                                uint32_t length, const char *chars)
{
  assert(chars != NULL);
  assert(length < UINT32_MAX);
  assert(object_type_is_string(type));
  union object *object
      = object_allocate_zero(arena, struct object_string, type);
  object->string.length = length;
  object->string.chars = chars;
  return object;
}

union object *object_new_float(struct arena *arena, double value)
{
  union object *object
      = object_allocate_zero(arena, struct object_float, OBJECT_FLOAT);
  object->float_obj.value = value;
  return object;
}

union object *object_new_int(struct arena *arena, int64_t value)
{
  union object *object
      = object_allocate_zero(arena, struct object_int, OBJECT_INT);
  object->int_obj.value = value;
  return object;
}

bool object_string_equals(const union object *object, uint32_t length,
                          const char *chars)
{
  assert(object_type_is_string(object->type));
  if (object->string.length != length) {
    return false;
  }
  if (length == 0) return true;
  return memcmp(object->string.chars, chars, length) == 0;
}

uint32_t object_string_length(const union object *object)
{
  assert(object_type_is_string(object->type));
  return object->string.length;
}

uint32_t object_tuple_length(const union object *object)
{
  assert(object->type == OBJECT_TUPLE);
  return object->tuple.length;
}

union object *object_tuple_at(union object *object, uint32_t index)
{
  assert(object->type == OBJECT_TUPLE);
  assert(index <= object->tuple.length);
  return object->tuple.items[index];
}

double object_float_value(const union object *object)
{
  assert(object->type == OBJECT_FLOAT);
  return object->float_obj.value;
}

int64_t object_int_value(const union object *object)
{
  assert(object->type == OBJECT_INT);
  return object->int_obj.value;
}

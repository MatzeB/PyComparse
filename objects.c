#include "objects.h"
#include "objects_types.h"

#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "adt/dynmemory.h"

bool objects_equal(const union object *object0, const union object *object1)
{
  if (object0 == NULL) {
    return object1 == NULL;
  }
  char type = object0->type;
  if (type != object1->type)
    return false;
  switch (type) {
  case TYPE_NONE:
  case TYPE_TRUE:
  case TYPE_FALSE:
    return true;

  case TYPE_STRING:
  case TYPE_ASCII: {
    uint32_t length = object0->string.length;
    if (length != object1->string.length)
      return false;
    return memcmp(object0->string.chars, object1->string.chars, length) == 0;
  }

  default:
    abort();
  }
}

static union object *object_allocate_zero_(struct arena *arena,
                                           size_t size, char type) {
  union object *object =
      (union object*)arena_allocate(arena, size, alignof(union object));
  memset(object, 0, size);
  object->type = type;
  return object;
}

#define object_allocate_zero(arena, type, type_id) \
    object_allocate_zero_((arena), sizeof(type), type_id)

union object *object_new_list(struct arena *arena)
{
  return object_allocate_zero(arena, struct object_list, TYPE_LIST);
}

static void object_list_grow(struct object_list *list, unsigned size)
{
    list->items = (union object**)dynmemory_grow(list->items, &list->capacity,
                                                 size, sizeof(list->items[0]));
    if (list->items == NULL) {
      abort();
    }
}

void object_list_append(union object *olist, union object *object)
{
  assert(olist->type == TYPE_LIST);
  struct object_list *list = &olist->list;
  unsigned length = list->length;
  unsigned new_length = length + 1;
  if (UNLIKELY(new_length >= list->capacity)) {
    object_list_grow(list, new_length);
  }
  list->items[length] = object;
  list->length = new_length;
}

union object *object_list_at(union object *olist, uint32_t index)
{
  assert(olist->type == TYPE_LIST);
  struct object_list *list = &olist->list;
  assert(index < list->length);
  return list->items[index];
}

union object *object_new_tuple(struct arena *arena, uint32_t length)
{
  union object *object;
  size_t size =
      sizeof(struct object_tuple) + length * sizeof(object->tuple.items[0]);
  object = object_allocate_zero_(arena, size, TYPE_TUPLE);
  object->tuple.length = length;
  return object;
}

union object *object_new_code(struct arena *arena)
{
  return object_allocate_zero(arena, struct object_code, TYPE_CODE);
}

bool object_type_is_singleton(char type)
{
  switch (type) {
  case TYPE_ELLIPSIS:
  case TYPE_FALSE:
  case TYPE_NONE:
  case TYPE_TRUE:
    return true;
  default:
    return false;
  }
}

union object *object_new_singleton(struct arena *arena, char type)
{
  assert(object_type_is_singleton(type));
  return object_allocate_zero(arena, struct object_base, type);
}

union object *make_string(struct arena *arena, char type,
                          uint32_t length, const char *chars)
{
  union object *object = object_allocate_zero(arena, struct object_string,
                                              type);
  object->string.length = length;
  object->string.chars = chars;
  return object;
}

union object *make_int(struct arena *arena, int32_t value)
{
  union object *object = object_allocate_zero(arena, struct object_int,
                                              TYPE_INT);
  object->int_obj.value = value;
  return object;
}

union object *make_bytes(struct arena *arena, uint32_t length,
                         const char *chars)
{
  return make_string(arena, TYPE_STRING, length, chars);
}

union object *make_ascii(struct arena *arena, const char *str)
{
  uint32_t length = strlen(str);
  return make_string(arena, TYPE_ASCII, length, str);
}

#include "objects.h"

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

struct object_list *object_new_list(struct arena *arena)
{
  struct object_list *list = arena_allocate_type(arena, struct object_list);
  memset(list, 0, sizeof(*list));
  list->base.type = TYPE_LIST;
  return list;
}

static void object_list_grow(struct object_list *list, unsigned size)
{
    list->items = (union object**)dynmemory_grow(list->items, &list->capacity,
                                                 size, sizeof(list->items[0]));
}

void object_list_append(struct object_list *list, union object *object)
{
  unsigned new_size = list->length + 1;
  if (UNLIKELY(new_size >= list->capacity)) {
    object_list_grow(list, new_size);
  }
  list->items[list->length++] = object;
}

struct object_tuple *object_new_tuple(struct arena *arena, uint32_t length)
{
  struct object_tuple *tuple;
  unsigned size = sizeof(struct object_tuple) + length * sizeof(tuple->items[0]);
  tuple = arena_allocate(arena, size, alignof(struct object_tuple));
  tuple->base.type = TYPE_TUPLE;
  tuple->length = length;
  return tuple;
}

struct object_code *object_new_code(struct arena *arena)
{
  struct object_code *code = arena_allocate_type(arena, struct object_code);
  memset(code, 0, sizeof(*code));
  code->base.type = TYPE_CODE;

  return code;
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

struct object_base *object_new_singleton(struct arena *arena, char type)
{
  assert(object_type_is_singleton(type));
  struct object_base *object = arena_allocate_type(arena, struct object_base);
  object->type = type;
  return object;
}

struct object_string *make_string(struct arena *arena, char type,
                                  uint32_t length, const char *chars)
{
  struct object_string *result =
      arena_allocate_type(arena, struct object_string);
  result->base.type = type;
  result->length = length;
  result->chars = chars;
  return result;
}

struct object_int *make_int(struct arena *arena, int32_t value)
{
  struct object_int *result = arena_allocate_type(arena, struct object_int);
  result->base.type = TYPE_INT;
  result->value = value;
  return result;
}

struct object_string *make_bytes(struct arena *arena, uint32_t length,
                                 const char *chars)
{
  return make_string(arena, TYPE_STRING, length, chars);
}

struct object_string *make_ascii(struct arena *arena, const char *str)
{
  uint32_t length = strlen(str);
  return make_string(arena, TYPE_ASCII, length, str);
}

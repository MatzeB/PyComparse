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

static void object_array_reserve(struct object_array *array, unsigned size)
{
  array->items = (union object **)dynmemory_grow(
      array->items, &array->capacity, size, sizeof(array->items[0]));
  if (array->items == NULL) {
    internal_error("out of memory");
  }
}

void object_array_append(struct object_array *array, union object *object)
{
  unsigned length = array->length;
  unsigned new_length = length + 1;
  if (UNLIKELY(new_length >= array->capacity)) {
    object_array_reserve(array, new_length);
  }
  array->items[length] = object;
  array->length = new_length;
}

union object *object_array_at(struct object_array *array, uint32_t index)
{
  assert(index < array->length);
  return array->items[index];
}

void object_array_set_at(struct object_array *array, uint32_t index,
                         union object *object)
{
  assert(index < array->length);
  array->items[index] = object;
}

uint32_t object_array_length(const struct object_array *array)
{
  return array->length;
}

union object *object_tuple_from_object_array(struct arena        *arena,
                                             struct object_array *array)
{
  uint32_t           length = array->length;
  struct tuple_prep *prep = object_new_tuple_begin(arena, length);
  for (uint32_t i = 0; i < length; ++i) {
    object_new_tuple_set_at(prep, i, array->items[i]);
  }
  return object_new_tuple_end(prep);
}

void object_array_free(struct object_array *array)
{
  free(array->items);
  array->items = NULL;
  array->length = 0;
  array->capacity = 0;
}

union object *object_new_code(struct arena *arena)
{
  return object_allocate_zero(arena, struct object_code, OBJECT_CODE);
}

struct tuple_prep *object_new_tuple_begin(struct arena *arena, uint32_t length)
{
  assert(length < UINT32_MAX);
  size_t length_size = (size_t)length;
  if (length_size > (SIZE_MAX - sizeof(struct object_tuple))
                        / sizeof(((union object *)0)->tuple.items[0])) {
    internal_error("tuple size overflow");
  }
  size_t size = sizeof(struct object_tuple)
                + length_size * sizeof(((union object *)0)->tuple.items[0]);
  union object *object;
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
  (void)object_type_is_singleton;
  assert(object_type_is_singleton(type));
  return object_allocate_zero(arena, struct object_base, type);
}

union object *object_new_string(struct arena *arena, enum object_type type,
                                uint32_t length, const char *chars)
{
  assert(chars != NULL);
  assert(length < UINT32_MAX);
  (void)object_type_is_string;
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

union object *object_new_complex(struct arena *arena, double real, double imag)
{
  union object *object
      = object_allocate_zero(arena, struct object_complex, OBJECT_COMPLEX);
  object->complex.real = real;
  object->complex.imag = imag;
  return object;
}

union object *object_new_int(struct arena *arena, int64_t value)
{
  assert(value != INT64_MIN);
  union object *object
      = object_allocate_zero(arena, struct object_int, OBJECT_INT);
  object->int_obj.value = value;
  return object;
}

union object *object_new_big_int(struct arena *arena, uint32_t num_pydigits,
                                 const uint16_t *nonnull pydigits)
{
  assert(num_pydigits > 0);
  union object *object
      = object_allocate_zero(arena, struct object_big_int, OBJECT_BIG_INT);
  size_t    size = (size_t)num_pydigits * sizeof(pydigits[0]);
  uint16_t *copy = (uint16_t *)arena_allocate(arena, size, alignof(uint16_t));
  memcpy(copy, pydigits, size);
  object->big_int.num_pydigits = num_pydigits;
  object->big_int.pydigits = copy;
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
  assert(chars != NULL);
  return memcmp(object->string.chars, chars, length) == 0;
}

uint32_t object_string_length(const union object *object)
{
  assert(object_type_is_string(object->type));
  return object->string.length;
}

const char *object_string_chars(const union object *object)
{
  assert(object_type_is_string(object->type));
  return object->string.chars;
}

uint32_t object_tuple_length(const union object *object)
{
  assert(object->type == OBJECT_TUPLE);
  return object->tuple.length;
}

union object *object_tuple_at(union object *object, uint32_t index)
{
  assert(object->type == OBJECT_TUPLE);
  assert(index < object->tuple.length);
  return object->tuple.items[index];
}

uint32_t object_frozenset_length(const union object *object)
{
  assert(object->type == OBJECT_FROZENSET);
  return object->tuple.length;
}

union object *object_frozenset_at(union object *object, uint32_t index)
{
  assert(object->type == OBJECT_FROZENSET);
  assert(index < object->tuple.length);
  return object->tuple.items[index];
}

double object_float_value(const union object *object)
{
  assert(object->type == OBJECT_FLOAT);
  return object->float_obj.value;
}

double object_complex_real(const union object *object)
{
  assert(object->type == OBJECT_COMPLEX);
  return object->complex.real;
}

double object_complex_imag(const union object *object)
{
  assert(object->type == OBJECT_COMPLEX);
  return object->complex.imag;
}

int64_t object_int_value(const union object *object)
{
  assert(object->type == OBJECT_INT);
  return object->int_obj.value;
}

uint32_t object_big_int_num_pydigits(const union object *object)
{
  assert(object->type == OBJECT_BIG_INT);
  return object->big_int.num_pydigits;
}

const uint16_t *object_big_int_pydigits(const union object *object)
{
  assert(object->type == OBJECT_BIG_INT);
  return object->big_int.pydigits;
}

uint32_t object_code_argcount(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.argcount;
}

uint32_t object_code_posonlyargcount(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.posonlyargcount;
}

uint32_t object_code_kwonlyargcount(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.kwonlyargcount;
}

uint32_t object_code_nlocals(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.nlocals;
}

uint32_t object_code_stacksize(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.stacksize;
}

uint32_t object_code_flags(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.flags;
}

uint32_t object_code_firstlineno(const union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.firstlineno;
}

union object *object_code_code(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.code;
}

union object *object_code_consts(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.consts;
}

union object *object_code_names(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.names;
}

union object *object_code_varnames(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.varnames;
}

union object *object_code_freevars(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.freevars;
}

union object *object_code_cellvars(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.cellvars;
}

union object *object_code_filename(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.filename;
}

union object *object_code_name(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.name;
}

union object *object_code_lnotab(union object *object)
{
  assert(object->type == OBJECT_CODE);
  return object->code.lnotab;
}

void object_code_set_argcount(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.argcount = value;
}

void object_code_set_posonlyargcount(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.posonlyargcount = value;
}

void object_code_set_kwonlyargcount(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.kwonlyargcount = value;
}

void object_code_set_nlocals(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.nlocals = value;
}

void object_code_set_stacksize(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.stacksize = value;
}

void object_code_set_flags(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.flags = value;
}

void object_code_set_firstlineno(union object *object, uint32_t value)
{
  assert(object->type == OBJECT_CODE);
  object->code.firstlineno = value;
}

void object_code_set_code(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.code = value;
}

void object_code_set_consts(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.consts = value;
}

void object_code_set_names(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.names = value;
}

void object_code_set_varnames(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.varnames = value;
}

void object_code_set_freevars(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.freevars = value;
}

void object_code_set_cellvars(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.cellvars = value;
}

void object_code_set_filename(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.filename = value;
}

void object_code_set_name(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.name = value;
}

void object_code_set_lnotab(union object *object, union object *value)
{
  assert(object->type == OBJECT_CODE);
  object->code.lnotab = value;
}

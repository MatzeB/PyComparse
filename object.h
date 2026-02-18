#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct arena;
struct object_array;
struct tuple_prep; /* deliberately incomplete */
union object;

#define CO_OPTIMIZED          0x0001
#define CO_NEWLOCALS          0x0002
#define CO_VARARGS            0x0004
#define CO_VARKEYWORDS        0x0008
#define CO_NESTED             0x0010
#define CO_GENERATOR          0x0020
#define CO_NOFREE             0x0040
#define CO_COROUTINE          0x0080
#define CO_ITERABLE_COROUTINE 0x0100
#define CO_ASYNC_GENERATOR    0x0200

#define CO_FUTURE_DIVISION         0x0020000
#define CO_FUTURE_ABSOLUTE_IMPORT  0x0040000
#define CO_FUTURE_WITH_STATEMENT   0x0080000
#define CO_FUTURE_PRINT_FUNCTION   0x0100000
#define CO_FUTURE_UNICODE_LITERALS 0x0200000

#define CO_FUTURE_BARRY_AS_BDFL  0x0400000
#define CO_FUTURE_GENERATOR_STOP 0x0800000
#define CO_FUTURE_ANNOTATIONS    0x1000000
#define CO_FUTURE_MASK                                                        \
  (CO_FUTURE_DIVISION | CO_FUTURE_ABSOLUTE_IMPORT | CO_FUTURE_WITH_STATEMENT  \
   | CO_FUTURE_PRINT_FUNCTION | CO_FUTURE_UNICODE_LITERALS                    \
   | CO_FUTURE_BARRY_AS_BDFL | CO_FUTURE_GENERATOR_STOP                       \
   | CO_FUTURE_ANNOTATIONS)

enum object_type {
  OBJECT_NULL = '0',
  OBJECT_NONE = 'N',
  OBJECT_TRUE = 'T',
  OBJECT_FALSE = 'F',
  OBJECT_FLOAT = 'g',
  OBJECT_COMPLEX = 'y',
  OBJECT_BYTES = 's',
  OBJECT_STRING = 'u',
  OBJECT_CODE = 'c',
  OBJECT_TUPLE = '(',
  OBJECT_FROZENSET = '>',
  OBJECT_ELLIPSIS = '.',
  OBJECT_INT = 'i',
  OBJECT_BIG_INT = 'I',
};

union object *object_new_singleton(struct arena *arena, enum object_type type);
union object *object_new_code(struct arena *arena);
union object *object_new_string(struct arena *arena, enum object_type type,
                                uint32_t length, const char *chars);
union object *object_new_float(struct arena *arena, double value);
union object *object_new_complex(struct arena *arena, double real,
                                 double imag);
union object *object_new_int(struct arena *arena, int64_t value);
union object *object_new_big_int(struct arena *arena, uint32_t num_pydigits,
                                 const uint16_t *nonnull pydigits);

struct tuple_prep *object_new_tuple_begin(struct arena *arena,
                                          uint32_t      length);
void          object_new_tuple_set_at(struct tuple_prep *tuple, uint32_t index,
                                      union object *object);
union object *object_new_tuple_end(struct tuple_prep *tuple_prep);

enum object_type object_type(const union object *object);

bool        object_string_equals(const union object *object, uint32_t length,
                                 const char *nullable chars);
uint32_t    object_string_length(const union object *object);
const char *object_string_chars(const union object *object);

uint32_t      object_tuple_length(const union object *object);
union object *object_tuple_at(union object *object, uint32_t index);

uint32_t      object_frozenset_length(const union object *object);
union object *object_frozenset_at(union object *object, uint32_t index);

double  object_float_value(const union object *object);
double  object_complex_real(const union object *object);
double  object_complex_imag(const union object *object);
int64_t object_int_value(const union object *object);

uint32_t        object_big_int_num_pydigits(const union object *object);
const uint16_t *object_big_int_pydigits(const union object *object);

uint32_t object_code_argcount(const union object *object);
uint32_t object_code_posonlyargcount(const union object *object);
uint32_t object_code_kwonlyargcount(const union object *object);
uint32_t object_code_nlocals(const union object *object);
uint32_t object_code_stacksize(const union object *object);
uint32_t object_code_flags(const union object *object);
uint32_t object_code_firstlineno(const union object *object);

union object *object_code_code(union object *object);
union object *object_code_consts(union object *object);
union object *object_code_names(union object *object);
union object *object_code_varnames(union object *object);
union object *object_code_freevars(union object *object);
union object *object_code_cellvars(union object *object);
union object *object_code_filename(union object *object);
union object *object_code_name(union object *object);
union object *object_code_lnotab(union object *object);

void object_code_set_argcount(union object *object, uint32_t value);
void object_code_set_posonlyargcount(union object *object, uint32_t value);
void object_code_set_kwonlyargcount(union object *object, uint32_t value);
void object_code_set_nlocals(union object *object, uint32_t value);
void object_code_set_stacksize(union object *object, uint32_t value);
void object_code_set_flags(union object *object, uint32_t value);
void object_code_set_firstlineno(union object *object, uint32_t value);

void object_code_set_code(union object *object, union object *value);
void object_code_set_consts(union object *object, union object *value);
void object_code_set_names(union object *object, union object *value);
void object_code_set_varnames(union object *object, union object *value);
void object_code_set_freevars(union object *object, union object *value);
void object_code_set_cellvars(union object *object, union object *value);
void object_code_set_filename(union object *object, union object *value);
void object_code_set_name(union object *object, union object *value);
void object_code_set_lnotab(union object *object, union object *value);

/* object_array is a mutable helper, not a Python object type */
void object_array_append(struct object_array *array, union object *object);
union object *object_array_at(struct object_array *array, uint32_t index);
void          object_array_set_at(struct object_array *array, uint32_t index,
                                  union object *object);
uint32_t      object_array_length(const struct object_array *array);
union object      *
object_tuple_from_object_array(struct arena                *arena,
                                    struct object_array *nonnull array);
void object_array_free(struct object_array *array);

ASSUME_NONNULL_END

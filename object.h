#pragma once

#include <stdbool.h>
#include <stdint.h>

struct arena;

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

enum object_type {
  OBJECT_NULL = '0',
  OBJECT_NONE = 'N',
  OBJECT_TRUE = 'T',
  OBJECT_FALSE = 'F',
  OBJECT_FLOAT = 'g',
  OBJECT_STRING = 's',
  OBJECT_UNICODE = 'u',
  OBJECT_CODE = 'c',
  OBJECT_TUPLE = '(',
  OBJECT_LIST = '[',
  OBJECT_ELLIPSIS = '.',

  OBJECT_ASCII = 'a',
  OBJECT_INT = 'i',

  OBJECT_SHORT_ASCII = 'z',
  OBJECT_SMALL_TUPLE = ')',

  /* convenience/make up for confusing python names... */
  OBJECT_BYTES = OBJECT_STRING,
};

union object;

union object *object_new_ascii(struct arena *arena, const char *cstring);
union object *object_new_singleton(struct arena *arena, enum object_type type);
union object *object_new_code(struct arena *arena);
union object *object_new_list(struct arena *arena);
union object *object_new_string(struct arena *arena, enum object_type type,
                                uint32_t length, const char *chars);
union object *object_new_float(struct arena *arena, double value);
union object *object_new_int(struct arena *arena, int32_t value);

union object *object_new_tuple_begin(struct arena *arena, uint32_t length);
void          object_new_tuple_set_at(union object *tuple, uint32_t index,
                                      union object *object);
void          object_new_tuple_end(union object *tuple);

enum object_type object_type(const union object *object);
bool objects_equal(const union object *object0, const union object *object1);

bool object_string_equals(const union object *object, uint32_t length,
                          const char *chars);

double  object_float_value(const union object *object);
int64_t object_int_value(const union object *object);

void          object_list_append(union object *list, union object *object);
union object *object_list_at(union object *list, uint32_t index);
uint32_t      object_list_length(union object *list);

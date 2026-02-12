#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct object_base {
  char type;
};

struct object_code {
  struct object_base base;
  uint32_t           argcount;
  uint32_t           posonlyargcount;
  uint32_t           kwonlyargcount;
  uint32_t           nlocals;
  uint32_t           stacksize;
  uint32_t           flags;
  uint32_t           firstlineno;
  union object      *code;
  union object      *consts;
  union object      *names;
  union object      *varnames;
  union object      *freevars;
  union object      *cellvars;
  union object      *filename;
  union object      *name;
  union object      *lnotab;
};

struct object_float {
  struct object_base base;
  double             value;
};

struct object_complex {
  struct object_base base;
  double             real;
  double             imag;
};

struct object_int {
  struct object_base base;
  int64_t value;
};

struct object_big_int {
  struct object_base          base;
  /* Marshal-format base-2^15 digits for arbitrary-size non-small integers. */
  uint32_t                 num_pydigits;
  const uint16_t *nullable pydigits;
};

struct object_list {
  struct object_base               base;
  uint32_t                         length;
  uint32_t                         capacity;
  union object * nonnull * nonnull items;
};

struct object_string {
  struct object_base base;
  uint32_t           length;
  const char        *chars;
};

struct object_tuple {
  struct object_base    base;
  uint32_t              length;
  union object *nonnull items[];
};

union object {
  char               type;
  struct object_base base;

  struct object_code    code;
  struct object_complex complex;
  struct object_float   float_obj;
  struct object_int     int_obj;
  struct object_big_int big_int;
  struct object_list    list;
  struct object_string  string;
  struct object_tuple   tuple;
};

ASSUME_NONNULL_END

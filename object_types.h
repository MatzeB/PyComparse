#pragma once

#include <stdbool.h>
#include <stdint.h>

struct object_base {
  char type;
};

struct object_string {
  struct object_base base;
  uint32_t           length;
  const char        *chars;
};

struct object_list {
  struct object_base base;
  uint32_t           length;
  uint32_t           capacity;
  union object     **items;
};

struct object_tuple {
  struct object_base base;
  uint32_t           length;
  union object      *items[];
};

struct object_int {
  struct object_base base;
  int64_t            value;
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

union object {
  char               type;
  struct object_base base;

  struct object_code   code;
  struct object_int    int_obj;
  struct object_list   list;
  struct object_string string;
  struct object_tuple  tuple;
};

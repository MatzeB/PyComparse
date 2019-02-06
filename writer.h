#ifndef WRITER_H
#define WRITER_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

struct bytecode_writer_state {
  FILE *out;
};

enum object_type {
  TYPE_NULL    = '0',
  TYPE_NONE    = 'N',
  TYPE_TRUE    = 'T',
  TYPE_FALSE   = 'F',
  TYPE_STRING  = 's',
  TYPE_UNICODE = 'u',
  TYPE_CODE    = 'c',
  TYPE_TUPLE   = '(',
  TYPE_LIST    = '[',

  TYPE_ASCII   = 'a',

  TYPE_SHORT_ASCII = 'z',
  TYPE_SMALL_TUPLE = ')',
};

union object;

struct object_base {
  char type;
};

struct object_string {
  struct object_base base;
  uint32_t len;
  const char *chars;
};

struct object_list_or_tuple {
  struct object_base base;
  uint32_t n_elements;
  union object *elements[];
};

struct object_code {
  struct object_base base;
  uint32_t argcount;
  uint32_t kwonlyargcount;
  uint32_t nlocals;
  uint32_t stacksize;
  uint32_t flags;
  uint32_t firstlineno;
  union object *code;
  union object *consts;
  union object *names;
  union object *varnames;
  union object *freevars;
  union object *cellvars;
  union object *filename;
  union object *name;
  union object *lnotab;
};

union object {
  char                        type;
  struct object_base          base;
  struct object_list_or_tuple list_or_tuple;
  struct object_string        string;
  struct object_code          code;
};

void writer_begin(struct bytecode_writer_state *s, FILE *out);

void write(struct bytecode_writer_state *s, const union object *object);

bool objects_equal(const union object *object0, const union object *object1);

#endif

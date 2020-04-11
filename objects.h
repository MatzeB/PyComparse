#pragma once

#include <stdbool.h>
#include <stdint.h>

struct arena;

enum object_type {
  TYPE_NULL     = '0',
  TYPE_NONE     = 'N',
  TYPE_TRUE     = 'T',
  TYPE_FALSE    = 'F',
  TYPE_STRING   = 's',
  TYPE_UNICODE  = 'u',
  TYPE_CODE     = 'c',
  TYPE_TUPLE    = '(',
  TYPE_LIST     = '[',
  TYPE_ELLIPSIS = '.',

  TYPE_ASCII   = 'a',
  TYPE_INT     = 'i',

  TYPE_SHORT_ASCII = 'z',
  TYPE_SMALL_TUPLE = ')',
};

union object;

struct object_base {
  char type;
};

struct object_string {
  struct object_base base;
  uint32_t length;
  const char *chars;
};

struct object_list {
  struct object_base base;
  uint32_t length;
  uint32_t capacity;
  union object **items;
};

struct object_tuple {
  struct object_base base;
  uint32_t length;
  union object *items[];
};

struct object_int {
  struct object_base base;
  int32_t value;
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
  char               type;
  struct object_base base;

  struct object_code   code;
  struct object_int    int_obj;
  struct object_list   list;
  struct object_string string;
  struct object_tuple  tuple;
};

struct object_base *object_new_singleton(struct arena *arena, char type);
struct object_code *object_new_code(struct arena *arena);
struct object_list *object_new_list(struct arena *arena);
struct object_tuple *object_new_tuple(struct arena *arena, uint32_t length);
struct object_string *make_string(struct arena *arena, char type,
                                  uint32_t length, const char *chars);
struct object_int *make_int(struct arena *arena, int32_t value);
struct object_string *make_bytes(struct arena *arena, uint32_t length,
                                 const char *chars);
struct object_string *make_ascii(struct arena *arena, const char *str);

void object_list_append(struct object_list *list, union object *object);

bool object_type_is_singleton(char type);

bool objects_equal(const union object *object0, const union object *object1);

#pragma once

#include "adt/arena.h"
#include "object_intern.h"

struct object_intern {
  struct arena  arena;
  union object *objects;
  union object *singleton_none;
  union object *singleton_true;
  union object *singleton_false;
  union object *singleton_ellipsis;
  union object *empty_tuple;
};

#pragma once

#include "adt/arena.h"
#include "adt/hashset.h"
#include "object_intern.h"

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct object_intern {
  struct arena  arena;
  union object *objects;
  struct hash_set string_set;
  struct object_intern_string_bucket *nullable string_buckets;
  union object *singleton_none;
  union object *singleton_true;
  union object *singleton_false;
  union object *singleton_ellipsis;
  union object *empty_tuple;
};

ASSUME_NONNULL_END

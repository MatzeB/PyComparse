#pragma once

#include "pycomparse/adt/arena.h"
#include "pycomparse/adt/hashset.h"
#include "pycomparse/object_types.h"

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct object_intern {
  struct arena                                 arena;
  struct hash_set                              string_set;
  struct object_intern_string_bucket *nullable string_buckets;
  struct hash_set                              object_set;
  struct object_intern_object_bucket *nullable object_buckets;
  union object                                *singleton_none;
  union object                                *singleton_true;
  union object                                *singleton_false;
  union object                                *singleton_ellipsis;
  union object                                *empty_tuple;
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

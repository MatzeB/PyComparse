#pragma once

#include "pycomparse/adt/arena.h"
#include "pycomparse/adt/hashset.h"

#ifdef __cplusplus
extern "C" {
#endif

struct symbol_table_bucket;

struct symbol_table {
  struct hash_set             set;
  struct symbol_table_bucket *buckets;
  struct arena                arena;
};

#ifdef __cplusplus
}
#endif

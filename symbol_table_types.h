#pragma once

#include "adt/arena.h"
#include "adt/hashset.h"

struct symbol_table_bucket;

struct symbol_table {
  struct hash_set             set;
  struct symbol_table_bucket *buckets;
  struct arena                arena;
};

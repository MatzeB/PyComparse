#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include "adt/arena.h"
#include "adt/hashset.h"

struct symbol_table_bucket;
struct arena;

struct symbol {
  const char *string;
  uint16_t    token_kind;
};

struct symbol_table {
  struct hash_set             set;
  struct symbol_table_bucket *buckets;
  struct arena                arena;
};

struct symbol *symbol_table_get_or_insert(struct symbol_table *symbol_table,
                                          const char *string);

void init_symbol_table(struct symbol_table *symbol_table);

void exit_symbol_table(struct symbol_table *symbol_table);

#endif

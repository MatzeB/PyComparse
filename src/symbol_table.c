#include "pycomparse/symbol_table.h"

#include "pycomparse/symbol_table_types.h"
#include "pycomparse/symbol_types.h"
#include "pycomparse/token_kinds.h"
#include "pycomparse/util.h"

struct symbol_table_bucket {
  struct symbol *symbol;
  unsigned       hash;
};

static unsigned fnv_hash_string(const char *string)
{
  unsigned hash = 2166136261;
  for (const char *p = string; *p != '\0'; ++p) {
    hash ^= (unsigned char)*p;
    hash *= 16777619;
  }
  return hash;
}

static struct symbol *
symbol_table_new_symbol(struct symbol_table *symbol_table, const char *string,
                        uint16_t token_kind)
{
  struct symbol *symbol = arena_allocate(
      &symbol_table->arena, sizeof(struct symbol), alignof(struct symbol));
  symbol->string = string;
  symbol->token_kind = token_kind;
  memset(&symbol->info, 0, sizeof(symbol->info));
  return symbol;
}

static struct symbol *
symbol_table_insert_new(struct symbol_table *symbol_table,
                        struct symbol *symbol, unsigned hash)
{
  hash_set_increment_num_elements(&symbol_table->set);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &symbol_table->set, hash);
  struct symbol_table_bucket *buckets = symbol_table->buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct symbol_table_bucket *bucket = &buckets[c.index];
    if (bucket->symbol == NULL) {
      bucket->symbol = symbol;
      bucket->hash = hash;
      return symbol;
    }
  }
}

static void symbol_table_resize(struct symbol_table *symbol_table,
                                unsigned             new_size)
{
  struct symbol_table_bucket *old_buckets = symbol_table->buckets;
  unsigned num_old_buckets = hash_set_num_buckets(&symbol_table->set);
  symbol_table->buckets = calloc(new_size, sizeof(symbol_table->buckets[0]));
  if (symbol_table->buckets == NULL) {
    internal_error("out of memory");
  }
  hash_set_init(&symbol_table->set, new_size);
  for (unsigned i = 0; i < num_old_buckets; ++i) {
    struct symbol_table_bucket *bucket = &old_buckets[i];
    if (bucket->symbol != NULL) {
      symbol_table_insert_new(symbol_table, bucket->symbol, bucket->hash);
    }
  }
  free(old_buckets);
}

struct symbol *symbol_table_get_or_insert(struct symbol_table *symbol_table,
                                          const char          *string)
{
  unsigned new_size = hash_set_should_resize(&symbol_table->set);
  if (UNLIKELY(new_size != 0)) {
    symbol_table_resize(symbol_table, new_size);
  }

  unsigned hash = fnv_hash_string(string);

  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &symbol_table->set, hash);
  struct symbol_table_bucket *buckets = symbol_table->buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct symbol_table_bucket *bucket = &buckets[c.index];
    if (bucket->symbol == NULL) {
      // not found: create a new entry.
      struct symbol *symbol
          = symbol_table_new_symbol(symbol_table, string, T_IDENTIFIER);
      bucket->symbol = symbol;
      bucket->hash = hash;
      hash_set_increment_num_elements(&symbol_table->set);
      return symbol;
    } else if (bucket->hash == hash
               && strcmp(bucket->symbol->string, string) == 0) {
      return bucket->symbol;
    }
  }
}

static void symbol_table_insert_predefined(struct symbol_table *symbol_table,
                                           const char          *string,
                                           uint16_t             token_kind)
{
  struct symbol *symbol
      = symbol_table_new_symbol(symbol_table, string, token_kind);
  unsigned hash = fnv_hash_string(string);
  symbol_table_insert_new(symbol_table, symbol, hash);
}

void symbol_table_init(struct symbol_table *symbol_table)
{
  arena_init(&symbol_table->arena);
  unsigned num_buckets = 512;
  hash_set_init(&symbol_table->set, num_buckets);
  symbol_table->buckets
      = calloc(num_buckets, sizeof(symbol_table->buckets[0]));
  if (symbol_table->buckets == NULL) {
    internal_error("out of memory");
  }

#define TCHAR(val, name, desc)
#define TDES(name, desc)
#define TDES_VAL(name, desc, val)
#define TID(id, name) symbol_table_insert_predefined(symbol_table, #id, name);
#include "pycomparse/tokens.h"
#undef TID
#undef TDES
#undef TDES_VAL
#undef TCHAR
}

void symbol_table_free(struct symbol_table *symbol_table)
{
  arena_free(&symbol_table->arena);
  free(symbol_table->buckets);
}

#include "pycomparse/adt/arena.h"
#include "pycomparse/adt/hash.h"
#include "pycomparse/adt/hashset.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static void alloc_some(struct arena *arena)
{
  for (int i = 0, e = rand() % 5000; i < e; ++i) {
    size_t sz = rand() % 33;
    char  *m = arena_allocate(arena, sz, 1);

    memset(m, rand(), sz);

    if (rand() % 10 == 0) {
      arena_grow_begin(arena, 1);
      for (char *c = "Hello World!"; *c != '\0'; ++c) {
        arena_grow_char(arena, *c);
      }
      arena_grow_char(arena, '\0');
      assert(arena_grow_current_size(arena) == 13);
      char *str = (char *)arena_grow_finish(arena);
      assert(strcmp(str, "Hello World!") == 0);
      (void)str;
    }

    if (rand() % 10 == 0) {
      arena_grow_begin(arena, 1);
      for (char *c = "abcdef"; *c != '\0'; ++c) {
        arena_grow_char(arena, *c);
      }
      arena_grow_truncate(arena, 3);
      arena_grow_char(arena, 'x');
      arena_grow_char(arena, '\0');
      char *str = (char *)arena_grow_finish(arena);
      assert(strcmp(str, "abcx") == 0);
      (void)str;
    }
  }
}

static void run_arena_test(int seed)
{
  printf("Seed: %d\n", seed);
  srand(seed);

  struct arena arena;
  arena_init(&arena);
  for (int i = 0; i < 2000; ++i) {
    alloc_some(&arena);
    arena_free(&arena);
  }
}

struct string_bucket {
  const char *key;
  unsigned    hash;
};

struct string_set {
  struct hash_set       set;
  struct string_bucket *buckets;
};

static void string_set_insert_raw(struct string_set *set, const char *key,
                                  unsigned hash)
{
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &set->set, hash);
  for (;; hash_set_chain_iteration_next(&c)) {
    struct string_bucket *bucket = &set->buckets[c.index];
    if (bucket->key == NULL) {
      bucket->key = key;
      bucket->hash = hash;
      hash_set_increment_num_elements(&set->set);
      return;
    }
  }
}

static void string_set_resize(struct string_set *set, unsigned new_size)
{
  struct string_bucket *old_buckets = set->buckets;
  unsigned              old_num_buckets = hash_set_num_buckets(&set->set);
  set->buckets = calloc(new_size, sizeof(set->buckets[0]));
  assert(set->buckets != NULL);
  hash_set_init(&set->set, new_size);
  for (unsigned i = 0; i < old_num_buckets; ++i) {
    struct string_bucket *bucket = &old_buckets[i];
    if (bucket->key != NULL) {
      string_set_insert_raw(set, bucket->key, bucket->hash);
    }
  }
  free(old_buckets);
}

static void string_set_init(struct string_set *set)
{
  hash_set_init(&set->set, 64u);
  set->buckets
      = calloc(hash_set_num_buckets(&set->set), sizeof(set->buckets[0]));
  assert(set->buckets != NULL);
}

static void string_set_free(struct string_set *set)
{
  free(set->buckets);
  set->buckets = NULL;
}

static const char *string_set_get_or_insert(struct string_set *set,
                                            const char        *key)
{
  unsigned hash = fnv_hash_cstring(key);
  for (;;) {
    struct hash_set_chain_iteration_state c;
    hash_set_chain_iteration_begin(&c, &set->set, hash);
    for (;; hash_set_chain_iteration_next(&c)) {
      struct string_bucket *bucket = &set->buckets[c.index];
      if (bucket->key == NULL) {
        unsigned new_size = hash_set_should_resize(&set->set);
        if (new_size != 0) {
          string_set_resize(set, new_size);
          break;
        }
        bucket->key = key;
        bucket->hash = hash;
        hash_set_increment_num_elements(&set->set);
        return bucket->key;
      }
      if (bucket->hash == hash && strcmp(bucket->key, key) == 0) {
        return bucket->key;
      }
    }
  }
}

static void test_duplicate_lookup_does_not_resize(void)
{
  struct string_set set;
  string_set_init(&set);

  unsigned start_buckets = hash_set_num_buckets(&set.set);
  unsigned threshold = start_buckets / 2;
  unsigned start_size = hash_set_size(&set.set);
  assert(start_size < threshold);

  unsigned num_new_symbols = threshold - start_size;
  char   **names = calloc(num_new_symbols + 1, sizeof(*names));
  assert(names != NULL);
  for (unsigned i = 0; i <= num_new_symbols; ++i) {
    size_t length = (size_t)snprintf(NULL, 0, "adt_id_%u", i) + 1;
    names[i] = malloc(length);
    assert(names[i] != NULL);
    (void)snprintf(names[i], length, "adt_id_%u", i);
  }

  for (unsigned i = 0; i < num_new_symbols; ++i) {
    (void)string_set_get_or_insert(&set, names[i]);
  }
  assert(hash_set_num_buckets(&set.set) == start_buckets);

  (void)string_set_get_or_insert(&set, names[0]);
  assert(hash_set_num_buckets(&set.set) == start_buckets);

  (void)string_set_get_or_insert(&set, names[num_new_symbols]);
  assert(hash_set_num_buckets(&set.set) == start_buckets * 2);

  string_set_free(&set);
  for (unsigned i = 0; i <= num_new_symbols; ++i) {
    free(names[i]);
  }
  free(names);
}

int main(int argc, char **argv)
{
  int seed = argc > 1 ? atoi(argv[1]) : (int)time(NULL);
  run_arena_test(seed);
  test_duplicate_lookup_does_not_resize();
  printf("adt_test: ok\n");
  return 0;
}

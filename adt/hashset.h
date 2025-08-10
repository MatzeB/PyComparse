#pragma once

#include <stdbool.h>
#include <stdlib.h>

#include "adt/bitfiddle.h"

struct hash_set {
  unsigned num_buckets;
  unsigned enlarge_threshold;
  unsigned shrink_threshold;
  unsigned num_elements;
  unsigned num_deleted;
  bool     consider_shrink;
};

static inline unsigned hash_set_size(const struct hash_set *set)
{
  return set->num_elements - set->num_deleted;
}

static inline void hash_set_init(struct hash_set *set, unsigned num_buckets)
{
  assert((num_buckets & (num_buckets - 1)) == 0);
  if (num_buckets >= (1u << (sizeof(unsigned) * CHAR_BIT - 1))) {
    // TODO: Proper error reporting?
    abort();
  }

  set->num_buckets = num_buckets;
  set->enlarge_threshold = num_buckets / 2;
  set->shrink_threshold = num_buckets / 5;
  set->num_elements = 0;
  set->num_deleted = 0;
  set->consider_shrink = false;
}

struct hash_set_chain_iteration_state {
  unsigned index;
  unsigned hashmask;
  unsigned num_probes;
};

static inline void
hash_set_chain_iteration_begin(struct hash_set_chain_iteration_state *state,
                               const struct hash_set *set, unsigned hash)
{
  unsigned hashmask = set->num_buckets - 1;
  state->index = hash & hashmask;
  state->hashmask = hashmask;
  state->num_probes = 0;
  assert((set->num_buckets & hashmask) == 0);
}

static inline void
hash_set_chain_iteration_next(struct hash_set_chain_iteration_state *state)
{
  ++state->num_probes;
  state->index = (state->index + state->num_probes) & state->hashmask;
}

static inline unsigned hash_set_should_resize(struct hash_set *set)
{
  unsigned min_buckets = 32;

  if (set->consider_shrink) {
    set->consider_shrink = false;
    unsigned size = hash_set_size(set);
    if (size > min_buckets && size <= set->shrink_threshold) {
      return ceil_po2(size);
    }
  }

  if (set->num_elements + 1 <= set->enlarge_threshold) {
    return 0;
  }

  unsigned new_size;
  if (hash_set_size(set) + 2 > set->enlarge_threshold) {
    new_size = set->num_buckets * 2;
  } else {
    // Just re-hash to clean out tombstones
    new_size = set->num_buckets;
  }
  return new_size;
}

static inline void hash_set_increment_num_elements(struct hash_set *set)
{
  ++set->num_elements;
}

static inline void hash_set_increment_num_deleted(struct hash_set *set)
{
  ++set->num_deleted;
}

static inline unsigned hash_set_num_buckets(const struct hash_set *set)
{
  return set->num_buckets;
}

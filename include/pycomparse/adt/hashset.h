#pragma once

#include <stdbool.h>
#include <stdlib.h>

#include "pycomparse/adt/bitfiddle.h"

#ifdef __cplusplus
extern "C" {
#endif

struct hash_set {
  unsigned num_buckets;
  unsigned enlarge_threshold;
  unsigned num_elements;
};

static inline unsigned hash_set_size(const struct hash_set *set)
{
  return set->num_elements;
}

static inline void hash_set_init(struct hash_set *set, unsigned num_buckets)
{
  assert(num_buckets > 0);
  assert((num_buckets & (num_buckets - 1)) == 0);
  if (num_buckets >= (1u << (sizeof(unsigned) * CHAR_BIT - 1))) {
    // TODO: Proper error reporting?
    abort();
  }

  set->num_buckets = num_buckets;
  set->enlarge_threshold = num_buckets / 2;
  set->num_elements = 0;
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
  assert(state->num_probes < state->hashmask);
  ++state->num_probes;
  state->index = (state->index + state->num_probes) & state->hashmask;
}

static inline unsigned hash_set_should_resize(const struct hash_set *set)
{
  if (set->num_elements + 1 <= set->enlarge_threshold) {
    return 0;
  }

  return set->num_buckets * 2;
}

static inline void hash_set_increment_num_elements(struct hash_set *set)
{
  assert(set->num_elements < set->num_buckets);
  ++set->num_elements;
}

static inline unsigned hash_set_num_buckets(const struct hash_set *set)
{
  return set->num_buckets;
}

#ifdef __cplusplus
}
#endif

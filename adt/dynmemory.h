#pragma once

#include <limits.h>
#include <stdlib.h>

#include "bitfiddle.h"

static inline void *dynmemory_grow(void *memory, unsigned *capacity,
                                   unsigned minimum_size, unsigned multiply)
{
  unsigned new_capacity = ceil_po2(minimum_size);
  new_capacity = new_capacity > 16 ? new_capacity : 16;
  if (multiply != 0 && new_capacity > UINT_MAX / multiply) {
    abort();
  }
  void *new_memory = realloc(memory, (size_t)new_capacity * multiply);
  if (new_memory == NULL) {
    abort();
  }
  memory = new_memory;
  *capacity = new_capacity;
  return memory;
}

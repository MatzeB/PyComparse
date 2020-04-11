#pragma once

#include "bitfiddle.h"

static inline void *dynmemory_grow(void *memory, unsigned *capacity,
                                   unsigned minimum_size, unsigned multiply)
{
  unsigned new_capacity = ceil_po2(minimum_size);
  new_capacity = new_capacity > 16 ? new_capacity : 16;
  memory = realloc(memory, new_capacity * multiply);
  *capacity = new_capacity;
  return memory;
}

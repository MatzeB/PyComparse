#pragma once

#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include "bitfiddle.h"

#define ADT_DYNMEMORY_HAVE_ASAN 0
#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#undef ADT_DYNMEMORY_HAVE_ASAN
#define ADT_DYNMEMORY_HAVE_ASAN 1
#endif
#endif
#if defined(__SANITIZE_ADDRESS__)
#undef ADT_DYNMEMORY_HAVE_ASAN
#define ADT_DYNMEMORY_HAVE_ASAN 1
#endif

#if ADT_DYNMEMORY_HAVE_ASAN
#include <sanitizer/asan_interface.h>
#endif

#if !defined(NDEBUG) && !ADT_DYNMEMORY_HAVE_ASAN
static const unsigned char dynmemory_poison_free_pattern = 0xDD;
#endif

static inline void dynmemory_poison_freed_(void *addr, size_t size)
{
  if (size == 0) return;
#if !defined(NDEBUG) && !ADT_DYNMEMORY_HAVE_ASAN
  memset(addr, dynmemory_poison_free_pattern, size);
#endif
#if ADT_DYNMEMORY_HAVE_ASAN
  __asan_poison_memory_region(addr, size);
#else
  (void)addr;
#endif
}

static inline void dynmemory_unpoison_alloc_(void *addr, size_t size)
{
  if (size == 0) return;
#if ADT_DYNMEMORY_HAVE_ASAN
  __asan_unpoison_memory_region(addr, size);
#else
  (void)addr;
#endif
}

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

  size_t total_size = (size_t)new_capacity * multiply;
  dynmemory_unpoison_alloc_(new_memory, total_size);

  memory = new_memory;
  *capacity = new_capacity;
  return memory;
}

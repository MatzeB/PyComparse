#pragma once

#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "bitfiddle.h"

#define ADT_HAVE_ASAN 0
#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#undef ADT_HAVE_ASAN
#define ADT_HAVE_ASAN 1
#endif
#endif
#if defined(__SANITIZE_ADDRESS__)
#undef ADT_HAVE_ASAN
#define ADT_HAVE_ASAN 1
#endif

#if ADT_HAVE_ASAN
#include <sanitizer/asan_interface.h>
#endif

#ifndef UNLIKELY
#define UNLIKELY(x) __builtin_expect((x), 0)
#endif

#pragma clang assume_nonnull begin

struct block_header {
  struct block_header *_Nullable prev;
  unsigned block_size;
};

struct arena {
  struct block_header *_Nullable block;
  unsigned allocated;
  unsigned limit;
  unsigned grow;
  unsigned grow_alignment;
};

static const unsigned arena_max_alignment = 64;
#if !defined(NDEBUG) && !ADT_HAVE_ASAN
static const unsigned char arena_poison_free_pattern = 0xDD;
#endif

static inline void arena_poison_freed_(void *addr, size_t size)
{
  if (size == 0) return;
#if !defined(NDEBUG) && !ADT_HAVE_ASAN
  memset(addr, arena_poison_free_pattern, size);
#endif
#if ADT_HAVE_ASAN
  __asan_poison_memory_region(addr, size);
#else
  (void)addr;
#endif
}

static inline void arena_unpoison_alloc_(void *addr, size_t size)
{
  if (size == 0) return;
#if ADT_HAVE_ASAN
  __asan_unpoison_memory_region(addr, size);
#else
  (void)addr;
#endif
}

static inline void arena_init(struct arena *arena)
{
  memset(arena, 0, sizeof(*arena));
}

static __attribute__((noinline)) void
arena_allocate_block_(struct arena *arena, unsigned size)
{
  const unsigned default_block_size = 16 * 1024;
  unsigned block_size = size < default_block_size - sizeof(struct block_header)
                            ? default_block_size
                            : ceil_po2(size + sizeof(struct block_header));
  void    *memory = malloc(block_size);
  if (memory == NULL) abort();
  struct block_header *header = (struct block_header *)memory;
  header->prev = arena->block;
  header->block_size = block_size;

  arena->block = header;
  arena->allocated = sizeof(struct block_header);
  arena->limit = block_size;
  arena_poison_freed_((char *)header + arena->allocated,
                      arena->limit - arena->allocated);
}

static inline void arena_align_(struct arena *arena, unsigned alignment)
{
  assert(alignment > 0);
  assert((alignment & (alignment - 1)) == 0 && "alignment must be power of 2");
  assert(alignment <= arena_max_alignment);
  unsigned mask = ~(alignment - 1);
  arena->allocated = (arena->allocated + alignment - 1) & mask;
}

static __attribute__((noinline)) void *
arena_allocate_slow_(struct arena *arena, size_t size, unsigned alignment)
{
  assert(size <= UINT_MAX - (alignment - 1));
  unsigned required_size = (unsigned)size + alignment - 1;

  if (arena->block == NULL
      || required_size > arena->limit - arena->allocated) {
    arena_allocate_block_(arena, required_size);
  }
  arena_align_(arena, alignment);
  if (UNLIKELY(size > arena->limit - arena->allocated)) {
    arena_allocate_block_(arena, required_size);
    arena_align_(arena, alignment);
  }
  void *result = (char *)arena->block + arena->allocated;
  arena->allocated += (unsigned)size;
  arena_unpoison_alloc_(result, size);
  return result;
}

static __attribute__((noinline)) void
arena_grow_begin_slow_(struct arena *arena, unsigned alignment)
{
  unsigned required_size = alignment - 1;
  if (arena->block == NULL
      || required_size > arena->limit - arena->allocated) {
    arena_allocate_block_(arena, required_size);
  }
  arena_align_(arena, alignment);
  arena->grow = arena->allocated;
  arena->grow_alignment = alignment;
}

static inline void *arena_allocate(struct arena *arena, size_t size,
                                   unsigned alignment)
{
  assert(size < UINT_MAX / 2);
  assert(arena->grow == 0);
  assert(alignment <= arena_max_alignment);
  assert(size <= UINT_MAX - (alignment - 1));

  unsigned required_size = (unsigned)size + alignment - 1;
  if (UNLIKELY(arena->block == NULL
               || required_size > arena->limit - arena->allocated)) {
    return arena_allocate_slow_(arena, size, alignment);
  }
  arena_align_(arena, alignment);
  void *result = (char *)arena->block + arena->allocated;
  arena->allocated += size;
  arena_unpoison_alloc_(result, size);
  return result;
}

static inline void arena_free(struct arena *arena)
{
  assert(arena->grow == 0);
  for (struct block_header *block = arena->block, *prev; block != NULL;
       block = prev) {
    prev = block->prev;
    arena_poison_freed_(block, block->block_size);
    free(block);
  }
  arena->block = NULL;
  arena->allocated = 0;
  arena->limit = 0;
}

static inline void arena_free_to(struct arena *arena, const void *free_up_to)
{
  assert(arena->grow == 0);
  struct block_header *block = arena->block;
  for (;;) {
    if ((const void *)block <= free_up_to
        && free_up_to < (const void *)((char *)block + block->block_size))
      break;
    struct block_header *prev = block->prev;
    arena_poison_freed_(block, block->block_size);
    free(block);
    block = prev;
    assert(block != NULL && "address must be part of arena");
  }

  const unsigned free_offset
      = (unsigned)((const char *)free_up_to - (const char *)block);
  assert(free_offset <= arena->allocated);
  arena_poison_freed_((char *)block + free_offset,
                      arena->allocated - free_offset);

  arena->block = block;
  arena->allocated = (const char *)free_up_to - (const char *)block;
  arena->limit = block->block_size;
}

static inline void arena_grow_begin(struct arena *arena, unsigned alignment)
{
  assert(arena->grow == 0);
  assert(alignment > 0);
  assert(alignment <= arena_max_alignment);

  unsigned required_size = alignment - 1;
  if (UNLIKELY(arena->block == NULL
               || required_size > arena->limit - arena->allocated)) {
    arena_grow_begin_slow_(arena, alignment);
    return;
  }
  arena_align_(arena, alignment);
  arena->grow = arena->allocated;
  arena->grow_alignment = alignment;
}

static inline unsigned arena_grow_current_size(const struct arena *arena)
{
  assert(arena->grow != 0);
  return arena->grow - arena->allocated;
}

static inline void arena_grow_truncate(struct arena *arena, unsigned size)
{
  assert(arena->grow != 0);
  assert(size <= arena_grow_current_size(arena));
  arena->grow = arena->allocated + size;
}

static inline void *arena_grow_current_base(const struct arena *arena)
{
  assert(arena->grow != 0);
  return (char *)arena->block + arena->allocated;
}

static __attribute__((noinline)) void
new_block_while_growing_(struct arena *arena, unsigned size)
{
  const void *old_begin = arena_grow_current_base(arena);
  unsigned    current_size = arena_grow_current_size(arena);
  arena_allocate_block_(arena, current_size + size);
  arena_align_(arena, arena->grow_alignment);
  void *new_begin = arena_grow_current_base(arena);
  arena_unpoison_alloc_(new_begin, current_size);
  memcpy(new_begin, old_begin, current_size);
  arena->grow = arena->allocated + current_size;
}

static inline void *arena_grow(struct arena *arena, unsigned size)
{
  assert(arena->grow != 0);
  if (UNLIKELY(size >= arena->limit - arena->grow)) {
    new_block_while_growing_(arena, size);
  }
  void *result = (char *)arena->block + arena->grow;
  arena->grow += size;
  arena_unpoison_alloc_(result, size);
  return result;
}

static inline void arena_grow_char(struct arena *arena, char c)
{
  char *addr = (char *)arena_grow(arena, 1);
  *addr = c;
}

static inline void *arena_grow_finish(struct arena *arena)
{
  assert(arena->grow >= arena->allocated);
  void *result = (char *)arena->block + arena->allocated;
  arena->allocated = arena->grow;
#ifndef NDEBUG
  arena->grow = 0;
#endif
  return result;
}

#define arena_allocate_type(arena, type)                                      \
  ((type *)arena_allocate((arena), sizeof(type), alignof(type)))

#pragma clang assume_nonnull end

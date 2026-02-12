#pragma once

#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "bitfiddle.h"

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
  return result;
}

static inline void arena_free(struct arena *arena)
{
  assert(arena->grow == 0);
  for (struct block_header *block = arena->block, *prev; block != NULL;
       block = prev) {
    prev = block->prev;
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
    free(block);
    block = prev;
    assert(block != NULL && "address must be part of arena");
  }
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

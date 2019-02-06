#ifndef ARENA_H
#define ARENA_H

#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "bitfiddle.h"

#ifndef UNLIKELY
#define UNLIKELY(x)    __builtin_expect((x), 0)
#endif

struct block_header {
  struct block_header *prev;
  unsigned block_size;
};

struct arena {
  struct block_header *block;
  unsigned allocated;
  unsigned limit;
  unsigned grow;
  unsigned grow_alignment;
};

static inline void arena_init(struct arena *arena)
{
  arena->block      = NULL;
  arena->grow       = 0;
  arena->allocated  = 0;
  arena->limit      = 0;
}

static __attribute__((noinline))
void arena_allocate_block_(struct arena *arena, unsigned size)
{
  const unsigned default_block_size = 16 * 1024;
  unsigned block_size = size < default_block_size
    ? default_block_size : ceil_po2(size + sizeof(struct block_header));
  char *memory = malloc(block_size);
  struct block_header *header = (struct block_header*)memory;
  header->prev       = arena->block;
  header->block_size = block_size;

  arena->block     = header;
  arena->allocated = sizeof(struct block_header);
  arena->limit     = block_size;
}

static inline void arena_align_(struct arena *arena, unsigned alignment)
{
  assert(alignment > 0);
  assert((alignment & (alignment - 1)) == 0 && "alignment must be power of 2");
  // Base address is aligned by malloc rules.
  assert(alignment <= alignof(long long) && alignment <= alignof(double) &&
         alignment <= alignof(long double));
  unsigned mask = ~(alignment - 1);
  arena->allocated = (arena->allocated + alignment - 1) & mask;
}

static inline void *arena_allocate(struct arena *arena, size_t size,
                                   unsigned alignment)
{
  assert(size < UINT_MAX / 2);
  assert(arena->grow == 0);
  if (UNLIKELY(size >= arena->limit - arena->allocated)) {
    arena_allocate_block_(arena, size);
  }
  arena_align_(arena, alignment);
  void *result = (char*)arena->block + arena->allocated;
  arena->allocated += size;
  return result;
}

static inline void arena_free_all(struct arena *arena)
{
  assert(arena->grow == 0);
  for (struct block_header *block = arena->block, *next; block != NULL;
       block = next) {
    next = block->prev;
    free(block);
  }
  arena->block     = NULL;
  arena->allocated = 0;
  arena->limit     = 0;
}

static inline void arena_free(struct arena *arena, const void *free_up_to)
{
  assert(arena->grow == 0);
  struct block_header *block = arena->block;
  assert(block != NULL);
  for (struct block_header *next; block != NULL; block = next) {
    if (free_up_to > (const void*)block &&
        free_up_to < (const void*)((char*)block + block->block_size))
      break;
    next = block->prev;
    free(block);
  }
  assert(block != NULL && "address must be part of arena");
  arena->block     = block;
  arena->allocated = (char*)free_up_to - (char*)block;
  arena->limit     = block->block_size;
}

static inline void arena_grow_begin(struct arena *arena, unsigned alignment)
{
  assert(arena->grow == 0);

  if (UNLIKELY(arena->block == NULL))
    arena_allocate_block_(arena, 0);
  arena_align_(arena, alignment);
  arena->grow           = arena->allocated;
  arena->grow_alignment = alignment;
}

static inline unsigned arena_grow_current_size(const struct arena *arena)
{
  assert(arena->grow != 0);
  return arena->grow - arena->allocated;
}

static inline void *arena_grow_current_base(const struct arena *arena)
{
  assert(arena->grow != 0);
  return (char*)arena->block + arena->allocated;
}

static __attribute__((noinline))
void new_block_while_growing_(struct arena *arena, unsigned size)
{
  const void *old_begin = arena_grow_current_base(arena);
  unsigned current_size = arena_grow_current_size(arena);
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
  void *result = (char*)arena->block + arena->grow;
  arena->grow += size;
  return result;
}

static inline void arena_grow_char(struct arena *arena, char c)
{
  char *addr = (char*)arena_grow(arena, 1);
  *addr = c;
}

static inline void *arena_grow_finish(struct arena *arena)
{
  assert(arena->grow >= arena->allocated);
  void *result = (char*)arena->block + arena->allocated;
  arena->allocated = arena->grow;
#ifndef NDEBUG
  arena->grow      = 0;
#endif
  return result;
}

#define arena_allocate_type(arena, type) \
  ((type*)arena_allocate((arena), sizeof(type), alignof(type)))

#endif

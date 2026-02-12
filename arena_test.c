#include "adt/arena.h"

#include <assert.h>
#include <stdint.h>
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

int main(int argc, char **argv)
{
  int seed;
  if (argc > 1) {
    seed = atoi(argv[1]);
  } else {
    seed = time(NULL);
  }
  printf("Seed: %d\n", seed);

  struct arena arena;
  arena_init(&arena);
  for (int i = 0; i < 2000; ++i) {
    alloc_some(&arena);
    arena_free(&arena);
  }
}

#pragma once

#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

struct idynarray {
  char    *inline_storage;
  char    *data;
  unsigned size;
  unsigned capacity;
};

static inline void idynarray_init(struct idynarray *a, void *inline_storage,
                                  unsigned capacity)
{
  a->inline_storage = (char *)inline_storage;
  a->data = (char *)inline_storage;
  a->size = 0;
  a->capacity = capacity;
}

static void idynarray_free(struct idynarray *a)
{
  char *data = a->data;
  if (data != a->inline_storage) {
    free(data);
  }
}

static void idynarray_clear(struct idynarray *a)
{
  a->size = 0;
}

static __attribute__((noinline)) char *
idynarray_grow_(struct idynarray *a, unsigned old_size, unsigned new_size)
{
  unsigned new_capacity = a->capacity >= 16 ? a->capacity : 16;
  while (new_capacity < new_size) {
    if (new_capacity > UINT_MAX / 2) {
      new_capacity = new_size;
      break;
    }
    new_capacity <<= 1;
  }

  char *old_data = a->data;
  char *new_data = malloc(new_capacity);
  if (new_data == NULL) {
    abort();
  }
  memcpy(new_data, old_data, old_size);
  if (old_data != a->inline_storage) free(old_data);

  a->data = new_data;
  a->capacity = new_capacity;
  return new_data;
}

static inline char *idynarray_append_size(struct idynarray *a, unsigned size)
{
  unsigned old_size = a->size;
  if (size > UINT_MAX - old_size) {
    abort();
  }
  unsigned new_size = old_size + size;
  char    *data = a->data;
  if (new_size > a->capacity) {
    data = idynarray_grow_(a, old_size, new_size);
  }
  a->size = new_size;
  return data + old_size;
}

#define idynarray_append(dynarray, type)                                      \
  ((type *)idynarray_append_size((dynarray), sizeof(type)))

static inline unsigned idynarray_size(struct idynarray *a)
{
  return a->size;
}

#define idynarray_length(dynarray, type)                                      \
  (idynarray_size((dynarray)) / (unsigned)sizeof(type))

static void *idynarray_data(struct idynarray *a)
{
  return a->data;
}

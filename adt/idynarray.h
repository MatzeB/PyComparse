#pragma once

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

static inline char *idynarray_append_size(struct idynarray *a, unsigned size)
{
  unsigned old_size = a->size;
  unsigned capacity = a->capacity;
  unsigned new_size = old_size + size;
  char    *data = a->data;
  if (new_size > capacity) {
    unsigned new_capacity = capacity << 1;
    assert(new_size < new_capacity);
    char *new_data = malloc(new_capacity);
    if (new_data == NULL) {
      abort();
    }
    memcpy(new_data, data, old_size);
    if (data != a->inline_storage) free(data);
    data = new_data;
    a->data = data;
    a->capacity = new_capacity;
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

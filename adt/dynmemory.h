#ifndef DYNMEMORY_H
#define DYNMEMORY_H

#ifndef UNLIKELY
#define UNLIKELY(x)    __builtin_expect((x), 0)
#endif

struct dynmemory {
  unsigned size;
  unsigned capacity;
  void *memory;
};

static inline void dynmemory_init(struct dynmemory *m)
{
  memset(m, 0, sizeof(*m));
}

static inline void dynmemory_free(struct dynmemory *m)
{
  free(m->memory);
}

static inline void *dynmemory_grow(struct dynmemory *m, unsigned size)
{
  if (UNLIKELY(m->size + size > m->capacity)) {
    unsigned new_capacity = ceil_po2(m->size + size);
    new_capacity = new_capacity > 64 ? new_capacity : 64;
    m->memory = realloc(m->memory, new_capacity);
    m->capacity = new_capacity;
  }
  void *result = (char*)m->memory + m->size;
  m->size += size;
  return result;
}

static inline void *dynmemory_shrink(struct dynmemory *m, unsigned size)
{
  assert(size <= m->size);
  if (UNLIKELY(m->size - size <= m->capacity / 4 && m->capacity > 64)) {
    unsigned new_capacity = m->capacity / 2;
    m->memory = realloc(m->memory, new_capacity);
    m->capacity = new_capacity;
  }
  m->size -= size;
}

static inline unsigned dunmemory_size(const struct dynmemory *m)
{
  return m->size;
}

#endif

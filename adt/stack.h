#pragma once

#include <assert.h>
#include <limits.h>
#include <stdlib.h>

#include "dynmemory.h"

struct stack {
  char    *data;
  unsigned size;
  unsigned capacity;
};

static inline void *stack_push(struct stack *stack, unsigned size)
{
  unsigned old_size = stack->size;
  if (size > UINT_MAX - old_size) {
    abort();
  }
  unsigned new_size = old_size + size;
  if (new_size > stack->capacity) {
    stack->data
        = (char *)dynmemory_grow(stack->data, &stack->capacity, new_size, 1);
    if (stack->data == NULL) {
      abort();
    }
  }
  stack->size = new_size;
  return stack->data + old_size;
}

static inline void *stack_last(struct stack *stack, unsigned size)
{
  assert(size <= stack->size);
  return stack->data + stack->size - size;
}

static inline void stack_pop(struct stack *stack, unsigned size)
{
  assert(size <= stack->size);
  stack->size -= size;
}

static inline void stack_free(struct stack *stack)
{
  free(stack->data);
}

static inline unsigned stack_size(struct stack *stack)
{
  return stack->size;
}

#ifndef BITFIDDLE_H
#define BITFIDDLE_H

#include <limits.h>

static inline __attribute__((const))
unsigned ceil_po2(unsigned x)
{
  if (x == 0)
    return 0;
  unsigned just_high_bit = 1u << (sizeof(x) * CHAR_BIT - 1);
  assert(x < just_high_bit);
  return just_high_bit >> (__builtin_clz(x) - 1);
}

#endif

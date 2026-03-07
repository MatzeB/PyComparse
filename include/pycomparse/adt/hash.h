#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

static inline unsigned fnv_hash_init(void)
{
  return 2166136261u;
}

static inline unsigned fnv_hash_append(unsigned hash, uint32_t length,
                                       const void *data)
{
  const unsigned char *bytes = (const unsigned char *)data;
  for (uint32_t i = 0; i < length; ++i) {
    hash ^= bytes[i];
    hash *= 16777619u;
  }
  return hash;
}

static inline unsigned fnv_hash_cstring(const char *string)
{
  unsigned hash = fnv_hash_init();
  for (const char *p = string; *p != '\0'; ++p) {
    hash ^= (unsigned char)*p;
    hash *= 16777619u;
  }
  return hash;
}

#ifdef __cplusplus
}
#endif

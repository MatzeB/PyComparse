#pragma once

#include <stdint.h>
#include <string.h>

#if !defined(PYCOMPARSE_HASH_ALGO)
#if defined(__SSE4_2__) && (defined(__x86_64__) || defined(__i386__))
#define PYCOMPARSE_HASH_ALGO 2
#else
#define PYCOMPARSE_HASH_ALGO 1
#endif
#endif

#if PYCOMPARSE_HASH_ALGO == 2
#include <nmmintrin.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

#if PYCOMPARSE_HASH_ALGO == 1
static inline unsigned hash_init(void)
{
  return 2166136261u;
}

static inline unsigned hash_append_byte(unsigned hash, unsigned char byte)
{
  hash ^= byte;
  hash *= 16777619u;
  return hash;
}

static inline unsigned hash_append(unsigned hash, uint32_t length,
                                   const void *data)
{
  const unsigned char *bytes = (const unsigned char *)data;
  for (uint32_t i = 0; i < length; ++i) {
    hash = hash_append_byte(hash, bytes[i]);
  }
  return hash;
}

static inline unsigned hash_cstring(const char *string)
{
  unsigned hash = hash_init();
  for (const char *p = string; *p != '\0'; ++p) {
    hash = hash_append_byte(hash, (unsigned char)*p);
  }
  return hash;
}
#elif PYCOMPARSE_HASH_ALGO == 2
static inline unsigned hash_init(void)
{
  return 0u;
}

static inline unsigned hash_append(unsigned hash, uint32_t length,
                                   const void *data)
{
  const unsigned char *bytes = (const unsigned char *)data;

#if UINTPTR_MAX > 0xffffffffu
  while (length >= 8) {
    uint64_t chunk;
    memcpy(&chunk, bytes, sizeof(chunk));
    hash = (unsigned)_mm_crc32_u64((uint64_t)hash, chunk);
    bytes += 8;
    length -= 8;
  }
#endif
  while (length > 0) {
    hash = (unsigned)_mm_crc32_u8(hash, *bytes++);
    --length;
  }
  return hash;
}

static inline unsigned hash_append_byte(unsigned hash, unsigned char byte)
{
  return (unsigned)_mm_crc32_u8(hash, byte);
}

static inline unsigned hash_cstring(const char *string)
{
  unsigned hash = hash_init();
  for (const char *p = string; *p != '\0'; ++p) {
    hash = hash_append_byte(hash, (unsigned char)*p);
  }
  return hash;
}
#else
#error "Unsupported PYCOMPARSE_HASH_ALGO value"
#endif

#ifdef __cplusplus
}
#endif

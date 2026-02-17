#include "object_intern.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "object_intern_types.h"
#include "object_types.h"
#include "util.h"

struct object_intern_string_bucket {
  union object *nullable object;
  unsigned               hash;
};

struct object_intern_object_bucket {
  union object *nullable object;
  unsigned               hash;
};

static unsigned fnv_hash_append(unsigned hash, uint32_t length,
                                const void *data)
{
  const char *bytes = (const char *)data;
  for (uint32_t i = 0; i < length; ++i) {
    hash ^= (unsigned char)bytes[i];
    hash *= 16777619u;
  }
  return hash;
}

static unsigned fnv_hash_mem(enum object_type type, uint32_t length,
                             const void *data)
{
  unsigned hash = 2166136261u;
  hash ^= (unsigned)type;
  hash *= 16777619u;
  hash ^= length;
  hash *= 16777619u;
  return fnv_hash_append(hash, length, data);
}

static unsigned fnv_hash_int(int64_t value)
{
  return fnv_hash_mem(OBJECT_INT, sizeof(value), &value);
}

static bool int_equal(const union object *object, int64_t value)
{
  return object_type(object) == OBJECT_INT
         && object_int_value(object) == value;
}

static unsigned fnv_hash_float(double value)
{
  return fnv_hash_mem(OBJECT_FLOAT, sizeof(value), &value);
}

static bool float_equal(const union object *object, double value)
{
  if (object_type(object) != OBJECT_FLOAT) return false;
  double object_value = object_float_value(object);
  return memcmp(&object_value, &value, sizeof(value)) == 0;
}

static unsigned fnv_hash_complex(double real, double imag)
{
  unsigned hash = fnv_hash_mem(OBJECT_COMPLEX, sizeof(real), &real);
  return fnv_hash_append(hash, sizeof(imag), &imag);
}

static bool complex_equal(const union object *object, double real, double imag)
{
  if (object_type(object) != OBJECT_COMPLEX) return false;
  double object_real = object_complex_real(object);
  double object_imag = object_complex_imag(object);
  return memcmp(&object_real, &real, sizeof(real)) == 0
         && memcmp(&object_imag, &imag, sizeof(imag)) == 0;
}

static unsigned fnv_hash_big_int(uint32_t        num_pydigits,
                                 const uint16_t *pydigits)
{
  return fnv_hash_mem(OBJECT_BIG_INT,
                      (uint32_t)(num_pydigits * sizeof(pydigits[0])),
                      pydigits);
}

static bool big_int_equal(const union object *object, uint32_t num_pydigits,
                          const uint16_t *pydigits)
{
  return object_type(object) == OBJECT_BIG_INT
         && object->big_int.num_pydigits == num_pydigits
         && memcmp(object->big_int.pydigits, pydigits,
                   (size_t)num_pydigits * sizeof(pydigits[0]))
                == 0;
}

static unsigned fnv_hash_tuple_like(enum object_type    type,
                                    const union object *tuple)
{
  uint32_t length = tuple->tuple.length;
  return fnv_hash_mem(type, length * (uint32_t)sizeof(tuple->tuple.items[0]),
                      tuple->tuple.items);
}

static bool tuple_like_equal(const union object *a, enum object_type type,
                             const union object *b)
{
  if (object_type(a) != type) return false;
  uint32_t length = b->tuple.length;
  if (a->tuple.length != length) return false;
  return memcmp(a->tuple.items, b->tuple.items,
                (size_t)length * sizeof(a->tuple.items[0]))
         == 0;
}

static void object_intern_insert_string_bucket(struct object_intern *s,
                                               union object         *string,
                                               unsigned              hash)
{
  hash_set_increment_num_elements(&s->string_set);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->string_set, hash);
  struct object_intern_string_bucket *buckets = s->string_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_string_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      bucket->object = string;
      bucket->hash = hash;
      return;
    }
  }
}

static void object_intern_resize_strings(struct object_intern *s,
                                         unsigned              new_size)
{
  struct object_intern_string_bucket *old_buckets = s->string_buckets;
  unsigned num_old_buckets = hash_set_num_buckets(&s->string_set);

  s->string_buckets = calloc(new_size, sizeof(s->string_buckets[0]));
  if (s->string_buckets == NULL) {
    internal_error("out of memory");
  }
  hash_set_init(&s->string_set, new_size);

  for (unsigned i = 0; i < num_old_buckets; ++i) {
    struct object_intern_string_bucket *bucket = &old_buckets[i];
    if (bucket->object != NULL) {
      object_intern_insert_string_bucket(s, bucket->object, bucket->hash);
    }
  }

  free(old_buckets);
}

static void object_intern_insert_object_bucket(struct object_intern *s,
                                               union object         *object,
                                               unsigned              hash)
{
  hash_set_increment_num_elements(&s->object_set);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      bucket->object = object;
      bucket->hash = hash;
      return;
    }
  }
}

static void object_intern_resize_objects(struct object_intern *s,
                                         unsigned              new_size)
{
  struct object_intern_object_bucket *old_buckets = s->object_buckets;
  unsigned num_old_buckets = hash_set_num_buckets(&s->object_set);

  s->object_buckets = calloc(new_size, sizeof(s->object_buckets[0]));
  if (s->object_buckets == NULL) {
    internal_error("out of memory");
  }
  hash_set_init(&s->object_set, new_size);

  for (unsigned i = 0; i < num_old_buckets; ++i) {
    struct object_intern_object_bucket *bucket = &old_buckets[i];
    if (bucket->object != NULL) {
      object_intern_insert_object_bucket(s, bucket->object, bucket->hash);
    }
  }

  free(old_buckets);
}

static void object_intern_ensure_object_set(struct object_intern *s)
{
  unsigned new_size = hash_set_should_resize(&s->object_set);
  if (UNLIKELY(new_size != 0)) {
    object_intern_resize_objects(s, new_size);
  }
}

void object_intern_init(struct object_intern *s)
{
  memset(s, 0, sizeof(*s));

  unsigned num_string_buckets = 4096;
  hash_set_init(&s->string_set, num_string_buckets);
  s->string_buckets = calloc(num_string_buckets, sizeof(s->string_buckets[0]));
  if (s->string_buckets == NULL) {
    internal_error("out of memory");
  }

  unsigned num_object_buckets = 256;
  hash_set_init(&s->object_set, num_object_buckets);
  s->object_buckets = calloc(num_object_buckets, sizeof(s->object_buckets[0]));
  if (s->object_buckets == NULL) {
    internal_error("out of memory");
  }

  s->singleton_none = object_new_singleton(&s->arena, OBJECT_NONE);
  s->singleton_true = object_new_singleton(&s->arena, OBJECT_TRUE);
  s->singleton_false = object_new_singleton(&s->arena, OBJECT_FALSE);
  s->singleton_ellipsis = object_new_singleton(&s->arena, OBJECT_ELLIPSIS);

  struct tuple_prep *empty_tuple_prep = object_new_tuple_begin(&s->arena, 0);
  s->empty_tuple = object_new_tuple_end(empty_tuple_prep);
}

void object_intern_free(struct object_intern *s)
{
  free(s->string_buckets);
  free(s->object_buckets);
  arena_free(&s->arena);
}

struct arena *object_intern_arena(struct object_intern *s)
{
  return &s->arena;
}

union object *object_intern_singleton(struct object_intern *s,
                                      enum object_type      type)
{
  switch (type) {
  case OBJECT_NONE:
    return s->singleton_none;
  case OBJECT_TRUE:
    return s->singleton_true;
  case OBJECT_FALSE:
    return s->singleton_false;
  case OBJECT_ELLIPSIS:
    return s->singleton_ellipsis;
  default:
    internal_error("object_intern_singleton called with non-singleton");
  }
}

union object *object_intern_empty_tuple(struct object_intern *s)
{
  return s->empty_tuple;
}

union object *object_intern_string(struct object_intern *s,
                                   enum object_type type, uint32_t length,
                                   const char *chars)
{
  assert(type == OBJECT_STRING || type == OBJECT_BYTES);
  unsigned new_size = hash_set_should_resize(&s->string_set);
  if (UNLIKELY(new_size != 0)) {
    object_intern_resize_strings(s, new_size);
  }

  unsigned hash = fnv_hash_mem(type, length, chars);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->string_set, hash);
  struct object_intern_string_bucket *buckets = s->string_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_string_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash) {
      union object *object = bucket->object;
      if ((enum object_type)object->type == type
          && object->string.length == length
          && (object->string.chars == chars || length == 0
              || memcmp(object->string.chars, chars, length) == 0)) {
        return object;
      }
    }
  }

  union object *string = object_new_string(&s->arena, type, length, chars);
  object_intern_insert_string_bucket(s, string, hash);
  return string;
}

union object *object_intern_cstring(struct object_intern *s,
                                    const char           *cstring)
{
  size_t length = strlen(cstring);
  assert(length == (uint32_t)length);
  return object_intern_string(s, OBJECT_STRING, (uint32_t)length, cstring);
}

union object *object_intern_float(struct object_intern *s, double value)
{
  object_intern_ensure_object_set(s);

  unsigned                              hash = fnv_hash_float(value);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash && float_equal(bucket->object, value)) {
      return bucket->object;
    }
  }

  union object *result = object_new_float(&s->arena, value);
  object_intern_insert_object_bucket(s, result, hash);
  return result;
}

union object *object_intern_complex(struct object_intern *s, double real,
                                    double imag)
{
  object_intern_ensure_object_set(s);

  unsigned                              hash = fnv_hash_complex(real, imag);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash && complex_equal(bucket->object, real, imag)) {
      return bucket->object;
    }
  }

  union object *result = object_new_complex(&s->arena, real, imag);
  object_intern_insert_object_bucket(s, result, hash);
  return result;
}

union object *object_intern_int(struct object_intern *s, int64_t value)
{
  assert(value != INT64_MIN);
  object_intern_ensure_object_set(s);

  unsigned                              hash = fnv_hash_int(value);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash && int_equal(bucket->object, value)) {
      return bucket->object;
    }
  }

  union object *result = object_new_int(&s->arena, value);
  object_intern_insert_object_bucket(s, result, hash);
  return result;
}

union object *object_intern_big_int(struct object_intern   *s,
                                    uint32_t                num_pydigits,
                                    const uint16_t *nonnull pydigits)
{
  assert(num_pydigits > 0);
  while (num_pydigits > 0 && pydigits[num_pydigits - 1] == 0) {
    --num_pydigits;
  }
  if (num_pydigits == 0) {
    return object_intern_int(s, 0);
  }

  object_intern_ensure_object_set(s);

  unsigned hash = fnv_hash_big_int(num_pydigits, pydigits);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash
        && big_int_equal(bucket->object, num_pydigits, pydigits)) {
      return bucket->object;
    }
  }

  union object *result = object_new_big_int(&s->arena, num_pydigits, pydigits);
  object_intern_insert_object_bucket(s, result, hash);
  return result;
}

struct tuple_prep *object_intern_tuple_begin(struct object_intern *s,
                                             uint32_t              length)
{
  return object_new_tuple_begin(&s->arena, length);
}

union object *object_intern_tuple_end(struct object_intern *s,
                                      struct tuple_prep    *tuple,
                                      bool                  may_free_arena)
{
  union object *tuple_obj = object_new_tuple_end(tuple);

  object_intern_ensure_object_set(s);

  unsigned hash = fnv_hash_tuple_like(OBJECT_TUPLE, tuple_obj);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash
        && tuple_like_equal(bucket->object, OBJECT_TUPLE, tuple_obj)) {
      if (may_free_arena) {
        arena_free_to(&s->arena, tuple_obj);
      }
      return bucket->object;
    }
  }

  object_intern_insert_object_bucket(s, tuple_obj, hash);
  return tuple_obj;
}

union object *object_intern_tuple_end_as_frozenset(struct object_intern *s,
                                                   struct tuple_prep    *tuple,
                                                   bool may_free_arena)
{
  union object *obj = object_new_tuple_end(tuple);

  object_intern_ensure_object_set(s);

  unsigned hash = fnv_hash_tuple_like(OBJECT_FROZENSET, obj);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &s->object_set, hash);
  struct object_intern_object_bucket *buckets = s->object_buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct object_intern_object_bucket *bucket = &buckets[c.index];
    if (bucket->object == NULL) {
      break;
    }
    if (bucket->hash == hash
        && tuple_like_equal(bucket->object, OBJECT_FROZENSET, obj)) {
      if (may_free_arena) {
        arena_free_to(&s->arena, obj);
      }
      return bucket->object;
    }
  }

  obj->type = OBJECT_FROZENSET;
  object_intern_insert_object_bucket(s, obj, hash);
  return obj;
}

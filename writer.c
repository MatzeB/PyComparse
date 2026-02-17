#include "writer.h"

#include <assert.h>
#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "object_types.h"
#include "opcodes.h"
#include "util.h"

enum write_types {
  OBJECT_ASCII = 'a',
  OBJECT_SHORT_ASCII = 'z',
  OBJECT_SMALL_TUPLE = ')',
};

struct writer_state {
  FILE *out;
};

static void write_char(struct writer_state *s, char value)
{
  fputc(value, s->out);
}

static void write_uint8(struct writer_state *s, uint8_t value)
{
  fputc(value, s->out);
}

static void write_uint64(struct writer_state *s, uint64_t value)
{
  write_uint8(s, (uint8_t)(value >> 0));
  write_uint8(s, (uint8_t)(value >> 8));
  write_uint8(s, (uint8_t)(value >> 16));
  write_uint8(s, (uint8_t)(value >> 24));
  write_uint8(s, (uint8_t)(value >> 32));
  write_uint8(s, (uint8_t)(value >> 40));
  write_uint8(s, (uint8_t)(value >> 48));
  write_uint8(s, (uint8_t)(value >> 56));
}

static void write_uint32(struct writer_state *s, uint32_t value)
{
  write_uint8(s, (uint8_t)(value >> 0));
  write_uint8(s, (uint8_t)(value >> 8));
  write_uint8(s, (uint8_t)(value >> 16));
  write_uint8(s, (uint8_t)(value >> 24));
}

static void write_int32(struct writer_state *s, int32_t value)
{
  write_uint32(s, (uint32_t)value);
}

static void write_uint16(struct writer_state *s, uint16_t value)
{
  write_uint8(s, (uint8_t)(value >> 0));
  write_uint8(s, (uint8_t)(value >> 8));
}

static void write_header(struct writer_state *s)
{
  write_uint16(s, 3413); // Python 3.8b4
  write_uint8(s, '\r');
  write_uint8(s, '\n');
  write_uint32(s, 0); // 0
  write_uint32(s, 0); // mtime
  write_uint32(s, 0); // source size
}

static void write_object(struct writer_state *s, const union object *object);

static void write_tuple_(struct writer_state *s, uint32_t length,
                         union object *const *items)
{
  if (length < 256) {
    write_char(s, OBJECT_SMALL_TUPLE);
    write_uint8(s, (uint8_t)length);
  } else {
    write_char(s, OBJECT_TUPLE);
    write_uint32(s, length);
  }
  for (uint32_t i = 0; i < length; ++i) {
    write_object(s, items[i]);
  }
}

static void write_tuple(struct writer_state       *s,
                        const struct object_tuple *tuple)
{
  write_tuple_(s, tuple->length, tuple->items);
}

static void write_frozenset(struct writer_state       *s,
                            const struct object_tuple *frozenset)
{
  write_char(s, OBJECT_FROZENSET);
  write_uint32(s, frozenset->length);
  for (uint32_t i = 0; i < frozenset->length; ++i) {
    write_object(s, frozenset->items[i]);
  }
}

static bool is_ascii(const char *chars, uint32_t length)
{
  for (uint32_t i = 0; i < length; i++) {
    if (((uint8_t)chars[i]) > 127) return false;
  }
  return true;
}

static void write_string(struct writer_state        *s,
                         const struct object_string *string)
{
  char type = string->base.type;
  assert(type == OBJECT_STRING || type == OBJECT_BYTES);
  uint32_t    length = string->length;
  const char *chars = string->chars;
  if (type == OBJECT_STRING && is_ascii(chars, length)) {
    if (length < 256) {
      write_char(s, OBJECT_SHORT_ASCII);
      write_uint8(s, (uint8_t)length);
    } else {
      write_char(s, OBJECT_ASCII);
      write_uint32(s, length);
    }
  } else {
    write_char(s, type);
    write_uint32(s, length);
  }
  fwrite(chars, 1, length, s->out);
}

static void write_float(struct writer_state       *s,
                        const struct object_float *float_obj)
{
  uint64_t bits;
  assert(sizeof(bits) == sizeof(float_obj->value));
  memcpy(&bits, &float_obj->value, sizeof(bits));

  write_char(s, OBJECT_FLOAT);
  write_uint64(s, bits);
}

static void write_complex(struct writer_state         *s,
                          const struct object_complex *complex)
{
  uint64_t real_bits;
  uint64_t imag_bits;
  assert(sizeof(real_bits) == sizeof(complex->real));
  assert(sizeof(imag_bits) == sizeof(complex->imag));
  memcpy(&real_bits, &complex->real, sizeof(real_bits));
  memcpy(&imag_bits, &complex->imag, sizeof(imag_bits));

  write_char(s, OBJECT_COMPLEX);
  write_uint64(s, real_bits);
  write_uint64(s, imag_bits);
}

static void write_int(struct writer_state *s, const struct object_int *int_obj)
{
  int64_t value = int_obj->value;
  assert(value != INT64_MIN);
  if (value >= INT32_MIN && value <= INT32_MAX) {
    write_char(s, OBJECT_INT);
    write_int32(s, (int32_t)value);
    return;
  }

  uint64_t magnitude;
  int32_t  sign = 1;
  if (value < 0) {
    sign = -1;
    magnitude = (uint64_t)(-value);
  } else {
    magnitude = (uint64_t)value;
  }

  uint32_t num_digits = 0;
  for (uint64_t c = magnitude; c != 0; c >>= 15) {
    ++num_digits;
  }
  if (num_digits > INT32_MAX) {
    internal_error("integer too large to marshal");
  }

  write_char(s, 'l');
  write_int32(s, (int32_t)num_digits * sign);
  for (uint64_t c = magnitude; c != 0; c >>= 15) {
    write_uint16(s, (uint16_t)(c & 0x7fff));
  }
}

static void write_big_int(struct writer_state         *s,
                          const struct object_big_int *big_int)
{
  assert(big_int->pydigits != NULL);
  uint32_t num_digits = big_int->num_pydigits;
  while (num_digits > 0 && big_int->pydigits[num_digits - 1] == 0) {
    --num_digits;
  }
  write_char(s, 'l');
  if (num_digits > INT32_MAX) {
    internal_error("integer too large to marshal");
  }
  write_int32(s, (int32_t)num_digits);
  for (uint32_t i = 0; i < num_digits; ++i) {
    write_uint16(s, big_int->pydigits[i]);
  }
}

static void write_code(struct writer_state *s, const struct object_code *code)
{
  write_char(s, OBJECT_CODE);
  write_uint32(s, code->argcount);
  write_uint32(s, code->posonlyargcount);
  write_uint32(s, code->kwonlyargcount);
  write_uint32(s, code->nlocals);
  write_uint32(s, code->stacksize);
  write_uint32(s, code->flags);
  write_object(s, code->code);
  write_object(s, code->consts);
  write_object(s, code->names);
  write_object(s, code->varnames);
  write_object(s, code->freevars);
  write_object(s, code->cellvars);
  write_object(s, code->filename);
  write_object(s, code->name);
  write_uint32(s, code->firstlineno);
  write_object(s, code->lnotab);
}

static void write_object(struct writer_state *s, const union object *object)
{
  if (object == NULL) {
    write_char(s, OBJECT_NULL);
    return;
  }
  switch (object->type) {
  case OBJECT_NONE:
  case OBJECT_TRUE:
  case OBJECT_FALSE:
  case OBJECT_ELLIPSIS:
    write_char(s, object->type);
    break;
  case OBJECT_TUPLE:
    write_tuple(s, &object->tuple);
    break;
  case OBJECT_FROZENSET:
    write_frozenset(s, &object->tuple);
    break;
  case OBJECT_CODE:
    write_code(s, &object->code);
    break;
  case OBJECT_BYTES:
  case OBJECT_STRING:
    write_string(s, &object->string);
    break;
  case OBJECT_FLOAT:
    write_float(s, &object->float_obj);
    break;
  case OBJECT_COMPLEX:
    write_complex(s, &object->complex);
    break;
  case OBJECT_INT:
    write_int(s, &object->int_obj);
    break;
  case OBJECT_BIG_INT:
    write_big_int(s, &object->big_int);
    break;
  case OBJECT_NULL:
  default:
    internal_error("invalid object in write_object");
  }
}

void write_module(FILE *out, const union object *object)
{
  struct writer_state s;
  s.out = out;
  write_header(&s);
  write_single_object(out, object);
}

void write_single_object(FILE *out, const union object *object)
{
  struct writer_state s;
  s.out = out;
  write_object(&s, object);
}

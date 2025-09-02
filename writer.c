#include "writer.h"

#include <assert.h>
#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/dynmemory.h"
#include "object.h"
#include "object_types.h"
#include "opcodes.h"

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

static void write_list(struct writer_state *s, const struct object_list *list)
{
  write_char(s, OBJECT_LIST);
  write_uint32(s, list->length);
  uint32_t length = list->length;
  assert(length < UINT32_MAX);
  for (uint32_t i = 0; i < length; ++i) {
    write_object(s, list->items[i]);
  }
}

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

static void write_list_as_tuple(struct writer_state *s,
                                const union object  *list)
{
  assert(list->type == OBJECT_LIST);
  write_tuple_(s, list->list.length, list->list.items);
}

static void write_string(struct writer_state        *s,
                         const struct object_string *string)
{
  char type = string->base.type;
  assert(type == OBJECT_STRING || type == OBJECT_ASCII);
  uint32_t length = string->length;
  if (length < 256 && type == OBJECT_ASCII) {
    write_char(s, OBJECT_SHORT_ASCII);
    write_uint8(s, (uint8_t)length);
  } else {
    write_char(s, type);
    write_uint32(s, length);
  }
  for (uint32_t i = 0; i < length; ++i) {
    assert(type == OBJECT_STRING
           || (((uint8_t)string->chars[i]) < 128 && "TODO: non-ascii"));
    write_char(s, string->chars[i]);
  }
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

static void write_int(struct writer_state *s, const struct object_int *int_obj)
{
  int64_t value = int_obj->value;
  if (INT32_MIN <= value && value <= INT32_MAX) {
    write_char(s, OBJECT_INT);
    write_uint32(s, (int32_t)int_obj->value);
    return;
  }
  bool negative = false;
  if (value < 0) {
    negative = true;
    if (value == INT64_MIN) {
      abort(); /* TODO, overflows -value */
    }
    value = -value;
  }
  unsigned num_digits = 0;
  for (int64_t c = value; c != 0; c >>= 15) {
    ++num_digits;
  }
  write_char(s, 'l');
  write_uint32(s, (uint32_t)(negative ? -(int32_t)num_digits : num_digits));
  for (int64_t c = value; c != 0; c >>= 15) {
    write_uint16(s, c & 0x7fff);
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
  write_list_as_tuple(s, code->consts);
  write_list_as_tuple(s, code->names);
  write_list_as_tuple(s, code->varnames);
  write_object(s, code->freevars);
  write_object(s, code->cellvars);
  write_object(s, code->filename);
  write_object(s, code->name);
  write_uint32(s, 1); // firstlineno
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
  case OBJECT_LIST:
    write_list(s, &object->list);
    break;
  case OBJECT_TUPLE:
    write_tuple(s, &object->tuple);
    break;
  case OBJECT_CODE:
    write_code(s, &object->code);
    break;
  case OBJECT_ASCII:
  case OBJECT_STRING:
    write_string(s, &object->string);
    break;
  case OBJECT_FLOAT:
    write_float(s, &object->float_obj);
    break;
  case OBJECT_INT:
    write_int(s, &object->int_obj);
    break;
  case OBJECT_NULL:
  default:
    abort();
  }
}

void write_module(FILE *out, const union object *object)
{
  struct writer_state s;
  s.out = out;
  write_header(&s);
  write_object(&s, object);
}

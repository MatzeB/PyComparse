#include "writer.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static void write_uint8(struct bytecode_writer_state *s, uint8_t value)
{
  fputc(value, s->out);
}

static void write_uint32(struct bytecode_writer_state *s, uint32_t value)
{
  write_uint8(s, value >> 0);
  write_uint8(s, value >> 8);
  write_uint8(s, value >> 16);
  write_uint8(s, value >> 24);
}

static void write_uint16(struct bytecode_writer_state *s, uint16_t value)
{
  write_uint8(s, value >> 0);
  write_uint8(s, value >> 8);
}

static void write_header(struct bytecode_writer_state *s)
{
  write_uint16(s, 3394);
  write_uint8(s, '\r');
  write_uint8(s, '\n');
  write_uint32(s, 0); // 0
  write_uint32(s, 0); // mtime
  write_uint32(s, 0); // source size
}

void writer_begin(struct bytecode_writer_state *s, FILE *out)
{
  memset(s, 0, sizeof(*s));
  s->out = out;
  write_header(s);
}

static void write_list_or_tuple(struct bytecode_writer_state *s,
    const struct object_list_or_tuple *list_or_tuple)
{
  char type = list_or_tuple->base.type;
  assert(type == TYPE_LIST || type == TYPE_TUPLE);
  uint32_t n_elements = list_or_tuple->n_elements;
  if (n_elements < 256 && type == TYPE_TUPLE) {
    write_uint8(s, TYPE_SMALL_TUPLE);
    write_uint8(s, n_elements);
  } else {
    write_uint8(s, type);
    write_uint32(s, n_elements);
  }
  for (uint32_t i = 0; i < n_elements; ++i) {
    write(s, list_or_tuple->elements[i]);
  }
}

static void write_string(struct bytecode_writer_state *s,
                         const struct object_string *string)
{
  char type = string->base.type;
  assert(type == TYPE_STRING || type == TYPE_ASCII);
  uint32_t len = string->len;
  if (len < 256 && type == TYPE_ASCII) {
    write_uint8(s, TYPE_SHORT_ASCII);
    write_uint8(s, len);
  } else {
    write_uint8(s, type);
    write_uint32(s, len);
  }
  for (uint32_t i = 0; i < len; ++i)
    write_uint8(s, string->chars[i]);
}

static void write_code(struct bytecode_writer_state *s,
                       const struct object_code *code)
{
  write_uint8(s, TYPE_CODE);
  write_uint32(s, code->argcount);
  write_uint32(s, code->kwonlyargcount);
  write_uint32(s, code->nlocals);
  write_uint32(s, code->stacksize);
  write_uint32(s, code->flags);
  write(s, code->code);
  write(s, code->consts);
  write(s, code->names);
  write(s, code->varnames);
  write(s, code->freevars);
  write(s, code->cellvars);
  write(s, code->filename);
  write(s, code->name);
  write_uint32(s, 1); // firstlineno
  write(s, code->lnotab);
}

void write(struct bytecode_writer_state *s, const union object *object)
{
  if (object == NULL) {
    write_uint8(s, TYPE_NULL);
    return;
  }
  switch (object->type) {
  case TYPE_NONE:
  case TYPE_TRUE:
  case TYPE_FALSE:
    write_uint8(s, object->type);
    break;
  case TYPE_LIST:
  case TYPE_TUPLE:
    write_list_or_tuple(s, &object->list_or_tuple);
    break;
  case TYPE_CODE:
    write_code(s, &object->code);
    break;
  case TYPE_ASCII:
  case TYPE_STRING:
    write_string(s, &object->string);
    break;
  case TYPE_NULL:
  default:
    abort();
  }
}

bool objects_equal(const union object *object0, const union object *object1)
{
  if (object0 == NULL) {
    return object1 == NULL;
  }
  char type = object0->type;
  if (type != object1->type)
    return false;
  switch (type) {
  case TYPE_NONE:
  case TYPE_TRUE:
  case TYPE_FALSE:
    return true;

  case TYPE_STRING:
  case TYPE_ASCII: {
    uint32_t len = object0->string.len;
    if (len != object1->string.len)
      return false;
    return memcmp(object0->string.chars, object1->string.chars, len) == 0;
  }

  default:
    abort();
  }
}

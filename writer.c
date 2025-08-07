#include "writer.h"

#include <assert.h>
#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/dynmemory.h"
#include "objects.h"
#include "objects_types.h"
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
  write_uint16(s, 3413);  // Python 3.8b4
  write_uint8(s, '\r');
  write_uint8(s, '\n');
  write_uint32(s, 0); // 0
  write_uint32(s, 0); // mtime
  write_uint32(s, 0); // source size
}

static void write_object(struct writer_state *s, const union object *object);

static void write_list(struct writer_state *s, const struct object_list *list)
{
  write_char(s, TYPE_LIST);
  write_uint32(s, list->length);
  uint32_t length = list->length;
  assert(length < UINT32_MAX);
  for (uint32_t i = 0; i < length; ++i) {
    write_object(s, list->items[i]);
  }
}

static void write_tuple_(struct writer_state *s,
                         uint32_t length, union object *const *items)
{
  if (length < 256) {
    write_char(s, TYPE_SMALL_TUPLE);
    write_uint8(s, (uint8_t)length);
  } else {
    write_char(s, TYPE_TUPLE);
    write_uint32(s, length);
  }
  for (uint32_t i = 0; i < length; ++i) {
    write_object(s, items[i]);
  }
}

static void write_tuple(struct writer_state *s,
                        const struct object_tuple *tuple)
{
  write_tuple_(s, tuple->length, tuple->items);
}

static void write_list_as_tuple(struct writer_state *s,
                                const union object *list)
{
  assert(list->type == TYPE_LIST);
  write_tuple_(s, list->list.length, list->list.items);
}

static void write_string(struct writer_state *s,
                         const struct object_string *string)
{
  char type = string->base.type;
  assert(type == TYPE_STRING || type == TYPE_ASCII);
  uint32_t length = string->length;
  if (length < 256 && type == TYPE_ASCII) {
    write_char(s, TYPE_SHORT_ASCII);
    write_uint8(s, (uint8_t)length);
  } else {
    write_char(s, type);
    write_uint32(s, length);
  }
  for (uint32_t i = 0; i < length; ++i) {
    write_char(s, string->chars[i]);
  }
}

static void write_int(struct writer_state *s,
                      const struct object_int *obj_int)
{
  write_char(s, TYPE_INT);
  write_uint32(s, (uint32_t)obj_int->value);
}

static void write_code(struct writer_state *s,
                       const struct object_code *code)
{
  write_char(s, TYPE_CODE);
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
    write_char(s, TYPE_NULL);
    return;
  }
  switch (object->type) {
  case TYPE_NONE:
  case TYPE_TRUE:
  case TYPE_FALSE:
  case TYPE_ELLIPSIS:
    write_char(s, object->type);
    break;
  case TYPE_LIST:
    write_list(s, &object->list);
    break;
  case TYPE_TUPLE:
    write_tuple(s, &object->tuple);
    break;
  case TYPE_CODE:
    write_code(s, &object->code);
    break;
  case TYPE_ASCII:
  case TYPE_STRING:
    write_string(s, &object->string);
    break;
  case TYPE_INT:
    write_int(s, &object->int_obj);
    break;
  case TYPE_NULL:
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

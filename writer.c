#include "writer.h"

#include <assert.h>
#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/dynmemory.h"
#include "objects.h"
#include "opcodes.h"

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

static void write_list(struct bytecode_writer_state *s,
                       const struct object_list *list)
{
  write_uint8(s, TYPE_LIST);
  write_uint32(s, list->length);
  uint32_t length = list->length;
  assert(length < UINT32_MAX);
  for (uint32_t i = 0; i < length; ++i) {
    write(s, list->items[i]);
  }
}

static void write_tuple_(struct bytecode_writer_state *s,
                         uint32_t length, union object *const *items)
{
  if (length < 256) {
    write_uint8(s, TYPE_SMALL_TUPLE);
    write_uint8(s, length);
  } else {
    write_uint8(s, TYPE_TUPLE);
    write_uint32(s, length);
  }
  for (uint32_t i = 0; i < length; ++i) {
    write(s, items[i]);
  }
}

static void write_tuple(struct bytecode_writer_state *s,
                        const struct object_tuple *tuple)
{
  write_tuple_(s, tuple->length, tuple->items);
}

static void write_list_as_tuple(struct bytecode_writer_state *s,
                                const struct object_list *list)
{
  write_tuple_(s, list->length, list->items);
}

static void write_string(struct bytecode_writer_state *s,
                         const struct object_string *string)
{
  char type = string->base.type;
  assert(type == TYPE_STRING || type == TYPE_ASCII);
  uint32_t length = string->length;
  if (length < 256 && type == TYPE_ASCII) {
    write_uint8(s, TYPE_SHORT_ASCII);
    write_uint8(s, length);
  } else {
    write_uint8(s, type);
    write_uint32(s, length);
  }
  for (uint32_t i = 0; i < length; ++i)
    write_uint8(s, string->chars[i]);
}

static void write_int(struct bytecode_writer_state *s,
                      const struct object_int *obj_int)
{
  write_uint8(s, TYPE_INT);
  write_uint32(s, (uint32_t)obj_int->value);
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
  assert(code->consts->type == TYPE_LIST);
  write_list_as_tuple(s, &code->consts->list);
  assert(code->names->type == TYPE_LIST);
  write_list_as_tuple(s, &code->names->list);
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
  case TYPE_ELLIPSIS:
    write_uint8(s, object->type);
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
    uint32_t length = object0->string.length;
    if (length != object1->string.length)
      return false;
    return memcmp(object0->string.chars, object1->string.chars, length) == 0;
  }

  default:
    abort();
  }
}

struct object_list *object_new_list(struct arena *arena)
{
  struct object_list *list = arena_allocate_type(arena, struct object_list);
  memset(list, 0, sizeof(*list));
  list->base.type = TYPE_LIST;
  return list;
}

static void object_list_grow(struct object_list *list, unsigned size)
{
    list->items = (union object**)dynmemory_grow(list->items, &list->capacity,
                                                 size, sizeof(list->items[0]));
}

void object_list_append(struct object_list *list, union object *object)
{
  unsigned new_size = list->length + 1;
  if (UNLIKELY(new_size >= list->capacity)) {
    object_list_grow(list, new_size);
  }
  list->items[list->length++] = object;
}

static struct object_tuple *object_new_tuple(struct arena *arena,
                                             uint32_t length)
{
  struct object_tuple *tuple;
  unsigned size = sizeof(struct object_tuple) + length * sizeof(tuple->items[0]);
  tuple = arena_allocate(arena, size, alignof(struct object_tuple));
  tuple->base.type = TYPE_TUPLE;
  tuple->length = length;
  return tuple;
}

static struct object_code *object_new_code(struct arena *arena)
{
  struct object_code *code = arena_allocate_type(arena, struct object_code);
  memset(code, 0, sizeof(*code));
  code->base.type = TYPE_CODE;

  return code;
}

static bool is_singleton_type(char type) {
  switch (type) {
  case TYPE_ELLIPSIS:
  case TYPE_FALSE:
  case TYPE_NONE:
  case TYPE_TRUE:
    return true;
  default:
    return false;
  }
}

static struct object_base *object_new_singleton(struct arena *arena,
                                                char type) {
  assert(is_singleton_type(type));
  struct object_base *object = arena_allocate_type(arena, struct object_base);
  object->type = type;
  return object;
}

static struct object_string *make_string(struct arena *arena, char type,
                                         uint32_t length, const char *chars)
{
  struct object_string *result =
      arena_allocate_type(arena, struct object_string);
  result->base.type = type;
  result->length = length;
  result->chars = chars;
  return result;
}

static struct object_int *make_int(struct arena *arena, int32_t value)
{
  struct object_int *result = arena_allocate_type(arena, struct object_int);
  result->base.type = TYPE_INT;
  result->value = value;
  return result;
}

static struct object_string *make_bytes(struct arena *arena, uint32_t length,
                                        const char *chars)
{
  return make_string(arena, TYPE_STRING, length, chars);
}

static struct object_string *make_ascii(struct arena *arena, const char *str)
{
  uint32_t length = strlen(str);
  return make_string(arena, TYPE_ASCII, length, str);
}

static bool is_absjump(uint32_t opcode) {
  switch (opcode) {
  case OPCODE_JUMP_ABSOLUTE:
  case OPCODE_JUMP_IF_FALSE_OR_POP:
  case OPCODE_JUMP_IF_TRUE_OR_POP:
  case OPCODE_POP_JUMP_IF_FALSE:
  case OPCODE_POP_JUMP_IF_TRUE:
    return true;
  }
  return false;
}

static bool is_jump(uint8_t opcode) {
  switch (opcode) {
  case OPCODE_FOR_ITER:
  case OPCODE_JUMP_ABSOLUTE:
  case OPCODE_JUMP_FORWARD:
  case OPCODE_JUMP_IF_FALSE_OR_POP:
  case OPCODE_JUMP_IF_TRUE_OR_POP:
  case OPCODE_POP_JUMP_IF_FALSE:
  case OPCODE_POP_JUMP_IF_TRUE:
  case OPCODE_SETUP_EXCEPT:
  case OPCODE_SETUP_FINALLY:
  case OPCODE_SETUP_LOOP:
  case OPCODE_SETUP_WITH:
    return true;
  }
  return false;
}

void write_op(struct bytecode_writer_state *s, uint8_t opcode, uint32_t arg)
{
  assert(!is_jump(opcode));
  if (arg >= 256) {
    assert(arg <= 0xffff);
    arena_grow_char(&s->opcodes, (char)OPCODE_EXTENDED_ARG);
    arena_grow_char(&s->opcodes, arg >> 8);
    arg &= 0xff;
  }
  arena_grow_char(&s->opcodes, opcode);
  arena_grow_char(&s->opcodes, arg);
}

static void push(struct bytecode_writer_state *s)
{
  s->code.stacksize++;
  if (s->code.stacksize > s->code.max_stacksize)
    s->code.max_stacksize = s->code.stacksize;
}

void writer_pop(struct bytecode_writer_state *s, unsigned n)
{
  assert(s->code.stacksize >= n);
  s->code.stacksize -= n;
}

void write_push_op(struct bytecode_writer_state *s, uint8_t opcode,
                   uint32_t arg)
{
  write_op(s, opcode, arg);
  push(s);
}

void write_pop_op(struct bytecode_writer_state *s, uint8_t opcode,
                  uint32_t arg)
{
  write_op(s, opcode, arg);
  writer_pop(s, 1);
}

unsigned writer_register_singleton(struct bytecode_writer_state *s, char type)
{
  assert(is_singleton_type(type));
  struct object_list *consts = s->code.consts;
  for (unsigned i = 0; i < consts->length; ++i) {
    const union object *object = consts->items[i];
    if (object->type == type)
      return i;
  }
  struct object_base *singleton = object_new_singleton(&s->objects, type);
  object_list_append(consts, (union object*)singleton);
  return consts->length - 1;
}

unsigned writer_register_string(struct bytecode_writer_state *s,
                                const char *chars, uint32_t length)
{
  struct object_list *consts = s->code.consts;
  for (unsigned i = 0; i < consts->length; ++i) {
    const union object *object = consts->items[i];
    if (object->type != TYPE_ASCII)
      continue;
    const struct object_string *string = &object->string;
    if (string->length == length && memcmp(string->chars, chars, length) == 0)
      return i;
  }
  struct object_string *string = make_string(&s->objects, TYPE_ASCII,
                                             length, chars);
  object_list_append(consts, (union object*)string);
  return consts->length - 1;
}

unsigned writer_register_int(struct bytecode_writer_state *s, int32_t value)
{
  struct object_list *consts = s->code.consts;
  for (unsigned i = 0; i < consts->length; ++i) {
    const union object *object = consts->items[i];
    if (object->type != TYPE_INT)
      continue;
    const struct object_int *int_obj = &object->int_obj;
    if (int_obj->value == value)
      return i;
  }
  struct object_int *int_obj = make_int(&s->objects, value);
  object_list_append(consts, (union object*)int_obj);
  return consts->length - 1;
}

unsigned writer_register_name(struct bytecode_writer_state *s,
                              const char *name)
{
  struct object_list *names = s->code.names;
#ifndef NDEBUG
  unsigned name_length = strlen(name);
  for (unsigned i = 0, length = names->length; i < length; ++i) {
    const union object *object = names->items[i];
    assert(object->type == TYPE_ASCII);
    if (object->string.length != name_length)
      continue;
    assert(memcmp(object->string.chars, name, name_length) != 0);
  }
#endif

  const struct object_string *string = make_ascii(&s->objects, name);
  object_list_append(names, (union object*)string);
  return names->length - 1;
}

struct basic_block *writer_allocate_block(struct bytecode_writer_state *s) {
  struct basic_block *block = arena_allocate_type(&s->objects,
                                                  struct basic_block);
  memset(block, 0, sizeof(*block));
  return block;
}

void writer_begin_block(struct bytecode_writer_state *s,
                        struct basic_block *block) {
  if (s->code.current_block != NULL) {
    assert(s->code.current_block->next == NULL);
    s->code.current_block->next = block;
  }

  assert(block->code_length == 0 && block->code_bytes == NULL);
  s->code.current_block = block;
  arena_grow_begin(&s->opcodes, 1);
}

struct basic_block *writer_end_block(struct bytecode_writer_state *s) {
  struct basic_block *block = s->code.current_block;
  block->code_length = arena_grow_current_size(&s->opcodes);
  block->code_bytes = arena_grow_finish(&s->opcodes);
  return block;
}

static void writer_begin_code(struct bytecode_writer_state *s,
                              struct code_writer_state *code) {
  code->had_return = false;
  code->max_stacksize = 0;
  code->stacksize = 0;
  code->consts = object_new_list(&s->objects);
  code->names = object_new_list(&s->objects);

  struct basic_block *first = writer_allocate_block(s);
  code->first_block = first;
  writer_begin_block(s, first);
}

void writer_begin_file(struct bytecode_writer_state *s, FILE *out)
{
  memset(s, 0, sizeof(*s));
  arena_init(&s->objects);
  arena_init(&s->opcodes);
  s->out = out;

  writer_begin_code(s, &s->code);
  write_header(s);
}

static struct object_code *writer_end_code(struct bytecode_writer_state *s) {
  assert(s->code.stacksize == 0);
  if (!s->code.had_return) {
    unsigned i_none = writer_register_singleton(s, TYPE_NONE);
    write_push_op(s, OPCODE_LOAD_CONST, i_none);
    write_pop_op(s, OPCODE_RETURN_VALUE, 0);
  }
  writer_end_block(s);

  unsigned offset = 0;
  for (struct basic_block *b = s->code.first_block; b != NULL;
       b = b->next) {
    b->offset = offset;
    offset += b->code_length;

#if 0
    printf("%u: block %p --\n", b->offset, (void*)b);
    assert(b->code_length % 2 == 0);
    for (unsigned i = 0; i < b->code_length; i += 2) {
      printf("  %02x %u\n", b->code_bytes[i], b->code_bytes[i+1]);
    }
    printf("Jump %u -> %p\n", b->jump_opcode, (void*)b->jump_target);
    printf("Default -> %p\n", (void*)b->default_target);
#endif

    unsigned jump_length = 0;
    if (b->jump_opcode != 0) {
      assert(is_jump(b->jump_opcode));
      jump_length += 2;
    } else {
      assert(b->jump_target == NULL);
    }
    if (b->default_target != NULL && b->default_target != b->next) {
      jump_length += 2;
    }
    offset += jump_length;
  }

  arena_grow_begin(&s->opcodes, 1);
  struct basic_block *first_block = s->code.first_block;
  for (struct basic_block *b = first_block; b != NULL; b = b->next) {
    unsigned jump_length = 0;
    if (b->jump_opcode != 0)
      jump_length += 2;
    if (b->default_target != NULL && b->default_target != b->next)
      jump_length += 2;
    unsigned block_length = b->code_length + jump_length;
    assert(b->next == NULL || b->next->offset == b->offset + block_length);

    void* dst = arena_grow(&s->opcodes, block_length);
    memcpy(dst, b->code_bytes, b->code_length);
    uint8_t* j = (uint8_t*)dst + b->code_length;
    if (b->jump_opcode != 0) {
      unsigned dest_offset = b->jump_target->offset;
      assert((dest_offset > 0 || (first_block->code_length == 0 &&
                                  first_block->jump_opcode == 0)) &&
             "target block not processed?");
      *j++ = b->jump_opcode;
      if (is_absjump(b->jump_opcode)) {
        if (dest_offset > 255)
          abort(); /* TODO */
        *j++ = (uint8_t)dest_offset;
      } else {
        unsigned delta = dest_offset - b->offset - block_length
          - ((b->default_target != NULL && b->default_target != b->next) ? 2 : 0);
        if (delta > 255)
          abort();
        *j++ = (uint8_t)delta;
      }
    }
    if (b->default_target != NULL && b->default_target != b->next) {
      unsigned dest_offset = b->default_target->offset;
      assert((dest_offset > 0 || (first_block->code_length == 0 &&
                                  first_block->jump_opcode == 0)) &&
             "target block not processed?");
      if (dest_offset <= b->offset) {
        if (dest_offset > 255)
          abort(); /* TODO */
        *j++ = OPCODE_JUMP_ABSOLUTE;
        *j++ = (uint8_t)dest_offset;
      } else {
        unsigned delta = dest_offset - b->offset - block_length;
        if (delta > 255)
          abort();
        *j++ = OPCODE_JUMP_FORWARD;
        *j++ = (uint8_t)delta;
      }
    }
  }

  unsigned code_length = arena_grow_current_size(&s->opcodes);
  char* code_bytes = arena_grow_finish(&s->opcodes);

  struct object_code *code = object_new_code(&s->objects);
  code->code = (union object*)make_bytes(&s->objects, code_length, code_bytes);
  code->consts = (union object*)s->code.consts;
  code->names = (union object*)s->code.names;
  code->varnames = (union object*)object_new_tuple(&s->objects, 0);
  code->freevars = (union object*)object_new_tuple(&s->objects, 0);
  code->cellvars = (union object*)object_new_tuple(&s->objects, 0);
  code->filename = (union object*)make_ascii(&s->objects, "simple.py");
  code->name = (union object*)make_ascii(&s->objects, "<module>");
  code->lnotab = (union object*)make_bytes(&s->objects, 0, NULL);
  return code;
}

void writer_end_file(struct bytecode_writer_state *s)
{
  struct object_code *code = writer_end_code(s);
  write(s, (const union object*)code);
  fflush(s->out);
}

#include "codegen.h"

#include <string.h>
#include <assert.h>

#include "objects.h"
#include "opcodes.h"

struct basic_block *cg_allocate_block(struct cg_state *s) {
  struct basic_block *block = arena_allocate_type(&s->objects,
                                                  struct basic_block);
  memset(block, 0, sizeof(*block));
  return block;
}

void cg_begin_block(struct cg_state *s, struct basic_block *block) {
  if (s->code.current_block != NULL) {
    assert(s->code.current_block->next == NULL);
    s->code.current_block->next = block;
  }

  assert(block->code_length == 0 && block->code_bytes == NULL);
  s->code.current_block = block;
  arena_grow_begin(&s->opcodes, 1);
}

struct basic_block *cg_end_block(struct cg_state *s) {
  struct basic_block *block = s->code.current_block;
  block->code_length = arena_grow_current_size(&s->opcodes);
  block->code_bytes = arena_grow_finish(&s->opcodes);
  return block;
}

static void cg_begin_code(struct cg_state *s, struct code_writer_state *code) {
  code->had_return = false;
  code->max_stacksize = 0;
  code->stacksize = 0;
  code->consts = object_new_list(&s->objects);
  code->names = object_new_list(&s->objects);

  struct basic_block *first = cg_allocate_block(s);
  code->first_block = first;
  cg_begin_block(s, first);
}

static struct object_code *cg_end_code(struct cg_state *s) {
  assert(s->code.stacksize == 0);
  if (!s->code.had_return) {
    unsigned i_none = cg_register_singleton(s, TYPE_NONE);
    cg_push_op(s, OPCODE_LOAD_CONST, i_none);
    cg_pop_op(s, OPCODE_RETURN_VALUE, 0);
  }
  cg_end_block(s);

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

void cg_begin_file(struct cg_state *s)
{
  memset(s, 0, sizeof(*s));
  arena_init(&s->objects);
  arena_init(&s->opcodes);

  cg_begin_code(s, &s->code);
}

struct object_code *cg_end_file(struct cg_state *s)
{
  return cg_end_code(s);
}

void cg_op(struct cg_state *s, uint8_t opcode, uint32_t arg)
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

static void push(struct cg_state *s)
{
  s->code.stacksize++;
  if (s->code.stacksize > s->code.max_stacksize)
    s->code.max_stacksize = s->code.stacksize;
}

void cg_pop(struct cg_state *s, unsigned n)
{
  assert(s->code.stacksize >= n);
  s->code.stacksize -= n;
}

void cg_push_op(struct cg_state *s, uint8_t opcode,
                   uint32_t arg)
{
  cg_op(s, opcode, arg);
  push(s);
}

void cg_pop_op(struct cg_state *s, uint8_t opcode,
                  uint32_t arg)
{
  cg_op(s, opcode, arg);
  cg_pop(s, 1);
}

unsigned cg_register_singleton(struct cg_state *s, char type)
{
  assert(object_type_is_singleton(type));
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

unsigned cg_register_string(struct cg_state *s,
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

unsigned cg_register_int(struct cg_state *s, int32_t value)
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

unsigned cg_register_name(struct cg_state *s, const char *name)
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



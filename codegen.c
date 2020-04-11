#include "codegen.h"
#include "codegen_types.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "adt/arena.h"
#include "objects.h"
#include "objects_types.h"
#include "opcodes.h"

static inline void arena_grow_u8(struct arena *arena, uint8_t value)
{
  arena_grow_char(arena, (char)value);
}

struct basic_block *cg_allocate_block(struct cg_state *s)
{
  struct basic_block *block = arena_allocate_type(&s->objects,
                                                  struct basic_block);
  memset(block, 0, sizeof(*block));
  return block;
}

void cg_begin_block(struct cg_state *s, struct basic_block *block)
{
  if (s->code.current_block != NULL) {
    assert(s->code.current_block->next == NULL);
    s->code.current_block->next = block;
  }

  assert(block->code_length == 0 && block->code_bytes == NULL);
  s->code.current_block = block;
  arena_grow_begin(&s->code.opcodes, 1);
}

struct basic_block *cg_end_block(struct cg_state *s)
{
  struct basic_block *block = s->code.current_block;
  block->code_length = arena_grow_current_size(&s->code.opcodes);
  block->code_bytes = arena_grow_finish(&s->code.opcodes);
  return block;
}

static void cg_init_code(struct cg_state *s, struct code_state *code)
{
  memset(code, 0, sizeof(*code));
  arena_init(&code->opcodes);
  code->consts = object_new_list(&s->objects);
  code->names = object_new_list(&s->objects);

  struct basic_block *first = cg_allocate_block(s);
  code->first_block = first;
  cg_begin_block(s, first);
}

static union object *cg_end_code(struct cg_state *s, const char *name)
{
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
    fprintf(stderr, "%u: block %p --\n", b->offset, (void*)b);
    assert(b->code_length % 2 == 0);
    for (unsigned i = 0; i < b->code_length; i += 2) {
      fprintf(stderr, "  %02x %u\n", b->code_bytes[i], b->code_bytes[i+1]);
    }
    fprintf(stderr, "Jump %u -> %p\n", b->jump_opcode, (void*)b->jump_target);
    fprintf(stderr, "Default -> %p\n", (void*)b->default_target);
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

  arena_grow_begin(&s->objects, 1);
  struct basic_block *first_block = s->code.first_block;
  for (struct basic_block *b = first_block; b != NULL; b = b->next) {
    unsigned jump_length = 0;
    if (b->jump_opcode != 0)
      jump_length += 2;
    if (b->default_target != NULL && b->default_target != b->next)
      jump_length += 2;
    unsigned block_length = b->code_length + jump_length;
    assert(b->next == NULL || b->next->offset == b->offset + block_length);

    void* dst = arena_grow(&s->objects, block_length);
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

  unsigned code_length = arena_grow_current_size(&s->objects);
  char* code_bytes = arena_grow_finish(&s->objects);

  union object *object = object_new_code(&s->objects);
  object->code.code = make_bytes(&s->objects, code_length, code_bytes);
  object->code.consts = s->code.consts;
  object->code.names = s->code.names;
  object->code.varnames = object_new_tuple(&s->objects, 0);
  object->code.freevars = object_new_tuple(&s->objects, 0);
  object->code.cellvars = object_new_tuple(&s->objects, 0);
  object->code.filename = make_ascii(&s->objects, "simple.py");
  object->code.name = make_ascii(&s->objects, name);
  object->code.lnotab = make_bytes(&s->objects, 0, NULL);

  arena_free_all(&s->code.opcodes);
  return object;
}

struct code_state *cg_push_code(struct cg_state *s)
{
  struct code_state *saved = arena_allocate_type(&s->codestack,
                                                 struct code_state);
  memcpy(saved, &s->code, sizeof(*saved));
  cg_init_code(s, &s->code);
  return saved;
}

union object *cg_pop_code(struct cg_state *s, struct code_state *saved,
                          const char *name)
{
  union object *object = cg_end_code(s, name);
  memcpy(&s->code, saved, sizeof(s->code));
  arena_free(&s->codestack, saved);
  return object;
}

void cg_begin(struct cg_state *s)
{
  memset(s, 0, sizeof(*s));
  arena_init(&s->objects);
  arena_init(&s->codestack);

  cg_init_code(s, &s->code);
  s->code.module_level = true;
}

union object *cg_end(struct cg_state *s)
{
  return cg_end_code(s, "<module>");
}

void cg_free(struct cg_state *s)
{
  arena_free_all(&s->code.opcodes);
  arena_free_all(&s->objects);
  arena_free_all(&s->codestack);
}

void cg_op(struct cg_state *s, uint8_t opcode, uint32_t arg)
{
  assert(!is_jump(opcode));
  if (arg >= 256) {
    assert(arg <= 0xffff);
    arena_grow_char(&s->code.opcodes, (char)OPCODE_EXTENDED_ARG);
    arena_grow_u8(&s->code.opcodes, (uint8_t)(arg >> 8));
    arg &= 0xff;
  }
  arena_grow_u8(&s->code.opcodes, opcode);
  arena_grow_u8(&s->code.opcodes, (uint8_t)arg);
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
  union object *consts = s->code.consts;
  for (unsigned i = 0; i < consts->list.length; ++i) {
    const union object *object = consts->list.items[i];
    if (object->type == type)
      return i;
  }
  union object *singleton = object_new_singleton(&s->objects, type);
  object_list_append(consts, singleton);
  return consts->list.length - 1;
}

unsigned cg_register_string(struct cg_state *s,
                                const char *chars, uint32_t length)
{
  union object *consts = s->code.consts;
  for (unsigned i = 0; i < consts->list.length; ++i) {
    const union object *object = consts->list.items[i];
    if (object->type != TYPE_ASCII)
      continue;
    const struct object_string *string = &object->string;
    if (string->length == length && memcmp(string->chars, chars, length) == 0) {
      return i;
    }
  }
  union object *string = make_string(&s->objects, TYPE_ASCII, length, chars);
  object_list_append(consts, string);
  return consts->list.length - 1;
}

unsigned cg_register_int(struct cg_state *s, int32_t value)
{
  union object *consts = s->code.consts;
  for (unsigned i = 0; i < consts->list.length; ++i) {
    const union object *object = consts->list.items[i];
    if (object->type != TYPE_INT)
      continue;
    const struct object_int *int_obj = &object->int_obj;
    if (int_obj->value == value)
      return i;
  }
  union object *int_obj = make_int(&s->objects, value);
  object_list_append(consts, int_obj);
  return consts->list.length - 1;
}

unsigned cg_register_name(struct cg_state *s, const char *name)
{
  union object *names = s->code.names;
  unsigned name_length = strlen(name);
  for (unsigned i = 0, length = names->list.length; i < length; ++i) {
    const union object *object = names->list.items[i];
    assert(object->type == TYPE_ASCII);
    if (object->string.length == name_length &&
        memcmp(object->string.chars, name, name_length) == 0) {
      return i;
    }
  }
  return cg_append_name(s, name);
}

unsigned cg_append_name(struct cg_state *s, const char *name)
{
  union object *names = s->code.names;
  union object *string = make_ascii(&s->objects, name);
  object_list_append(names, string);
  return names->list.length - 1;
}

unsigned cg_register_code(struct cg_state *s, union object *code)
{
  assert(code->type == TYPE_CODE);
  union object *consts = s->code.consts;
  object_list_append(consts, code);
  return consts->list.length - 1;
}

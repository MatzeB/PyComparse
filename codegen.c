#include "codegen.h"
#include "codegen_types.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "adt/arena.h"
#include "adt/dynmemory.h"
#include "objects.h"
#include "objects_types.h"
#include "opcodes.h"
#include "symbol_types.h"

struct saved_symbol_info {
  struct symbol     *symbol;
  struct symbol_info info;
};

static inline void arena_grow_u8(struct arena *arena, uint8_t value)
{
  arena_grow_char(arena, (char)value);
}

struct basic_block *cg_allocate_block(struct cg_state *s)
{
  struct arena *arena = object_intern_arena(&s->objects);
  struct basic_block *block = arena_allocate_type(arena, struct basic_block);
  memset(block, 0, sizeof(*block));
  return block;
}

void cg_block_begin(struct cg_state *s, struct basic_block *block)
{
  struct basic_block *last_block = s->code.last_block;
  if (last_block != NULL) {
    assert(last_block->next == NULL);
    last_block->next = block;
  }

  assert(block->code_length == 0 && block->code_bytes == NULL);
  assert(s->code.current_block == NULL);
  s->code.current_block = block;
  arena_grow_begin(&s->code.opcodes, 1);
}

struct basic_block *cg_block_end(struct cg_state *s)
{
  struct basic_block *block = s->code.current_block;
  s->code.current_block = NULL;
  s->code.last_block = block;

  block->code_length = arena_grow_current_size(&s->code.opcodes);
  block->code_bytes = arena_grow_finish(&s->code.opcodes);
  return block;
}

bool cg_in_block(struct cg_state *s)
{
  return s->code.current_block != NULL;
}

void cg_code_begin(struct cg_state *s, bool use_locals)
{
  struct code_state *code = &s->code;
  memset(code, 0, sizeof(*code));
  arena_init(&code->opcodes);
  struct arena *arena = object_intern_arena(&s->objects);
  code->consts = object_new_list(arena);
  code->names = object_new_list(arena);
  code->varnames = object_new_list(arena);
  if (s->next_scope_id == UINT16_MAX) {
    abort();
  }
  code->scope_id = s->next_scope_id++;
  code->cg_stack_begin = stack_size(&s->stack);
  code->use_locals = use_locals;

  if (use_locals)
    code->flags |= CO_NEWLOCALS | CO_OPTIMIZED;

  struct basic_block *first = cg_allocate_block(s);
  code->first_block = first;
  cg_block_begin(s, first);
}

static void pop_symbol_infos(struct cg_state *s)
{
  unsigned cg_stack_begin = s->code.cg_stack_begin;
  assert(stack_size(&s->stack) >= cg_stack_begin);
  while (stack_size(&s->stack) > cg_stack_begin) {
    struct saved_symbol_info *saved =
      (struct saved_symbol_info*)stack_last(&s->stack, sizeof(*saved));
    struct symbol *symbol = saved->symbol;
    memcpy(&symbol->info, &saved->info, sizeof(symbol->info));
    stack_pop(&s->stack, sizeof(*saved));
  }
}

union object *cg_code_end(struct cg_state *s, const char *name)
{
  assert(s->code.stacksize == 0);
  assert(!cg_in_block(s));

  pop_symbol_infos(s);

  // TODO: compute NOFREE...
  s->code.flags |= CO_NOFREE;

  // Finalize block layout: First assign minimum offsets necessary,
  // then expand as necessary.
  unsigned offset = 0;
  struct basic_block *first_block = s->code.first_block;
  for (struct basic_block *b = first_block; b != NULL; b = b->next) {
    b->offset = offset;
    offset += b->code_length;

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

  // TODO: expand blocks/offsets as necessary.

  // Finalize bytecode layout.
  struct arena *arena = object_intern_arena(&s->objects);
  arena_grow_begin(arena, 1);
  for (struct basic_block *b = first_block; b != NULL; b = b->next) {
    unsigned jump_length = 0;
    if (b->jump_opcode != 0)
      jump_length += 2;
    if (b->default_target != NULL && b->default_target != b->next)
      jump_length += 2;
    unsigned block_length = b->code_length + jump_length;
    assert(b->next == NULL || b->next->offset == b->offset + block_length);

    void* dst = arena_grow(arena, block_length);
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

  unsigned code_length = arena_grow_current_size(arena);
  char* code_bytes = arena_grow_finish(arena);

  union object *object = object_new_code(arena);
  object->code.argcount = s->code.argcount;
  object->code.posonlyargcount = 0;
  object->code.kwonlyargcount = 0;
  object->code.nlocals = s->code.varnames->list.length;
  object->code.stacksize = s->code.max_stacksize;
  object->code.flags = s->code.flags;
  object->code.code = object_new_string(arena, OBJECT_BYTES,
                                        code_length, code_bytes);
  object->code.consts = s->code.consts;
  object->code.names = s->code.names;
  object->code.varnames = s->code.varnames;
  object->code.freevars = object_new_tuple(arena, 0);
  object->code.cellvars = object_new_tuple(arena, 0);
  object->code.filename = object_intern_cstring(&s->objects, "simple.py");
  object->code.name = object_intern_cstring(&s->objects, name);
  object->code.lnotab = object_intern_string(&s->objects, OBJECT_BYTES,
                                             0, NULL);

  arena_free_all(&s->code.opcodes);
  return object;
}

void cg_push_code(struct cg_state *s)
{
  void* slot = stack_push(&s->stack, sizeof(s->code));
  memcpy(slot, &s->code, sizeof(s->code));
}

union object *cg_pop_code(struct cg_state *s, const char *name)
{
  union object *object = cg_code_end(s, name);
  memcpy(&s->code, stack_last(&s->stack, sizeof(s->code)), sizeof(s->code));
  stack_pop(&s->stack, sizeof(s->code));
  return object;
}

void cg_init(struct cg_state *s, struct symbol_table *symbol_table)
{
  memset(s, 0, sizeof(*s));
  object_intern_init(&s->objects);
  s->symbol_table = symbol_table;
  s->next_scope_id = 1;
}

void cg_free(struct cg_state *s)
{
  arena_free_all(&s->code.opcodes);
  object_intern_free(&s->objects);
  stack_free(&s->stack);
}

bool cg_use_locals(struct cg_state *s)
{
  return s->code.use_locals;
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

static void cg_push(struct cg_state *s)
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

void cg_push_op(struct cg_state *s, uint8_t opcode, uint32_t arg)
{
  cg_op(s, opcode, arg);
  cg_push(s);
}

void cg_pop_op(struct cg_state *s, uint8_t opcode, uint32_t arg)
{
  cg_op(s, opcode, arg);
  cg_pop(s, 1);
}

unsigned cg_register_unique_object(struct cg_state *s, union object *object)
{
  union object *consts = s->code.consts;
  object_list_append(consts, object);
  return object_list_length(consts) - 1;
}

unsigned cg_register_object(struct cg_state *s, union object *object)
{
  union object *consts = s->code.consts;
  for (uint32_t i = 0, l = object_list_length(consts); i < l; i++) {
    if (object_list_at(consts, i) == object) return i;
  }
  return cg_register_unique_object(s, object);
}

void cg_load_const(struct cg_state *s, union object *object)
{
  unsigned index = cg_register_object(s, object);
  cg_push_op(s, OPCODE_LOAD_CONST, index);
}

unsigned cg_register_name(struct cg_state *s, const char *name)
{
  union object *names = s->code.names;
  unsigned length = strlen(name);
  for (unsigned i = 0; i < names->list.length; ++i) {
    const union object *object = names->list.items[i];
    if (object->type != OBJECT_ASCII)
      continue;
    const struct object_string *string = &object->string;
    if (string->length == length && memcmp(string->chars, name, length) == 0) {
      return i;
    }
  }
  return cg_append_name(s, name);
}

unsigned cg_append_name(struct cg_state *s, const char *name)
{
  union object *names = s->code.names;
  union object *string = object_intern_cstring(&s->objects, name);
  object_list_append(names, string);
  return names->list.length - 1;
}

unsigned cg_append_varname(struct cg_state *s, const char *name)
{
  union object *varnames = s->code.varnames;
  union object *string = object_intern_cstring(&s->objects, name);
  object_list_append(varnames, string);
  return varnames->list.length - 1;
}

struct symbol_info *cg_symbol_info(struct cg_state *s, struct symbol* symbol)
{
  if (symbol->info.scope_id != s->code.scope_id) {
    return NULL;
  }
  return &symbol->info;
}

struct symbol_info *cg_new_symbol_info(struct cg_state *s,
                                       struct symbol* symbol)
{
  assert(symbol->info.scope_id != s->code.scope_id);

  if (symbol->info.scope_id != 0) {
    struct saved_symbol_info *saved =
        (struct saved_symbol_info*)stack_push(&s->stack, sizeof(*saved));
    saved->symbol = symbol;
    memcpy(&saved->info, &symbol->info, sizeof(saved->info));
  }

  memset(&symbol->info, 0, sizeof(symbol->info));
  symbol->info.scope_id = s->code.scope_id;
  return &symbol->info;
}

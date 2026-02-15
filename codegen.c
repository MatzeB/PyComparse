#include "codegen.h"
#include "codegen_types.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "adt/dynmemory.h"
#include "diagnostics.h"
#include "object.h"
#include "object_intern.h"
#include "object_types.h"
#include "opcodes.h"
#include "symbol_types.h"
#include "util.h"

#define INVALID_BLOCK_OFFSET (~0u)

#define DELAYED_BLOCK_MARKER (~0u)
struct saved_symbol_info {
  struct symbol     *symbol;
  struct symbol_info info;
};

struct code_index_cache_bucket {
  const void *nullable key;
  unsigned             hash;
  uint32_t             index;
};

static unsigned pointer_hash(const void *key)
{
  uintptr_t x = (uintptr_t)key;
  x ^= x >> 16;
  x *= 0x85ebca6bu;
  x ^= x >> 13;
  x *= 0xc2b2ae35u;
  x ^= x >> 16;
  return (unsigned)x;
}

static void code_index_cache_insert_raw(struct code_index_cache *cache,
                                        const void *key, unsigned hash,
                                        uint32_t index)
{
  hash_set_increment_num_elements(&cache->set);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &cache->set, hash);
  for (;; hash_set_chain_iteration_next(&c)) {
    struct code_index_cache_bucket *bucket = &cache->buckets[c.index];
    if (bucket->key == NULL) {
      bucket->key = key;
      bucket->hash = hash;
      bucket->index = index;
      return;
    }
  }
}

static void code_index_cache_resize(struct code_index_cache *cache,
                                    unsigned                 new_size)
{
  struct code_index_cache_bucket *old_buckets = cache->buckets;
  unsigned old_num_buckets = hash_set_num_buckets(&cache->set);

  cache->buckets = calloc(new_size, sizeof(cache->buckets[0]));
  if (cache->buckets == NULL) {
    internal_error("out of memory");
  }
  hash_set_init(&cache->set, new_size);

  if (old_buckets != NULL) {
    for (unsigned i = 0; i < old_num_buckets; ++i) {
      struct code_index_cache_bucket *bucket = &old_buckets[i];
      if (bucket->key != NULL) {
        code_index_cache_insert_raw(cache, bucket->key, bucket->hash,
                                    bucket->index);
      }
    }
  }

  free(old_buckets);
}

static void code_index_cache_init(struct code_index_cache *cache,
                                  unsigned                 minimum_size)
{
  if (minimum_size < 32u) {
    minimum_size = 32u;
  } else {
    minimum_size = ceil_po2(minimum_size);
  }
  code_index_cache_resize(cache, minimum_size);
}

static void code_index_cache_insert(struct code_index_cache *cache,
                                    const void *key, unsigned hash,
                                    uint32_t index)
{
  unsigned new_size = hash_set_should_resize(&cache->set);
  if (UNLIKELY(new_size != 0)) {
    code_index_cache_resize(cache, new_size);
  }
  code_index_cache_insert_raw(cache, key, hash, index);
}

static bool code_index_cache_lookup(struct code_index_cache *cache,
                                    const void *key, unsigned hash,
                                    uint32_t *out_index)
{
  if (cache->buckets == NULL) {
    return false;
  }

  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &cache->set, hash);
  for (;; hash_set_chain_iteration_next(&c)) {
    struct code_index_cache_bucket *bucket = &cache->buckets[c.index];
    if (bucket->key == NULL) {
      return false;
    }
    if (bucket->hash == hash && bucket->key == key) {
      *out_index = bucket->index;
      return true;
    }
  }
}

static void code_index_cache_free(struct code_index_cache *cache)
{
  free(cache->buckets);
  memset(cache, 0, sizeof(*cache));
}

static void code_state_free_index_caches(struct code_state *code)
{
  code_index_cache_free(&code->const_index_cache);
  code_index_cache_free(&code->name_index_cache);
}

static inline void arena_grow_u8(struct arena *arena, uint8_t value)
{
  arena_grow_char(arena, (char)value);
}

static bool is_dunder_debug(const struct symbol *name)
{
  return strcmp(name->string, "__debug__") == 0;
}

static void cg_op_with_arena(struct arena *arena, enum opcode opcode,
                             uint32_t arg)
{
  if (arg > 0xff) {
    if (arg > 0xffffff) {
      arena_grow_char(arena, (char)OPCODE_EXTENDED_ARG);
      arena_grow_u8(arena, (uint8_t)(arg >> 24));
    }
    if (arg > 0xffff) {
      arena_grow_char(arena, (char)OPCODE_EXTENDED_ARG);
      arena_grow_u8(arena, (uint8_t)(arg >> 16));
    }
    arena_grow_char(arena, (char)OPCODE_EXTENDED_ARG);
    arena_grow_u8(arena, (uint8_t)(arg >> 8));
  }
  arena_grow_u8(arena, opcode);
  arena_grow_u8(arena, (uint8_t)arg);
}

void cg_op(struct cg_state *s, enum opcode opcode, uint32_t arg)
{
  assert(!is_jump(opcode));
  cg_op_with_arena(&s->code.opcodes, opcode, arg);
}

void cg_push(struct cg_state *s, unsigned n)
{
  s->code.stacksize += n;
  if (s->code.stacksize > s->code.max_stacksize)
    s->code.max_stacksize = s->code.stacksize;
}

void cg_pop(struct cg_state *s, unsigned n)
{
  assert(s->code.stacksize >= n);
  s->code.stacksize -= n;
}

void cg_mark_max_stack_extra(struct cg_state *s, unsigned extra)
{
  unsigned with_extra = s->code.stacksize + extra;
  if (with_extra > s->code.max_stacksize) {
    s->code.max_stacksize = with_extra;
  }
}

void cg_op_pop_push(struct cg_state *s, enum opcode opcode, uint32_t arg,
                    unsigned pop, unsigned push)
{
  cg_pop(s, pop);
  cg_op(s, opcode, arg);
  cg_push(s, push);
}

void cg_op_pop1(struct cg_state *s, enum opcode opcode, uint32_t arg)
{
  cg_pop(s, 1);
  cg_op(s, opcode, arg);
}

void cg_op_push1(struct cg_state *s, enum opcode opcode, uint32_t arg)
{
  cg_op(s, opcode, arg);
  cg_push(s, 1);
}

void cg_set_lineno(struct cg_state *s, unsigned lineno)
{
  if (lineno == 0 || lineno == s->code.current_lineno) return;
  if (s->code.first_lineno == 0) s->code.first_lineno = lineno;
  s->code.current_lineno = lineno;

  if (!cg_in_block(s)) return;

  unsigned length = s->code.lnotab_marks_length;
  if (length >= s->code.lnotab_marks_capacity) {
    s->code.lnotab_marks
        = dynmemory_grow(s->code.lnotab_marks, &s->code.lnotab_marks_capacity,
                         length + 1, sizeof(struct lnotab_mark));
  }
  struct lnotab_mark *mark = &s->code.lnotab_marks[length];
  mark->block = s->code.current_block;
  mark->offset_in_block = arena_grow_current_size(&s->code.opcodes);
  mark->lineno = lineno;
  s->code.lnotab_marks_length = length + 1;
}

struct basic_block *cg_block_allocate(struct cg_state *s)
{
  struct arena       *arena = object_intern_arena(&s->objects);
  struct basic_block *block = arena_allocate_type(arena, struct basic_block);
  memset(block, 0, sizeof(*block));
  block->offset = INVALID_BLOCK_OFFSET;
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
  arena_grow_begin(&s->code.opcodes, /*alignment=*/1);
}

struct basic_block *cg_block_end(struct cg_state *s)
{
  struct basic_block *block = s->code.current_block;
  s->code.current_block = NULL;
  if (block->code_length != DELAYED_BLOCK_MARKER) {
    s->code.last_block = block;
  }

  block->code_length = arena_grow_current_size(&s->code.opcodes);
  block->code_bytes = arena_grow_finish(&s->code.opcodes);
  return block;
}

void cg_block_insert_delayed(struct cg_state *s, struct basic_block *block)
{
  assert(s->code.current_block == NULL);
  struct basic_block *last_block = s->code.last_block;
  if (last_block != NULL) {
    assert(last_block->next == NULL);
    last_block->next = block;
  }
  s->code.last_block = block;
  block->code_length = DELAYED_BLOCK_MARKER;
}

void cg_block_begin_delayed(struct cg_state *s, struct basic_block *block)
{
  assert(block->code_length == DELAYED_BLOCK_MARKER
         && block->code_bytes == NULL);
  assert(s->code.current_block == NULL);
  s->code.current_block = block;
  arena_grow_begin(&s->code.opcodes, /*alignment=*/1);
}

void cg_condjump(struct cg_state *s, enum opcode opcode,
                 struct basic_block          *target,
                 struct basic_block *nullable fallthrough)
{
  struct basic_block *block = cg_block_end(s);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->jump_opcode = opcode;
  block->jump_target = target;
  block->default_target = fallthrough;
}

void cg_jump(struct cg_state *s, struct basic_block *target)
{
  struct basic_block *block = cg_block_end(s);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->default_target = target;
}

bool cg_in_block(struct cg_state *s)
{
  return s->code.current_block != NULL;
}

void cg_code_begin(struct cg_state *s, bool in_function)
{
  uint32_t           inherited_future_flags = s->code.flags & CO_FUTURE_MASK;
  struct code_state *code = &s->code;
  memset(code, 0, sizeof(*code));
  arena_init(&code->opcodes);
  struct arena *arena = object_intern_arena(&s->objects);
  code->consts = object_new_list(arena);
  code->names = object_new_list(arena);
  code->varnames = object_new_list(arena);
  code->freevars = object_new_list(arena);
  code->cellvars = object_new_list(arena);
  if (s->next_scope_id == UINT16_MAX) {
    internal_error("Ran out of scope identifiers");
  }
  code->scope_id = s->next_scope_id++;
  code->cg_stack_begin = stack_size(&s->stack);
  code->in_function = in_function;
  code->flags |= inherited_future_flags;

  if (in_function) {
    code->flags |= CO_NEWLOCALS | CO_OPTIMIZED;
    object_list_append(code->consts,
                       object_intern_singleton(&s->objects, OBJECT_NONE));
  }

  struct basic_block *first = cg_block_allocate(s);
  code->first_block = first;
  cg_block_begin(s, first);
}

static void pop_symbol_infos(struct cg_state *s)
{
  unsigned cg_stack_begin = s->code.cg_stack_begin;
  assert(stack_size(&s->stack) >= cg_stack_begin);
  while (stack_size(&s->stack) > cg_stack_begin) {
    struct saved_symbol_info *saved
        = (struct saved_symbol_info *)stack_last(&s->stack, sizeof(*saved));
    struct symbol *symbol = saved->symbol;
    memcpy(&symbol->info, &saved->info, sizeof(symbol->info));
    stack_pop(&s->stack, sizeof(*saved));
  }
}

static void cg_ensure_const_index_cache(struct cg_state *s)
{
  struct code_index_cache *cache = &s->code.const_index_cache;
  if (cache->buckets != NULL) {
    return;
  }
  union object *consts = s->code.consts;
  uint32_t      length = object_list_length(consts);
  code_index_cache_init(cache, length * 2u);
  for (uint32_t i = 0; i < length; ++i) {
    union object *object = object_list_at(consts, i);
    code_index_cache_insert_raw(cache, object, pointer_hash(object), i);
  }
}

static void cg_ensure_name_index_cache(struct cg_state *s)
{
  struct code_index_cache *cache = &s->code.name_index_cache;
  if (cache->buckets != NULL) {
    return;
  }
  union object *names = s->code.names;
  uint32_t      length = object_list_length(names);
  code_index_cache_init(cache, length * 2u);
  for (uint32_t i = 0; i < length; ++i) {
    union object *object = object_list_at(names, i);
    code_index_cache_insert_raw(cache, object, pointer_hash(object), i);
  }
}

static uint8_t opcode_with_parameter_size(uint32_t parameter)
{
  if (parameter <= 0xff) return 2;
  if (parameter <= 0xffff) return 4;
  if (parameter <= 0xffffff) return 6;
  return 8;
}

static bool relax_jumps_in_block(struct basic_block *block,
                                 unsigned            offset_adjust_forward)
{
  unsigned offset = block->offset + block->code_length;

  bool                jump_size_increased = false;
  struct basic_block *jump_target = block->jump_target;
  if (jump_target != NULL) {
    enum opcode jump_opcode = block->jump_opcode;
    unsigned    dest_offset = jump_target->offset;
    if (dest_offset == INVALID_BLOCK_OFFSET) dest_offset = offset + 8;
    if (!block->jump_backwards) dest_offset += offset_adjust_forward;

    uint8_t old_jump_size = block->jump_size;

    uint32_t parameter;
    if (is_absjump(jump_opcode)) {
      parameter = dest_offset;
    } else {
      assert(dest_offset > offset);
      parameter
          = dest_offset - offset - (old_jump_size > 0 ? old_jump_size : 2);
    }

    uint8_t jump_size = opcode_with_parameter_size(parameter);
    assert(jump_size >= old_jump_size);
    if (jump_size != old_jump_size) {
      block->jump_size = jump_size;
      jump_size_increased = true;
    }
    offset += jump_size;
  }

  struct basic_block *default_target = block->default_target;
  if (default_target != NULL) {
    unsigned dest_offset = default_target->offset;
    if (dest_offset == INVALID_BLOCK_OFFSET) dest_offset = offset + 8;
    if (!block->default_jump_backwards) dest_offset += offset_adjust_forward;

    uint8_t old_default_jump_size = block->default_jump_size;

    /* There is JUMP_ABSOLUTE and JUMP_FORWARD. We should pick whatever is
     * shorter to encode; JUMP_FORWARD does not work for backward jumps. */
    uint32_t parameter_abs = dest_offset;
    uint8_t  default_jump_size = opcode_with_parameter_size(parameter_abs);
    if (!block->default_jump_backwards) {
      uint32_t parameter_rel
          = dest_offset - offset
            - (old_default_jump_size > 0 ? old_default_jump_size : 2);
      uint8_t rel_jump_size = opcode_with_parameter_size(parameter_rel);
      if (rel_jump_size < default_jump_size) {
        default_jump_size = rel_jump_size;
      }
    }
    assert(default_jump_size >= old_default_jump_size);

    if (default_jump_size != old_default_jump_size) {
      block->default_jump_size = default_jump_size;
      jump_size_increased = true;
    }
  } else {
    assert(block->default_jump_size == 0);
  }
  return jump_size_increased;
}

static struct basic_block *skip_empty_blocks(struct basic_block *target)
{
  while (target->code_length == 0 && target->jump_opcode == 0) {
    struct basic_block *default_target = target->default_target;
    if (default_target == NULL) break;
    if (default_target == target) {
      break;
    }
    target = default_target;
  }
  return target;
}

static bool code_uses_setup_finally(struct basic_block *first_block)
{
  for (struct basic_block *block = first_block; block != NULL;
       block = block->next) {
    if (block->jump_opcode == OPCODE_SETUP_FINALLY) {
      return true;
    }
    unsigned code_length = block->code_length;
    uint8_t *code = block->code_bytes;
    for (unsigned i = 0; i + 1 < code_length; i += 2) {
      if (code[i] == OPCODE_SETUP_FINALLY) {
        return true;
      }
    }
  }
  return false;
}

union object *cg_code_end(struct cg_state *s, const char *name)
{
  assert(s->code.stacksize == 0);
  assert(!cg_in_block(s));

  pop_symbol_infos(s);

  if (object_list_length(s->code.freevars) == 0
      && object_list_length(s->code.cellvars) == 0) {
    s->code.flags |= CO_NOFREE;
  } else {
    s->code.flags &= ~CO_NOFREE;
  }

  unsigned prologue_length = 0;
  if (s->code.setup_annotations) prologue_length += 2;

  // Jump relaxation: First assign minimum offsets necessary, then expand as
  // necessary.
  unsigned            offset = prologue_length;
  struct basic_block *first_block = s->code.first_block;
  for (struct basic_block *block = first_block, *next; block != NULL;
       block = next) {
    next = block->next;
    assert(block->code_bytes != NULL);
    block->offset = offset;

    struct basic_block *jump_target = block->jump_target;
    if (jump_target != NULL) {
      enum opcode jump_opcode = block->jump_opcode;
      assert(is_jump(jump_opcode));
      struct basic_block *skipped_target = skip_empty_blocks(jump_target);
      /* Note: relative jumps can only go forward */
      if (is_absjump(jump_opcode)
          || (skipped_target->offset != INVALID_BLOCK_OFFSET
              && skipped_target->offset > offset)) {
        jump_target = skipped_target;
        block->jump_target = jump_target;
      }
      /* we iterate forward and assign offsets, to if target alread has an
       * offset assigned, it must be a backwards jump. */
      if (jump_target->offset != INVALID_BLOCK_OFFSET) {
        block->jump_backwards = true;
      }
    }

    struct basic_block *default_target = block->default_target;

    if (default_target != NULL) {
      default_target = skip_empty_blocks(default_target);
      struct basic_block *next_skip = next;
      while (next_skip->next != NULL && next_skip->code_length == 0
             && next_skip->jump_target == NULL
             && next_skip->default_target == next_skip->next) {
        next_skip = next_skip->next;
      }
      if (default_target == next_skip) {
        default_target = NULL;
      }
      block->default_target = default_target;

      /* we iterate forward and assign offsets, to if target alread has an
       * offset assigned, it must be a backwards jump. */
      if (default_target != NULL
          && default_target->offset != INVALID_BLOCK_OFFSET) {
        block->default_jump_backwards = true;
      }
    }

    relax_jumps_in_block(block, /*offset_adjust_forward=*/0);
    offset += block->code_length + block->jump_size + block->default_jump_size;
  }

  /* Keep relaxing jumps until fix point.
   * TODO: What's the complexity here? Can we do better? */
  bool changed;
  do {
    changed = false;

    unsigned offset = prologue_length;
    for (struct basic_block *block = first_block; block != NULL;
         block = block->next) {
      unsigned offset_adjust_forward = 0;
      if (block->offset != offset) {
        assert(offset > block->offset);
        offset_adjust_forward = offset - block->offset;
        block->offset = offset;
        changed = true;
      }
      changed |= relax_jumps_in_block(block, offset_adjust_forward);
      offset
          += block->code_length + block->jump_size + block->default_jump_size;
    }
  } while (changed);

  // Emit code.
  struct arena *arena = object_intern_arena(&s->objects);
  arena_grow_begin(arena, /*alignment=*/1);
  if (s->code.setup_annotations) {
    cg_op_with_arena(arena, OPCODE_SETUP_ANNOTATIONS, 0);
  }

  for (struct basic_block *block = first_block, *next; block != NULL;
       block = next) {
    next = block->next;
    unsigned offset = block->offset;
    assert(offset == arena_grow_current_size(arena));

    unsigned code_length = block->code_length;
    void    *dst = arena_grow(arena, code_length);
    memcpy(dst, block->code_bytes, code_length);
    offset += block->code_length;

    struct basic_block *jump_target = block->jump_target;
    if (jump_target != NULL) {
      enum opcode jump_opcode = block->jump_opcode;
      unsigned    dest_offset = jump_target->offset;
      assert(dest_offset != INVALID_BLOCK_OFFSET);

      uint8_t  jump_size = block->jump_size;
      uint32_t parameter;
      if (is_absjump(jump_opcode)) {
        parameter = dest_offset;
      } else {
        assert(dest_offset > offset);
        parameter = dest_offset - offset - jump_size;
      }
      assert(opcode_with_parameter_size(parameter) == jump_size);
      cg_op_with_arena(arena, jump_opcode, parameter);
      offset += jump_size;
    }
    struct basic_block *default_target = block->default_target;
    if (default_target != NULL) {
      unsigned dest_offset = default_target->offset;
      assert(dest_offset != INVALID_BLOCK_OFFSET);

      uint8_t default_jump_size = block->default_jump_size;
      /* There is JUMP_ABSOLUTE and JUMP_FORWARD. We should pick whatever is
       * shorter to encode; JUMP_FORWARD does not work for backward jumps. */
      bool     jump_abs = true;
      uint32_t parameter_abs = dest_offset;
      uint32_t parameter_rel = 0;
      uint8_t  jump_size_abs = opcode_with_parameter_size(parameter_abs);
      if (!block->default_jump_backwards) {
        parameter_rel = dest_offset - offset - default_jump_size;
        uint8_t jump_size_rel = opcode_with_parameter_size(parameter_rel);
        if (jump_size_rel < jump_size_abs) {
          assert(default_jump_size == jump_size_rel);
          jump_abs = false;
        }
      }
      if (jump_abs) {
        assert(default_jump_size == jump_size_abs);
        cg_op_with_arena(arena, OPCODE_JUMP_ABSOLUTE, parameter_abs);
      } else {
        cg_op_with_arena(arena, OPCODE_JUMP_FORWARD, parameter_rel);
      }
    }
  }

  unsigned code_length = arena_grow_current_size(arena);
  char    *code_bytes = arena_grow_finish(arena);

  union object *code = object_intern_string(&s->objects, OBJECT_BYTES,
                                            code_length, code_bytes);

  /* Build lnotab from marks. */
  unsigned first_lineno = s->code.first_lineno;
  if (first_lineno == 0) first_lineno = 1;

  arena_grow_begin(arena, /*alignment=*/1);
  unsigned prev_offset = 0;
  unsigned prev_lineno = first_lineno;
  for (unsigned i = 0; i < s->code.lnotab_marks_length; ++i) {
    struct lnotab_mark *mark = &s->code.lnotab_marks[i];
    unsigned abs_offset = mark->block->offset + mark->offset_in_block;
    unsigned lineno = mark->lineno;
    if (lineno == prev_lineno) continue;

    /* Collapse consecutive marks at the same bytecode offset: only emit
     * one entry using the last mark's line number. */
    while (i + 1 < s->code.lnotab_marks_length) {
      struct lnotab_mark *next = &s->code.lnotab_marks[i + 1];
      unsigned next_offset = next->block->offset + next->offset_in_block;
      if (next_offset != abs_offset) break;
      lineno = next->lineno;
      ++i;
    }
    if (lineno == prev_lineno) continue;

    unsigned offset_delta = abs_offset - prev_offset;
    int      line_delta = (int)lineno - (int)prev_lineno;
    while (offset_delta > 255) {
      arena_grow_u8(arena, 255);
      arena_grow_u8(arena, 0);
      offset_delta -= 255;
    }

    if (line_delta >= 0) {
      while (line_delta > 127) {
        arena_grow_u8(arena, (uint8_t)offset_delta);
        arena_grow_u8(arena, 127);
        offset_delta = 0;
        line_delta -= 127;
      }
    } else {
      while (line_delta < -128) {
        arena_grow_u8(arena, (uint8_t)offset_delta);
        arena_grow_u8(arena, (uint8_t)(-128 & 0xff));
        offset_delta = 0;
        line_delta += 128;
      }
    }
    arena_grow_u8(arena, (uint8_t)offset_delta);
    arena_grow_u8(arena, (uint8_t)(line_delta & 0xff));

    prev_offset = abs_offset;
    prev_lineno = lineno;
  }
  unsigned lnotab_length = arena_grow_current_size(arena);
  char    *lnotab_bytes = arena_grow_finish(arena);

  union object *lnotab = object_intern_string(&s->objects, OBJECT_BYTES,
                                              lnotab_length, lnotab_bytes);
  union object *filename = object_intern_cstring(&s->objects, s->filename);
  union object *name_string = object_intern_cstring(&s->objects, name);

  union object *object = object_new_code(arena);
  object->code.argcount = s->code.argcount;
  object->code.posonlyargcount = s->code.positional_only_argcount;
  object->code.kwonlyargcount = s->code.keyword_only_argcount;
  object->code.nlocals = object_list_length(s->code.varnames);
  unsigned stacksize = s->code.max_stacksize;
  if (code_uses_setup_finally(first_block)) {
    /*
     * Our linear stack simulation intentionally approximates exceptional
     * control-flow. Keep a small conservative headroom for SETUP_FINALLY
     * paths so emitted code has sufficient stack.
     */
    stacksize += 5;
  }
  object->code.stacksize = stacksize;
  object->code.flags = s->code.flags;
  object->code.firstlineno = first_lineno;
  object->code.code = code;
  object->code.consts = s->code.consts;
  object->code.names = s->code.names;
  object->code.varnames = s->code.varnames;
  object->code.freevars = s->code.freevars;
  object->code.cellvars = s->code.cellvars;
  object->code.filename = filename;
  object->code.name = name_string;
  object->code.lnotab = lnotab;

  code_state_free_index_caches(&s->code);
  free(s->code.lnotab_marks);
  arena_free(&s->code.opcodes);
  return object;
}

void cg_push_code(struct cg_state *s)
{
  void *slot = stack_push(&s->stack, sizeof(s->code));
  memcpy(slot, &s->code, sizeof(s->code));
}

union object *cg_pop_code(struct cg_state *s, const char *name)
{
  union object *object = cg_code_end(s, name);
  memcpy(&s->code, stack_last(&s->stack, sizeof(s->code)), sizeof(s->code));
  stack_pop(&s->stack, sizeof(s->code));
  return object;
}

const char *cg_build_qualname(struct cg_state *s, const char *name)
{
  const char *prefix = s->code.qualname_prefix;
  if (prefix == NULL) return name;
  size_t        plen = strlen(prefix);
  size_t        nlen = strlen(name);
  struct arena *arena = object_intern_arena(&s->objects);
  char         *buf = arena_allocate(arena, plen + nlen + 1, 1);
  memcpy(buf, prefix, plen);
  memcpy(buf + plen, name, nlen + 1);
  return buf;
}

void cg_init(struct cg_state *s, struct symbol_table *symbol_table,
             const char *filename, struct diagnostics_state *diagnostics)
{
  memset(s, 0, sizeof(*s));
  object_intern_init(&s->objects);
  s->symbol_table = symbol_table;
  s->next_scope_id = 1;
  s->filename = filename;
  s->d = diagnostics;
}

void cg_free(struct cg_state *s)
{
  code_state_free_index_caches(&s->code);
  arena_free(&s->code.opcodes);
  object_intern_free(&s->objects);
  stack_free(&s->stack);
}

bool cg_in_function(struct cg_state *s)
{
  return s->code.in_function;
}

unsigned cg_register_unique_object(struct cg_state *s, union object *object)
{
  union object *consts = s->code.consts;
  object_list_append(consts, object);
  uint32_t index = object_list_length(consts) - 1;
  if (s->code.const_index_cache.buckets != NULL) {
    code_index_cache_insert(&s->code.const_index_cache, object,
                            pointer_hash(object), index);
  }
  return index;
}

unsigned cg_register_object(struct cg_state *s, union object *object)
{
  cg_ensure_const_index_cache(s);
  uint32_t index;
  unsigned hash = pointer_hash(object);
  if (code_index_cache_lookup(&s->code.const_index_cache, object, hash,
                              &index)) {
    return index;
  }

  return cg_register_unique_object(s, object);
}

void cg_load_const(struct cg_state *s, union object *object)
{
  unsigned index = cg_register_object(s, object);
  cg_op_push1(s, OPCODE_LOAD_CONST, index);
}

void cg_set_function_docstring(struct cg_state *s, union object *nullable doc)
{
  assert(s->code.in_function);
  if (doc == NULL) {
    doc = object_intern_singleton(&s->objects, OBJECT_NONE);
  }
  union object *consts = s->code.consts;
  assert(object_list_length(consts) > 0);
  consts->list.items[0] = doc;
}

static unsigned cg_append_name(struct cg_state *s, union object *string)
{
  union object *names = s->code.names;
  object_list_append(names, string);
  uint32_t index = object_list_length(names) - 1;
  if (s->code.name_index_cache.buckets != NULL) {
    code_index_cache_insert(&s->code.name_index_cache, string,
                            pointer_hash(string), index);
  }
  return index;
}

unsigned cg_register_name_from_cstring(struct cg_state *s, const char *cstring)
{
  union object *string = object_intern_cstring(&s->objects, cstring);
  cg_ensure_name_index_cache(s);
  uint32_t index;
  unsigned hash = pointer_hash(string);
  if (code_index_cache_lookup(&s->code.name_index_cache, string, hash,
                              &index)) {
    return index;
  }
  return cg_append_name(s, string);
}

unsigned cg_register_name(struct cg_state *s, struct symbol *name)
{
  return cg_register_name_from_cstring(s, name->string);
}

static unsigned cg_append_varname(struct cg_state *s, struct symbol *name)
{
  const char   *cstring = name->string;
  union object *varnames = s->code.varnames;
  union object *string = object_intern_cstring(&s->objects, cstring);
  object_list_append(varnames, string);
  return object_list_length(varnames) - 1;
}

static unsigned cg_append_freevar(struct cg_state *s, struct symbol *name)
{
  const char   *cstring = name->string;
  union object *freevars = s->code.freevars;
  union object *string = object_intern_cstring(&s->objects, cstring);
  object_list_append(freevars, string);
  return object_list_length(freevars) - 1;
}

unsigned cg_register_freevar(struct cg_state *s, struct symbol *name)
{
  const char   *cstring = name->string;
  size_t        length = strlen(cstring);
  union object *freevars = s->code.freevars;
  for (unsigned i = 0, num_freevars = object_list_length(freevars);
       i < num_freevars; ++i) {
    const union object *object = object_list_at(freevars, i);
    if (object->type != OBJECT_STRING) continue;
    const struct object_string *string = &object->string;
    if (string->length == length
        && memcmp(string->chars, cstring, length) == 0) {
      return i;
    }
  }
  return cg_append_freevar(s, name);
}

static unsigned cg_append_cellvar(struct cg_state *s, struct symbol *name)
{
  const char   *cstring = name->string;
  union object *cellvars = s->code.cellvars;
  union object *string = object_intern_cstring(&s->objects, cstring);
  object_list_append(cellvars, string);
  return object_list_length(cellvars) - 1;
}

static struct symbol_info *cg_symbol_info(struct cg_state *s,
                                          struct symbol   *symbol)
{
  if (symbol->info.scope_id != s->code.scope_id) {
    return NULL;
  }
  return &symbol->info;
}

static struct symbol_info *cg_new_symbol_info(struct cg_state *s,
                                              struct symbol   *symbol)
{
  assert(symbol->info.scope_id != s->code.scope_id);

  if (symbol->info.scope_id != 0) {
    struct saved_symbol_info *saved
        = (struct saved_symbol_info *)stack_push(&s->stack, sizeof(*saved));
    saved->symbol = symbol;
    memcpy(&saved->info, &symbol->info, sizeof(saved->info));
  }

  memset(&symbol->info, 0, sizeof(symbol->info));
  symbol->info.scope_id = s->code.scope_id;
  return &symbol->info;
}

static struct symbol_info *get_or_init_info(struct cg_state *s,
                                            struct symbol *name, bool is_def)
{
  struct symbol_info *info = cg_symbol_info(s, name);
  if (info != NULL) return info;
  info = cg_new_symbol_info(s, name);
  if (cg_in_function(s)) {
    if (is_def) {
      info->type = SYMBOL_LOCAL;
      info->index = cg_append_varname(s, name);
    } else {
      info->type = SYMBOL_GLOBAL;
      info->index = cg_register_name(s, name);
    }
  } else {
    info->type = SYMBOL_NAME;
    info->index = cg_register_name(s, name);
  }
  return info;
}

static inline bool is_class_body_class_symbol(struct cg_state *s,
                                              struct symbol   *name)
{
  return s->code.in_class_body && strcmp(name->string, "__class__") == 0;
}

bool cg_declare(struct cg_state *s, struct symbol *name,
                enum symbol_info_type type)
{
  struct symbol_info *info = cg_symbol_info(s, name);
  if (info != NULL) {
    if (info->type == SYMBOL_LOCAL && type == SYMBOL_CELL) {
      info->type = SYMBOL_CELL;
      info->index = cg_append_cellvar(s, name);
      return true;
    }
    return info->type == type;
  }
  info = cg_new_symbol_info(s, name);
  info->type = type;
  if (type == SYMBOL_GLOBAL) {
    info->index = cg_register_name(s, name);
  } else if (type == SYMBOL_LOCAL) {
    info->index = cg_append_varname(s, name);
  } else if (type == SYMBOL_CELL) {
    info->index = cg_append_cellvar(s, name);
  } else {
    assert(type == SYMBOL_NONLOCAL);
    info->index = cg_register_freevar(s, name);
  }
  return true;
}

bool cg_promote_to_cell(struct cg_state *s, struct symbol *name)
{
  return cg_declare(s, name, SYMBOL_CELL);
}

bool cg_symbol_is_global(struct cg_state *s, struct symbol *name)
{
  struct symbol_info *info = cg_symbol_info(s, name);
  return info != NULL && info->type == SYMBOL_GLOBAL;
}

unsigned cg_closure_index(struct cg_state *s, struct symbol *name)
{
  struct symbol_info *info = cg_symbol_info(s, name);
  if (info != NULL) {
    switch ((enum symbol_info_type)info->type) {
    case SYMBOL_CELL:
      return info->index;
    case SYMBOL_NONLOCAL:
      return object_list_length(s->code.cellvars) + info->index;
    default:
      break;
    }
  }

  union object *freevars = s->code.freevars;
  const char   *cstring = name->string;
  size_t        length = strlen(cstring);
  for (unsigned i = 0, num_freevars = object_list_length(freevars);
       i < num_freevars; ++i) {
    const union object *object = object_list_at(freevars, i);
    if (object->type != OBJECT_STRING) continue;
    const struct object_string *string = &object->string;
    if (string->length == length
        && memcmp(string->chars, cstring, length) == 0) {
      return object_list_length(s->code.cellvars) + i;
    }
  }
  internal_error("symbol is not a closure variable");
}

void cg_load(struct cg_state *s, struct symbol *name)
{
  if (is_dunder_debug(name)) {
    cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_TRUE));
    return;
  }
  struct symbol_info *info = get_or_init_info(s, name, /*is_def=*/false);
  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_op_push1(s, OPCODE_LOAD_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_op_push1(s, OPCODE_LOAD_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_op_push1(s, OPCODE_LOAD_FAST, info->index);
    return;
  case SYMBOL_CELL:
    if (is_class_body_class_symbol(s, name)) {
      union object *freevars = s->code.freevars;
      size_t        length = strlen(name->string);
      for (unsigned i = 0, n = object_list_length(freevars); i < n; ++i) {
        const union object *object = object_list_at(freevars, i);
        if (object->type != OBJECT_STRING) continue;
        const struct object_string *string = &object->string;
        if (string->length == length
            && memcmp(string->chars, name->string, length) == 0) {
          cg_op_push1(s, OPCODE_LOAD_CLASSDEREF,
                      object_list_length(s->code.cellvars) + i);
          return;
        }
      }
      unsigned index = cg_register_name(s, name);
      cg_op_push1(s, OPCODE_LOAD_NAME, index);
      return;
    }
    cg_op_push1(s, OPCODE_LOAD_DEREF, info->index);
    return;
  case SYMBOL_NONLOCAL:
    cg_op_push1(
        s, s->code.in_class_body ? OPCODE_LOAD_CLASSDEREF : OPCODE_LOAD_DEREF,
        object_list_length(s->code.cellvars) + info->index);
    return;
  }
  internal_error("load from invalid symbol type");
}

void cg_store(struct cg_state *s, struct symbol *name)
{
  if (is_dunder_debug(name)) {
    diag_begin_error(s->d, INVALID_LOCATION);
    diag_frag(s->d, "cannot assign to __debug__");
    diag_end(s->d);
  }
  struct symbol_info *info = get_or_init_info(s, name, /*is_def=*/true);
  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_op_pop1(s, OPCODE_STORE_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_op_pop1(s, OPCODE_STORE_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_op_pop1(s, OPCODE_STORE_FAST, info->index);
    return;
  case SYMBOL_CELL:
    if (is_class_body_class_symbol(s, name)) {
      unsigned index = cg_register_name(s, name);
      cg_op_pop1(s, OPCODE_STORE_NAME, index);
      return;
    }
    cg_op_pop1(s, OPCODE_STORE_DEREF, info->index);
    return;
  case SYMBOL_NONLOCAL:
    cg_op_pop1(s, OPCODE_STORE_DEREF,
               object_list_length(s->code.cellvars) + info->index);
    return;
  }
  internal_error("store to invalid symbol type");
}

void cg_delete(struct cg_state *s, struct symbol *name)
{
  if (is_dunder_debug(name)) {
    diag_begin_error(s->d, INVALID_LOCATION);
    diag_frag(s->d, "cannot delete __debug__");
    diag_end(s->d);
  }
  struct symbol_info *info = get_or_init_info(s, name, /*is_def=*/true);
  switch ((enum symbol_info_type)info->type) {
  case SYMBOL_NAME:
    cg_op(s, OPCODE_DELETE_NAME, info->index);
    return;
  case SYMBOL_GLOBAL:
    cg_op(s, OPCODE_DELETE_GLOBAL, info->index);
    return;
  case SYMBOL_LOCAL:
    cg_op(s, OPCODE_DELETE_FAST, info->index);
    return;
  case SYMBOL_CELL:
    if (is_class_body_class_symbol(s, name)) {
      unsigned index = cg_register_name(s, name);
      cg_op(s, OPCODE_DELETE_NAME, index);
      return;
    }
    cg_op(s, OPCODE_DELETE_DEREF, info->index);
    return;
  case SYMBOL_NONLOCAL:
    cg_op(s, OPCODE_DELETE_DEREF,
          object_list_length(s->code.cellvars) + info->index);
    return;
  }
  internal_error("del invalid symbol type");
}

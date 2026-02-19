#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "pycomparse/adt/arena.h"
#include "pycomparse/adt/hashset.h"
#include "pycomparse/adt/stack.h"
#include "pycomparse/nullable.h"
#include "pycomparse/object_intern_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

union object;
struct diagnostics_state;
struct ast_statement_list;
struct pending_finally_state;

struct lnotab_mark {
  struct basic_block *block;
  unsigned            offset_in_block;
  unsigned            lineno;
};

struct basic_block {
  uint8_t                     *code_bytes;
  unsigned                     code_length;
  unsigned                     offset;
  uint8_t                      jump_opcode;
  uint8_t                      jump_size;
  uint8_t                      default_jump_size;
  uint8_t                      jump_backwards : 1;
  uint8_t                      default_jump_backwards : 1;
  struct basic_block *nullable jump_target;
  struct basic_block *nullable default_target;
  struct basic_block *nullable next;
  /* offset, jump_size, default_jump_size, default_jump_backwards
   * only valid during layout / jump relaxation. */
};

struct loop_state {
  struct basic_block *nullable           continue_block;
  struct basic_block *nullable           break_block;
  struct pending_finally_state *nullable pending_at_loop;
  unsigned                               finally_depth;
  bool                                   pop_on_break;
};

struct code_index_cache_bucket;

struct code_index_cache {
  struct hash_set                          set;
  struct code_index_cache_bucket *nullable buckets;
};

struct code_state {
  struct arena                           opcodes;
  struct object_array                    consts;
  struct object_array                    names;
  struct object_array                    varnames;
  struct object_array                    freevars;
  struct object_array                    cellvars;
  struct code_index_cache                const_index_cache;
  struct code_index_cache                name_index_cache;
  struct basic_block                    *current_block;
  struct basic_block                    *first_block;
  struct basic_block                    *last_block;
  struct loop_state                      loop_state;
  unsigned                               cg_stack_begin;
  uint32_t                               outer_scope_id;
  uint32_t                               scope_id;
  unsigned                               argcount;
  unsigned                               positional_only_argcount;
  unsigned                               keyword_only_argcount;
  unsigned                               nlocals;
  unsigned                               stacksize;
  unsigned                               max_stacksize;
  uint32_t                               flags;
  bool                                   in_function;
  bool                                   in_async_function;
  bool                                   in_class_body;
  unsigned                               active_finally_body_depth;
  bool                                   setup_annotations;
  const char *nullable                   qualname_prefix;
  struct pending_finally_state *nullable pending_finally;
  unsigned                               current_lineno;
  unsigned                               first_lineno;
  struct lnotab_mark *nullable           lnotab_marks;
  unsigned                               lnotab_marks_length;
  unsigned                               lnotab_marks_capacity;
};

struct cg_state {
  struct object_intern      objects;
  struct code_state         code;
  struct stack              stack;
  struct symbol_table      *symbol_table;
  const char               *filename;
  struct diagnostics_state *d;

  uint32_t next_scope_id;
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

#pragma once

#include <stdint.h>

#include "adt/arena.h"
#include "adt/stack.h"
#include "nullable.h"
#include "object_intern_types.h"

ASSUME_NONNULL_BEGIN

union object;
struct diagnostics_state;
struct ast_statement_list;
struct pending_finally_state;

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
  struct basic_block *nullable continue_block;
  struct basic_block *nullable break_block;
  bool                         pop_on_break;
};

struct code_state {
  struct arena                           opcodes;
  union object                          *consts;
  union object                          *names;
  union object                          *varnames;
  union object                          *freevars;
  union object                          *cellvars;
  struct basic_block                    *current_block;
  struct basic_block                    *first_block;
  struct basic_block                    *last_block;
  struct loop_state                      loop_state;
  unsigned                               cg_stack_begin;
  uint16_t                               outer_scope_id;
  uint16_t                               scope_id;
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
  bool                                   setup_annotations;
  struct pending_finally_state *nullable pending_finally;
};

struct cg_state {
  struct object_intern      objects;
  struct code_state         code;
  struct stack              stack;
  struct symbol_table      *symbol_table;
  const char               *filename;
  struct diagnostics_state *d;

  uint16_t next_scope_id;
};

ASSUME_NONNULL_END

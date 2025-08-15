#pragma once

#include <stdint.h>

#include "adt/arena.h"
#include "adt/stack.h"
#include "object_intern_types.h"

union object;

struct basic_block {
  uint8_t            *code_bytes;
  unsigned            code_length;
  unsigned            offset;
  uint8_t             jump_opcode;
  struct basic_block *jump_target;
  struct basic_block *default_target;
  struct basic_block *next;
};

struct loop_state {
  struct basic_block *continue_block;
  struct basic_block *break_block;
  bool                pop_on_break;
};

struct code_state {
  struct arena        opcodes;
  union object       *consts;
  union object       *names;
  union object       *varnames;
  struct basic_block *current_block;
  struct basic_block *first_block;
  struct basic_block *last_block;
  struct loop_state   loop_state;
  unsigned            cg_stack_begin;
  uint16_t            outer_scope_id;
  uint16_t            scope_id;
  unsigned            argcount;
  unsigned            nlocals;
  unsigned            stacksize;
  unsigned            max_stacksize;
  uint32_t            flags;
  bool                use_locals;
};

struct cg_state {
  struct object_intern objects;
  struct code_state    code;
  struct stack         stack;
  struct symbol_table *symbol_table;
  const char          *filename;

  uint16_t next_scope_id;
};

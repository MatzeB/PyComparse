#pragma once

#include <stdint.h>

#include "adt/arena.h"
#include "adt/stack.h"

union object;

struct basic_block {
  uint8_t* code_bytes;
  unsigned code_length;
  unsigned offset;
  uint8_t jump_opcode;
  struct basic_block *jump_target;
  struct basic_block *default_target;
  struct basic_block *next;
};

struct code_state {
  struct arena opcodes;
  union object *consts;
  union object *names;
  union object *varnames;
  struct basic_block *current_block;
  struct basic_block *first_block;
  unsigned cg_stack_begin;
  uint16_t outer_scope_id;
  uint16_t scope_id;
  unsigned argcount;
  unsigned nlocals;
  unsigned stacksize;
  unsigned max_stacksize;
  bool had_return;
  bool module_level;
  bool use_locals;
};

struct cg_state {
  struct arena objects;
  struct code_state code;
  struct stack stack;

  uint16_t next_scope_id;
};

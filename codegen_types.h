#pragma once

#include <stdint.h>

#include "adt/arena.h"

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
  struct basic_block *current_block;
  struct basic_block *first_block;
  unsigned stacksize;
  unsigned max_stacksize;
  bool had_return;
  bool module_level;
};

struct cg_state {
  struct arena objects;
  struct arena codestack;
  struct code_state code;
};

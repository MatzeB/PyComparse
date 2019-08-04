#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdint.h>

#include "adt/arena.h"

struct object_list;
struct object_code;
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
  struct object_list *consts;
  struct object_list *names;
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

unsigned cg_register_name(struct cg_state *s, const char *name);
unsigned cg_register_string(struct cg_state *s, const char *chars,
                            uint32_t length);
unsigned cg_register_int(struct cg_state *s, int32_t value);
unsigned cg_register_singleton(struct cg_state *s, char type);
unsigned cg_register_code(struct cg_state *s, struct object_code *code);

unsigned cg_append_name(struct cg_state *s, const char *name);

void cg_pop(struct cg_state *s, unsigned n);

void cg_op(struct cg_state *s, uint8_t opcoode, uint32_t arg);
void cg_push_op(struct cg_state *s, uint8_t opcode, uint32_t arg);
void cg_pop_op(struct cg_state *s, uint8_t opcode, uint32_t arg);

struct basic_block *cg_allocate_block(struct cg_state *s);

void cg_begin_block(struct cg_state *s, struct basic_block *block);
struct basic_block *cg_end_block(struct cg_state *s);

struct code_state *cg_push_code(struct cg_state *s);
struct object_code *cg_pop_code(struct cg_state *s, struct code_state *saved,
                                const char *name);

void cg_begin(struct cg_state *s);
struct object_code *cg_end(struct cg_state *s);
void cg_free(struct cg_state *s);

#endif

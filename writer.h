#ifndef WRITER_H
#define WRITER_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

#include "adt/arena.h"

struct object_list;
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

struct code_writer_state {
  bool had_return;
  unsigned stacksize;
  unsigned max_stacksize;
  struct object_list *consts;
  struct object_list *names;
  struct basic_block *current_block;
  struct basic_block *first_block;
};

struct bytecode_writer_state {
  FILE *out;

  struct arena objects;
  struct arena opcodes;
  struct code_writer_state code;
};

void writer_begin_file(struct bytecode_writer_state *s, FILE *out);

void writer_end_file(struct bytecode_writer_state *s);

void write(struct bytecode_writer_state *s, const union object *object);

void write_op(struct bytecode_writer_state *s, uint8_t opcoode, uint32_t arg);

void write_push_op(struct bytecode_writer_state *s, uint8_t opcode,
                   uint32_t arg);

void writer_pop(struct bytecode_writer_state *s, unsigned n);

void write_pop_op(struct bytecode_writer_state *s, uint8_t opcode,
                  uint32_t arg);

struct basic_block *writer_allocate_block(struct bytecode_writer_state *s);

void writer_begin_block(struct bytecode_writer_state *s,
                        struct basic_block *block);

struct basic_block *writer_end_block(struct bytecode_writer_state *s);

unsigned writer_register_name(struct bytecode_writer_state *s,
                              const char *name);

unsigned writer_register_string(struct bytecode_writer_state *s,
                                const char *chars, uint32_t length);

unsigned writer_register_int(struct bytecode_writer_state *s, int32_t value);

unsigned writer_register_singleton(struct bytecode_writer_state *s, char type);

#endif

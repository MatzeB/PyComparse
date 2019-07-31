#ifndef WRITER_H
#define WRITER_H

#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

#include "adt/arena.h"

struct object_list;
union object;

struct bytecode_writer_state {
  FILE *out;

  struct arena objects;
  struct arena opcodes;
  bool had_return;
  unsigned stacksize;
  unsigned max_stacksize;
  struct object_list *consts;
  struct object_list *names;
};

void writer_begin(struct bytecode_writer_state *s, FILE *out);

void writer_finish(struct bytecode_writer_state *s);

void write(struct bytecode_writer_state *s, const union object *object);

void write_op(struct bytecode_writer_state *s, uint8_t opcoode, uint32_t arg);

void write_push_op(struct bytecode_writer_state *s, uint8_t opcode,
                   uint32_t arg);

void writer_pop(struct bytecode_writer_state *s, unsigned n);

void write_pop_op(struct bytecode_writer_state *s, uint8_t opcode,
                  uint32_t arg);

unsigned writer_register_name(struct bytecode_writer_state *s,
                              const char *name);

unsigned writer_register_string(struct bytecode_writer_state *s,
                                const char *chars, uint32_t length);

unsigned writer_register_int(struct bytecode_writer_state *s, int32_t value);

unsigned writer_register_singleton(struct bytecode_writer_state *s, char type);

#endif

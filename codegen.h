#pragma once

#include <stdint.h>

union object;
struct cg_state;
struct basic_block;

unsigned cg_register_name(struct cg_state *s, const char *name);
unsigned cg_register_string(struct cg_state *s, const char *chars,
                            uint32_t length);
unsigned cg_register_int(struct cg_state *s, int32_t value);
unsigned cg_register_singleton(struct cg_state *s, char type);
unsigned cg_register_code(struct cg_state *s, union object *code);

unsigned cg_append_name(struct cg_state *s, const char *name);

void cg_pop(struct cg_state *s, unsigned n);

void cg_op(struct cg_state *s, uint8_t opcoode, uint32_t arg);
void cg_push_op(struct cg_state *s, uint8_t opcode, uint32_t arg);
void cg_pop_op(struct cg_state *s, uint8_t opcode, uint32_t arg);

struct basic_block *cg_allocate_block(struct cg_state *s);

void cg_begin_block(struct cg_state *s, struct basic_block *block);
struct basic_block *cg_end_block(struct cg_state *s);

struct code_state *cg_push_code(struct cg_state *s);
union object *cg_pop_code(struct cg_state *s, struct code_state *saved,
                          const char *name);

void cg_begin(struct cg_state *s);
union object *cg_end(struct cg_state *s);
void cg_free(struct cg_state *s);

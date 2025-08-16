#pragma once

#include <stdbool.h>
#include <stdint.h>

struct basic_block;
struct cg_state;
struct diagnostics_state;
struct name_info;
struct symbol;
struct symbol_table;
union object;

unsigned cg_register_code(struct cg_state *s, union object *code);
unsigned cg_register_object(struct cg_state *s, union object *object);
unsigned cg_register_unique_object(struct cg_state *s, union object *object);

unsigned cg_register_name_from_cstring(struct cg_state *s,
                                       const char      *cstring);
unsigned cg_register_name(struct cg_state *s, struct symbol *symbol);
unsigned cg_append_name(struct cg_state *s, struct symbol *symbol);
unsigned cg_append_varname(struct cg_state *s, struct symbol *symbol);

bool cg_use_locals(struct cg_state *s);

struct symbol_info *cg_symbol_info(struct cg_state *s, struct symbol *symbol);
struct symbol_info *cg_new_symbol_info(struct cg_state *s,
                                       struct symbol   *symbol);

void cg_push(struct cg_state *s, unsigned n);
void cg_pop(struct cg_state *s, unsigned n);

void cg_op(struct cg_state *s, uint8_t opcoode, uint32_t arg);
void cg_push_op(struct cg_state *s, uint8_t opcode, uint32_t arg);
void cg_pop_op(struct cg_state *s, uint8_t opcode, uint32_t arg);

struct basic_block *cg_allocate_block(struct cg_state *s);

void cg_block_begin(struct cg_state *s, struct basic_block *block);
struct basic_block *cg_block_end(struct cg_state *s);
bool                cg_in_block(struct cg_state *s);

void          cg_code_begin(struct cg_state *s, bool use_locals);
union object *cg_code_end(struct cg_state *s, const char *name);

void          cg_push_code(struct cg_state *s);
union object *cg_pop_code(struct cg_state *s, const char *name);

void cg_load_const(struct cg_state *s, union object *object);

void cg_init(struct cg_state *s, struct symbol_table *symbol_table,
             const char *filename);
void cg_free(struct cg_state *s);

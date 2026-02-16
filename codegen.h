#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "nullable.h"
#include "opcodes.h"
#include "symbol_info_types.h"

ASSUME_NONNULL_BEGIN

struct basic_block;
struct cg_state;
struct diagnostics_state;
struct symbol;
struct symbol_table;
union object;

unsigned cg_register_object(struct cg_state *s, union object *object);
unsigned cg_register_unique_object(struct cg_state *s, union object *object);

unsigned cg_register_name_from_cstring(struct cg_state *s,
                                       const char      *cstring);
unsigned cg_register_name(struct cg_state *s, struct symbol *symbol);
unsigned cg_register_freevar(struct cg_state *s, struct symbol *symbol);

void cg_op(struct cg_state *s, enum opcode opcode, uint32_t arg);
void cg_push(struct cg_state *s, unsigned n);
void cg_pop(struct cg_state *s, unsigned n);
void cg_mark_max_stack_extra(struct cg_state *s, unsigned extra);
void cg_op_pop_push(struct cg_state *s, enum opcode opcode, uint32_t arg,
                    unsigned pop, unsigned push);
void cg_op_pop1(struct cg_state *s, enum opcode opcode, uint32_t arg);
void cg_op_push1(struct cg_state *s, enum opcode opcode, uint32_t arg);

struct basic_block *cg_block_allocate(struct cg_state *s);
void cg_block_begin(struct cg_state *s, struct basic_block *block);
struct basic_block *cg_block_end(struct cg_state *s);
bool                cg_in_block(struct cg_state *s);
void                cg_condjump(struct cg_state *s, enum opcode opcode,
                                struct basic_block          *target,
                                struct basic_block *nullable fallthrough);
void                cg_jump(struct cg_state *s, struct basic_block *target);
void cg_block_insert_delayed(struct cg_state *s, struct basic_block *block);
void cg_block_begin_delayed(struct cg_state *s, struct basic_block *block);

void cg_set_lineno(struct cg_state *s, unsigned lineno);

void          cg_code_begin(struct cg_state *s, bool in_function);
union object *cg_code_end(struct cg_state *s, const char *name);
bool          cg_in_function(struct cg_state *s);

void          cg_push_code(struct cg_state *s);
union object *cg_pop_code(struct cg_state *s, const char *name);

void cg_load_const(struct cg_state *s, union object *object);
void cg_set_function_docstring(struct cg_state *s, union object *nullable doc);

bool     cg_declare(struct cg_state *s, struct symbol *name,
                    enum symbol_info_type type);
bool     cg_promote_to_cell(struct cg_state *s, struct symbol *name);
bool     cg_symbol_is_global(struct cg_state *s, struct symbol *name);
unsigned cg_closure_index(struct cg_state *s, struct symbol *name);
void     cg_load(struct cg_state *s, struct symbol *name);
void     cg_store(struct cg_state *s, struct symbol *name);
void     cg_delete(struct cg_state *s, struct symbol *name);

const char *cg_build_qualname(struct cg_state *s, const char *name);

void cg_init(struct cg_state *s, struct symbol_table *symbol_table,
             const char *filename, struct diagnostics_state *d);
void cg_free(struct cg_state *s);

ASSUME_NONNULL_END

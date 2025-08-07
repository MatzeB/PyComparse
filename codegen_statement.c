#include "codegen_statement.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#include "ast_types.h"
#include "codegen.h"
#include "codegen_expression.h"
#include "codegen_types.h"
#include "objects.h"
#include "opcodes.h"
#include "symbol_types.h"
#include "symbol_info_types.h"

static bool unreachable(struct cg_state *s)
{
  return !cg_in_block(s);
}

static void emit_code_end(struct cg_state *s)
{
  if (unreachable(s)) return;
  unsigned i_none = cg_register_singleton(s, TYPE_NONE);
  cg_push_op(s, OPCODE_LOAD_CONST, i_none);
  cg_pop_op(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);
}

void emit_module_begin(struct cg_state *s)
{
  cg_begin(s);
  cg_code_begin(s, /*use_locals=*/false);
}

union object *emit_module_end(struct cg_state *s)
{
  emit_code_end(s);
  return cg_code_end(s, "<module>");
}

void emit_expression_statement(struct cg_state *s,
                               union ast_expression *expression)
{
  if (unreachable(s)) return;
  emit_expression_drop_result(s, expression);
}

static unsigned emit_dotted_name(struct cg_state *s, struct dotted_name *name)
{
  unsigned num_symbols = name->num_symbols;
  unsigned length = num_symbols;
  for (unsigned i = 0; i < num_symbols; i++) {
    length += strlen(name->symbols[i]->string);
  }
  char *chars = (char*)arena_allocate(&s->objects, length, 1);
  char *c = chars;
  for (unsigned i = 0; i < num_symbols; i++) {
    if (i > 0) *c++ = '.';
    const char *symbol_string = name->symbols[i]->string;
    size_t symbol_length = strlen(symbol_string);
    memcpy(c, symbol_string, symbol_length);
    c += symbol_length;
  }
  *c++ = '\0';
  assert(c - chars == length);
  return cg_register_name(s, chars);
}

void emit_import_statement(struct cg_state *s, struct dotted_name *name,
                           struct symbol *as)
{
  if (unreachable(s)) return;
  unsigned name_index = emit_dotted_name(s, name);
  int i_level = cg_register_int(s, 0);
  cg_push_op(s, OPCODE_LOAD_CONST, i_level);
  int i_fromlist = cg_register_singleton(s, TYPE_NONE);
  cg_push_op(s, OPCODE_LOAD_CONST, i_fromlist);
  cg_pop_op(s, OPCODE_IMPORT_NAME, name_index);

  assert(as == NULL && "TODO");
  emit_store(s, name->symbols[0]);
}

void emit_return_statement(struct cg_state *s,
                           union ast_expression *expression)
{
  if (unreachable(s)) return;
  if (expression != NULL) {
    emit_expression(s, expression);
  } else {
    unsigned i_none = cg_register_singleton(s, TYPE_NONE);
    cg_push_op(s, OPCODE_LOAD_CONST, i_none);
  }
  cg_pop_op(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);
}

static void emit_condjump(struct cg_state *s, uint8_t opcode,
                          struct basic_block *target,
                          struct basic_block *fallthrough)
{
  struct basic_block *block = cg_block_end(s);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->jump_opcode = opcode;
  block->jump_target = target;
  block->default_target = fallthrough;
}

static void emit_jump(struct cg_state *s, struct basic_block *target)
{
  struct basic_block *block = cg_block_end(s);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->default_target = target;
}

void emit_if_begin(struct cg_state *s, struct if_state *state,
                   union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }

  struct basic_block *true_block = cg_allocate_block(s);
  struct basic_block *false_block = cg_allocate_block(s);
  emit_expression(s, expression);
  emit_condjump(s, OPCODE_POP_JUMP_IF_FALSE, false_block, true_block);
  cg_pop(s, 1);

  cg_block_begin(s, true_block);
  state->false_or_footer = false_block;
}

void emit_else_begin(struct cg_state *s, struct if_state *state)
{
  struct basic_block *false_block = state->false_or_footer;
  if (false_block == NULL) return;
  struct basic_block *footer = cg_allocate_block(s);
  state->false_or_footer = footer;
  if (!unreachable(s)) {
    emit_jump(s, footer);
  }

  cg_block_begin(s, false_block);
}

void emit_if_end(struct cg_state *s, struct if_state *state)
{
  struct basic_block *footer = state->false_or_footer;
  if (footer == NULL) return;
  if (!unreachable(s)) {
    emit_jump(s, footer);
  }

  cg_block_begin(s, footer);
}

void emit_for_begin(struct cg_state *s, struct for_state *state,
                    union ast_expression *target,
                    union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }

  struct basic_block *setup = cg_allocate_block(s);
  state->header = cg_allocate_block(s);
  state->body = cg_allocate_block(s);
  state->after = cg_allocate_block(s);

  emit_jump(s, setup);
  cg_block_begin(s, setup);
  emit_expression(s, expression);
  cg_op(s, OPCODE_GET_ITER, 0);
  emit_jump(s, state->header);

  cg_block_begin(s, state->header);
  emit_condjump(s, OPCODE_FOR_ITER, state->after, state->body);

  cg_block_begin(s, state->body);
  emit_assignment(s, target);
}

void emit_for_end(struct cg_state *s, struct for_state *state)
{
  struct basic_block *after = state->after;
  if (after == NULL) return;

  if (!unreachable(s)) {
    emit_jump(s, state->header);
  }

  cg_block_begin(s, after);
}

void emit_while_begin(struct cg_state *s, struct while_state *state,
                      union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }
  state->header = cg_allocate_block(s);
  state->body = cg_allocate_block(s);
  state->after = cg_allocate_block(s);

  emit_jump(s, state->header);
  cg_block_begin(s, state->header);
  emit_expression(s, expression);
  emit_condjump(s, OPCODE_POP_JUMP_IF_FALSE, state->after, state->body);
  cg_pop(s, 1);

  cg_block_begin(s, state->body);
}

void emit_while_end(struct cg_state *s, struct while_state *state)
{
  struct basic_block *after = state->after;
  if (after == NULL) return;

  if (!unreachable(s)) {
    emit_jump(s, state->header);
  }

  cg_block_begin(s, after);
}

void emit_def_begin(struct cg_state *s)
{
  cg_push_code(s);
  cg_code_begin(s, /*use_locals=*/true);
}

bool emit_parameter(struct cg_state *s, struct symbol *symbol)
{
  struct symbol_info *info = cg_symbol_info(s, symbol);
  if (info != NULL) {
    return false;
  }
  info = cg_new_symbol_info(s, symbol);
  info->type = SYMBOL_LOCAL;
  info->index = cg_append_varname(s, symbol->string);
  return true;
}

void emit_def_end(struct cg_state *s, struct symbol *symbol)
{
  emit_code_end(s);

  union object *code = cg_pop_code(s, symbol->string);
  unsigned code_index = cg_register_object(s, code);
  cg_push_op(s, OPCODE_LOAD_CONST, code_index);

  const char *chars = symbol->string;
  uint32_t length = strlen(chars);
  unsigned name_const_index = cg_register_string(s, chars, length);
  cg_push_op(s, OPCODE_LOAD_CONST, name_const_index);

  cg_op(s, OPCODE_MAKE_FUNCTION, 0);
  cg_pop(s, 1);
  emit_store(s, symbol);
}

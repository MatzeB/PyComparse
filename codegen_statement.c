#include "codegen_statement.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#include "codegen.h"
#include "codegen_ast.h"
#include "codegen_types.h"
#include "objects.h"
#include "opcodes.h"
#include "symbol_types.h"
#include "symbol_info_types.h"

void emit_expression_statement(struct cg_state *s, union ast_node *expression)
{
  if (!cg_in_block(s)) return;
  emit_expression_drop_result(s, expression);
}

void emit_return_statement(struct cg_state *s, union ast_node *expression)
{
  if (!cg_in_block(s)) return;
  if (expression != NULL) {
    emit_expression(s, expression);
  } else {
    unsigned i_none = cg_register_singleton(s, TYPE_NONE);
    cg_push_op(s, OPCODE_LOAD_CONST, i_none);
  }
  cg_pop_op(s, OPCODE_RETURN_VALUE, 0);
  cg_end_block(s);
}

static void emit_condjump(struct cg_state *s, uint8_t opcode,
                          struct basic_block *target,
                          struct basic_block *fallthrough)
{
  struct basic_block *block = cg_end_block(s);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->jump_opcode = opcode;
  block->jump_target = target;
  block->default_target = fallthrough;
}

static void emit_jump(struct cg_state *s, struct basic_block *target)
{
  struct basic_block *block = cg_end_block(s);
  assert(block->jump_opcode == 0 && block->jump_target == NULL);
  block->default_target = target;
}

void emit_begin_if(struct cg_state *s, struct if_state *state,
                   union ast_node *expression)
{
  if (!cg_in_block(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }

  struct basic_block *true_block = cg_allocate_block(s);
  struct basic_block *false_block = cg_allocate_block(s);
  emit_expression(s, expression);
  emit_condjump(s, OPCODE_POP_JUMP_IF_FALSE, false_block, true_block);
  cg_pop(s, 1);

  cg_begin_block(s, true_block);
  state->false_or_footer = false_block;
}

void emit_begin_else(struct cg_state *s, struct if_state *state)
{
  struct basic_block *false_block = state->false_or_footer;
  if (false_block == NULL) return;
  struct basic_block *footer = cg_allocate_block(s);
  state->false_or_footer = footer;
  if (cg_in_block(s)) {
    emit_jump(s, footer);
  }

  cg_begin_block(s, false_block);
}

void emit_end_if(struct cg_state *s, struct if_state *state)
{
  struct basic_block *footer = state->false_or_footer;
  if (footer == NULL) return;
  if (cg_in_block(s)) {
    emit_jump(s, footer);
  }

  cg_begin_block(s, footer);
}

void emit_begin_for(struct cg_state *s, struct for_state *state,
                    union ast_node *target, union ast_node *expression)
{
  if (!cg_in_block(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }

  struct basic_block *setup = cg_allocate_block(s);
  state->header = cg_allocate_block(s);
  state->body = cg_allocate_block(s);
  state->footer = cg_allocate_block(s);
  state->after = cg_allocate_block(s);

  emit_condjump(s, OPCODE_SETUP_LOOP, state->after, setup);

  cg_begin_block(s, setup);
  emit_expression(s, expression);
  cg_op(s, OPCODE_GET_ITER, 0);
  emit_jump(s, state->header);

  cg_begin_block(s, state->header);
  emit_condjump(s, OPCODE_FOR_ITER, state->footer, state->body);

  cg_begin_block(s, state->body);
  emit_assignment(s, target);
}

void emit_end_for(struct cg_state *s, struct for_state *state)
{
  struct basic_block *footer = state->footer;
  if (footer == NULL) return;

  if (cg_in_block(s)) {
    emit_jump(s, state->header);
  }

  cg_begin_block(s, footer);
  cg_op(s, OPCODE_POP_BLOCK, 0);
  cg_end_block(s);

  cg_begin_block(s, state->after);
}

void emit_begin_while(struct cg_state *s, struct while_state *state,
                      union ast_node *expression)
{
  if (!cg_in_block(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }
  state->header = cg_allocate_block(s);
  state->body = cg_allocate_block(s);
  state->footer = cg_allocate_block(s);
  state->after = cg_allocate_block(s);

  emit_condjump(s, OPCODE_SETUP_LOOP, state->after, state->header);

  cg_begin_block(s, state->header);
  emit_expression(s, expression);
  emit_condjump(s, OPCODE_POP_JUMP_IF_FALSE, state->footer, state->body);
  cg_pop(s, 1);

  cg_begin_block(s, state->body);
}

void emit_end_while(struct cg_state *s, struct while_state *state)
{
  struct basic_block *footer = state->footer;
  if (footer == NULL) return;

  if (cg_in_block(s)) {
    emit_jump(s, state->header);
  }

  cg_begin_block(s, footer);
  cg_op(s, OPCODE_POP_BLOCK, 0);
  cg_end_block(s);

  cg_begin_block(s, state->after);
}

void emit_begin_def(struct cg_state *s)
{
  cg_push_code(s);
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

void emit_end_def(struct cg_state *s, struct symbol *symbol)
{
  union object *code = cg_pop_code(s, symbol->string);
  unsigned code_index = cg_register_code(s, code);
  cg_push_op(s, OPCODE_LOAD_CONST, code_index);

  const char *chars = symbol->string;
  uint32_t length = strlen(chars);
  unsigned name_const_index = cg_register_string(s, chars, length);
  cg_push_op(s, OPCODE_LOAD_CONST, name_const_index);

  cg_op(s, OPCODE_MAKE_FUNCTION, 0);
  cg_pop(s, 1);
  emit_store(s, symbol);
}

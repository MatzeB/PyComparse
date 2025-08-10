#include "codegen_statement.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>

#include "ast.h"
#include "ast_types.h"
#include "codegen.h"
#include "codegen_expression.h"
#include "codegen_types.h"
#include "object.h"
#include "object_intern.h"
#include "opcodes.h"
#include "symbol_info_types.h"
#include "symbol_table.h"
#include "symbol_types.h"

static bool unreachable(struct cg_state *s)
{
  return !cg_in_block(s);
}

void emit_code_end(struct cg_state *s)
{
  if (unreachable(s)) return;
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
  cg_pop_op(s, OPCODE_RETURN_VALUE, 0);
  cg_block_end(s);
}

void emit_module_begin(struct cg_state *s)
{
  cg_code_begin(s, /*use_locals=*/false);
}

union object *emit_module_end(struct cg_state *s)
{
  emit_code_end(s);
  return cg_code_end(s, "<module>");
}

void emit_expression_statement(struct cg_state      *s,
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
  struct arena *arena = object_intern_arena(&s->objects);
  char         *chars = (char *)arena_allocate(arena, length, 1);
  char         *c = chars;
  for (unsigned i = 0; i < num_symbols; i++) {
    if (i > 0) *c++ = '.';
    const char *symbol_string = name->symbols[i]->string;
    size_t      symbol_length = strlen(symbol_string);
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
  unsigned      name_index = emit_dotted_name(s, name);
  union object *object = object_intern_int(&s->objects, 0);
  cg_load_const(s, object);
  cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
  cg_pop_op(s, OPCODE_IMPORT_NAME, name_index);

  assert(as == NULL && "TODO");
  emit_store(s, name->symbols[0]);
}

void emit_return_statement(struct cg_state      *s,
                           union ast_expression *expression)
{
  if (unreachable(s)) return;
  if (expression != NULL) {
    emit_expression(s, expression);
  } else {
    cg_load_const(s, object_intern_singleton(&s->objects, OBJECT_NONE));
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

static void emit_for_begin_impl(struct cg_state *s, struct for_state *state,
                                union ast_expression *target)
{
  state->header = cg_allocate_block(s);
  state->body = cg_allocate_block(s);
  state->after = cg_allocate_block(s);

  emit_jump(s, state->header);

  cg_block_begin(s, state->header);
  emit_condjump(s, OPCODE_FOR_ITER, state->after, state->body);
  cg_push(s, 1);

  cg_block_begin(s, state->body);
  emit_assignment(s, target);
}

void emit_for_begin(struct cg_state *s, struct for_state *state,
                    union ast_expression *target,
                    union ast_expression *expression)
{
  if (unreachable(s)) {
    memset(state, 0, sizeof(*state));
    return;
  }
  emit_expression(s, expression);
  cg_op(s, OPCODE_GET_ITER, 0);
  emit_for_begin_impl(s, state, target);
}

void emit_for_end(struct cg_state *s, struct for_state *state)
{
  struct basic_block *after = state->after;
  if (after == NULL) return;

  if (!unreachable(s)) {
    emit_jump(s, state->header);
  }

  cg_block_begin(s, after);
  cg_pop(s, 1);
}

void emit_continue(struct cg_state *s, struct for_state *state)
{
  if (unreachable(s)) return;
  emit_jump(s, state->header);
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

void emit_class_begin(struct cg_state *s, struct symbol *symbol)
{
  cg_push_op(s, OPCODE_LOAD_BUILD_CLASS, 0);

  cg_push_code(s);
  cg_code_begin(s, /*use_locals=*/false);

  emit_load(s, symbol_table_get_or_insert(s->symbol_table, "__name__"));
  emit_store(s, symbol_table_get_or_insert(s->symbol_table, "__module__"));
  cg_load_const(s, object_intern_cstring(&s->objects, symbol->string));
  emit_store(s, symbol_table_get_or_insert(s->symbol_table, "__qualname__"));
}

static void emit_decorator_calls(struct cg_state *s, unsigned num_decorators)
{
  for (unsigned i = 0; i < num_decorators; i++) {
    cg_pop_op(s, OPCODE_CALL_FUNCTION, 1);
  }
}

void emit_class_end(struct cg_state *s, struct symbol *symbol,
                    unsigned num_decorators)
{
  emit_code_end(s);
  union object *code = cg_pop_code(s, symbol->string);
  unsigned      code_index = cg_register_unique_object(s, code);
  cg_push_op(s, OPCODE_LOAD_CONST, code_index);

  union object *string = object_intern_cstring(&s->objects, symbol->string);
  cg_load_const(s, string);
  cg_pop_op(s, OPCODE_MAKE_FUNCTION, 0);
  cg_load_const(s, string);
  cg_op(s, OPCODE_CALL_FUNCTION, 2);
  cg_pop(s, 2);
  emit_decorator_calls(s, num_decorators);
  emit_store(s, symbol);
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

void emit_def_end(struct cg_state *s, struct symbol *symbol,
                  unsigned num_decorators)
{
  emit_code_end(s);
  union object *code = cg_pop_code(s, symbol->string);
  unsigned      code_index = cg_register_unique_object(s, code);
  cg_push_op(s, OPCODE_LOAD_CONST, code_index);
  cg_load_const(s, object_intern_cstring(&s->objects, symbol->string));
  cg_pop_op(s, OPCODE_MAKE_FUNCTION, 0);
  emit_decorator_calls(s, num_decorators);
  emit_store(s, symbol);
}

static void emit_generator_expression_part(
    struct cg_state *s, struct ast_generator_expression *generator_expression,
    unsigned part_index, struct for_state *outer_for)
{
  if (part_index >= generator_expression->num_parts) {
    emit_expression(s, generator_expression->expression);
    if (generator_expression->base.type == AST_LIST_COMPREHENSION) {
      /* Need to compute position of list in stack...
       * using "number of for" + 1 for now but this feels sketchy... */
      unsigned num_for = 0;
      unsigned num_parts = generator_expression->num_parts;
      for (unsigned i = 0; i < num_parts; i++) {
        if (generator_expression->parts[i].type
            == GENERATOR_EXPRESSION_PART_FOR) {
          num_for++;
        }
      }
      cg_pop_op(s, OPCODE_LIST_APPEND, num_for + 1);
    } else {
      s->code.flags |= CO_GENERATOR;
      cg_op(s, OPCODE_YIELD_VALUE, 0);
      cg_pop_op(s, OPCODE_POP_TOP, 0);
    }
    return;
  }
  struct generator_expression_part *part
      = &generator_expression->parts[part_index];
  if (part->type == GENERATOR_EXPRESSION_PART_FOR) {
    struct for_state state;
    emit_for_begin(s, &state, part->target, part->expression);

    emit_generator_expression_part(s, generator_expression, part_index + 1,
                                   /*outer_for=*/&state);

    emit_for_end(s, &state);
    return;
  }
  assert(part->type == GENERATOR_EXPRESSION_PART_IF);
  struct basic_block *false_block = cg_allocate_block(s);

  emit_expression(s, part->expression);
  struct basic_block *loop_header = outer_for->header;
  emit_condjump(s, OPCODE_POP_JUMP_IF_FALSE, loop_header, false_block);
  cg_pop(s, 1);
  cg_block_begin(s, false_block);

  emit_generator_expression_part(s, generator_expression, part_index + 1,
                                 outer_for);
}

void emit_generator_expression_code(
    struct cg_state *s, struct ast_generator_expression *generator_expression)
{
  emit_parameter(s, symbol_table_get_or_insert(s->symbol_table, ".0"));
  s->code.argcount = 1;

  enum ast_expression_type type = generator_expression->base.type;
  assert(type == AST_GENERATOR_EXPRESSION || type == AST_LIST_COMPREHENSION);
  if (type == AST_LIST_COMPREHENSION) {
    cg_push_op(s, OPCODE_BUILD_LIST, 0);
  }

  struct generator_expression_part *part = generator_expression->parts;
  cg_push_op(s, OPCODE_LOAD_FAST, 0);
  struct for_state state;
  emit_for_begin_impl(s, &state, part->target);

  emit_generator_expression_part(s, generator_expression, /*part_index=*/1,
                                 /*outer_for=*/&state);

  emit_for_end(s, &state);

  if (type == AST_LIST_COMPREHENSION) {
    cg_pop_op(s, OPCODE_RETURN_VALUE, 0);
    cg_block_end(s);
  }
}

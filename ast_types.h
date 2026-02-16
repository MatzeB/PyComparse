#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "nullable.h"
#include "scanner_location.h"

ASSUME_NONNULL_BEGIN

struct ast_node_base {
  uint8_t type;
};

struct ast_expression_base {
  struct ast_node_base base;
};

struct ast_statement_base {
  struct ast_node_base base;
  struct location      location;
};

struct symbol;

struct ast_scope_bindings {
  unsigned                           num_globals;
  struct symbol * nonnull * nullable globals;
  unsigned                           num_locals;
  struct symbol * nonnull * nullable locals;
  unsigned                           num_cellvars;
  struct symbol * nonnull * nullable cellvars;
  unsigned                           num_freevars;
  struct symbol * nonnull * nullable freevars;
};

static const struct ast_scope_bindings empty_scope_bindings = { 0 };

static inline const struct ast_scope_bindings *
scope_bindings_or_empty(const struct ast_scope_bindings *nullable scope)
{
  return scope != NULL ? scope : &empty_scope_bindings;
}

struct parameter_shape {
  unsigned num_parameters;
  unsigned keyword_only_begin;
  unsigned positional_only_argcount;
};

ASSUME_NONNULL_END

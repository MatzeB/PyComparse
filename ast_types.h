#pragma once

#include <stdbool.h>
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

struct parameter_shape {
  unsigned num_parameters;
  unsigned keyword_only_begin;
  unsigned positional_only_argcount;
};

ASSUME_NONNULL_END

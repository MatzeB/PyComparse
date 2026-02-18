#pragma once

#include <stdbool.h>

#include "adt/arena.h"
#include "diagnostics_types.h"
#include "nullable.h"
#include "object.h"
#include "object_intern_types.h"
#include "scanner_types.h"
#include "token_kinds.h"

ASSUME_NONNULL_BEGIN

struct parser_state {
  struct scanner_state      scanner;
  struct arena              ast;
  struct object_intern     *objects;
  struct diagnostics_state *d;
  struct symbol *nullable   private_class_name;
  bool *nullable            current_function_has_yield;
  bool                      parsed_await_expression;
  uint32_t                  future_flags;
  bool                      top_level_future_imports_allowed;
  bool                      top_level_seen_any_statement;

  uint16_t anchor_set[NUM_TOKENS];
};

ASSUME_NONNULL_END

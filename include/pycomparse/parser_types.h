#pragma once

#include <stdbool.h>

#include "pycomparse/adt/arena.h"
#include "pycomparse/diagnostics_types.h"
#include "pycomparse/nullable.h"
#include "pycomparse/object.h"
#include "pycomparse/object_intern_types.h"
#include "pycomparse/scanner_types.h"
#include "pycomparse/token_kinds.h"

#ifdef __cplusplus
extern "C" {
#endif

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

  int      nesting_depth;
  uint16_t anchor_set[NUM_TOKENS];
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

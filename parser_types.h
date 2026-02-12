#pragma once

#include <stdbool.h>

#include "adt/arena.h"
#include "codegen_types.h"
#include "diagnostics_types.h"
#include "nullable.h"
#include "object.h"
#include "scanner_types.h"
#include "token_kinds.h"

ASSUME_NONNULL_BEGIN

struct parser_state {
  struct scanner_state      scanner;
  struct arena              ast;
  struct cg_state           cg;
  struct diagnostics_state *d;
  bool *nullable            current_function_has_yield;
  uint32_t                  future_flags;
  bool                      top_level_future_imports_allowed;
  bool                      top_level_seen_any_statement;

  unsigned short anchor_set[NUM_TOKENS];
};

ASSUME_NONNULL_END

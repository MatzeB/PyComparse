#pragma once

#include <stdbool.h>

#include "adt/arena.h"
#include "codegen_types.h"
#include "diagnostics_types.h"
#include "nullable.h"
#include "scanner_types.h"
#include "token_kinds.h"

ASSUME_NONNULL_BEGIN

struct parser_state {
  struct scanner_state      scanner;
  struct arena              ast;
  struct cg_state           cg;
  struct diagnostics_state *d;

  unsigned short anchor_set[NUM_TOKENS];
};

ASSUME_NONNULL_END

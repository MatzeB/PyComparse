#pragma once

#include <stdbool.h>

#include "adt/arena.h"
#include "codegen_types.h"
#include "scanner_types.h"
#include "token_kinds.h"

struct parser_state {
  struct scanner_state scanner;
  struct arena         ast;
  struct cg_state      cg;
  bool                 error;
#ifndef NDEBUG
  bool in_error;
#endif
  unsigned short anchor_set[NUM_TOKENS];
};

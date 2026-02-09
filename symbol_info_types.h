#pragma once

#include "nullable.h"

ASSUME_NONNULL_BEGIN

enum symbol_info_type {
  SYMBOL_GLOBAL,
  SYMBOL_LOCAL,
  SYMBOL_CELL,
  SYMBOL_NAME,
  SYMBOL_NONLOCAL,
};

struct symbol_info {
  unsigned char type;
  uint16_t      scope_id;
  unsigned      index;
};

ASSUME_NONNULL_END

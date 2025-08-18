#pragma once

#include <stdint.h>

#include "nullable.h"
#include "symbol_info_types.h"

ASSUME_NONNULL_BEGIN

struct symbol {
  const char        *string;
  uint16_t           token_kind;
  struct symbol_info info;
};

ASSUME_NONNULL_END

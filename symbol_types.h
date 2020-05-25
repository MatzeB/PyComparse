#pragma once

#include <stdint.h>

#include "symbol_info_types.h"

struct symbol {
  const char        *string;
  uint16_t           token_kind;
  struct symbol_info info;
};

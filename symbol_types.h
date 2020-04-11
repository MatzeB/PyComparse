#pragma once

#include <stdint.h>

struct symbol {
  const char *string;
  uint16_t    token_kind;
  uint16_t    name_index;
};

#pragma once

#include <stdint.h>

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

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
  uint32_t      scope_id;
  unsigned      index;
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

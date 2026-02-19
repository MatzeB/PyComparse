#pragma once

#include <stdint.h>

#include "pycomparse/nullable.h"
#include "pycomparse/symbol_info_types.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct symbol {
  const char        *string;
  uint16_t           token_kind;
  struct symbol_info info;
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

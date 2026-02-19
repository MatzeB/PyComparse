#pragma once

#include <stdbool.h>
#include <stdio.h>

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct diagnostics_state {
  FILE       *out;
  const char *default_filename;
  bool        had_error;
  bool        in_diagnostic;
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

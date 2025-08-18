#pragma once

#include <stdbool.h>
#include <stdio.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct diagnostics_state {
  FILE       *out;
  const char *default_filename;
  bool        had_error;
  bool        in_diagnostic;
};

ASSUME_NONNULL_END

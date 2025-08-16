#pragma once

#include <stdbool.h>
#include <stdio.h>

struct diagnostics_state {
  FILE       *out;
  const char *default_filename;
  bool        had_error;
  bool        in_diagnostic;
};

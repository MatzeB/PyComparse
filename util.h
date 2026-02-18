#pragma once

#include <stdnoreturn.h>

#define PYCOMPARSE_FALLTHROUGH __attribute__((fallthrough))

noreturn void internal_error(const char *what);

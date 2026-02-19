#pragma once

#include <stdnoreturn.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PYCOMPARSE_FALLTHROUGH __attribute__((fallthrough))

noreturn void internal_error(const char *what);

#ifdef __cplusplus
}
#endif

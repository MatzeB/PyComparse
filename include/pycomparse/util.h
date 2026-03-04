#pragma once

#include <stdnoreturn.h>

#ifdef __cplusplus
extern "C" {
#endif

#define PYCOMPARSE_FALLTHROUGH __attribute__((fallthrough))
#define PYCOMPARSE_UNLIKELY(x) __builtin_expect((x), 0)

noreturn void internal_error(const char *what);

#ifdef __cplusplus
}
#endif

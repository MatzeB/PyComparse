#pragma once

#include <stdnoreturn.h>

noreturn void unimplemented(const char *what);
noreturn void internal_error(const char *what);

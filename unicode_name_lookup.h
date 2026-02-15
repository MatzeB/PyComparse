#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

bool unicode_name_lookup(const char *name, size_t name_len, uint32_t *out);

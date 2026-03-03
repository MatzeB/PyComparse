#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

/*
 * Diagnostics are stored as a stream of tagged elements in a byte buffer.
 * Each element is a uint8_t tag followed by the element's payload struct.
 */

enum diag_elem_tag {
  DIAG_ELEM_BEGIN,
  DIAG_ELEM_CSTR,
  DIAG_ELEM_SYMBOL,
  DIAG_ELEM_QUOTED_CHAR,
  DIAG_ELEM_TOKEN,
  DIAG_ELEM_END,
  DIAG_ELEM_INLINE_STR,
};

struct symbol;

struct diag_elem_begin {
  uint32_t    line; /* 0 == no location */
  const char *filename;
};

struct diag_elem_cstr {
  const char *str;
};

struct diag_elem_symbol {
  const struct symbol *symbol;
};

struct diag_elem_quoted_char {
  char c;
};

struct diag_elem_token {
  uint16_t    kind;
  const void *data;
};

/* Followed by `len` char bytes (no NUL). */
struct diag_elem_inline_str {
  uint8_t len;
};

struct diagnostics_state {
  char *nullable elems;
  unsigned       elems_size;
  unsigned       elems_capacity;
  const char    *default_filename;
  unsigned       num_errors;
  bool           in_diagnostic;
};

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

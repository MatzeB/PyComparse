#pragma once

#include <stddef.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef PYCOMPARSE_NO_ICONV
enum encoding_error_kind {
  ENCODING_ERROR_NONE = 0,
  ENCODING_ERROR_UNKNOWN_ENCODING,
  ENCODING_ERROR_BOM_COOKIE_MISMATCH,
  ENCODING_ERROR_DECODE_FAILED,
};

struct encoding_error {
  enum encoding_error_kind kind;
  char                     encoding[64];
};

struct encoding_error
encoding_maybe_transcode_to_utf8(FILE **input_io, FILE **owned_input_out,
                                 char **owned_source_out);

/* Transcode buf (length len) to UTF-8 if needed.
 * If transcoding is required, *owned_out is set to a malloc'd NUL-terminated
 * UTF-8 copy and *out_len to its length (excluding the NUL).  The caller owns
 * the buffer.  If the source is already UTF-8 (or empty), *owned_out is NULL
 * and *out_len is 0.  On error an encoding_error with a non-NONE kind is
 * returned; *owned_out is NULL in that case as well. */
struct encoding_error encoding_maybe_transcode_to_utf8_from_string(
    const char *buf, size_t len, char **owned_out, size_t *out_len);
#endif

#ifdef __cplusplus
}
#endif

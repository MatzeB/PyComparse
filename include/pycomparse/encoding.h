#pragma once

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
#endif

#ifdef __cplusplus
}
#endif

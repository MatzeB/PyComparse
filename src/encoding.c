#include "pycomparse/encoding.h"

#ifndef PYCOMPARSE_NO_ICONV
#include <ctype.h>
#include <errno.h>
#include <iconv.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#endif

#if defined(PYCOMPARSE_NO_ICONV)
typedef int encoding_no_iconv_translation_unit;
#else

struct source_encoding_info {
  bool has_utf8_bom;
  bool has_encoding_cookie;
  char encoding[64];
};

static inline unsigned char ascii_lower(unsigned char c)
{
  if (c >= 'A' && c <= 'Z') {
    return (unsigned char)(c + ('a' - 'A'));
  }
  return c;
}

static bool detect_pep263_encoding_in_line(const unsigned char *line,
                                           size_t length, char *encoding_out,
                                           size_t encoding_size)
{
  size_t i = 0;
  while (i < length
         && (line[i] == ' ' || line[i] == '\t' || line[i] == '\f')) {
    ++i;
  }
  if (i >= length || line[i] != '#') {
    return false;
  }

  while (i + 6 <= length) {
    if (ascii_lower(line[i]) == 'c' && ascii_lower(line[i + 1]) == 'o'
        && ascii_lower(line[i + 2]) == 'd' && ascii_lower(line[i + 3]) == 'i'
        && ascii_lower(line[i + 4]) == 'n'
        && ascii_lower(line[i + 5]) == 'g') {
      size_t j = i + 6;
      if (j >= length || (line[j] != ':' && line[j] != '=')) {
        ++i;
        continue;
      }
      ++j;
      while (j < length && (line[j] == ' ' || line[j] == '\t')) {
        ++j;
      }
      size_t start = j;
      while (j < length
             && (isalnum(line[j]) || line[j] == '-' || line[j] == '_'
                 || line[j] == '.')) {
        ++j;
      }
      if (j == start) {
        return false;
      }
      size_t name_len = j - start;
      if (name_len >= encoding_size) {
        name_len = encoding_size - 1;
      }
      memcpy(encoding_out, line + start, name_len);
      encoding_out[name_len] = '\0';
      return true;
    }
    ++i;
  }

  return false;
}

static struct encoding_error make_encoding_error(enum encoding_error_kind kind,
                                                 const char *encoding)
{
  struct encoding_error result = { kind, { 0 } };
  if (encoding != NULL) {
    strncpy(result.encoding, encoding, sizeof(result.encoding));
    result.encoding[sizeof(result.encoding) - 1] = '\0';
  }
  return result;
}

static void detect_source_encoding(const unsigned char         *prefix,
                                   size_t                       prefix_size,
                                   struct source_encoding_info *result)
{
  memset(result, 0, sizeof(*result));
  strncpy(result->encoding, "utf-8", sizeof(result->encoding));
  result->encoding[sizeof(result->encoding) - 1] = '\0';

  size_t offset = 0;
  if (prefix_size >= 3 && prefix[0] == 0xEF && prefix[1] == 0xBB
      && prefix[2] == 0xBF) {
    offset = 3;
    result->has_utf8_bom = true;
  }

  size_t line_start = offset;
  for (unsigned line_no = 0; line_no < 2 && line_start < prefix_size;
       ++line_no) {
    size_t line_end = line_start;
    while (line_end < prefix_size && prefix[line_end] != '\n') {
      ++line_end;
    }
    size_t line_length = line_end - line_start;
    if (line_length > 0 && prefix[line_start + line_length - 1] == '\r') {
      --line_length;
    }
    if (detect_pep263_encoding_in_line(prefix + line_start, line_length,
                                       result->encoding,
                                       sizeof(result->encoding))) {
      result->has_encoding_cookie = true;
      return;
    }
    if (line_end >= prefix_size) {
      break;
    }
    line_start = line_end + 1;
  }
}

static bool encoding_is_utf8(const char *encoding)
{
  char     normalized[32];
  unsigned n = 0;
  for (const unsigned char *p = (const unsigned char *)encoding;
       *p != '\0' && n < sizeof(normalized) - 1; ++p) {
    if (*p == '-' || *p == '_') {
      continue;
    }
    normalized[n++] = (char)ascii_lower(*p);
  }
  normalized[n] = '\0';
  return strcmp(normalized, "utf8") == 0;
}

static bool encoding_is_bom_compatible_utf8(const char *encoding)
{
  char     lower[32];
  unsigned n = 0;
  for (const unsigned char *p = (const unsigned char *)encoding;
       *p != '\0' && n < sizeof(lower) - 1; ++p) {
    lower[n++] = (char)ascii_lower(*p);
  }
  lower[n] = '\0';
  return strcmp(lower, "utf-8") == 0;
}

static void normalize_encoding_alias(const char *encoding, char *out,
                                     size_t out_size)
{
  unsigned n = 0;
  for (const unsigned char *p = (const unsigned char *)encoding;
       *p != '\0' && n + 1 < out_size; ++p) {
    if (*p == '-' || *p == '_' || *p == '.') {
      continue;
    }
    out[n++] = (char)ascii_lower(*p);
  }
  out[n] = '\0';
}

static iconv_t open_utf8_converter(const char *encoding)
{
  iconv_t converter = iconv_open("UTF-8", encoding);
  if (converter != (iconv_t)-1) {
    return converter;
  }

  char normalized[64];
  normalize_encoding_alias(encoding, normalized, sizeof(normalized));
  if (strcmp(normalized, "latin1") == 0
      || strcmp(normalized, "iso88591") == 0) {
    return iconv_open("UTF-8", "ISO-8859-1");
  }

  return (iconv_t)-1;
}

static unsigned char *read_all_bytes(FILE *input, size_t *raw_size)
{
  unsigned char *raw = NULL;
  size_t         size = 0;
  size_t         capacity = 0;
  for (;;) {
    if (size == capacity) {
      size_t         new_capacity = capacity == 0 ? 4096 : capacity * 2;
      unsigned char *new_raw = realloc(raw, new_capacity);
      if (new_raw == NULL) {
        free(raw);
        return NULL;
      }
      raw = new_raw;
      capacity = new_capacity;
    }

    size_t read_size = fread(raw + size, 1, capacity - size, input);
    size += read_size;
    if (read_size > 0) {
      continue;
    }
    if (feof(input)) {
      break;
    }
    if (ferror(input)) {
      free(raw);
      return NULL;
    }
  }

  *raw_size = size;
  return raw;
}

struct encoding_error encoding_maybe_transcode_to_utf8(FILE **input_io,
                                                       FILE **owned_input_out,
                                                       char **owned_source_out)
{
  struct encoding_error no_error = { ENCODING_ERROR_NONE, { 0 } };
  FILE                 *input = *input_io;
  if (input == NULL) {
    return no_error;
  }

  unsigned char prefix[1024];
  size_t        prefix_size = fread(prefix, 1, sizeof(prefix), input);
  if (ferror(input)) {
    clearerr(input);
    (void)fseek(input, 0, SEEK_SET);
    return no_error;
  }
  if (fseek(input, 0, SEEK_SET) != 0) {
    return no_error;
  }

  struct source_encoding_info source_encoding;
  detect_source_encoding(prefix, prefix_size, &source_encoding);
  if (source_encoding.has_utf8_bom && source_encoding.has_encoding_cookie
      && !encoding_is_bom_compatible_utf8(source_encoding.encoding)) {
    return make_encoding_error(ENCODING_ERROR_BOM_COOKIE_MISMATCH,
                               source_encoding.encoding);
  }
  if (encoding_is_utf8(source_encoding.encoding)) {
    return no_error;
  }

  size_t         raw_size = 0;
  unsigned char *raw = read_all_bytes(input, &raw_size);
  if (raw == NULL) {
    if (ferror(input)) {
      clearerr(input);
      (void)fseek(input, 0, SEEK_SET);
    }
    return no_error;
  }
  (void)fseek(input, 0, SEEK_SET);

  iconv_t converter = open_utf8_converter(source_encoding.encoding);
  if (converter == (iconv_t)-1) {
    free(raw);
    return make_encoding_error(ENCODING_ERROR_UNKNOWN_ENCODING,
                               source_encoding.encoding);
  }

  size_t out_capacity = raw_size * 4 + 16;
  char  *out = malloc(out_capacity);
  if (out == NULL) {
    iconv_close(converter);
    free(raw);
    return no_error;
  }

  char  *in_ptr = (char *)raw;
  size_t in_left = raw_size;
  char  *out_ptr = out;
  size_t out_left = out_capacity;
  while (in_left > 0) {
    size_t rc = iconv(converter, &in_ptr, &in_left, &out_ptr, &out_left);
    if (rc != (size_t)-1) {
      continue;
    }
    if (errno == E2BIG) {
      size_t used = out_capacity - out_left;
      size_t new_capacity = out_capacity * 2;
      char  *new_out = realloc(out, new_capacity);
      if (new_out == NULL) {
        iconv_close(converter);
        free(raw);
        free(out);
        return no_error;
      }
      out = new_out;
      out_capacity = new_capacity;
      out_ptr = out + used;
      out_left = out_capacity - used;
      continue;
    }
    iconv_close(converter);
    free(raw);
    free(out);
    return make_encoding_error(ENCODING_ERROR_DECODE_FAILED,
                               source_encoding.encoding);
  }
  iconv_close(converter);
  free(raw);

  size_t out_size = out_capacity - out_left;
  FILE  *memory_input = fmemopen(out, out_size, "rb");
  if (memory_input == NULL) {
    free(out);
    return no_error;
  }

  *input_io = memory_input;
  *owned_input_out = memory_input;
  *owned_source_out = out;
  return no_error;
}

struct encoding_error
encoding_maybe_transcode_to_utf8_from_string(const char *buf, size_t len,
                                             char **owned_out, size_t *out_len)
{
  struct encoding_error no_error = { ENCODING_ERROR_NONE, { 0 } };
  *owned_out = NULL;
  *out_len = 0;

  struct source_encoding_info source_encoding;
  detect_source_encoding((const unsigned char *)buf, len, &source_encoding);
  if (source_encoding.has_utf8_bom && source_encoding.has_encoding_cookie
      && !encoding_is_bom_compatible_utf8(source_encoding.encoding)) {
    return make_encoding_error(ENCODING_ERROR_BOM_COOKIE_MISMATCH,
                               source_encoding.encoding);
  }
  if (encoding_is_utf8(source_encoding.encoding)) {
    return no_error;
  }

  iconv_t converter = open_utf8_converter(source_encoding.encoding);
  if (converter == (iconv_t)-1) {
    return make_encoding_error(ENCODING_ERROR_UNKNOWN_ENCODING,
                               source_encoding.encoding);
  }

  size_t out_capacity = len * 4 + 16;
  char  *out = malloc(out_capacity + 1); /* +1 for NUL terminator */
  if (out == NULL) {
    iconv_close(converter);
    return no_error;
  }

  char  *in_ptr = (char *)buf; /* iconv takes non-const; cast is safe */
  size_t in_left = len;
  char  *out_ptr = out;
  size_t out_left = out_capacity;
  while (in_left > 0) {
    size_t rc = iconv(converter, &in_ptr, &in_left, &out_ptr, &out_left);
    if (rc != (size_t)-1) {
      continue;
    }
    if (errno == E2BIG) {
      size_t used = out_capacity - out_left;
      size_t new_capacity = out_capacity * 2;
      char  *new_out = realloc(out, new_capacity + 1);
      if (new_out == NULL) {
        iconv_close(converter);
        free(out);
        return no_error;
      }
      out = new_out;
      out_capacity = new_capacity;
      out_ptr = out + used;
      out_left = out_capacity - used;
      continue;
    }
    iconv_close(converter);
    free(out);
    return make_encoding_error(ENCODING_ERROR_DECODE_FAILED,
                               source_encoding.encoding);
  }
  iconv_close(converter);

  size_t result_len = out_capacity - out_left;
  out[result_len] = '\0';
  *owned_out = out;
  *out_len = result_len;
  return no_error;
}
#endif

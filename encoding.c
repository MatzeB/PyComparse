#include "encoding.h"

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

static void detect_source_encoding(const unsigned char *prefix,
                                   size_t prefix_size, char *encoding_out,
                                   size_t encoding_size)
{
  strncpy(encoding_out, "utf-8", encoding_size);
  encoding_out[encoding_size - 1] = '\0';

  size_t offset = 0;
  if (prefix_size >= 3 && prefix[0] == 0xEF && prefix[1] == 0xBB
      && prefix[2] == 0xBF) {
    offset = 3;
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
                                       encoding_out, encoding_size)) {
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

void encoding_maybe_transcode_to_utf8(FILE **input_io, FILE **owned_input_out,
                                      char **owned_source_out)
{
  FILE *input = *input_io;
  if (input == NULL) {
    return;
  }

  unsigned char prefix[1024];
  size_t        prefix_size = fread(prefix, 1, sizeof(prefix), input);
  if (ferror(input)) {
    clearerr(input);
    (void)fseek(input, 0, SEEK_SET);
    return;
  }
  if (fseek(input, 0, SEEK_SET) != 0) {
    return;
  }

  char encoding[64];
  detect_source_encoding(prefix, prefix_size, encoding, sizeof(encoding));
  if (encoding_is_utf8(encoding)) {
    return;
  }

  size_t         raw_size = 0;
  unsigned char *raw = read_all_bytes(input, &raw_size);
  if (raw == NULL) {
    if (ferror(input)) {
      clearerr(input);
      (void)fseek(input, 0, SEEK_SET);
    }
    return;
  }
  (void)fseek(input, 0, SEEK_SET);

  iconv_t converter = iconv_open("UTF-8", encoding);
  if (converter == (iconv_t)-1) {
    free(raw);
    return;
  }

  size_t out_capacity = raw_size * 4 + 16;
  char  *out = malloc(out_capacity);
  if (out == NULL) {
    iconv_close(converter);
    free(raw);
    return;
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
        return;
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
    return;
  }
  iconv_close(converter);
  free(raw);

  size_t out_size = out_capacity - out_left;
  FILE  *memory_input = fmemopen(out, out_size, "rb");
  if (memory_input == NULL) {
    free(out);
    return;
  }

  *input_io = memory_input;
  *owned_input_out = memory_input;
  *owned_source_out = out;
}
#endif

#include "pycomparse/scanner.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#if (defined(__x86_64__) || defined(__i386__))                                \
    && (defined(__clang__) || defined(__GNUC__))
#define PYCOMPARSE_HAVE_X86_AVX2 1
#include <immintrin.h>
#else
#define PYCOMPARSE_HAVE_X86_AVX2 0
#endif

#if PYCOMPARSE_HAVE_X86_AVX2 && defined(__ELF__)
#if defined(__has_attribute)
#if __has_attribute(ifunc)
#define PYCOMPARSE_HAVE_GNU_IFUNC 1
#else
#define PYCOMPARSE_HAVE_GNU_IFUNC 0
#endif
#else
#define PYCOMPARSE_HAVE_GNU_IFUNC 1
#endif
#else
#define PYCOMPARSE_HAVE_GNU_IFUNC 0
#endif

#include "pycomparse/adt/arena.h"
#include "pycomparse/adt/dynmemory.h"
#include "pycomparse/diagnostics.h"
#include "pycomparse/encoding.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/object_types.h"
#include "pycomparse/scanner_types.h"
#include "pycomparse/symbol_table.h"
#include "pycomparse/symbol_table_types.h"
#include "pycomparse/symbol_types.h"
#include "pycomparse/token_kinds.h"
#include "pycomparse/unicode_name_lookup.h"
#include "pycomparse/unicode_nfkc_table.h"
#include "pycomparse/unicode_xid_table.h"
#include "pycomparse/util.h"

#define UNLIKELY(x) __builtin_expect((x), 0)

#define IDENTIFIER_START_CASES_WITHOUT_B_F_R_U                                \
  'A' : case 'C':                                                             \
  case 'D':                                                                   \
  case 'E':                                                                   \
  case 'G':                                                                   \
  case 'H':                                                                   \
  case 'I':                                                                   \
  case 'J':                                                                   \
  case 'K':                                                                   \
  case 'L':                                                                   \
  case 'M':                                                                   \
  case 'N':                                                                   \
  case 'O':                                                                   \
  case 'P':                                                                   \
  case 'Q':                                                                   \
  case 'S':                                                                   \
  case 'T':                                                                   \
  case 'V':                                                                   \
  case 'W':                                                                   \
  case 'X':                                                                   \
  case 'Y':                                                                   \
  case 'Z':                                                                   \
  case '_':                                                                   \
  case 'a':                                                                   \
  case 'c':                                                                   \
  case 'd':                                                                   \
  case 'e':                                                                   \
  case 'g':                                                                   \
  case 'h':                                                                   \
  case 'i':                                                                   \
  case 'j':                                                                   \
  case 'k':                                                                   \
  case 'l':                                                                   \
  case 'm':                                                                   \
  case 'n':                                                                   \
  case 'o':                                                                   \
  case 'p':                                                                   \
  case 'q':                                                                   \
  case 's':                                                                   \
  case 't':                                                                   \
  case 'v':                                                                   \
  case 'w':                                                                   \
  case 'x':                                                                   \
  case 'y':                                                                   \
  case 'z'

#define DIGIT_CASES                                                           \
  '0' : case '1':                                                             \
  case '2':                                                                   \
  case '3':                                                                   \
  case '4':                                                                   \
  case '5':                                                                   \
  case '6':                                                                   \
  case '7':                                                                   \
  case '8':                                                                   \
  case '9'

#define HEX_DIGIT_CASES                                                       \
  DIGIT_CASES:                                                                \
  case 'a':                                                                   \
  case 'b':                                                                   \
  case 'c':                                                                   \
  case 'd':                                                                   \
  case 'e':                                                                   \
  case 'f':                                                                   \
  case 'A':                                                                   \
  case 'B':                                                                   \
  case 'C':                                                                   \
  case 'D':                                                                   \
  case 'E':                                                                   \
  case 'F'

#define IDENTIFIER_CASES                                                      \
  DIGIT_CASES:                                                                \
  case IDENTIFIER_START_CASES_WITHOUT_B_F_R_U:                                \
  case 'B':                                                                   \
  case 'F':                                                                   \
  case 'R':                                                                   \
  case 'U':                                                                   \
  case 'b':                                                                   \
  case 'f':                                                                   \
  case 'r':                                                                   \
  case 'u'

struct scan_string_flags {
  uint8_t unicode : 1;
  uint8_t raw : 1;
  uint8_t format : 1;
  uint8_t continue_format : 1;
  uint8_t format_spec : 1;
};

static const unsigned TABSIZE = 8;
static const int      C_EOF = -1;
#if defined(NDEBUG)
static const size_t scanner_read_buffer_size = 32 * 1024 - 16;
#else
/* Small odd-sized buffer in debug builds to exercise refill edge cases. */
static const size_t scanner_read_buffer_size = 123;
#endif

// TODO: Measure if the faster string operations on aligned addresses make up
// for less dense packing...
static const unsigned string_alignment = alignof(void *);

struct scan_decimal_integer_result {
  bool had_error;
  bool leading_zero_nonzero;
};

/* Refill the read buffer from the input file.  The buffer is a fixed
 * allocation (s->read_buffer) that is reused across refills, so pointers
 * into it remain valid after a refill -- only the data changes.  This
 * property is relied upon by lookahead/rollback code (scan_starts_exponent_
 * part, pushback_char) that restores s->p via pointer arithmetic. */
static int __attribute__((noinline)) refill_buffer(struct scanner_state *s)
{
  assert(s->c != C_EOF && "not allowed to advance past EOF");
  if (s->input == NULL) {
    return C_EOF;
  }
  if (s->fstring_debug.depth > 0) {
    assert(s->fstring_debug.tail_start != NULL);
    assert(s->fstring_debug.tail_start <= s->buffer_end);
    size_t flush_size = (size_t)(s->buffer_end - s->fstring_debug.tail_start);
    if (flush_size > 0) {
      unsigned flush_u = (unsigned)flush_size;
      assert((size_t)flush_u == flush_size);
      unsigned new_size = s->fstring_debug.spilled_size + flush_u;
      if (new_size < s->fstring_debug.spilled_size) {
        internal_error("scanner: f-string debug capture overflow");
      }
      if (new_size > s->fstring_debug.spilled_capacity) {
        s->fstring_debug.spilled_prefix = dynmemory_grow(
            s->fstring_debug.spilled_prefix,
            &s->fstring_debug.spilled_capacity, new_size, sizeof(char));
      }
      char *dst
          = s->fstring_debug.spilled_prefix + s->fstring_debug.spilled_size;
      memcpy(dst, s->fstring_debug.tail_start, flush_size);
      s->fstring_debug.spilled_size = new_size;
    }
    s->fstring_debug.tail_start = s->buffer_end;
  }
  size_t read_size = fread(s->read_buffer, 1, s->read_buffer_size, s->input);
  if (ferror(s->input)) {
    s->read_error_seen = true;
  }
  if (read_size < s->read_buffer_size) {
    if (read_size == 0) {
      if (s->read_error_seen) {
        s->read_error_token_pending = true;
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "failed to read source");
        diag_end(s->d);
      }
      return C_EOF;
    }
  }
  const char *start = s->read_buffer;
  s->p = start + 1;
  s->buffer_end = start + read_size;
  if (s->fstring_debug.depth > 0) {
    s->fstring_debug.tail_start = start;
  }
  return (unsigned char)*start;
}

static void next_char(struct scanner_state *s)
{
  s->c = UNLIKELY(s->p >= s->buffer_end) ? refill_buffer(s)
                                         : (unsigned char)*(s->p++);
}

static void eat_char(struct scanner_state *s, int c)
{
  (void)c;
  assert(s->c == c);
  next_char(s);
}

static void pushback_char(struct scanner_state *s, int c)
{
  assert(c != C_EOF);
  assert(s->c == c);
  assert(s->p != NULL);
  assert(s->buffer_end != NULL);
  assert(s->input == NULL
         || (s->read_buffer != NULL && s->p > s->read_buffer));
  assert(s->p <= s->buffer_end);
  assert((unsigned char)s->p[-1] == (unsigned char)c);
  --s->p;
  s->c = c;
}

static inline const char *nullable
scanner_current_char_ptr(struct scanner_state *s)
{
  if (s->c == C_EOF || s->p == NULL || s->buffer_end == NULL) {
    return NULL;
  }
  if (s->input != NULL && s->read_buffer == NULL) {
    return NULL;
  }
  if ((s->input == NULL || s->p > s->read_buffer) && s->p[-1] == (char)s->c) {
    return s->p - 1;
  }
  if (s->p < s->buffer_end && s->p[0] == (char)s->c) {
    return s->p;
  }
  return NULL;
}

static inline void scanner_set_current_char_ptr(struct scanner_state *s,
                                                const char           *p)
{
  assert(p < s->buffer_end);
  s->c = (unsigned char)*p;
  s->p = p + 1;
}

/* Decode a UTF-8 codepoint starting from the lead byte already in s->c.
 * Consumes the continuation bytes via next_char(). After a successful
 * decode, s->c holds the last continuation byte; the caller must call
 * next_char() to advance past it.
 *
 * On success: writes decoded codepoint to *out, writes raw UTF-8 bytes
 *             to bytes_out[0..return-1], returns byte count (2-4).
 * On failure: returns 0. s->c may point at an unconsumed byte. */
static int decode_utf8(struct scanner_state *s, uint32_t *out,
                       char bytes_out[4])
{
  unsigned int lead = (unsigned int)s->c;
  uint32_t     cp;
  int          n; /* expected continuation bytes */

  if (lead < 0xC2) {
    /* 0x80-0xBF are continuation bytes (not valid as lead).
     * 0xC0-0xC1 are overlong 2-byte encodings. */
    return 0;
  } else if (lead < 0xE0) {
    cp = lead & 0x1F;
    n = 1;
  } else if (lead < 0xF0) {
    cp = lead & 0x0F;
    n = 2;
  } else if (lead <= 0xF4) {
    cp = lead & 0x07;
    n = 3;
  } else {
    /* 0xF5-0xFF cannot appear in valid UTF-8 */
    return 0;
  }

  bytes_out[0] = (char)lead;

  for (int i = 0; i < n; i++) {
    next_char(s);
    if ((s->c & 0xC0) != 0x80) {
      /* Not a continuation byte. Don't consume it. */
      return 0;
    }
    bytes_out[i + 1] = (char)s->c;
    cp = (cp << 6) | (s->c & 0x3F);
  }

  /* Reject surrogates (U+D800..U+DFFF) and out-of-range */
  if (cp >= 0xD800 && cp <= 0xDFFF) return 0;
  if (cp > 0x10FFFF) return 0;

  /* Reject overlong 3-byte encodings (< U+0800) */
  if (n == 2 && cp < 0x0800) return 0;
  /* Reject overlong 4-byte encodings (< U+10000) */
  if (n == 3 && cp < 0x10000) return 0;

  *out = cp;
  return n + 1;
}

static void scan_skip_inline_spaces(struct scanner_state *s)
{
  for (;;) {
    if (s->c != ' ' && s->c != '\t' && s->c != '\014') {
      return;
    }

    const char *p = scanner_current_char_ptr(s);
    if (p == NULL) {
      next_char(s);
      continue;
    }
    while (p < s->buffer_end && (*p == ' ' || *p == '\t' || *p == '\014')) {
      ++p;
    }
    if (p < s->buffer_end) {
      scanner_set_current_char_ptr(s, p);
      return;
    }

    s->p = s->buffer_end;
    next_char(s);
  }
}

static void scan_indentation_spaces(struct scanner_state *s, unsigned *column)
{
  for (;;) {
    if (s->c != ' ') {
      return;
    }

    const char *p = scanner_current_char_ptr(s);
    if (p == NULL) {
      next_char(s);
      ++*column;
      continue;
    }
    const char *begin = p;
    while (p < s->buffer_end && *p == ' ') {
      ++p;
    }
    *column += (unsigned)(p - begin);
    if (p < s->buffer_end) {
      scanner_set_current_char_ptr(s, p);
      return;
    }

    s->p = s->buffer_end;
    next_char(s);
  }
}

static void arena_grow_utf8_codepoint(struct arena *strings,
                                      uint32_t      codepoint)
{
  if (codepoint <= 0x7F) {
    arena_grow_char(strings, (char)codepoint);
  } else if (codepoint <= 0x7FF) {
    arena_grow_char(strings, (char)(0xC0 | (codepoint >> 6)));
    arena_grow_char(strings, (char)(0x80 | (codepoint & 0x3F)));
  } else if (codepoint <= 0xFFFF) {
    arena_grow_char(strings, (char)(0xE0 | (codepoint >> 12)));
    arena_grow_char(strings, (char)(0x80 | ((codepoint >> 6) & 0x3F)));
    arena_grow_char(strings, (char)(0x80 | (codepoint & 0x3F)));
  } else if (codepoint <= 0x10FFFF) {
    arena_grow_char(strings, (char)(0xF0 | (codepoint >> 18)));
    arena_grow_char(strings, (char)(0x80 | ((codepoint >> 12) & 0x3F)));
    arena_grow_char(strings, (char)(0x80 | ((codepoint >> 6) & 0x3F)));
    arena_grow_char(strings, (char)(0x80 | (codepoint & 0x3F)));
  }
}

static void scan_line_comment(struct scanner_state *s)
{
  eat_char(s, '#');
  for (;;) {
    switch (s->c) {
    case '\r':
      next_char(s);
      if (s->c == '\n') next_char(s);
      goto new_line;
    case '\n':
      next_char(s);
      /* fallthrough */
    new_line:
      return;
    case C_EOF:
      return;
    default:
      next_char(s);
      continue;
    }
  }
}

/* Scan the continuation of an identifier. The arena grow must already be
 * started and the first character(s) already written.
 * `has_non_ascii` indicates whether non-ASCII bytes have already been seen
 * (e.g. from the identifier start character). */
static void scan_identifier_continue(struct scanner_state *s,
                                     struct arena *arena, bool has_non_ascii)
{
  for (;;) {
    switch (s->c) {
    case IDENTIFIER_CASES:
      arena_grow_char(arena, (char)s->c);
      next_char(s);
      continue;
    default:
      if (s->c >= 128) {
        uint32_t cp;
        char     utf8[4];
        int      len = decode_utf8(s, &cp, utf8);
        if (len && is_xid_continue(cp)) {
          has_non_ascii = true;
          for (int i = 0; i < len; i++)
            arena_grow_char(arena, utf8[i]);
          next_char(s);
          continue;
        }
        /* Not XID_Continue or invalid UTF-8. End the identifier.
         * If decode consumed continuation bytes, s->c holds the byte
         * after the sequence; if it failed immediately, s->c is the
         * same lead byte. Either way, don't consume — the main
         * dispatcher will handle it as a new token. */
        break;
      }
      break;
    }
    break;
  }

  /* NFKC normalize non-ASCII identifiers (PEP 3131). */
  if (has_non_ascii) {
    unsigned raw_len = arena_grow_current_size(arena);
    char    *raw = arena_grow_current_base(arena);
    int nfkc_len = nfkc_normalize((uint8_t *)raw, (int)raw_len, (int)raw_len);
    if (nfkc_len >= 0) {
      /* NFKC result fits (it's always <= original length for identifiers).
       * Truncate the arena grow region to the normalized length. */
      arena_grow_truncate(arena, (unsigned)nfkc_len);
    }
    /* If nfkc_normalize returns -1 (shouldn't happen for valid identifiers),
     * fall through and use the raw string. */
  }

  arena_grow_char(arena, '\0');
  char          *string = arena_grow_finish(arena);
  struct symbol *symbol = symbol_table_get_or_insert(s->symbol_table, string);
  if (symbol->string != string) {
    arena_free_to(arena, string);
  }

  s->token.kind = symbol->token_kind;
  s->token.u.symbol = symbol;
}

static void scan_identifier(struct scanner_state *s, char first_char,
                            char second_char)
{
  struct arena *arena = &s->symbol_table->arena;
  arena_grow_begin(arena, string_alignment);
  arena_grow_char(arena, first_char);
  if (second_char != 0) arena_grow_char(arena, second_char);
  scan_identifier_continue(s, arena, /*has_non_ascii=*/false);
}

struct bigint_accum {
  uint16_t *nullable digits;
  uint32_t           length;
  uint32_t           capacity;
};

#define FAST_INT_MAX ((uint64_t)INT64_MAX)

static void bigint_accum_free(struct bigint_accum *accum)
{
  free(accum->digits);
  accum->digits = NULL;
  accum->length = 0;
  accum->capacity = 0;
}

static void bigint_accum_reserve(struct bigint_accum *accum, uint32_t minimum)
{
  if (minimum <= accum->capacity) return;
  uint32_t new_capacity = accum->capacity > 0 ? accum->capacity : 16;
  while (new_capacity < minimum) {
    if (new_capacity > UINT32_MAX / 2) {
      new_capacity = minimum;
      break;
    }
    new_capacity *= 2;
  }
  uint16_t *new_digits
      = realloc(accum->digits, (size_t)new_capacity * sizeof(*new_digits));
  if (new_digits == NULL) {
    internal_error("out of memory");
  }
  accum->digits = new_digits;
  accum->capacity = new_capacity;
}

static void bigint_accum_append_digit(struct bigint_accum *accum,
                                      uint16_t             digit)
{
  if (accum->length >= accum->capacity) {
    bigint_accum_reserve(accum, accum->length + 1);
  }
  accum->digits[accum->length++] = digit;
}

static void bigint_accum_init_from_u64(struct bigint_accum *accum,
                                       uint64_t             value)
{
  while (value != 0) {
    bigint_accum_append_digit(accum, (uint16_t)(value & 0x7fff));
    value >>= 15;
  }
}

static void bigint_accum_mul_add(struct bigint_accum *accum, uint32_t mul,
                                 uint32_t add)
{
  assert(mul <= 16);
  uint32_t carry = add;
  for (uint32_t i = 0; i < accum->length; ++i) {
    uint32_t tmp = (uint32_t)accum->digits[i] * mul + carry;
    accum->digits[i] = (uint16_t)(tmp & 0x7fff);
    carry = tmp >> 15;
  }
  while (carry != 0) {
    bigint_accum_append_digit(accum, (uint16_t)(carry & 0x7fff));
    carry >>= 15;
  }
}

static union object *
bigint_accum_intern(struct scanner_state              *s,
                    const struct bigint_accum *nonnull accum)
{
  uint32_t length = accum->length;
  while (length > 0 && accum->digits[length - 1] == 0) {
    --length;
  }
  if (length == 0) {
    return object_intern_int(s->objects, 0);
  }
  return object_intern_big_int(s->objects, length, accum->digits);
}

static bool end_number_literal(struct scanner_state *s, uint64_t value,
                               const struct bigint_accum *nullable big_value,
                               char last, bool *had_error,
                               const char *literal_kind)
{
  if ((('a' <= s->c && s->c <= 'z') || ('A' <= s->c && s->c <= 'Z')
       || ('0' <= s->c && s->c <= '9'))) {
    if (!*had_error) {
      diag_begin_error(s->d, scanner_location(s));
      diag_frag(s->d, "invalid digit for ");
      diag_frag(s->d, literal_kind);
      diag_frag(s->d, " literal: ");
      diag_quoted_char(s->d, (char)s->c);
      diag_end(s->d);
      *had_error = true;
    }
    return true;
  }
  if (last == '_' && !*had_error) {
    diag_begin_error(s->d, scanner_location(s));
    diag_frag(s->d, literal_kind);
    diag_frag(s->d, " literal cannot end with ");
    diag_quoted_char(s->d, '_');
    diag_end(s->d);
    *had_error = true;
  }
  if (last == 0 && !*had_error) {
    diag_begin_error(s->d, scanner_location(s));
    diag_frag(s->d, literal_kind);
    diag_frag(s->d, " literal had no digits");
    diag_end(s->d);
    *had_error = true;
  }
  if (big_value != NULL && big_value->length > 0) {
    s->token.u.object = bigint_accum_intern(s, big_value);
  } else if (value <= FAST_INT_MAX) {
    s->token.u.object = object_intern_int(s->objects, (int64_t)value);
  } else {
    struct bigint_accum value_as_bigint = { 0 };
    bigint_accum_init_from_u64(&value_as_bigint, value);
    s->token.u.object = bigint_accum_intern(s, &value_as_bigint);
    bigint_accum_free(&value_as_bigint);
  }
  s->token.kind = T_INTEGER;
  return false;
}

static void scan_hexadecimal_integer(struct scanner_state *s)
{
  uint64_t            value = 0;
  bool                had_error = false;
  bool                use_big_value = false;
  char                last = 0;
  struct bigint_accum big_value = { 0 };
  for (;; last = s->c) {
    next_char(s);
    uint32_t digit_value;
    switch (s->c) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      digit_value = s->c - '0';
      break;
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
      digit_value = (s->c - 'a') + 10;
      break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
      digit_value = (s->c - 'A') + 10;
      break;
    case '_':
      if (last == '_' && !had_error) {
        diag_begin_error(s->d, scanner_location(s));
        if (last == '_') {
          diag_frag(s->d, "hexadecimal literal cannot have consecutive ");
          diag_quoted_char(s->d, '_');
        }
        diag_end(s->d);
        had_error = true;
      }
      continue;
    default:
      if (end_number_literal(s, value, use_big_value ? &big_value : NULL, last,
                             &had_error, "hexadecimal")) {
        continue;
      }
      bigint_accum_free(&big_value);
      return;
    }
    if (use_big_value) {
      bigint_accum_mul_add(&big_value, 16, digit_value);
      continue;
    }
    if (value > ((FAST_INT_MAX - digit_value) >> 4)) {
      use_big_value = true;
      bigint_accum_init_from_u64(&big_value, value);
      bigint_accum_mul_add(&big_value, 16, digit_value);
      continue;
    }
    value = (value << 4) | digit_value;
  }
}

static void scan_octal_integer(struct scanner_state *s)
{
  uint64_t            value = 0;
  bool                had_error = false;
  bool                use_big_value = false;
  char                last = 0;
  struct bigint_accum big_value = { 0 };
  for (;; last = s->c) {
    next_char(s);
    uint64_t digit_value;
    switch (s->c) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      digit_value = s->c - '0';
      break;
    case '_':
      if (last == '_' && !had_error) {
        diag_begin_error(s->d, scanner_location(s));
        if (last == '_') {
          diag_frag(s->d, "octal literal cannot have consecutive ");
          diag_quoted_char(s->d, '_');
        }
        diag_end(s->d);
        had_error = true;
      }
      continue;
    default:
      if (end_number_literal(s, value, use_big_value ? &big_value : NULL, last,
                             &had_error, "octal")) {
        continue;
      }
      bigint_accum_free(&big_value);
      return;
    }
    if (use_big_value) {
      bigint_accum_mul_add(&big_value, 8, (uint32_t)digit_value);
      continue;
    }
    if (value > ((FAST_INT_MAX - digit_value) >> 3)) {
      use_big_value = true;
      bigint_accum_init_from_u64(&big_value, value);
      bigint_accum_mul_add(&big_value, 8, (uint32_t)digit_value);
      continue;
    }
    value = (value << 3) | digit_value;
  }
}

static void scan_binary_integer(struct scanner_state *s)
{
  uint64_t            value = 0;
  bool                had_error = false;
  bool                use_big_value = false;
  char                last = 0;
  struct bigint_accum big_value = { 0 };
  for (;; last = s->c) {
    next_char(s);
    uint32_t digit_value;
    switch (s->c) {
    case '0':
    case '1':
      digit_value = s->c - '0';
      break;
    case '_':
      if (last == '_' && !had_error) {
        diag_begin_error(s->d, scanner_location(s));
        if (last == '_') {
          diag_frag(s->d, "binary literal cannot have consecutive ");
          diag_quoted_char(s->d, '_');
        }
        diag_end(s->d);
        had_error = true;
      }
      continue;
    default:
      if (end_number_literal(s, value, use_big_value ? &big_value : NULL, last,
                             &had_error, "binary")) {
        continue;
      }
      bigint_accum_free(&big_value);
      return;
    }
    if (use_big_value) {
      bigint_accum_mul_add(&big_value, 2, digit_value);
      continue;
    }
    if (value > ((FAST_INT_MAX - digit_value) >> 1)) {
      use_big_value = true;
      bigint_accum_init_from_u64(&big_value, value);
      bigint_accum_mul_add(&big_value, 2, digit_value);
      continue;
    }
    value = (value << 1) | digit_value;
  }
}

static struct scan_decimal_integer_result
scan_decimal_integer(struct scanner_state *s, struct arena *arena, char first)
{
  struct scan_decimal_integer_result result = { 0 };
  arena_grow_char(arena, first);
  char last = first;
  for (;; last = s->c, next_char(s)) {
    switch (s->c) {
    case '0':
      break;
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      if (first == '0') {
        result.leading_zero_nonzero = true;
      }
      break;
    case '_':
      if (last == '_' && !result.had_error) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "decimal literal cannot have consecutive ");
        diag_quoted_char(s->d, '_');
        diag_end(s->d);
        result.had_error = true;
      }
      continue;
    default:
      if ((('a' <= s->c && s->c <= 'z') || ('A' <= s->c && s->c <= 'Z')
           || ('0' <= s->c && s->c <= '9'))
          && s->c != 'e' && s->c != 'E' && s->c != 'j' && s->c != 'J') {
        if (!result.had_error) {
          diag_begin_error(s->d, scanner_location(s));
          diag_frag(s->d, "invalid digit for decimal literal: ");
          diag_quoted_char(s->d, (char)s->c);
          diag_end(s->d);
          result.had_error = true;
        }
        continue;
      }
      if (last == '_' && !result.had_error) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "decimal literal cannot end with ");
        diag_quoted_char(s->d, '_');
        diag_end(s->d);
        result.had_error = true;
      }
      return result;
    }
    arena_grow_char(arena, (char)s->c);
  }
}

static bool scan_starts_exponent_part(struct scanner_state *s)
{
  assert(s->c == 'e' || s->c == 'E');

  int exp_char = s->c;
  int consumed = 0;

  next_char(s);
  if (s->c != C_EOF) {
    consumed = 1;
  }

  if (s->c == '+' || s->c == '-') {
    /* Across refill boundaries, defer to full exponent parsing. */
    if (s->p >= s->buffer_end) {
      if (consumed > 0) {
        s->p -= consumed;
      }
      s->c = exp_char;
      return true;
    }
    next_char(s);
    if (s->c != C_EOF) {
      consumed = 2;
    }
  }

  bool starts_exponent = ('0' <= s->c && s->c <= '9');

  /* Roll back consumed lookahead chars.  This is safe because s->p points
   * into s->read_buffer, a fixed allocation that persists across refills. */
  if (consumed > 0) {
    s->p -= consumed;
  }
  s->c = exp_char;
  return starts_exponent;
}

static void scan_float_fraction(struct scanner_state *s)
{
  struct arena *strings = s->strings;
  bool          had_error = false;
  bool          had_digit = false;
  bool          last_was_underscore = false;

  /* decimal beginning already on growing arena */
  for (;;) {
    if ('0' <= s->c && s->c <= '9') {
      arena_grow_char(strings, s->c);
      had_digit = true;
      last_was_underscore = false;
      next_char(s);
      continue;
    }
    if (s->c == '_') {
      if ((!had_digit || last_was_underscore) && !had_error) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "decimal literal cannot have consecutive ");
        diag_quoted_char(s->d, '_');
        diag_end(s->d);
        had_error = true;
      }
      last_was_underscore = true;
      next_char(s);
      continue;
    }
    break;
  }
  if (last_was_underscore && !had_error) {
    diag_begin_error(s->d, scanner_location(s));
    diag_frag(s->d, "decimal literal cannot end with ");
    diag_quoted_char(s->d, '_');
    diag_end(s->d);
    had_error = true;
  }

  if ((s->c == 'e' || s->c == 'E') && scan_starts_exponent_part(s)) {
    arena_grow_char(strings, s->c);
    next_char(s);
    if (s->c == '+' || s->c == '-') {
      arena_grow_char(strings, s->c);
      next_char(s);
    }
    had_digit = false;
    last_was_underscore = false;
    while (('0' <= s->c && s->c <= '9') || s->c == '_') {
      if ('0' <= s->c && s->c <= '9') {
        arena_grow_char(strings, s->c);
        had_digit = true;
        last_was_underscore = false;
        next_char(s);
        continue;
      }
      if ((!had_digit || last_was_underscore) && !had_error) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "decimal literal cannot have consecutive ");
        diag_quoted_char(s->d, '_');
        diag_end(s->d);
        had_error = true;
      }
      last_was_underscore = true;
      next_char(s);
    }
    if (last_was_underscore && !had_error) {
      diag_begin_error(s->d, scanner_location(s));
      diag_frag(s->d, "decimal literal cannot end with ");
      diag_quoted_char(s->d, '_');
      diag_end(s->d);
      had_error = true;
    }
    if (!had_digit) {
      if (!had_error) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "invalid decimal exponent");
        diag_end(s->d);
      }
      void *chars = arena_grow_finish(strings);
      arena_free_to(strings, chars);
      s->token.kind = T_INVALID;
      return;
    }
  }
  bool is_imag = false;
  if (s->c == 'j' || s->c == 'J') {
    is_imag = true;
    next_char(s);
  }
  arena_grow_char(strings, '\0');
  char *chars = (char *)arena_grow_finish(strings);

  char *endptr;
  errno = 0;
  double value = strtod(chars, &endptr);
  if (endptr == chars || *endptr != '\0' || (errno != 0 && errno != ERANGE)) {
    internal_error("scanner: invalid float literal parse state");
  }
  arena_free_to(strings, chars);

  if (is_imag) {
    s->token.u.object = object_intern_complex(s->objects, 0.0, value);
  } else {
    s->token.u.object = object_intern_float(s->objects, value);
  }
  s->token.kind = T_FLOAT;
}

static void scan_float_dot(struct scanner_state *s)
{
  assert('0' <= s->c && s->c <= '9');
  struct arena *strings = s->strings;
  arena_grow_begin(strings, alignof(char));
  arena_grow_char(strings, '.');

  scan_float_fraction(s);
}

static union object *scan_decimal_big_integer(struct scanner_state *s,
                                              const char           *chars)
{
  struct bigint_accum big_value = { 0 };
  for (const char *c = chars; *c != '\0'; ++c) {
    assert('0' <= *c && *c <= '9');
    bigint_accum_mul_add(&big_value, 10, (uint32_t)(*c - '0'));
  }
  union object *object = bigint_accum_intern(s, &big_value);
  bigint_accum_free(&big_value);
  return object;
}

static void scan_number(struct scanner_state *s)
{
  char first = s->c;
  next_char(s);
  if (first == '0') {
    switch (s->c) {
    case 'x':
    case 'X': {
      scan_hexadecimal_integer(s);
      return;
    }
    case 'b':
    case 'B':
      scan_binary_integer(s);
      return;
    case 'o':
    case 'O':
      scan_octal_integer(s);
      return;
    default:
      break;
    }
  }

  struct arena *strings = s->strings;
  arena_grow_begin(strings, alignof(char));
  struct scan_decimal_integer_result decimal_result
      = scan_decimal_integer(s, strings, first);

  if (s->c == 'j' || s->c == 'J') {
    next_char(s);
    arena_grow_char(strings, '\0');
    char *chars = (char *)arena_grow_finish(strings);

    char *endptr;
    errno = 0;
    double value = strtod(chars, &endptr);
    if (endptr == chars || *endptr != '\0'
        || (errno != 0 && errno != ERANGE)) {
      internal_error("scanner: invalid imaginary literal parse state");
    }
    arena_free_to(strings, chars);

    s->token.u.object = object_intern_complex(s->objects, 0.0, value);
    s->token.kind = T_FLOAT;
    return;
  }

  if (s->c == '.') {
    arena_grow_char(strings, s->c);
    next_char(s);
    scan_float_fraction(s);
    return;
  } else if (s->c == 'e' || s->c == 'E') {
    if (scan_starts_exponent_part(s)) {
      scan_float_fraction(s);
      return;
    }
  }
  if (decimal_result.leading_zero_nonzero && !decimal_result.had_error) {
    diag_begin_error(s->d, scanner_location(s));
    diag_frag(s->d,
              "leading zeros in decimal integer literals are not permitted; "
              "use an 0o prefix for octal integers");
    diag_end(s->d);
  }
  arena_grow_char(strings, '\0');
  char *chars = (char *)arena_grow_finish(strings);

  char              *endptr;
  unsigned long long value;
  errno = 0;
  value = strtoull(chars, &endptr, 10);
  if (endptr == chars || *endptr != '\0' || (errno != 0 && errno != ERANGE)) {
    internal_error("scanner: invalid decimal literal parse state");
  }
  if (errno == ERANGE) {
    s->token.u.object = scan_decimal_big_integer(s, chars);
    arena_free_to(strings, chars);
    s->token.kind = T_INTEGER;
    return;
  }
  if (value > (unsigned long long)INT64_MAX) {
    s->token.u.object = scan_decimal_big_integer(s, chars);
    arena_free_to(strings, chars);
    s->token.kind = T_INTEGER;
    return;
  }
  arena_free_to(strings, chars);

  s->token.u.object = object_intern_int(s->objects, (int64_t)value);
  s->token.kind = T_INTEGER;
}

enum scan_named_escape_status {
  SCAN_NAMED_ESCAPE_OK = 0,
  SCAN_NAMED_ESCAPE_MALFORMED,
  SCAN_NAMED_ESCAPE_UNKNOWN_NAME,
};

static enum scan_named_escape_status scan_named_escape(struct scanner_state *s,
                                                       uint32_t *codepoint)
{
  assert(s->c == 'N');
  next_char(s);

  if (s->c != '{') {
    return SCAN_NAMED_ESCAPE_MALFORMED;
  }
  next_char(s);

  char    *name = NULL;
  unsigned length = 0;
  unsigned capacity = 0;
  for (;;) {
    if (s->c == C_EOF || s->c == '\n' || s->c == '\r' || s->c == '\''
        || s->c == '\"' || s->c == '}') {
      break;
    }

    if (length == UINT_MAX) {
      free(name);
      return SCAN_NAMED_ESCAPE_MALFORMED;
    }
    unsigned next_length = length + 1;
    if (next_length > capacity) {
      name = dynmemory_grow(name, &capacity, next_length, sizeof(char));
    }
    name[length] = (char)s->c;
    length = next_length;
    next_char(s);
  }

  if (length == 0 || s->c != '}') {
    free(name);
    return SCAN_NAMED_ESCAPE_MALFORMED;
  }

  bool ok = unicode_name_lookup(name, length, codepoint);
  free(name);

  next_char(s);
  return ok ? SCAN_NAMED_ESCAPE_OK : SCAN_NAMED_ESCAPE_UNKNOWN_NAME;
}

static void error_invalid_utf8_source(struct scanner_state *s)
{
  diag_begin_error(s->d, scanner_location(s));
  diag_frag(s->d, "failed to decode source with encoding: utf-8");
  diag_end(s->d);
}

static void scan_escape_sequence(struct scanner_state *s,
                                 struct arena *strings, bool is_unicode)
{
  assert(s->c == '\\');
  next_char(s);

  char        decoded_single;
  uint32_t    codepoint;
  int         expected_hex_digits;
  const char *hex_escape = NULL;
  switch (s->c) {
  case '\\':
  case '\'':
  case '\"':
    decoded_single = s->c;
    break;
  case 'a':
    decoded_single = 0x07;
    break;
  case 'b':
    decoded_single = 0x08;
    break;
  case 'f':
    decoded_single = 0x0c;
    break;
  case 'n':
    decoded_single = 0x0a;
    break;
  case 'r':
    decoded_single = 0x0d;
    break;
  case 't':
    decoded_single = 0x09;
    break;
  case 'v':
    decoded_single = 0x0b;
    break;
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    codepoint = s->c - '0';
    int num_digits = 1;
    for (;;) {
      next_char(s);
      if (s->c < '0' || s->c > '7' || ++num_digits > 3) {
        break;
      }
      codepoint <<= 3;
      codepoint |= s->c - '0';
    }
    goto append_codepoint;
  case 'x':
    next_char(s);
    expected_hex_digits = 2;
    hex_escape = "\\x";
    goto parse_hex;
  case 'N':
    if (!is_unicode) {
      arena_grow_char(strings, '\\');
      arena_grow_char(strings, 'N');
      next_char(s);
      return;
    }
    switch (scan_named_escape(s, &codepoint)) {
    case SCAN_NAMED_ESCAPE_OK:
      goto append_codepoint;
    case SCAN_NAMED_ESCAPE_MALFORMED:
      diag_begin_error(s->d, scanner_location(s));
      diag_frag(s->d, "malformed \\N character escape");
      diag_end(s->d);
      codepoint = 0xfffc;
      goto append_codepoint;
    case SCAN_NAMED_ESCAPE_UNKNOWN_NAME:
      diag_begin_error(s->d, scanner_location(s));
      diag_frag(s->d, "unknown Unicode character name");
      diag_end(s->d);
      codepoint = 0xfffc;
      goto append_codepoint;
    }
    internal_error("scanner: invalid named escape status");
  case 'u':
    next_char(s);
    expected_hex_digits = 4;
    hex_escape = "\\u";
    goto parse_hex;
  case 'U':
    next_char(s);
    expected_hex_digits = 8;
    hex_escape = "\\U";
    goto parse_hex;
  case '\r':
    next_char(s);
    if (s->c == '\n') next_char(s);
    goto new_line;
  case '\n':
    next_char(s);
    /* fallthrough */
  new_line:
    ++s->line;
    return;
  case C_EOF:
    return;
  parse_hex:
    codepoint = 0;
    do {
      int digit;
      if ('0' <= s->c && s->c <= '9') {
        digit = s->c - '0';
      } else if ('a' <= s->c && s->c <= 'f') {
        digit = 10 + (s->c - 'a');
      } else if ('A' <= s->c && s->c <= 'F') {
        digit = 10 + (s->c - 'A');
      } else {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "malformed ");
        assert(hex_escape != NULL);
        diag_frag(s->d, hex_escape);
        diag_frag(s->d, " character escape");
        diag_end(s->d);
        codepoint = 0xfffc;
        goto append_codepoint;
      }
      next_char(s);
      codepoint <<= 4;
      codepoint |= digit;
      --expected_hex_digits;
    } while (expected_hex_digits > 0);
    goto append_codepoint;
  append_codepoint:
    if (is_unicode && codepoint > 127) {
      if (codepoint > 0x10ffff) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d,
                  "invalid unicode codepoint: must be smaller than 10ffff");
        diag_end(s->d);
        codepoint = 0xfffc;
      }
      arena_grow_utf8_codepoint(strings, codepoint);
    } else {
      arena_grow_char(strings, (char)codepoint);
    }
    return;
  default:
    /* TODO: report error */
    arena_grow_char(strings, '\\');
    if ((unsigned char)s->c >= 0x80) {
      uint32_t cp;
      char     utf8[4];
      int      len = decode_utf8(s, &cp, utf8);
      if (len == 0) {
        error_invalid_utf8_source(s);
        if ((unsigned char)s->c >= 0x80) {
          next_char(s);
        }
        return;
      }
      for (int i = 0; i < len; i++) {
        arena_grow_char(strings, utf8[i]);
      }
      next_char(s);
      return;
    }
    arena_grow_char(strings, s->c);
    next_char(s);
    return;
  }
  next_char(s);
  arena_grow_char(strings, decoded_single);
}

static void error_unterminated_string(struct scanner_state *s)
{
  diag_begin_error(s->d, scanner_location(s));
  diag_frag(s->d, "unterminated string literal");
  diag_end(s->d);
}

static void fstring_push(struct scanner_state *s, enum string_quote quote,
                         bool format_spec)
{
  assert(s->fstring_stack_top < MAX_FSTRING_NESTING);
  s->fstring_stack[s->fstring_stack_top++] = s->fstring;
  memset(&s->fstring, 0, sizeof(s->fstring));
  s->fstring.paren_level = s->paren_level;
  s->fstring.quote = quote;
  s->fstring.format_spec = format_spec;
}

static void fstring_pop(struct scanner_state *s)
{
  assert(s->fstring_stack_top > 0);
  s->fstring = s->fstring_stack[--s->fstring_stack_top];
}

static const char *
scan_string_plain_span_find_stop_scalar_nofmt_double(const char *cursor,
                                                     const char *end)
{
  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '"' || c == '\\' || c == '\n' || c == '\r' || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

static const char *
scan_string_plain_span_find_stop_scalar_nofmt_single(const char *cursor,
                                                     const char *end)
{
  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '\'' || c == '\\' || c == '\n' || c == '\r' || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

static const char *
scan_string_plain_span_find_stop_scalar_fmt_double(const char *cursor,
                                                   const char *end)
{
  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '"' || c == '\\' || c == '\n' || c == '\r' || c == '{' || c == '}'
        || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

static const char *
scan_string_plain_span_find_stop_scalar_fmt_single(const char *cursor,
                                                   const char *end)
{
  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '\'' || c == '\\' || c == '\n' || c == '\r' || c == '{'
        || c == '}' || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

#if PYCOMPARSE_HAVE_X86_AVX2 && PYCOMPARSE_HAVE_GNU_IFUNC

__attribute__((target("avx2"))) static const char *
scan_string_plain_span_find_stop_avx2_nofmt_double(const char *cursor,
                                                   const char *end)
{
  const __m256i quote = _mm256_set1_epi8('"');
  const __m256i backslash = _mm256_set1_epi8('\\');
  const __m256i newline = _mm256_set1_epi8('\n');
  const __m256i carriage_return = _mm256_set1_epi8('\r');
  const __m256i zeros = _mm256_setzero_si256();

  while ((size_t)(end - cursor) >= 32) {
    __m256i chunk = _mm256_loadu_si256((const __m256i *)(const void *)cursor);
    __m256i mask = _mm256_or_si256(
        _mm256_or_si256(_mm256_cmpeq_epi8(chunk, quote),
                        _mm256_cmpeq_epi8(chunk, backslash)),
        _mm256_or_si256(_mm256_cmpeq_epi8(chunk, newline),
                        _mm256_cmpeq_epi8(chunk, carriage_return)));
    mask = _mm256_or_si256(mask, _mm256_cmpgt_epi8(zeros, chunk));
    unsigned bits = (unsigned)_mm256_movemask_epi8(mask);
    if (bits != 0u) return cursor + (unsigned)__builtin_ctz(bits);
    cursor += 32;
  }

  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '"' || c == '\\' || c == '\n' || c == '\r' || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

__attribute__((target("avx2"))) static const char *
scan_string_plain_span_find_stop_avx2_nofmt_single(const char *cursor,
                                                   const char *end)
{
  const __m256i quote = _mm256_set1_epi8('\'');
  const __m256i backslash = _mm256_set1_epi8('\\');
  const __m256i newline = _mm256_set1_epi8('\n');
  const __m256i carriage_return = _mm256_set1_epi8('\r');
  const __m256i zeros = _mm256_setzero_si256();

  while ((size_t)(end - cursor) >= 32) {
    __m256i chunk = _mm256_loadu_si256((const __m256i *)(const void *)cursor);
    __m256i mask = _mm256_or_si256(
        _mm256_or_si256(_mm256_cmpeq_epi8(chunk, quote),
                        _mm256_cmpeq_epi8(chunk, backslash)),
        _mm256_or_si256(_mm256_cmpeq_epi8(chunk, newline),
                        _mm256_cmpeq_epi8(chunk, carriage_return)));
    mask = _mm256_or_si256(mask, _mm256_cmpgt_epi8(zeros, chunk));
    unsigned bits = (unsigned)_mm256_movemask_epi8(mask);
    if (bits != 0u) return cursor + (unsigned)__builtin_ctz(bits);
    cursor += 32;
  }

  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '\'' || c == '\\' || c == '\n' || c == '\r' || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

__attribute__((target("avx2"))) static const char *
scan_string_plain_span_find_stop_avx2_fmt_double(const char *cursor,
                                                 const char *end)
{
  const __m256i quote = _mm256_set1_epi8('"');
  const __m256i backslash = _mm256_set1_epi8('\\');
  const __m256i newline = _mm256_set1_epi8('\n');
  const __m256i carriage_return = _mm256_set1_epi8('\r');
  const __m256i lcurly = _mm256_set1_epi8('{');
  const __m256i rcurly = _mm256_set1_epi8('}');
  const __m256i zeros = _mm256_setzero_si256();

  while ((size_t)(end - cursor) >= 32) {
    __m256i chunk = _mm256_loadu_si256((const __m256i *)(const void *)cursor);
    __m256i mask = _mm256_or_si256(
        _mm256_or_si256(
            _mm256_or_si256(_mm256_cmpeq_epi8(chunk, quote),
                            _mm256_cmpeq_epi8(chunk, backslash)),
            _mm256_or_si256(_mm256_cmpeq_epi8(chunk, newline),
                            _mm256_cmpeq_epi8(chunk, carriage_return))),
        _mm256_or_si256(_mm256_cmpeq_epi8(chunk, lcurly),
                        _mm256_cmpeq_epi8(chunk, rcurly)));
    mask = _mm256_or_si256(mask, _mm256_cmpgt_epi8(zeros, chunk));
    unsigned bits = (unsigned)_mm256_movemask_epi8(mask);
    if (bits != 0u) return cursor + (unsigned)__builtin_ctz(bits);
    cursor += 32;
  }

  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '"' || c == '\\' || c == '\n' || c == '\r' || c == '{' || c == '}'
        || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}

__attribute__((target("avx2"))) static const char *
scan_string_plain_span_find_stop_avx2_fmt_single(const char *cursor,
                                                 const char *end)
{
  const __m256i quote = _mm256_set1_epi8('\'');
  const __m256i backslash = _mm256_set1_epi8('\\');
  const __m256i newline = _mm256_set1_epi8('\n');
  const __m256i carriage_return = _mm256_set1_epi8('\r');
  const __m256i lcurly = _mm256_set1_epi8('{');
  const __m256i rcurly = _mm256_set1_epi8('}');
  const __m256i zeros = _mm256_setzero_si256();

  while ((size_t)(end - cursor) >= 32) {
    __m256i chunk = _mm256_loadu_si256((const __m256i *)(const void *)cursor);
    __m256i mask = _mm256_or_si256(
        _mm256_or_si256(
            _mm256_or_si256(_mm256_cmpeq_epi8(chunk, quote),
                            _mm256_cmpeq_epi8(chunk, backslash)),
            _mm256_or_si256(_mm256_cmpeq_epi8(chunk, newline),
                            _mm256_cmpeq_epi8(chunk, carriage_return))),
        _mm256_or_si256(_mm256_cmpeq_epi8(chunk, lcurly),
                        _mm256_cmpeq_epi8(chunk, rcurly)));
    mask = _mm256_or_si256(mask, _mm256_cmpgt_epi8(zeros, chunk));
    unsigned bits = (unsigned)_mm256_movemask_epi8(mask);
    if (bits != 0u) return cursor + (unsigned)__builtin_ctz(bits);
    cursor += 32;
  }

  while (cursor < end) {
    unsigned char c = *cursor;
    if (c == '\'' || c == '\\' || c == '\n' || c == '\r' || c == '{'
        || c == '}' || c >= 0x80) {
      break;
    }
    ++cursor;
  }
  return cursor;
}
#endif

#if PYCOMPARSE_HAVE_GNU_IFUNC
static void *resolve_scan_string_plain_span_find_stop_nofmt_double(void)
{
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return (void *)scan_string_plain_span_find_stop_avx2_nofmt_double;
  }
  return (void *)scan_string_plain_span_find_stop_scalar_nofmt_double;
}

static void *resolve_scan_string_plain_span_find_stop_nofmt_single(void)
{
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return (void *)scan_string_plain_span_find_stop_avx2_nofmt_single;
  }
  return (void *)scan_string_plain_span_find_stop_scalar_nofmt_single;
}

static void *resolve_scan_string_plain_span_find_stop_fmt_double(void)
{
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return (void *)scan_string_plain_span_find_stop_avx2_fmt_double;
  }
  return (void *)scan_string_plain_span_find_stop_scalar_fmt_double;
}

static void *resolve_scan_string_plain_span_find_stop_fmt_single(void)
{
  __builtin_cpu_init();
  if (__builtin_cpu_supports("avx2")) {
    return (void *)scan_string_plain_span_find_stop_avx2_fmt_single;
  }
  return (void *)scan_string_plain_span_find_stop_scalar_fmt_single;
}

static const char *
scan_string_plain_span_find_stop_nofmt_double(const char *cursor,
                                              const char *end)
    __attribute__((
        ifunc("resolve_scan_string_plain_span_find_stop_nofmt_double")));
static const char *
scan_string_plain_span_find_stop_nofmt_single(const char *cursor,
                                              const char *end)
    __attribute__((
        ifunc("resolve_scan_string_plain_span_find_stop_nofmt_single")));
static const char *
scan_string_plain_span_find_stop_fmt_double(const char *cursor,
                                            const char *end)
    __attribute__((
        ifunc("resolve_scan_string_plain_span_find_stop_fmt_double")));
static const char *
scan_string_plain_span_find_stop_fmt_single(const char *cursor,
                                            const char *end)
    __attribute__((
        ifunc("resolve_scan_string_plain_span_find_stop_fmt_single")));
#else
static const char *
scan_string_plain_span_find_stop_nofmt_double(const char *cursor,
                                              const char *end)
{
  return scan_string_plain_span_find_stop_scalar_nofmt_double(cursor, end);
}

static const char *
scan_string_plain_span_find_stop_nofmt_single(const char *cursor,
                                              const char *end)
{
  return scan_string_plain_span_find_stop_scalar_nofmt_single(cursor, end);
}

static const char *
scan_string_plain_span_find_stop_fmt_double(const char *cursor,
                                            const char *end)
{
  return scan_string_plain_span_find_stop_scalar_fmt_double(cursor, end);
}

static const char *
scan_string_plain_span_find_stop_fmt_single(const char *cursor,
                                            const char *end)
{
  return scan_string_plain_span_find_stop_scalar_fmt_single(cursor, end);
}
#endif

static bool scan_string_plain_span(struct scanner_state *s,
                                   struct arena *strings, char quote_char,
                                   bool format)
{
  const char *start = scanner_current_char_ptr(s);
  if (start == NULL) {
    return false;
  }

  const char *end = s->buffer_end;
  const char *cursor = NULL;
  if (quote_char == '"') {
    cursor = format
                 ? scan_string_plain_span_find_stop_fmt_double(start, end)
                 : scan_string_plain_span_find_stop_nofmt_double(start, end);
  } else {
    assert(quote_char == '\'');
    cursor = format
                 ? scan_string_plain_span_find_stop_fmt_single(start, end)
                 : scan_string_plain_span_find_stop_nofmt_single(start, end);
  }

  size_t span_size = (size_t)(cursor - start);
  if (span_size == 0) {
    return false;
  }

  char *dst = (char *)arena_grow(strings, (unsigned)span_size);
  memcpy(dst, start, span_size);

  s->p = cursor;
  next_char(s);
  return true;
}

static void scan_string_literal(struct scanner_state    *s,
                                struct scan_string_flags flags)
{
  struct arena *strings = s->strings;
  arena_grow_begin(strings, alignof(char));

  if (flags.format && s->fstring_stack_top >= MAX_FSTRING_NESTING) {
    diag_begin_error(s->d, scanner_location(s));
    diag_frag(s->d, "too many nested f-strings");
    diag_end(s->d);
    flags.format = false;
  }

  enum token_kind kind;
  char            quote_char;
  uint8_t         quote;
  if (flags.continue_format) {
    eat_char(s, '}');
    quote = s->fstring.quote;
    quote_char = (quote & QUOTE_QUOTATION_MARK) ? '"' : '\'';
    assert(quote != 0);
    kind = T_FSTRING_END;
  } else if (flags.format_spec) {
    eat_char(s, ':');
    quote = s->fstring.quote;
    quote_char = (quote & QUOTE_QUOTATION_MARK) ? '"' : '\'';
    assert(quote != 0);
    kind = T_STRING;
  } else {
    assert(s->c == '"' || s->c == '\'');
    quote_char = s->c;
    quote = s->c == '"' ? QUOTE_QUOTATION_MARK : QUOTE_APOSTROPHE;
    next_char(s);
    kind = T_STRING;

    if (s->c == quote_char) {
      next_char(s);
      if (s->c == quote_char) {
        next_char(s);
        quote |= QUOTE_TRIPLE;
      } else {
        goto quote_finish_string;
      }
    }
  }

  for (;;) {
    if (scan_string_plain_span(s, strings, quote_char, flags.format)) {
      continue;
    }
    /* vectorize this search? */
    switch (s->c) {
    case '"':
    case '\'': {
      if (s->c != quote_char) {
        break;
      }
      next_char(s);
      if ((quote & QUOTE_TRIPLE) == 0) {
        goto quote_finish_string;
      }
      if (s->c != quote_char) {
        arena_grow_char(strings, quote_char);
        continue;
      }
      next_char(s);
      if (s->c != quote_char) {
        arena_grow_char(strings, quote_char);
        arena_grow_char(strings, quote_char);
        continue;
      }
      next_char(s);
      goto quote_finish_string;
    }
    case '\\':
      if (flags.raw) {
        /* take backslash literally, except that the next char is literal too.
         * Which mostly means '\'' won't end the string. */
        arena_grow_char(strings, s->c);
        next_char(s);
        if (flags.format && (s->c == '{' || s->c == '}')) {
          /* In raw f-strings, braces still delimit replacements/escapes. */
          continue;
        }
        break;
      }
      scan_escape_sequence(s, strings, flags.unicode);
      continue;
    case '\r':
      next_char(s);
      if (s->c == '\n') next_char(s);
      goto new_line;
    case '\n':
      next_char(s);
      goto new_line;
    case '{':
      if (!flags.format) break;
      next_char(s);
      if (s->c == '{') break;
      if (!flags.continue_format) {
        fstring_push(s, quote, flags.format_spec);
        kind = T_FSTRING_START;
      } else {
        kind = T_FSTRING_FRAGMENT;
      }
      goto finish_string;
    case '}':
      if (!flags.format) break;
      if (flags.format_spec) goto quote_finish_string;
      next_char(s);
      if (s->c == '}') break;
      diag_begin_error(s->d, scanner_location(s));
      diag_frag(s->d, "single `}` not allowed in f-string");
      diag_end(s->d);
      arena_grow_char(strings, '}');
      continue;
    new_line:
      ++s->line;
      if ((quote & QUOTE_TRIPLE) == 0) {
        error_unterminated_string(s);
        goto finish_string;
      }
      arena_grow_char(strings, '\n');
      continue;
    case C_EOF:
      error_unterminated_string(s);
      goto finish_string;
    default:
      if ((unsigned char)s->c >= 0x80) {
        uint32_t cp;
        char     utf8[4];
        int      len = decode_utf8(s, &cp, utf8);
        if (len == 0) {
          error_invalid_utf8_source(s);
          if ((unsigned char)s->c >= 0x80) {
            next_char(s);
          }
          continue;
        }
        for (int i = 0; i < len; i++) {
          arena_grow_char(strings, utf8[i]);
        }
        next_char(s);
        continue;
      }
      break;
    }
    arena_grow_char(strings, (char)s->c);
    next_char(s);
  }

quote_finish_string:
  if (kind == T_FSTRING_END) {
    fstring_pop(s);
  }

finish_string:;
  size_t           length = arena_grow_current_size(strings);
  char            *chars = (char *)arena_grow_finish(strings);
  enum object_type type = flags.unicode ? OBJECT_STRING : OBJECT_BYTES;
  union object *object = object_intern_string(s->objects, type, length, chars);
  if (object->string.chars != chars) {
    arena_free_to(strings, chars);
  }
  s->token.u.object = object;
  s->token.kind = kind;
  s->token.string_is_fstring = (kind == T_STRING && flags.format);
}

static void scan_eof(struct scanner_state *s)
{
  if (s->read_error_token_pending) {
    s->read_error_token_pending = false;
    s->token.kind = T_INVALID;
    return;
  }
  if (s->fstring_stack_top > 0) {
    error_unterminated_string(s);
    fstring_pop(s);
    s->token.u.object = object_intern_cstring(s->objects, "");
    s->token.kind = T_FSTRING_END;
    return;
  }
  if (s->last_line_indent > 0) {
    assert(s->indentation_stack_top > 0);
    s->pending_dedents = s->indentation_stack_top;
    s->last_line_indent = 0;
  }

  /* Add artificial newline, for EOF in the middle of a line. */
  s->at_begin_of_line = true;
  s->token.kind = T_NEWLINE;
}

static bool scan_indentation(struct scanner_state *s)
{
  assert(s->at_begin_of_line);
  if (s->pending_dedents > 0) {
    assert(s->indentation_stack_top > 0);
    --s->indentation_stack_top;
    --s->pending_dedents;
    if (s->pending_dedents == 0 && s->c != C_EOF) {
      s->at_begin_of_line = false;
    }
    s->token.kind = T_DEDENT;
    return true;
  }

  unsigned column = 0;
  for (;;) {
    switch (s->c) {
    case '\r':
      next_char(s);
      if (s->c == '\n') next_char(s);
      goto new_line;
    case '\n':
      next_char(s);
      /* fallthrough */
    new_line:
      // TODO: empty line as NEWLINE in interactive mode
      ++s->line;
      column = 0;
      continue;
    case ' ':
      scan_indentation_spaces(s, &column);
      continue;
    case '\t':
      next_char(s);
      column = (column / TABSIZE + 1) * TABSIZE;
      continue;
    case '\014':
      next_char(s);
      column = 0;
      continue;
    case '#':
      scan_line_comment(s);
      goto new_line;
    case C_EOF:
      if (s->read_error_token_pending) {
        scan_eof(s);
        return true;
      }
      /* Trailing spaces before EOF are not real indentation. */
      column = 0;
      if (s->last_line_indent > 0) break;
      s->token.kind = T_EOF;
      return true;
    default:
      break;
    }
    break;
  }

  unsigned last_line_indent = s->last_line_indent;
  if (column > last_line_indent) {
    s->indentation_stack[s->indentation_stack_top] = last_line_indent;
    s->last_line_indent = column;
    if (s->indentation_stack_top >= MAXINDENT - 1) {
      if (s->token.kind != T_INVALID) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "too many levels of indentation");
        diag_end(s->d);
      }
      s->token.kind = T_INVALID;
      s->c = C_EOF;
      s->at_begin_of_line = true;
      return true;
    }
    ++s->indentation_stack_top;
    s->token.kind = T_INDENT;
    s->at_begin_of_line = false;
    return true;
  }
  if (column < last_line_indent) {
    unsigned dedents = 1;
    for (unsigned t = s->indentation_stack_top; t-- > 0;) {
      if (column >= s->indentation_stack[t]) {
        break;
      }
      ++dedents;
    }
    unsigned target_cols
        = s->indentation_stack[s->indentation_stack_top - dedents];
    /* we report 1 dedent right now, the rest is pending */
    unsigned pending_dedents = dedents - 1;
    s->pending_dedents = pending_dedents;
    s->last_line_indent = column;
    s->at_begin_of_line = pending_dedents > 0 || s->c == C_EOF;
    if (column != target_cols) {
      diag_begin_error(s->d, scanner_location(s));
      diag_frag(s->d, "indentation level mismatch");
      diag_end(s->d);
      s->token.kind = T_INVALID;
    } else {
      s->indentation_stack_top--;
      s->token.kind = T_DEDENT;
    }
    return true;
  }
  s->at_begin_of_line = false;
  return false;
}

void scanner_next_token(struct scanner_state *s)
{
begin_new_line:
  s->token.string_is_fstring = false;
  if (s->at_begin_of_line) {
    if (scan_indentation(s)) {
      return;
    }
  }

  int invalid_c;
  for (;;) {
    switch (s->c) {
    case '\r':
      next_char(s);
      if (s->c == '\n') next_char(s);
      goto new_line;
    case '\n':
      next_char(s);
      /* fallthrough */
    new_line:
      ++s->line;
      if (s->fstring.quote != 0 && (s->fstring.quote & QUOTE_TRIPLE) == 0) {
        error_unterminated_string(s);
        fstring_pop(s);
        /* TODO: avoid multiple errors when exiting nested fstrings? */
        s->token.u.object = object_intern_cstring(s->objects, "");
        s->token.kind = T_FSTRING_END;
        return;
      }
      if (s->paren_level > 0) {
        /* no token; continue on next line without indentation check. */
        continue;
      }
      if (s->at_begin_of_line) {
        /* no token; continue on next line with indentation check. */
        goto begin_new_line;
      }
      s->at_begin_of_line = true;
      s->token.kind = T_NEWLINE;
      return;

    case ' ':
    case '\t':
    case '\014':
      scan_skip_inline_spaces(s);
      continue;

    case '#':
      scan_line_comment(s);
      goto new_line;

    case IDENTIFIER_START_CASES_WITHOUT_B_F_R_U: {
      char first_char = (char)s->c;
      next_char(s);
      scan_identifier(s, first_char, /*second_char=*/0);
      return;
    }

    case DIGIT_CASES: {
      scan_number(s);
      return;
    }

    case 'b':
    case 'B': {
      char first_char = (char)s->c;
      next_char(s);
      struct scan_string_flags flags = { .unicode = false };
      char                     second_char = (char)s->c;
      if (second_char == 'r' || second_char == 'R') {
        next_char(s);
        flags.raw = true;
      } else {
        second_char = 0;
      }
      if (s->c == '"' || s->c == '\'') {
        scan_string_literal(s, flags);
        return;
      } else {
        scan_identifier(s, first_char, second_char);
      }
      return;
    }

    case 'f':
    case 'F': {
      char first_char = (char)s->c;
      next_char(s);
      struct scan_string_flags flags = {
        .unicode = true,
        .format = true,
      };
      char second_char = (char)s->c;
      if (second_char == 'r' || second_char == 'R') {
        next_char(s);
        flags.raw = true;
      } else {
        second_char = 0;
      }
      if (s->c == '"' || s->c == '\'') {
        scan_string_literal(s, flags);
        return;
      } else {
        scan_identifier(s, first_char, second_char);
      }
      return;
    }

    case 'r':
    case 'R': {
      char first_char = (char)s->c;
      next_char(s);
      struct scan_string_flags flags = {
        .raw = true,
        .unicode = true,
      };
      char second_char = (char)s->c;
      if (second_char == 'b' || second_char == 'B') {
        next_char(s);
        flags.unicode = false;
      } else if (second_char == 'f' || second_char == 'F') {
        next_char(s);
        flags.format = true;
      } else {
        second_char = 0;
      }
      if (s->c == '"' || s->c == '\'') {
        scan_string_literal(s, flags);
        return;
      } else {
        scan_identifier(s, first_char, second_char);
      }
      return;
    }

    case 'u':
    case 'U': {
      char first_char = (char)s->c;
      next_char(s);
      if (s->c == '"' || s->c == '\'') {
        struct scan_string_flags flags = { .unicode = true };
        scan_string_literal(s, flags);
        return;
      } else {
        scan_identifier(s, first_char, /*second_char=*/0);
      }
      return;
    }

    case '\'':
    case '"': {
      struct scan_string_flags flags = { .unicode = true };
      scan_string_literal(s, flags);
      return;
    }

    case '=':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_EQUALS_EQUALS;
      } else {
        s->token.kind = T_EQUALS;
      }
      return;

    case '!':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_EXCLAMATIONMARK_EQUALS;
      } else {
        s->token.kind = T_EXCLAMATIONMARK;
      }
      return;

    case '<':
      next_char(s);
      switch (s->c) {
      case '>':
        next_char(s);
        s->token.kind = T_LESS_THAN_GREATER_THAN;
        return;
      case '=':
        next_char(s);
        s->token.kind = T_LESS_THAN_EQUALS;
        return;
      case '<':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          s->token.kind = T_LESS_THAN_LESS_THAN_EQUALS;
        } else {
          s->token.kind = T_LESS_THAN_LESS_THAN;
        }
        return;
      default:
        s->token.kind = T_LESS_THAN;
        return;
      }

    case '>':
      next_char(s);
      switch (s->c) {
      case '>':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          s->token.kind = T_GREATER_THAN_GREATER_THAN_EQUALS;
        } else {
          s->token.kind = T_GREATER_THAN_GREATER_THAN;
        }
        return;
      case '=':
        next_char(s);
        s->token.kind = T_GREATER_THAN_EQUALS;
        return;
      default:
        s->token.kind = T_GREATER_THAN;
        return;
      }

    case '+':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_PLUS_EQUALS;
      } else {
        s->token.kind = T_PLUS;
      }
      return;

    case '-':
      next_char(s);
      switch (s->c) {
      case '=':
        next_char(s);
        s->token.kind = T_MINUS_EQUALS;
        return;
      case '>':
        next_char(s);
        s->token.kind = T_MINUS_GREATER_THAN;
        return;
      default:
        s->token.kind = T_MINUS;
        return;
      }

    case '*':
      next_char(s);
      switch (s->c) {
      case '=':
        next_char(s);
        s->token.kind = T_ASTERISK_EQUALS;
        return;
      case '*':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          s->token.kind = T_ASTERISK_ASTERISK_EQUALS;
        } else {
          s->token.kind = T_ASTERISK_ASTERISK;
        }
        return;
      default:
        s->token.kind = T_ASTERISK;
        return;
      }

    case '/':
      next_char(s);
      switch (s->c) {
      case '=':
        next_char(s);
        s->token.kind = T_SLASH_EQUALS;
        return;
      case '/':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          s->token.kind = T_SLASH_SLASH_EQUALS;
        } else {
          s->token.kind = T_SLASH_SLASH;
        }
        return;
      default:
        s->token.kind = T_SLASH;
        return;
      }

    case '|':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_BAR_EQUALS;
      } else {
        s->token.kind = T_BAR;
      }
      return;

    case '%':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_PERCENT_EQUALS;
      } else {
        s->token.kind = T_PERCENT;
      }
      return;

    case '&':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_AMPERSAND_EQUALS;
      } else {
        s->token.kind = T_AMPERSAND;
      }
      return;

    case '^':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_CARET_EQUALS;
      } else {
        s->token.kind = T_CARET;
      }
      return;

    case '@':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_AT_EQUALS;
      } else {
        s->token.kind = T_AT;
      }
      return;

    case '.':
      next_char(s);
      switch (s->c) {
      case DIGIT_CASES:
        scan_float_dot(s);
        return;
      case '.':
        next_char(s);
        if (s->c == '.') {
          next_char(s);
          s->token.kind = T_DOT_DOT_DOT;
        } else {
          if (s->c != C_EOF) {
            pushback_char(s, s->c);
          }
          s->token.kind = '.';
          s->c = '.';
        }
        return;
      default:
        s->token.kind = T_DOT;
        return;
      }

    case ':':
      if (s->fstring.quote != 0 && s->fstring.paren_level == s->paren_level) {
        if (s->token.kind != ':') {
          s->token.kind = ':';
          return;
        }
        struct scan_string_flags flags = {
          .unicode = true,
          .format = true,
          .format_spec = true,
        };
        scan_string_literal(s, flags);
        return;
      }
      next_char(s);

      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_COLON_EQUALS;
      } else {
        s->token.kind = ':';
      }
      return;

    case '(':
    case '[':
    case '{':
      ++s->paren_level;
      goto single_char_token;

    case '}':
      if (s->fstring.quote != 0 && s->fstring.paren_level == s->paren_level) {
        struct scan_string_flags flags = {
          .unicode = true,
          .format = true,
          .continue_format = true,
          .format_spec = s->fstring.format_spec,
        };
        scan_string_literal(s, flags);
        return;
      }
      PYCOMPARSE_FALLTHROUGH;
    case ')':
    case ']':
      if (s->paren_level > 0) {
        --s->paren_level;
      }
      goto single_char_token;

    case ',':
    case ';':
    case '~':
    single_char_token:
      s->token.kind = (uint16_t)s->c;
      next_char(s);
      return;

    case '\\':
      next_char(s);
      if (s->c == '\r') {
        next_char(s);
      }
      if (s->c == '\n') {
        next_char(s);
        s->line++;
        /* no token; continue on next line without indentation check. */
        continue;
      }
      invalid_c = '\\';
      goto invalid_char;

    case C_EOF:
      scan_eof(s);
      return;

    default:
      if (s->c >= 128) {
        uint32_t cp;
        char     utf8[4];
        int      len = decode_utf8(s, &cp, utf8);
        if (len && is_xid_start(cp)) {
          struct arena *arena = &s->symbol_table->arena;
          arena_grow_begin(arena, string_alignment);
          for (int i = 0; i < len; i++)
            arena_grow_char(arena, utf8[i]);
          next_char(s);
          scan_identifier_continue(s, arena, /*has_non_ascii=*/true);
          return;
        }
        /* Invalid: not a valid UTF-8 sequence or not XID_Start. */
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "invalid character in identifier");
        diag_end(s->d);
        s->token.kind = T_INVALID;
        if (s->c >= 128) next_char(s);
        continue;
      }
      invalid_c = s->c;
      next_char(s);
    invalid_char:
      if (s->token.kind != T_INVALID) {
        diag_begin_error(s->d, scanner_location(s));
        diag_frag(s->d, "unexpected input char ");
        diag_quoted_char(s->d, (char)invalid_c);
        diag_end(s->d);
      }
      s->token.kind = T_INVALID;
      continue;
    }
  }
}

struct location scanner_location(struct scanner_state *s)
{
  struct location location = { s->line };
  return location;
}

void scanner_init(struct scanner_state *s, FILE *input, const char *filename,
                  struct symbol_table  *symbol_table,
                  struct object_intern *objects, struct arena *strings,
                  struct diagnostics_state *diagnostics)
{
  memset(s, 0, sizeof(*s));
  s->read_buffer = malloc(scanner_read_buffer_size);
  if (s->read_buffer == NULL) {
    internal_error("out of memory");
  }
  s->read_buffer_size = scanner_read_buffer_size;
  s->input = input;
  s->filename = filename;
  s->line = 1;
  s->symbol_table = symbol_table;
  s->objects = objects;
  s->strings = strings;
  s->fstring_debug.spilled_prefix = NULL;
  s->fstring_debug.spilled_size = 0;
  s->fstring_debug.spilled_capacity = 0;
  s->fstring_debug.depth = 0;
  s->fstring_debug.tail_start = NULL;
  s->at_begin_of_line = true;
  s->d = diagnostics;
#ifndef PYCOMPARSE_NO_ICONV
  struct encoding_error encoding_error = encoding_maybe_transcode_to_utf8(
      &s->input, &s->transcoded_input, &s->transcoded_source);
  if (encoding_error.kind != ENCODING_ERROR_NONE) {
    diag_begin_error(s->d, scanner_location(s));
    switch (encoding_error.kind) {
    case ENCODING_ERROR_UNKNOWN_ENCODING:
      diag_frag(s->d, "unknown encoding: ");
      diag_frag(s->d, encoding_error.encoding);
      break;
    case ENCODING_ERROR_BOM_COOKIE_MISMATCH:
      diag_frag(s->d, "encoding problem: ");
      diag_frag(s->d, encoding_error.encoding);
      diag_frag(s->d, " with BOM");
      break;
    case ENCODING_ERROR_DECODE_FAILED:
      diag_frag(s->d, "failed to decode source with encoding: ");
      diag_frag(s->d, encoding_error.encoding);
      break;
    case ENCODING_ERROR_NONE:
      break;
    }
    diag_end(s->d);
  }
#endif
  s->c = refill_buffer(s);
  if (s->c == 0xEF && s->p + 1 < s->buffer_end
      && (unsigned char)s->p[0] == 0xBB && (unsigned char)s->p[1] == 0xBF) {
    next_char(s);
    next_char(s);
    next_char(s);
  }
}

void scanner_free(struct scanner_state *s)
{
#ifndef PYCOMPARSE_NO_ICONV
  if (s->transcoded_input != NULL) {
    fclose(s->transcoded_input);
  }
  free(s->transcoded_source);
#endif
  free(s->fstring_debug.spilled_prefix);
  free(s->read_buffer);
}

void scanner_init_from_buffer(struct scanner_state *s, const void *buf,
                              size_t buf_len, const char *filename,
                              struct symbol_table      *symbol_table,
                              struct object_intern     *objects,
                              struct arena             *strings,
                              struct diagnostics_state *diagnostics,
                              bool                      is_utf8)
{
  memset(s, 0, sizeof(*s));
  s->filename = filename;
  s->line = 1;
  s->symbol_table = symbol_table;
  s->objects = objects;
  s->strings = strings;
  s->d = diagnostics;
  s->at_begin_of_line = true;

  const char *effective_buf = buf;
  size_t      len = buf_len;
#ifndef PYCOMPARSE_NO_ICONV
  if (!is_utf8) {
    char                 *transcoded = NULL;
    size_t                transcoded_len = 0;
    struct encoding_error encoding_error
        = encoding_maybe_transcode_to_utf8_from_string(
            effective_buf, len, &transcoded, &transcoded_len);
    if (encoding_error.kind != ENCODING_ERROR_NONE) {
      diag_begin_error(s->d, scanner_location(s));
      switch (encoding_error.kind) {
      case ENCODING_ERROR_UNKNOWN_ENCODING:
        diag_frag(s->d, "unknown encoding: ");
        diag_frag(s->d, encoding_error.encoding);
        break;
      case ENCODING_ERROR_BOM_COOKIE_MISMATCH:
        diag_frag(s->d, "encoding problem: ");
        diag_frag(s->d, encoding_error.encoding);
        diag_frag(s->d, " with BOM");
        break;
      case ENCODING_ERROR_DECODE_FAILED:
        diag_frag(s->d, "failed to decode source with encoding: ");
        diag_frag(s->d, encoding_error.encoding);
        break;
      case ENCODING_ERROR_NONE:
        break;
      }
      diag_end(s->d);
    }
    if (transcoded != NULL) {
      s->transcoded_source = transcoded;
      effective_buf = transcoded;
      len = transcoded_len;
    }
  }
#endif
  if (len == 0) {
    s->c = C_EOF;
  } else {
    s->c = (unsigned char)effective_buf[0];
    s->p = effective_buf + 1;
    s->buffer_end = effective_buf + len;
    /* Skip UTF-8 BOM (0xEF 0xBB 0xBF) if present. */
    if (s->c == 0xEF && s->p + 1 < s->buffer_end
        && (unsigned char)s->p[0] == 0xBB && (unsigned char)s->p[1] == 0xBF) {
      next_char(s);
      next_char(s);
      next_char(s);
    }
  }
}

static unsigned
scanner_fstring_debug_capture_current_size_(struct scanner_state *s)
{
  assert(s->fstring_debug.tail_start != NULL);
  unsigned    size = s->fstring_debug.spilled_size;
  const char *current = scanner_current_char_ptr(s);
  if (current != NULL) {
    assert(s->fstring_debug.tail_start != NULL);
    assert(s->fstring_debug.tail_start <= current);
    size += (unsigned)(current - s->fstring_debug.tail_start);
  }
  return size;
}

static void scanner_fstring_debug_capture_copy_range_(struct scanner_state *s,
                                                      unsigned start,
                                                      unsigned end, char *dst)
{
  assert(start <= end);

  unsigned prefix_size = s->fstring_debug.spilled_size;
  char    *prefix = s->fstring_debug.spilled_prefix;

  if (start < prefix_size) {
    unsigned n = prefix_size < end ? prefix_size - start : end - start;
    memcpy(dst, prefix + start, n);
    dst += n;
  }
  if (end > prefix_size) {
    assert(s->fstring_debug.tail_start != NULL);
    unsigned tail_start = start > prefix_size ? start - prefix_size : 0;
    unsigned tail_end = end - prefix_size;
    memcpy(dst, s->fstring_debug.tail_start + tail_start,
           tail_end - tail_start);
  }
}

void scanner_fstring_debug_capture_begin(struct scanner_state *s)
{
  if (s->fstring_debug.depth == 0) {
    s->fstring_debug.spilled_size = 0;
    s->fstring_debug.tail_start = scanner_current_char_ptr(s);
    assert(s->fstring_debug.tail_start != NULL);
  }
  assert(s->fstring_debug.depth < MAX_FSTRING_NESTING);
  s->fstring_debug.starts[s->fstring_debug.depth]
      = scanner_fstring_debug_capture_current_size_(s);
  ++s->fstring_debug.depth;
}

void scanner_fstring_debug_capture_discard(struct scanner_state *s)
{
  assert(s->fstring_debug.depth > 0);
  --s->fstring_debug.depth;
  if (s->fstring_debug.depth == 0) {
    s->fstring_debug.spilled_size = 0;
    s->fstring_debug.tail_start = NULL;
  }
}

union object *scanner_fstring_debug_capture_finish(struct scanner_state *s)
{
  assert(s->fstring_debug.depth > 0);
  unsigned start = s->fstring_debug.starts[s->fstring_debug.depth - 1];
  --s->fstring_debug.depth;
  unsigned end = scanner_fstring_debug_capture_current_size_(s);
  assert(start <= end);
  unsigned captured_size = end - start;

  struct arena *intern_arena = object_intern_arena(s->objects);
  char         *chars = arena_allocate(intern_arena, (size_t)captured_size, 1);
  if (captured_size > 0) {
    scanner_fstring_debug_capture_copy_range_(s, start, end, chars);
  }
  unsigned debug_end = captured_size;
  for (unsigned i = captured_size; i > 0; --i) {
    if (chars[i - 1] == '=') {
      debug_end = i;
      while (debug_end < captured_size
             && (chars[debug_end] == ' ' || chars[debug_end] == '\t'
                 || chars[debug_end] == '\n' || chars[debug_end] == '\r'
                 || chars[debug_end] == '\f')) {
        ++debug_end;
      }
      break;
    }
  }

  if (debug_end < captured_size) {
    captured_size = debug_end;
  }

  if (s->fstring_debug.depth == 0) {
    s->fstring_debug.spilled_size = 0;
    s->fstring_debug.tail_start = NULL;
  }
  return object_intern_string(s->objects, OBJECT_STRING, captured_size, chars);
}

static const char *const token_names[] = {
#define TCHAR(val, name, desc)    [name] = "`" desc "`",
#define TDES_VAL(name, desc, val) [name] = desc,
#define TDES(name, desc)          [name] = desc,
#define TID(id, name)             [name] = "`" #id "`",
#include "pycomparse/tokens.h"
#undef TID
#undef TDES
#undef TDES_VAL
#undef TCHAR
};

const char *token_kind_name(enum token_kind token_kind)
{
  assert(token_kind < sizeof(token_names) / sizeof(token_names[0]));
  assert(token_names[token_kind] != NULL);
  return token_names[token_kind];
}

void print_token(FILE *out, const struct token *token)
{
  enum token_kind kind = token->kind;
  switch (kind) {
  case T_FSTRING_START:
  case T_FSTRING_FRAGMENT:
  case T_FSTRING_END:
  case T_STRING: {
    union object *object = token->u.object;
    if (kind == T_STRING) {
      if (object->type == OBJECT_BYTES) {
        fputs("b\"", out);
      } else {
        fputs("\"", out);
      }
    } else {
      fputs(token_kind_name(kind), out);
      fputs(" \"", out);
    }
    struct object_string *string = &object->string;
    for (const char *c = string->chars, *e = c + string->length; c != e; ++c) {
      /* TODO: more escaping... */
      if (isprint(*c)) {
        fputc(*c, out);
      } else {
        fprintf(out, "\\x%02x", (unsigned char)*c);
      }
    }
    fputc('"', out);
    break;
  }
  case T_FLOAT:
    fprintf(out, "%f", token->u.object->float_obj.value);
    break;
  case T_INTEGER: {
    const union object *integer = token->u.object;
    if (integer->type == OBJECT_INT) {
      const struct object_int *int_obj = &integer->int_obj;
      fprintf(out, "%" PRId64, int_obj->value);
      break;
    }
    if (integer->type == OBJECT_BIG_INT) {
      const struct object_big_int *big_int = &integer->big_int;
      fprintf(out, "<bigint:%upydigits>", big_int->num_pydigits);
      break;
    }
    internal_error("invalid integer object type");
    break;
  }
  case T_IDENTIFIER:
    fprintf(out, "`%s`", token->u.symbol->string);
    break;
  default:
    fprintf(out, "%s", token_kind_name(kind));
    break;
  }
}

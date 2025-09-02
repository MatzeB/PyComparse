#include "scanner.h"
#include "scanner_types.h"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/arena.h"
#include "object_intern.h"
#include "object_types.h"
#include "symbol_table.h"
#include "symbol_table_types.h"
#include "symbol_types.h"
#include "token_kinds.h"

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

static const unsigned TABSIZE = 8;
static const int      C_EOF = -1;

// TODO: Measure if the faster string operations on aligned addresses make up
// for less dense packing...
static const unsigned string_alignment = alignof(void *);

static int __attribute__((noinline)) refill_buffer(struct scanner_state *s)
{
  assert(s->c != C_EOF && "not allowed to advance past EOF");
  size_t read_size = fread(s->read_buffer, 1, s->read_buffer_size, s->input);
  if (read_size < s->read_buffer_size) {
    if (ferror(s->input)) {
      fprintf(stderr, "Error: Read Error! TODO: report error\n");
    }

    if (read_size == 0) {
      return C_EOF;
    }
  }
  s->p = s->read_buffer + 1;
  s->buffer_end = s->read_buffer + read_size;
  return *s->read_buffer;
}

static void next_char(struct scanner_state *s)
{
  s->c = UNLIKELY(s->p >= s->buffer_end) ? refill_buffer(s) : *(s->p++);
}

static void eat(struct scanner_state *s, int c)
{
  (void)c;
  assert(s->c == c);
  next_char(s);
}

static void scan_line_comment(struct scanner_state *s)
{
  eat(s, '#');
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
      return;
    case C_EOF:
      return;
    default:
      next_char(s);
      continue;
    }
  }
}

static void scan_identifier(struct scanner_state *s, char first_char,
                            char second_char)
{
  struct arena *arena = &s->symbol_table->arena;
  arena_grow_begin(arena, string_alignment);
  arena_grow_char(arena, first_char);
  if (second_char != 0) arena_grow_char(arena, second_char);
  for (;;) {
    switch (s->c) {
    case IDENTIFIER_CASES:
      arena_grow_char(arena, (char)s->c);
      next_char(s);
      continue;
    default:
      break;
    }
    break;
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

static void scan_hexinteger(struct scanner_state *s, struct arena *arena)
{
  arena_grow_char(arena, 'x');
  for (;;) {
    next_char(s);
    switch (s->c) {
    case HEX_DIGIT_CASES:
    case '_':
      break;
    default:
      return;
    }
    arena_grow_char(arena, (char)s->c);
  }
}

static void scan_octinteger(struct scanner_state *s, struct arena *arena)
{
  (void)s;
  (void)arena;
  abort();
}

static void scan_bininteger(struct scanner_state *s, struct arena *arena)
{
  (void)s;
  (void)arena;
  abort();
}

static void scan_decinteger_zero(struct scanner_state *s, struct arena *arena)
{
  for (;;) {
    switch (s->c) {
    case '0':
    case '_':
      break;
    default:
      return;
    }
    arena_grow_char(arena, (char)s->c);
    next_char(s);
  }
}

static void scan_decinteger(struct scanner_state *s, struct arena *arena)
{
  for (;;) {
    next_char(s);
    switch (s->c) {
    case DIGIT_CASES:
    case '_':
      break;
    default:
      return;
    }
    arena_grow_char(arena, (char)s->c);
  }
}

static void scan_float_fraction(struct scanner_state *s)
{
  struct arena *strings = s->strings;

  /* decimal beginning already on growing arena */
  while ('0' <= s->c && s->c <= '9') {
    arena_grow_char(strings, s->c);
    next_char(s);
  }

  if (s->c == 'e' || s->c == 'E') {
    arena_grow_char(strings, s->c);
    next_char(s);
    if (s->c == '+' || s->c == '-') {
      arena_grow_char(strings, s->c);
      next_char(s);
    }
    if (s->c < '0' || s->c > '9') {
      // TODO: show error
      abort();
    }
    do {
      arena_grow_char(strings, s->c);
      next_char(s);
    } while ('0' <= s->c && s->c <= '9');
  }
  arena_grow_char(strings, '\0');
  char *chars = (char *)arena_grow_finish(strings);

  char *endptr;
  errno = 0;
  double value = strtod(chars, &endptr);
  if (endptr == chars || *endptr != '\0') {
    abort(); // TODO: error handling
  }
  if (errno != 0) {
    abort(); // TODO: error handling
  }
  arena_free_to(strings, chars);

  s->token.u.object = object_intern_float(s->objects, value);
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

static void scan_number(struct scanner_state *s)
{
  struct arena *strings = s->strings;
  arena_grow_begin(strings, alignof(char));
  arena_grow_char(strings, (char)s->c);
  if (s->c == '0') {
    next_char(s);
    switch (s->c) {
    case 'x':
    case 'X':
      scan_hexinteger(s, strings);
      break;
    case 'b':
      scan_bininteger(s, strings);
      break;
    case 'o':
      scan_octinteger(s, strings);
      break;
    default:
      scan_decinteger_zero(s, strings);
      break;
    }
  } else {
    scan_decinteger(s, strings);
  }

  if (s->c == '.') {
    arena_grow_char(strings, s->c);
    next_char(s);
    scan_float_fraction(s);
    return;
  } else if (s->c == 'e' || s->c == 'E') {
    scan_float_fraction(s);
    return;
  }
  arena_grow_char(strings, '\0');
  char *chars = (char *)arena_grow_finish(strings);

  char *endptr;
  errno = 0;
  long value = strtol(chars, &endptr, 0);
  if (endptr == chars || *endptr != '\0') {
    /* TODO: report error */
    abort();
  }
  if ((value == LONG_MIN || value == LONG_MAX) && errno != 0) {
    /* TODO: report error */
    abort();
  }
  arena_free_to(strings, chars);

  assert(INT64_MIN <= value && value <= INT64_MAX);
  s->token.u.object = object_intern_int(s->objects, (int64_t)value);
  s->token.kind = T_INTEGER;
}

static void scan_escape_sequence(struct scanner_state *s,
                                 struct arena *strings, bool is_unicode)
{
  assert(s->c == '\\');
  next_char(s);

  char decoded_single;
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
  case '7': {
    int num = s->c - '0';
    int num_digits = 1;
    for (;;) {
      next_char(s);
      if (s->c < '0' || s->c > '7' || ++num_digits > 3) {
        break;
      }
      num = (num * 8) + (s->c - '0');
    }
    if (num > 0377) {
      /* TODO report warning */
      abort();
    }
    if (num > 127 && is_unicode) {
      /* TODO: encode utf-8 in string */
      abort();
    }
    arena_grow_char(strings, (char)num);
    return;
  }
  case 'x': {
    next_char(s);
    int number = 0;
    int num_digits = 0;
    do {
      int digit;
      if ('0' <= s->c && s->c <= '9') {
        digit = s->c - '0';
      } else if ('a' <= s->c && s->c <= 'f') {
        digit = 10 + (s->c - 'a');
      } else if ('A' <= s->c && s->c <= 'F') {
        digit = 10 + (s->c - 'A');
      } else {
        abort();
        /* TODO: report invalid hex digit */
        break;
      }
      next_char(s);
      number *= 16;
      number += digit;
      ++num_digits;
    } while (num_digits < 2);
    if (is_unicode && number > 127) {
      abort(); /* TODO: encode */
    } else {
      arena_grow_char(strings, (char)number);
    }
    return;
  }
  case 'N':
    /* TODO */
    abort();
  case 'u':
    /* TODO */
    abort();
  case 'U':
    /* TODO */
    abort();
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
    /* TODO: report error */
    abort();
  default:
    /* TODO: report error */
    arena_grow_char(strings, '\\');
    arena_grow_char(strings, s->c);
    next_char(s);
    return;
  }
  next_char(s);
  arena_grow_char(strings, decoded_single);
}

static void scan_string_literal(struct scanner_state *s, uint16_t token_kind,
                                bool is_unicode, bool is_raw)
{
  assert(s->c == '"' || s->c == '\'');
  int quote = s->c;
  next_char(s);

  struct arena *strings = s->strings;
  s->token.kind = token_kind;
  arena_grow_begin(strings, alignof(char));
  bool triplequote = false;
  if (s->c == quote) {
    next_char(s);
    if (s->c == quote) {
      next_char(s);
      triplequote = true;
    } else {
      goto finish_string;
    }
  }
  for (;;) {
    /* vectorize this search? */
    switch (s->c) {
    case '"':
    case '\'': {
      if (s->c != quote) {
        break;
      }
      next_char(s);
      if (!triplequote) goto finish_string;
      if (s->c != quote) {
        arena_grow_char(strings, quote);
        continue;
      }
      next_char(s);
      if (s->c != quote) {
        arena_grow_char(strings, quote);
        arena_grow_char(strings, quote);
        continue;
      }
      next_char(s);
      goto finish_string;
    }
    case '\\':
      if (is_raw) {
        break;
      }
      scan_escape_sequence(s, strings, is_unicode);
      continue;
    case '\r':
      next_char(s);
      if (s->c == '\n') next_char(s);
      goto new_line;
    case '\n':
      next_char(s);
      goto new_line;
    new_line:
      ++s->line;
      if (!triplequote) {
        fprintf(stderr, "TODO: error unterminated string literal\n");
        abort();
      }
      arena_grow_char(strings, '\n');
      continue;
    case C_EOF:
      // TODO: complain
      fprintf(stderr, "TODO\n");
      abort();
    default:
      break;
    }
    arena_grow_char(strings, (char)s->c);
    next_char(s);
  }

finish_string:;
  size_t           length = arena_grow_current_size(strings);
  char            *chars = (char *)arena_grow_finish(strings);
  enum object_type type = is_unicode ? OBJECT_ASCII : OBJECT_BYTES;
  union object *object = object_intern_string(s->objects, type, length, chars);
  if (object->string.chars != chars) {
    arena_free_to(strings, chars);
  }
  s->token.u.object = object;
  s->token.kind = T_STRING;
}

static void scan_eof(struct scanner_state *s)
{
  /* Add artificial newline, for EOF in the middle of a line. */
  if (!s->at_begin_of_line) {
    s->at_begin_of_line = true;
    s->token.kind = T_NEWLINE;
    return;
  }

  if (s->last_line_indent > 0) {
    assert(s->indentation_stack_top > 0);
    s->last_line_indent = 0;
    s->pending_dedents = s->indentation_stack_top - 1;
    s->token.kind = T_DEDENT;
    if (s->pending_dedents == 0) {
      s->at_begin_of_line = false;
    }
    return;
  }

  s->token.kind = T_EOF;
}

static bool scan_indentation(struct scanner_state *s)
{
  assert(s->at_begin_of_line);
  if (s->pending_dedents > 0) {
    assert(s->indentation_stack_top > 0);
    --s->indentation_stack_top;
    --s->pending_dedents;
    if (s->pending_dedents == 0) {
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
      next_char(s);
      ++column;
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
      column = 0;
      continue;
    case C_EOF:
      scan_eof(s);
      return true;
    default:
      break;
    }
    break;
  }

  unsigned last_line_indent = s->last_line_indent;
  if (column > last_line_indent) {
    if (s->indentation_stack_top >= MAXINDENT) {
      s->token.kind = T_ETOODEEP;
      s->c = C_EOF;
    } else {
      s->indentation_stack[s->indentation_stack_top++] = last_line_indent;
      s->last_line_indent = column;
      s->token.kind = T_INDENT;
      s->at_begin_of_line = false;
    }
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
    s->at_begin_of_line = pending_dedents > 0;
    if (column != target_cols) {
      fprintf(stderr, "%d: invalid indentation\n", s->line);
      s->token.kind = T_EDEDENT;
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
restart:
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
      if (s->paren_level > 0) {
        continue;
      }
      s->at_begin_of_line = true;
      s->token.kind = T_NEWLINE;
      return;

    case ' ':
    case '\t':
    case '\014':
      next_char(s);
      continue;

    case '#':
      scan_line_comment(s);
      s->at_begin_of_line = true;
      goto restart;

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
      bool is_raw = false;
      char second_char = (char)s->c;
      if (second_char == 'r' || second_char == 'R') {
        next_char(s);
        is_raw = true;
      } else {
        second_char = 0;
      }
      if (s->c == '"' || s->c == '\'') {
        scan_string_literal(s, T_BYTE_STRING, /*is_unicode=*/false,
                            /*is_raw=*/is_raw);
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
      if (s->c == '"' || s->c == '\'') {
        scan_string_literal(s, T_FORMAT_STRING, /*is_unicode=*/true,
                            /*is_raw=*/false);
        return;
      } else {
        scan_identifier(s, first_char, /*second_char=*/0);
      }
      return;
    }

    case 'r':
    case 'R': {
      char first_char = (char)s->c;
      next_char(s);
      bool is_unicode = true;
      char second_char = (char)s->c;
      if (second_char == 'b' || second_char == 'B') {
        next_char(s);
        is_unicode = false;
      } else {
        second_char = 0;
      }
      if (s->c == '"' || s->c == '\'') {
        scan_string_literal(s, T_RAW_STRING, /*is_unicode=*/is_unicode,
                            /*is_raw=*/true);
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
        scan_string_literal(s, T_UNICODE_STRING, /*is_unicode=*/true,
                            /*is_raw=*/false);
        return;
      } else {
        scan_identifier(s, first_char, /*second_char=*/0);
      }
      return;
    }

    case '\'':
    case '"':
      scan_string_literal(s, T_STRING, /*is_unicode=*/true, /*is_raw=*/false);
      return;

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
      invalid_c = s->c;
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        s->token.kind = T_EXCLAMATIONMARK_EQUALS;
      } else {
        goto invalid_char;
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
          // TODO: pushback? just for this?
          s->token.kind = T_DOT_DOT_DOT;
        }
        return;
      default:
        s->token.kind = T_DOT;
        return;
      }

    case ':':
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

    case ')':
    case ']':
    case '}':
      --s->paren_level;
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
        continue;
      }
      /* report error? */
      abort();

    case C_EOF:
      scan_eof(s);
      return;

    default:
      if (s->c >= 128) {
        // TODO: unicode caracters
      }
      invalid_c = s->c;
      next_char(s);
    invalid_char:
      fprintf(stderr, "%s:%u error: Unexpected input char '%c'\n", s->filename,
              s->line, invalid_c);
      return;
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
                  struct object_intern *objects, struct arena *strings)
{
  memset(s, 0, sizeof(*s));
  s->filename = filename;
  s->line = 1;
  s->input = input;
  s->at_begin_of_line = true;
  s->read_buffer = malloc(16 * 1024 - 16);
  s->read_buffer_size = 4096;
  s->symbol_table = symbol_table;
  s->objects = objects;
  s->strings = strings;
  next_char(s);
}

void scanner_free(struct scanner_state *s)
{
  free(s->read_buffer);
}

static const char *const token_names[] = {
#define TCHAR(val, name, desc)    [name] = "`" desc "`",
#define TDES_VAL(name, desc, val) [name] = desc,
#define TDES(name, desc)          [name] = desc,
#define TID(id, name)             [name] = "`" #id "`",
#include "tokens.h"
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
  switch (token->kind) {
  case T_FORMAT_STRING:
    fputc('f', out);
    goto string;
  case T_RAW_STRING:
    fputc('r', out);
    goto string;
  case T_UNICODE_STRING:
    fputc('u', out);
    goto string;
  case T_BYTE_STRING:
    fputc('b', out);
    goto string;
  case T_STRING:
  string:
    fputc('"', out);
    struct object_string *string = &token->u.object->string;
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
  case T_FLOAT:
    fprintf(out, "%f", token->u.object->float_obj.value);
    break;
  case T_INTEGER:
    fprintf(out, "%" PRId64, token->u.object->int_obj.value);
    break;
  case T_IDENTIFIER:
    fprintf(out, "`%s`", token->u.symbol->string);
    break;
  default:
    fprintf(out, "%s", token_kind_name(token->kind));
    break;
  }
}

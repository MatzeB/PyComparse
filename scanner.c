#include "scanner.h"

#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/arena.h"
#include "symbol_table.h"
#include "token_kinds.h"

#define UNLIKELY(x)    __builtin_expect((x), 0)

#define IDENTIFIER_START_CASES_WITHOUT_B_F_R_U \
  'A': \
  case 'C': \
  case 'D': \
  case 'E': \
  case 'G': \
  case 'H': \
  case 'I': \
  case 'J': \
  case 'K': \
  case 'L': \
  case 'M': \
  case 'N': \
  case 'O': \
  case 'P': \
  case 'Q': \
  case 'S': \
  case 'T': \
  case 'V': \
  case 'W': \
  case 'X': \
  case 'Y': \
  case 'Z': \
  case '_': \
  case 'a': \
  case 'c': \
  case 'd': \
  case 'e': \
  case 'g': \
  case 'h': \
  case 'i': \
  case 'j': \
  case 'l': \
  case 'm': \
  case 'n': \
  case 'o': \
  case 'p': \
  case 'q': \
  case 's': \
  case 't': \
  case 'v': \
  case 'w': \
  case 'x': \
  case 'y': \
  case 'z'

#define DIGIT_CASES \
  '0': \
  case '1': \
  case '2': \
  case '3': \
  case '4': \
  case '5': \
  case '6': \
  case '7': \
  case '8': \
  case '9'

#define IDENTIFIER_CASES \
  DIGIT_CASES: \
  case IDENTIFIER_START_CASES_WITHOUT_B_F_R_U: \
  case 'B': \
  case 'F': \
  case 'R': \
  case 'U': \
  case 'b': \
  case 'f': \
  case 'r': \
  case 'u'

static const unsigned TABSIZE   = 8;
static const int      C_EOF     = -1;

// TODO: Measure if the faster string operations on aligned addresses make up
// for less dense packing...
static const unsigned string_alignment = alignof(void*);

static int __attribute__((noinline)) refill_buffer(struct scanner_state *s)
{
  assert(s->c != C_EOF && "not allowed to advance past EOF");
  size_t read_size = fread(s->read_buffer, 1, s->read_buffer_size,
                           s->input);
  if (read_size < s->read_buffer_size) {
    if (ferror(s->input)) {
      fprintf(stderr, "Error: Read Error! TODO: report error\n");
    }

    if (read_size == 0) {
      return C_EOF;
    }
  }
  s->p          = s->read_buffer + 1;
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

static void eat_line_comment(struct scanner_state *s)
{
  eat(s, '#');
  for (;;) {
    switch (s->c) {
    case '\r':
      next_char(s);
      if (s->c == '\n') {
        /* fallthrough */
    case '\n':
        next_char(s);
      }
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

static void scan_identifier(struct scanner_state *s, char first_char)
{
  struct arena *arena = &s->symbol_table->arena;
  arena_grow_begin(arena, string_alignment);
  arena_grow_char(arena, first_char);
  for (;;) {
    switch (s->c) {
    case IDENTIFIER_CASES:
      arena_grow_char(arena, s->c);
      next_char(s);
      continue;
    default:
      break;
    }
    break;
  }

  arena_grow_char(arena, '\0');
  char *string = arena_grow_finish(arena);
  struct symbol *symbol = symbol_table_get_or_insert(s->symbol_table, string);
  if (symbol->string != string) {
    arena_free(arena, string);
  }

  s->token.kind     = symbol->token_kind;
  s->token.u.symbol = symbol;
}

static void scan_string_literal(struct scanner_state *s, uint16_t token_kind)
{
  assert(s->c == '"' || s->c == '\'');
  char quote = s->c;
  next_char(s);

  s->token.kind = token_kind;
  struct arena *arena = s->strings;
  arena_grow_begin(arena, alignof(char));
  bool triplequote = false;
  if (s->c == quote) {
    next_char(s);
    if (s->c == quote) {
      next_char(s);
      triplequote = true;
      fprintf(stderr, "TODO\n");
      abort();
    } else {
      goto finish_string;
    }
  }
  for (;;) {
    switch (s->c) {
    case '"':
    case '\'': {
      bool end = s->c == quote;
      // TODO: check if it matches the begin
      next_char(s);
      if (end)
        break;
      continue;
    }
    case '\\':
      next_char(s);
      next_char(s);
      fprintf(stderr, "TODO\n");
      abort();
    case '\r':
      next_char(s);
      if (s->c == '\n') {
        /* fallthrough */
    case '\n':
        next_char(s);
      }
      ++s->line;
      fprintf(stderr, "TODO: complain about newline\n");
      abort();
      break;
    case C_EOF:
      // TODO: complain
      fprintf(stderr, "TODO\n");
      abort();
    default:
      if (s->c & 0x80) {
        fprintf(stderr, "TODO: non-ascii\n");
        abort();
      }
      arena_grow_char(arena, s->c);
      next_char(s);
      continue;
    }
    break;
  }

finish_string:
  arena_grow_char(arena, '\0');
  s->token.u.string = (char*)arena_grow_finish(arena);
}

static void scan_eof(struct scanner_state *s)
{
  if (s->last_line_indent > 0) {
    s->last_line_indent = 0;
    s->pending_dedents  = s->indentation_stack_top;
    s->at_begin_of_line = true;
    s->token.kind = T_DEDENT;
    return;
  }
  s->token.kind = T_EOF;
}

static bool scan_indentation(struct scanner_state *s)
{
  if (s->pending_dedents > 0) {
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
      if (s->c == '\n') {
        /* fallthrough */
    case '\n':
        next_char(s);
      }
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
      eat_line_comment(s);
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
      if (last_line_indent > 0) {
        s->indentation_stack[s->indentation_stack_top++] = last_line_indent;
      }
      s->last_line_indent = column;
      s->token.kind = T_INDENT;
      s->at_begin_of_line = false;
    }
    return true;
  }
  if (column < last_line_indent) {
    unsigned pending_dedents = 0;
    for (unsigned t = s->indentation_stack_top; t-- > 0;) {
      if (column <= s->indentation_stack[t]) {
        ++pending_dedents;
      }
    }
    unsigned target_cols = pending_dedents == s->indentation_stack_top
      ? 0
      : s->indentation_stack[s->indentation_stack_top - pending_dedents + 1];
    s->pending_dedents  = pending_dedents;
    s->last_line_indent = column;
    s->at_begin_of_line = pending_dedents > 0;
    if (column != target_cols) {
      fprintf(stderr, "invalid indentation\n");
      s->token.kind = T_EDEDENT;
    } else {
      s->token.kind = T_DEDENT;
    }
    return true;
  }
  s->at_begin_of_line = false;
  return false;
}

void next_token(struct scanner_state *s)
{
  if (s->at_begin_of_line) {
    if (scan_indentation(s))
      return;
  }

  unsigned invalid_c;
  for (;;) {
    switch (s->c) {
    case '\r':
      next_char(s);
      if (s->c == '\n') {
        /* fallthrough */
    case '\n':
        next_char(s);
      }
      ++s->line;
      if (s->paren_level > 0)
        continue;
      s->at_begin_of_line = true;
      s->token.kind = T_NEWLINE;
      return;

    case ' ':
    case '\t':
    case '\014':
      next_char(s);
      continue;

    case '#':
      eat_line_comment(s);
      continue;

    case IDENTIFIER_START_CASES_WITHOUT_B_F_R_U: {
      char first_char = s->c;
      next_char(s);
      scan_identifier(s, first_char);
      return;
    }

    case 'b':
    case 'B': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        scan_string_literal(s, T_BINARY_STRING);
        return;
      } else {
        scan_identifier(s, first_char);
      }
      return;
    }

    case 'f':
    case 'F': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        scan_string_literal(s, T_FORMAT_STRING);
        return;
      } else {
        scan_identifier(s, first_char);
      }
      return;
    }

    case 'r':
    case 'R': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        scan_string_literal(s, T_RAW_STRING);
        return;
      } else {
        scan_identifier(s, first_char);
      }
      return;
    }

    case 'u':
    case 'U': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        scan_string_literal(s, T_UNICODE_STRING);
        return;
      } else {
        scan_identifier(s, first_char);
      }
      return;
    }

    case '\'':
    case '"':
      scan_string_literal(s, T_STRING);
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
        s->token.kind = T_EXCLAMATIONMARKEQUALS;
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
        s->token.kind = T_PERCENT_EQUALS;
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
    case ':':
    case ';':
    case '~':
single_char_token:
      s->token.kind = s->c;
      next_char(s);
      return;

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
      fprintf(stderr, "Unexpected input char '%c'\n", invalid_c);
      return;
    }
  }
}

void scanner_init(struct scanner_state *s, FILE *input,
                  struct symbol_table *symbol_table, struct arena *strings)
{
  memset(s, 0, sizeof(*s));
  s->input            = input,
  s->at_begin_of_line = true,
  s->read_buffer      = malloc(16 * 1024 - 16),
  s->read_buffer_size = 4096,
  s->symbol_table     = symbol_table,
  s->strings          = strings,
  next_char(s);
}

void scanner_free(struct scanner_state *s)
{
  free(s->read_buffer);
}

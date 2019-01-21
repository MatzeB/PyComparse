#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "adt/hashset.h"
#include "adt/obst.h"

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

enum token_kind {
#define TCHAR(val, name, desc)  name = val,
#define TDES(name, desc)        name,
#define TID(id, name)           name,
#include "tokens.h"
#undef TID
#undef TDES
#undef TCHAR
};

struct symbol {
  const char *string;
  uint16_t    token_kind;
};

struct symbol_table_bucket {
  struct symbol *symbol;
  unsigned       hash;
};

struct symbol_table {
  struct hash_set             set;
  struct symbol_table_bucket *buckets;
  struct obstack              obst;
};

static unsigned fnv_hash_string(const char *string)
{
  unsigned hash = 2166136261;
  for(const char *p = string; *p != '\0'; ++p) {
    hash *= 16777619;
    hash ^= (unsigned char)*p;
  }
  return hash;
}

static struct symbol *symbol_table_new_symbol(struct symbol_table *symbol_table,
                                              const char *string,
                                              uint16_t token_kind)
{
  struct symbol *symbol = obstack_alloc(&symbol_table->obst, sizeof(symbol[0]));
  symbol->string     = string;
  symbol->token_kind = token_kind;
  return symbol;
}

static struct symbol *symbol_table_insert_new(struct symbol_table *symbol_table,
                                              struct symbol *symbol,
                                              unsigned hash)
{
  hash_set_increment_num_elements(&symbol_table->set);
  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &symbol_table->set, hash);
  struct symbol_table_bucket *buckets = symbol_table->buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct symbol_table_bucket *bucket = &buckets[c.index];
    if (bucket->symbol == NULL) {
      bucket->symbol = symbol;
      bucket->hash   = hash;
      return symbol;
    }
  }
}

static void symbol_table_resize(struct symbol_table *symbol_table,
                                unsigned new_size)
{
  struct symbol_table_bucket *old_buckets = symbol_table->buckets;
  unsigned num_old_buckets = hash_set_num_buckets(&symbol_table->set);
  symbol_table->buckets = calloc(new_size, sizeof(symbol_table->buckets[0]));
  hash_set_init(&symbol_table->set, new_size);
  for (unsigned i = 0; i < num_old_buckets; ++i) {
    struct symbol_table_bucket *bucket = &old_buckets[i];
    if (bucket->symbol != NULL) {
      symbol_table_insert_new(symbol_table, bucket->symbol, bucket->hash);
    }
  }
}

static struct symbol *symbol_table_get_or_insert(
    struct symbol_table *symbol_table, const char *string)
{
  unsigned new_size = hash_set_should_resize(&symbol_table->set);
  if (UNLIKELY(new_size != 0)) {
    symbol_table_resize(symbol_table, new_size);
  }

  unsigned hash = fnv_hash_string(string);

  struct hash_set_chain_iteration_state c;
  hash_set_chain_iteration_begin(&c, &symbol_table->set, hash);
  struct symbol_table_bucket *buckets = symbol_table->buckets;
  for (;; hash_set_chain_iteration_next(&c)) {
    struct symbol_table_bucket *bucket = &buckets[c.index];
    if (bucket->symbol == NULL) {
      // not found: create a new entry.
      struct symbol *symbol =
          symbol_table_new_symbol(symbol_table, string, T_ID);
      bucket->symbol = symbol;
      bucket->hash = hash;
      return symbol;
    } else if (bucket->hash == hash &&
               strcmp(bucket->symbol->string, string) == 0) {
      return bucket->symbol;
    }
  }
}

static void symbol_table_insert_predefined(struct symbol_table *symbol_table,
                                           const char *string,
                                           uint16_t token_kind)
{
  struct symbol *symbol =
      symbol_table_new_symbol(symbol_table, string, token_kind);
  unsigned hash = fnv_hash_string(string);
  symbol_table_insert_new(symbol_table, symbol, hash);
}

static void init_symbol_table(struct symbol_table *symbol_table)
{
  obstack_init(&symbol_table->obst);
  unsigned num_buckets = 512;
  hash_set_init(&symbol_table->set, num_buckets);
  symbol_table->buckets = calloc(num_buckets, sizeof(symbol_table->buckets[0]));

#define TCHAR(val, name, desc)
#define TDES(name, desc)
#define TID(id, name)  symbol_table_insert_predefined(symbol_table, #id, name);
#include "tokens.h"
#undef TID
#undef TDES
#undef TCHAR
}

static void exit_symbol_table(struct symbol_table *symbol_table)
{
  obstack_free(&symbol_table->obst, NULL);
}

static const unsigned TABSIZE   = 8;
static const int      C_EOF     = -1;
static const unsigned MAXINDENT = 100;

struct token {
  uint16_t       kind;
  struct symbol *symbol;
};

struct scanner_state {
  int c;

  char  *p;
  char  *buffer_end;
  FILE  *input;
  char  *read_buffer;
  size_t read_buffer_size;

  unsigned line;
  unsigned paren_level;

  struct symbol_table *symbol_table;

  bool     at_begin_of_line;
  uint8_t  pending_dedents;
  unsigned last_line_indent;
  unsigned indentation_stack_top;
  unsigned indentation_stack[MAXINDENT];
};

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

static void parse_identifier(struct scanner_state *s, struct token *result,
                             char first_char)
{
  struct obstack *symbol_obst = &s->symbol_table->obst;
  assert(obstack_object_size(symbol_obst) == 0);
  obstack_1grow(symbol_obst, first_char);
  for (;;) {
    switch (s->c) {
    case IDENTIFIER_CASES:
      obstack_1grow(symbol_obst, s->c);
      next_char(s);
      continue;
    default:
      break;
    }
    break;
  }

  obstack_1grow(symbol_obst, '\0');
  char *string = obstack_finish(symbol_obst);
  struct symbol *symbol = symbol_table_get_or_insert(s->symbol_table, string);
  if (symbol->string != string) {
    obstack_free(symbol_obst, string);
  }

  result->kind   = symbol->token_kind;
  result->symbol = symbol;
}

static void parse_string_literal(struct scanner_state *s, struct token *result,
                                 uint16_t token_kind)
{
  assert(s->c == '"' || s->c == '\'');
  // TODO: check for triple quotes
  next_char(s);
  for (;;) {
    switch (s->c) {
    case '"':
    case '\'':
      // TODO: check if it matches the begin
      next_char(s);
      break;
    case '\\':
      next_char(s);
      next_char(s);
      continue;
    case C_EOF:
      // TODO: complain
      break;
    default:
      next_char(s);
      continue;
    }
    break;
  }
  result->kind = token_kind;
  // TODO
}

static void eof(struct scanner_state *s, struct token *result)
{
  if (s->last_line_indent > 0) {
    s->last_line_indent = 0;
    s->pending_dedents  = s->indentation_stack_top;
    s->at_begin_of_line = true;
    result->kind = T_DEDENT;
    return;
  }
  result->kind = T_EOF;
}

static bool parse_indentation(struct scanner_state *s, struct token *result)
{
  (void)result;

  if (s->pending_dedents > 0) {
    --s->indentation_stack_top;
    --s->pending_dedents;
    if (s->pending_dedents == 0) {
      s->at_begin_of_line = false;
    }
    result->kind = T_DEDENT;
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
      eof(s, result);
      return true;
    default:
      break;
    }
    break;
  }

  unsigned last_line_indent = s->last_line_indent;
  if (column > last_line_indent) {
    if (s->indentation_stack_top >= MAXINDENT) {
      result->kind = T_ETOODEEP;
      s->c = C_EOF;
    } else {
      if (last_line_indent > 0) {
        s->indentation_stack[s->indentation_stack_top++] = last_line_indent;
      }
      s->last_line_indent = column;
      result->kind = T_INDENT;
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
      result->kind = T_EDEDENT;
    } else {
      result->kind = T_DEDENT;
    }
    return true;
  }
  s->at_begin_of_line = false;
  return false;
}

static __attribute__((noinline))
void next_token(struct scanner_state *s, struct token *result)
{
  if (s->at_begin_of_line) {
    if (parse_indentation(s, result))
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
      result->kind = T_NEWLINE;
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
      parse_identifier(s, result, first_char);
      return;
    }

    case 'b':
    case 'B': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        parse_string_literal(s, result, T_BINARY_STRING);
        return;
      } else {
        parse_identifier(s, result, first_char);
      }
      return;
    }

    case 'f':
    case 'F': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        parse_string_literal(s, result, T_FORMAT_STRING);
        return;
      } else {
        parse_identifier(s, result, first_char);
      }
      return;
    }

    case 'r':
    case 'R': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        parse_string_literal(s, result, T_RAW_STRING);
        return;
      } else {
        parse_identifier(s, result, first_char);
      }
      return;
    }

    case 'u':
    case 'U': {
      char first_char = s->c;
      next_char(s);
      if (s->c == '"') {
        parse_string_literal(s, result, T_UNICODE_STRING);
        return;
      } else {
        parse_identifier(s, result, first_char);
      }
      return;
    }

    case '\'':
    case '"':
      parse_string_literal(s, result, T_STRING);
      return;

    case '=':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_EQUALS_EQUALS;
      } else {
        result->kind = T_EQUALS;
      }
      return;

    case '!':
      invalid_c = s->c;
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_EXCLAMATIONMARKEQUALS;
      } else {
        goto invalid_char;
      }
      return;

    case '<':
      next_char(s);
      switch (s->c) {
      case '>':
        next_char(s);
        result->kind = T_LESS_THAN_GREATER_THAN;
        return;
      case '=':
        next_char(s);
        result->kind = T_LESS_THAN_EQUALS;
        return;
      case '<':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          result->kind = T_LESS_THAN_LESS_THAN_EQUALS;
        } else {
          result->kind = T_LESS_THAN_LESS_THAN;
        }
        return;
      default:
        result->kind = T_LESS_THAN;
        return;
      }

    case '>':
      next_char(s);
      switch (s->c) {
      case '>':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          result->kind = T_GREATER_THAN_GREATER_THAN_EQUALS;
        } else {
          result->kind = T_GREATER_THAN_GREATER_THAN;
        }
        return;
      case '=':
        result->kind = T_GREATER_THAN_EQUALS;
        return;
      default:
        result->kind = T_GREATER_THAN;
        return;
      }

    case '+':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_PLUS_EQUALS;
      } else {
        result->kind = T_PLUS;
      }
      return;

    case '-':
      next_char(s);
      switch (s->c) {
      case '=':
        next_char(s);
        result->kind = T_MINUS_EQUALS;
        return;
      case '>':
        next_char(s);
        result->kind = T_MINUS_GREATER_THAN;
        return;
      default:
        result->kind = T_MINUS;
        return;
      }

    case '*':
      next_char(s);
      switch (s->c) {
      case '=':
        next_char(s);
        result->kind = T_ASTERISK_EQUALS;
        return;
      case '*':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          result->kind = T_ASTERISK_ASTERISK_EQUALS;
        } else {
          result->kind = T_ASTERISK_ASTERISK;
        }
        return;
      default:
        result->kind = T_ASTERISK;
        return;
      }

    case '/':
      next_char(s);
      switch (s->c) {
      case '=':
        next_char(s);
        result->kind = T_SLASH_EQUALS;
        return;
      case '/':
        next_char(s);
        if (s->c == '=') {
          next_char(s);
          result->kind = T_SLASH_SLASH_EQUALS;
        } else {
          result->kind = T_SLASH_SLASH;
        }
        return;
      default:
        result->kind = T_SLASH;
        return;
      }

    case '|':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_BAR_EQUALS;
      } else {
        result->kind = T_BAR;
      }
      return;

    case '%':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_PERCENT_EQUALS;
      } else {
        result->kind = T_PERCENT_EQUALS;
      }
      return;

    case '&':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_AMPERSAND_EQUALS;
      } else {
        result->kind = T_AMPERSAND;
      }
      return;

    case '^':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_CARET_EQUALS;
      } else {
        result->kind = T_CARET;
      }
      return;

    case '@':
      next_char(s);
      if (s->c == '=') {
        next_char(s);
        result->kind = T_AT_EQUALS;
      } else {
        result->kind = T_AT;
      }
      return;

    case '.':
      next_char(s);
      switch (s->c) {
      case '.':
        next_char(s);
        if (s->c == '.') {
          next_char(s);
          result->kind = T_DOT_DOT_DOT;
        } else {
          // TODO: pushback? just for this?
          result->kind = T_DOT_DOT_DOT;
        }
        return;
      default:
        result->kind = T_DOT;
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
      result->kind = s->c;
      next_char(s);
      return;

    case C_EOF:
      eof(s, result);
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

static const char*const token_names[] = {
#define TCHAR(val, name, desc)  TDES(name, desc)
#define TDES(name, desc)        [name] = desc,
#define TID(id, name)           [name] = #id,
#include "tokens.h"
#undef TID
#undef TDES
#undef TCHAR
};

int main(int argc, char **argv)
{
  FILE *input = stdin;
  if (argc > 1) {
    const char *filename = argv[1];
    input = fopen(filename, "r");
    if (input == NULL) {
      fprintf(stderr, "Failed to open '%s' TODO: print error\n", filename);
      return 1;
    }
  }

  struct symbol_table symbol_table;
  init_symbol_table(&symbol_table);

  struct scanner_state s = {
    .input            = input,
    .at_begin_of_line = true,
    .read_buffer      = malloc(4096),
    .read_buffer_size = 4096,
    .symbol_table     = &symbol_table,
  };
  next_char(&s);
  struct token t;
  do {
    next_token(&s, &t);
    if (t.kind == T_ID) {
      printf("identifier %s\n", t.symbol->string);
    } else {
      printf("%s\n", token_names[t.kind]);
    }
  } while(t.kind != T_EOF);

  if (input != stdin) {
    fclose(input);
  }

  exit_symbol_table(&symbol_table);
  return 0;
}

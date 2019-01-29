#include "parser.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "adt/arena.h"
#include "scanner.h"
#include "token_kinds.h"
#include "writer.h"

#define UNLIKELY(x)    __builtin_expect((x), 0)

#define EXPRESSION_START_CASES \
  T_IDENTIFIER:  \
  case T_STRING: \
  case T_RAW_STRING: \
  case T_UNICODE_STRING: \
  case T_FORMAT_STRING: \
  case T_BYTE_STRING

enum precedence {
  PREC_TEST,
  PREC_LOGICAL_OR,  /* OR               left to right */
  PREC_LOGICAL_AND, /* AND              left to right */
  PREC_LOGICAL_NOT, /* prefix NOT */
  PREC_COMPARISON,  /* <, >, ==, >=, <=, <>, !=, in, not in, is, is not */

  PREC_XOR,         /* ^                left to right */
  PREC_AND,         /* &                left to right */
  PREC_SHIFT,       /* <<, >>           left to right */
  PREC_ARITH,       /* +, -             left to right */
  PREC_TERM,        /* *, @, /, %, //   left to right */

  PREC_FACTOR,      /* prefix +, -, ~   */
  PREC_POWER,       /* prefix **        */

  PREC_POSTFIX,     /* postfix ( */
  PREC_PRIMARY,
};

static void unimplemented(void)
{
  fprintf(stderr, "unimplemented\n");
  abort();
}

static inline bool peek(const struct parser_state *s, uint16_t token_kind)
{
  return s->scanner.token.kind == token_kind;
}

static inline void next_token(struct parser_state *s)
{
  scanner_next_token(&s->scanner);
}

static inline void eat(struct parser_state *s, uint16_t token_kind)
{
  assert(s->scanner.token.kind == token_kind);
  (void)token_kind;
  next_token(s);
}

static inline bool accept(struct parser_state *s, uint16_t token_kind)
{
  if (peek(s, token_kind)) {
    next_token(s);
    return true;
  }
  return false;
}

static void add_anchor(struct parser_state *s, uint16_t token_kind)
{
  assert(token_kind < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]));
  ++s->anchor_set[token_kind];
}

static void remove_anchor(struct parser_state *s, uint16_t token_kind)
{
  assert(token_kind < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]));
  assert(s->anchor_set[token_kind] > 0);
  --s->anchor_set[token_kind];
}

static void eat_until_matching_token(struct parser_state *s,
                                     uint16_t token_kind)
{
  uint16_t end_token_kind;
  switch (token_kind) {
  case '(': end_token_kind = ')'; break;
  case '{': end_token_kind = '}'; break;
  case '[': end_token_kind = ']'; break;
  default: end_token_kind = token_kind; break;
  }

  unsigned parenthesis_count = 0;
  unsigned brace_count       = 0;
  unsigned bracket_count     = 0;
  while (!peek(s, end_token_kind)
         || parenthesis_count != 0
         || brace_count       != 0
         || bracket_count     != 0) {
    switch (s->scanner.token.kind) {
    case T_EOF: return;
    case '(': ++parenthesis_count; break;
    case '{': ++brace_count;       break;
    case '[': ++bracket_count;     break;

    case ')':
      if (parenthesis_count > 0)
        --parenthesis_count;
      goto check_stop;

    case '}':
      if (brace_count > 0)
        --brace_count;
      goto check_stop;

    case ']':
      if (bracket_count > 0)
        --bracket_count;
check_stop:
      if (peek(s, end_token_kind)
          && parenthesis_count == 0
          && brace_count       == 0
          && bracket_count     == 0)
        return;
      break;
    default:
      break;
    }
    next_token(s);
  }
}

static void eat_until_anchor(struct parser_state *s)
{
  while (s->anchor_set[s->scanner.token.kind] == 0) {
    if (peek(s, '(') || peek(s, '{') || peek(s, '['))
      eat_until_matching_token(s, s->scanner.token.kind);
    next_token(s);
  }
}

static bool skip_till(struct parser_state *s, uint16_t expected_token_kind)
{
  if (UNLIKELY(!peek(s, expected_token_kind))) {
    fprintf(stderr, "expected %s, got ", token_kind_name(expected_token_kind));
    print_token(stderr, &s->scanner.token);
    fputc('\n', stderr);
    s->error = true;

    add_anchor(s, expected_token_kind);
    eat_until_anchor(s);
    remove_anchor(s, expected_token_kind);
    if (!peek(s, expected_token_kind))
      return false;
  }
  return true;
}

static void expect(struct parser_state *s, uint16_t expected_token_kind)
{
  if (skip_till(s, expected_token_kind))
    eat(s, expected_token_kind);
}

static void parse_subexpression(struct parser_state *s,
                                enum precedence precedence);

static void parse_argument(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case '*':
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  case T_ASTERISK_ASTERISK:
    parse_subexpression(s, PREC_TEST);
    unimplemented();
  default:
    parse_subexpression(s, PREC_TEST);
    if (accept(s, '=')) {
      parse_subexpression(s, PREC_TEST);
    }
  }
}

static void parse_call(struct parser_state *s)
{
  eat(s, '(');
  add_anchor(s, ')');
  add_anchor(s, ',');

  if (!peek(s, ')')) {
    do {
      parse_argument(s);
    } while(accept(s, ',') && !peek(s, ')'));
  }
  remove_anchor(s, ',');
  remove_anchor(s, ')');
  expect(s, ')');

  /* TODO: semantic */
}

#if 0
static inline void parse_binexpr(struct parser_state *s,
                                 uint16_t token_kind,
                                 enum precedence prec_right)
{
  eat(s, token_kind);
  parse_subexpression(s, prec_right);

  // TODO: Semantic
}
#endif

static void parse_identifier(struct parser_state *s)
{
  eat(s, T_IDENTIFIER);
}

static void parse_string(struct parser_state *s)
{
  eat(s, T_STRING);
}

static void parse_atom(struct parser_state *s)
{
  switch (s->scanner.token.kind) {
  case T_IDENTIFIER: parse_identifier(s); return;
  case T_STRING:     parse_string(s); return;
  }
}

typedef void (*parser_func)(struct parser_state *s);
struct expression_parser {
  parser_func prefix;
  parser_func infix;
  enum precedence precedence;
};

static const struct expression_parser parsers[] = {
  ['('] = {
    .infix      = parse_call,
    .precedence = PREC_POSTFIX,
  },
};

void parse_subexpression(struct parser_state *s, enum precedence precedence)
{
  uint16_t token_kind = s->scanner.token.kind;
  if (token_kind >= sizeof(parsers) / sizeof(parsers[0])) {
    parse_atom(s);
  } else {
    parser_func prefix_parser = parsers[token_kind].prefix;
    if (prefix_parser != NULL) {
      prefix_parser(s);
    } else {
      parse_atom(s);
    }
  }

  for (;;) {
    uint16_t infix_token_kind = s->scanner.token.kind;
    if (infix_token_kind >= sizeof(parsers) / sizeof(parsers[0]))
      break;
    parser_func infix_parser = parsers[infix_token_kind].infix;
    if (infix_parser == NULL ||
        parsers[infix_token_kind].precedence < precedence)
      break;
    infix_parser(s);
  }
}

static void parse_expression_statement(struct parser_state *s)
{
  do {
    /* TODO: star_expr, etc. */
    parse_subexpression(s, PREC_TEST);
  } while (accept(s, ';'));

  if (peek(s, T_EOF))
    return;
  expect(s, T_NEWLINE);
}

static struct object_code *allocate_code(struct arena *arena)
{
  struct object_code *code = arena_allocate_type(arena, struct object_code);
  memset(code, 0, sizeof(*code));

  return code;
}

static union object *make_string(struct arena *arena, uint32_t len,
                                 const char *chars)
{
  struct object_string *string =
      arena_allocate_type(arena, struct object_string);
  string->base.type = TYPE_STRING;
  string->len = len;
  string->chars = chars;
  return (union object*)string;
}

void parse(struct parser_state *s)
{
  next_token(s);

  writer_begin(&s->writer, stdout);
  arena_init(&s->objects);
  s->code = allocate_code(&s->objects);

  arena_init(&s->opcodes);
  arena_grow_begin(&s->opcodes, 4);

  add_anchor(s, T_EOF);
  struct token *t = &s->scanner.token;
  for (;;) {
    switch (t->kind) {
    case T_NEWLINE:
      continue;
    case EXPRESSION_START_CASES:
      parse_expression_statement(s);
      continue;
    case T_EOF:
      break;
    default:
      fprintf(stderr, "Unexpected token:");
      print_token(stderr, t);
      fputc('\n', stderr);
      next_token(s);
      continue;
    }
    break;
  }

#ifndef NDEBUG
  remove_anchor(s, T_EOF);
  for (size_t i = 0; i < sizeof(s->anchor_set)/sizeof(s->anchor_set[0]); ++i) {
    if (s->anchor_set[i] != 0) {
      fprintf(stderr, "Anchor for token %s not removed\n", token_kind_name(i));
      abort();
    }
  }
#endif

  uint32_t len = arena_grow_current_size(&s->opcodes);
  char *opcodes = arena_grow_finish(&s->opcodes);
  s->code->code = make_string(&s->objects, len, opcodes);
}

void parser_init(struct parser_state *s)
{
  s->error = false;
  memset(s->anchor_set, 0, sizeof(s->anchor_set));
}

void parser_free(struct parser_state *s)
{
  (void)s;
}

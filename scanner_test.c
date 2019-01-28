#include "scanner.h"
#include "symbol_table.h"
#include "token_kinds.h"

static const char* const token_names[] = {
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
  struct arena strings;
  arena_init(&strings);

  struct scanner_state s;
  scanner_init(&s, input, &symbol_table, &strings);

  struct token t;
  do {
    next_token(&s, &t);
    switch (t.kind) {
    case T_FORMAT_STRING:  putchar('f'); goto string;
    case T_RAW_STRING:     putchar('r'); goto string;
    case T_UNICODE_STRING: putchar('u'); goto string;
    case T_BINARY_STRING:  putchar('b'); goto string;
    case T_STRING:
string:
      printf("\"%s\"\n", t.u.string);
      break;
    case T_ID:
      printf("identifier %s\n", t.u.symbol->string);
      break;
    default:
      printf("%s\n", token_names[t.kind]);
      break;
    }
  } while(t.kind != T_EOF);

  if (input != stdin) {
    fclose(input);
  }

  scanner_free(&s);
  arena_free_all(&strings);
  exit_symbol_table(&symbol_table);
  return 0;
}

#include "object_intern.h"
#include "object_intern_types.h"
#include "scanner.h"
#include "scanner_types.h"
#include "symbol_table.h"
#include "symbol_table_types.h"
#include "token_kinds.h"

int main(int argc, char **argv)
{
  FILE       *input = stdin;
  const char *filename = "-";
  if (argc > 1) {
    filename = argv[1];
    input = fopen(filename, "r");
    if (input == NULL) {
      fprintf(stderr, "Failed to open '%s' TODO: print error\n", filename);
      return 1;
    }
  }

  struct symbol_table symbol_table;
  symbol_table_init(&symbol_table);
  struct arena strings;
  arena_init(&strings);

  struct scanner_state s;
  struct object_intern objects;
  object_intern_init(&objects);
  scanner_init(&s, input, filename, &symbol_table, &objects, &strings);

  const struct token *token = &s.token;
  do {
    scanner_next_token(&s);
    printf("%u: ", s.line);
    print_token(stdout, token);
    fputc('\n', stdout);
  } while (token->kind != T_EOF);

  if (input != stdin) {
    fclose(input);
  }

  scanner_free(&s);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return 0;
}

#include "scanner.h"
#include "scanner_types.h"
#include "symbol_table.h"
#include "symbol_table_types.h"
#include "token_kinds.h"

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

  const struct token *token = &s.token;
  do {
    scanner_next_token(&s);
    print_token(stdout, token);
  } while(token->kind != T_EOF);

  if (input != stdin) {
    fclose(input);
  }

  scanner_free(&s);
  arena_free_all(&strings);
  exit_symbol_table(&symbol_table);
  return 0;
}

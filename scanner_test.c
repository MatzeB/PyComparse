#include "diagnostics.h"
#include "diagnostics_types.h"
#include "object_intern.h"
#include "object_intern_types.h"
#include "scanner.h"
#include "scanner_types.h"
#include "symbol_table.h"
#include "symbol_table_types.h"
#include "token_kinds.h"

#include <errno.h>
#include <string.h>

int main(int argc, char **argv)
{
  FILE       *input = stdin;
  const char *filename = "-";
  if (argc > 1) {
    filename = argv[1];
    input = fopen(filename, "r");
    if (input == NULL) {
      fprintf(stderr, "Failed to open '%s': %s\n", filename, strerror(errno));
      return 1;
    }
  }

  struct symbol_table symbol_table;
  symbol_table_init(&symbol_table);
  struct arena strings;
  arena_init(&strings);

  struct diagnostics_state diagnostics;
  diag_init(&diagnostics, stderr, filename);

  struct scanner_state s;
  struct object_intern objects;
  object_intern_init(&objects);
  scanner_init(&s, input, filename, &symbol_table, &objects, &strings,
               &diagnostics);

  const struct token *token = &s.token;
  do {
    scanner_next_token(&s);
    struct location location = scanner_location(&s);
    printf("%u: ", location.line);
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

#include <errno.h>
#include <string.h>

#include "pycomparse/diagnostics.h"
#include "pycomparse/diagnostics_types.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/object_intern_types.h"
#include "pycomparse/scanner.h"
#include "pycomparse/scanner_types.h"
#include "pycomparse/symbol_table.h"
#include "pycomparse/symbol_table_types.h"
#include "pycomparse/token_kinds.h"

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
  object_intern_free(&objects);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return 0;
}

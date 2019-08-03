#include "parser.h"

#include "adt/arena.h"
#include "symbol_table.h"

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

  struct parser_state s;
  parser_init(&s);
  scanner_init(&s.scanner, input, &symbol_table, &strings);

  struct object_code *code = parse(&s);
  write(stdout, (union object*)code);

  if (input != stdin) {
    fclose(input);
  }

  scanner_free(&s.scanner);
  arena_free_all(&strings);
  exit_symbol_table(&symbol_table);
  return 0;
}

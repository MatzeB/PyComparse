#include "adt/arena.h"
#include "parser.h"
#include "parser_types.h"
#include "scanner.h"
#include "scanner_types.h"
#include "symbol_table.h"
#include "symbol_table_types.h"
#include "writer.h"

int main(int argc, char **argv)
{
  if (argc != 2) {
    fprintf(stderr, "Missing filename argument\n");
  }
  const char *filename = argv[1];
  FILE *input = fopen(filename, "r");
  if (input == NULL) {
    fprintf(stderr, "Failed to open '%s' TODO: print error\n", filename);
    return 1;
  }

  struct symbol_table symbol_table;
  init_symbol_table(&symbol_table);
  struct arena strings;
  arena_init(&strings);

  struct parser_state parser;
  parser_init(&parser);
  scanner_init(&parser.scanner, input, &symbol_table, &parser.cg.objects,
               &strings);

  union object *code = parse(&parser, filename);
  write_module(stdout, code);

  fclose(input);

  scanner_free(&parser.scanner);
  arena_free_all(&strings);
  exit_symbol_table(&symbol_table);
  return parser.error ? 1 : 0;
}

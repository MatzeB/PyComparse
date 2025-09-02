#include "adt/arena.h"
#include "diagnostics.h"
#include "diagnostics_types.h"
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
  FILE       *input = fopen(filename, "r");
  if (input == NULL) {
    fprintf(stderr, "Failed to open '%s' TODO: print error\n", filename);
    return 1;
  }

  struct symbol_table symbol_table;
  symbol_table_init(&symbol_table);
  struct arena strings;
  arena_init(&strings);

  struct diagnostics_state diagnostics;
  diag_init(&diagnostics, stderr, filename);

  struct parser_state parser;
  parser_init(&parser, &diagnostics);
  scanner_init(&parser.scanner, input, filename, &symbol_table,
               &parser.cg.objects, &strings, &diagnostics);

  union object *code = parse(&parser, filename);
  bool          had_errors = parser_had_errors(&parser);
  if (!had_errors) {
    write_module(stdout, code);
  }

  fclose(input);

  scanner_free(&parser.scanner);
  parser_free(&parser);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return had_errors ? 1 : 0;
}

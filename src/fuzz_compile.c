#include <stdint.h>
#include <stdio.h>

#include "pycomparse/adt/arena.h"
#include "pycomparse/ast_fold_constants.h"
#include "pycomparse/ast_types.h"
#include "pycomparse/codegen.h"
#include "pycomparse/codegen_statement.h"
#include "pycomparse/codegen_types.h"
#include "pycomparse/diagnostics.h"
#include "pycomparse/diagnostics_types.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/object_intern_types.h"
#include "pycomparse/parser.h"
#include "pycomparse/parser_types.h"
#include "pycomparse/scanner.h"
#include "pycomparse/scanner_types.h"
#include "pycomparse/symbol_table.h"
#include "pycomparse/symbol_table_types.h"

static FILE *dev_null;

int LLVMFuzzerInitialize(int *argc, char ***argv);
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size);

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
  (void)argc;
  (void)argv;
  dev_null = fopen("/dev/null", "w");
  return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
  FILE *input = fmemopen((void *)data, size, "r");
  if (input == NULL) return 0;

  struct symbol_table      symbol_table;
  struct arena             strings;
  struct diagnostics_state diagnostics;
  struct object_intern     objects;
  struct cg_state          cg;
  struct parser_state      parser;

  symbol_table_init(&symbol_table);
  arena_init(&strings);
  diag_init(&diagnostics, dev_null, "<fuzz>");
  object_intern_init(&objects);
  cg_init(&cg, &objects, &symbol_table, "<fuzz>", &diagnostics);
  parser_init(&parser, &objects, &diagnostics);
  scanner_init(&parser.scanner, input, "<fuzz>", &symbol_table, &objects,
               &strings, &diagnostics);

  struct ast_module *module = parse_module(&parser);
  fclose(input);

  if (!diag_had_errors(&diagnostics)) {
    ast_fold_constants(&objects, &parser.ast, module);
    emit_module(&cg, module);
  }

  scanner_free(&parser.scanner);
  parser_free(&parser);
  cg_free(&cg);
  object_intern_free(&objects);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return 0;
}

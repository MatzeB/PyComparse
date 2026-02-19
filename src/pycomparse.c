#include <errno.h>
#include <stdio.h>
#include <string.h>

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
#include "pycomparse/writer.h"

static void print_usage(FILE *out, const char *prog)
{
  fprintf(out, "Usage: %s -o <out.pyc> <input.py>\n", prog);
  fprintf(out, "       %s --out <out.pyc> <input.py>\n", prog);
  fprintf(out, "       %s -h|--help\n", prog);
}

int main(int argc, char **argv)
{
  const char *input_filename = NULL;
  const char *out_filename = NULL;

  for (int i = 1; i < argc; ++i) {
    const char *arg = argv[i];

    if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0) {
      print_usage(stdout, argv[0]);
      return 0;
    }

    if (strcmp(arg, "-o") == 0 || strcmp(arg, "--out") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Missing value for %s\n", arg);
        print_usage(stderr, argv[0]);
        return 1;
      }
      out_filename = argv[++i];
      continue;
    }

    if (strncmp(arg, "--out=", strlen("--out=")) == 0) {
      out_filename = arg + strlen("--out=");
      if (*out_filename == '\0') {
        fprintf(stderr, "Missing value for --out\n");
        print_usage(stderr, argv[0]);
        return 1;
      }
      continue;
    }

    if (arg[0] == '-') {
      fprintf(stderr, "Unknown option: %s\n", arg);
      print_usage(stderr, argv[0]);
      return 1;
    }

    if (input_filename != NULL) {
      fprintf(stderr, "Unexpected argument: %s\n", arg);
      print_usage(stderr, argv[0]);
      return 1;
    }
    input_filename = arg;
  }

  if (input_filename == NULL || out_filename == NULL) {
    print_usage(stderr, argv[0]);
    return 1;
  }

  FILE *input = fopen(input_filename, "r");
  if (input == NULL) {
    fprintf(stderr, "Failed to open '%s': %s\n", input_filename,
            strerror(errno));
    return 1;
  }

  struct symbol_table symbol_table;
  symbol_table_init(&symbol_table);
  struct arena strings;
  arena_init(&strings);

  struct diagnostics_state diagnostics;
  diag_init(&diagnostics, stderr, input_filename);

  struct object_intern objects;
  object_intern_init(&objects);

  struct cg_state cg;
  cg_init(&cg, &objects, &symbol_table, input_filename, &diagnostics);

  struct parser_state parser;
  parser_init(&parser, &objects, &diagnostics);
  scanner_init(&parser.scanner, input, input_filename, &symbol_table, &objects,
               &strings, &diagnostics);

  struct ast_module *module = parse(&parser);

  fclose(input);

  union object *code = NULL;
  if (!diag_had_errors(&diagnostics)) {
    ast_fold_constants(&objects, &parser.ast, module);
    code = emit_module(&cg, module);
  }

  bool io_error = false;
  if (!diag_had_errors(&diagnostics)) {
    FILE *out = fopen(out_filename, "wb");
    if (out == NULL) {
      fprintf(stderr, "Failed to open '%s': %s\n", out_filename,
              strerror(errno));
      io_error = true;
    } else {
      write_module(out, code);
      bool write_failed = ferror(out);
      write_failed |= fclose(out) != 0;
      if (write_failed) {
        fprintf(stderr, "Failed to write '%s': %s\n", out_filename,
                strerror(errno));
        io_error = true;
      }
    }
  }

  scanner_free(&parser.scanner);
  parser_free(&parser);
  cg_free(&cg);
  object_intern_free(&objects);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return (diag_had_errors(&diagnostics) || io_error) ? 1 : 0;
}

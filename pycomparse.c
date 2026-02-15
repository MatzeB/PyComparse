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

#include <errno.h>
#include <stdio.h>
#include <string.h>

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

  struct parser_state parser;
  parser_init(&parser, &diagnostics);
  scanner_init(&parser.scanner, input, input_filename, &symbol_table,
               &parser.cg.objects, &strings, &diagnostics);

  union object *code = parse(&parser, input_filename);
  bool          had_errors = parser_had_errors(&parser);

  fclose(input);

  if (!had_errors) {
    FILE *out = fopen(out_filename, "wb");
    if (out == NULL) {
      fprintf(stderr, "Failed to open '%s': %s\n", out_filename,
              strerror(errno));
      had_errors = true;
    } else {
      write_module(out, code);
      fclose(out);
    }
  }

  scanner_free(&parser.scanner);
  parser_free(&parser);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return had_errors ? 1 : 0;
}

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
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
  fprintf(out, "       Use --out - to write bytecode to stdout.\n");
  fprintf(out, "       %s [-O|-OO] -o <out.pyc> <input.py>\n", prog);
  fprintf(out,
          "       %s --interactive-test [--interactive-out-prefix <path>]\n",
          prog);
  fprintf(out, "       -O   disable assert statements\n");
  fprintf(out, "       -OO  disable assert statements and strip docstrings\n");
  fprintf(out,
          "       --interactive-test reads stdin line-by-line and emits\n");
  fprintf(out,
          "         `incomplete`, `error` or `ok: <path>` after each line.\n");
  fprintf(out, "       %s -h|--help\n", prog);
}

static bool append_bytes(char **buffer, size_t *len, size_t *capacity,
                         const char *src, size_t src_len)
{
  size_t needed = *len + src_len + 1;
  if (needed > *capacity) {
    size_t new_capacity = *capacity == 0 ? 256 : *capacity;
    while (new_capacity < needed) {
      new_capacity *= 2;
    }
    char *new_buffer = realloc(*buffer, new_capacity);
    if (new_buffer == NULL) {
      return false;
    }
    *buffer = new_buffer;
    *capacity = new_capacity;
  }
  memcpy(*buffer + *len, src, src_len);
  *len += src_len;
  (*buffer)[*len] = '\0';
  return true;
}

static bool write_module_to_file(const char *out_filename, union object *code)
{
  FILE *out = fopen(out_filename, "wb");
  if (out == NULL) {
    fprintf(stderr, "Failed to open '%s': %s\n", out_filename,
            strerror(errno));
    return false;
  }
  write_module(out, code);
  bool write_failed = ferror(out) || fclose(out) != 0;
  if (write_failed) {
    fprintf(stderr, "Failed to write '%s': %s\n", out_filename,
            strerror(errno));
    return false;
  }
  return true;
}

static enum parser_single_result
compile_single_buffer(const char *source, size_t source_len,
                      const char *source_name, const char *out_filename,
                      int optimize_level)
{
  struct symbol_table symbol_table;
  symbol_table_init(&symbol_table);
  struct arena strings;
  arena_init(&strings);

  struct diagnostics_state diagnostics;
  diag_init(&diagnostics, source_name);

  struct object_intern objects;
  object_intern_init(&objects);

  struct cg_state cg;
  cg_init(&cg, &objects, &symbol_table, source_name, &diagnostics);
  cg.optimize_no_assertions = (optimize_level >= 1);
  cg.optimize_no_docstrings = (optimize_level >= 2);

  struct parser_state parser;
  parser_init(&parser, &objects, &diagnostics);
  scanner_init_from_buffer(&parser.scanner, source, source_len, source_name,
                           &symbol_table, &objects, &strings, &diagnostics,
                           /*is_utf8=*/false);

  struct ast_module        *module = NULL;
  enum parser_single_result result
      = parse_single_statement_with_status(&parser, &module);

  union object *code = NULL;
  if (result == PARSER_SINGLE_OK && !diag_had_errors(&diagnostics)) {
    ast_fold_constants(&objects, &parser.ast, module);
    code = emit_module(&cg, module);
  }

  if (result == PARSER_SINGLE_INCOMPLETE) {
    /* parse_single_statement_with_status only returns INCOMPLETE when there
       are no diagnostics errors - the two states are mutually exclusive. */
    assert(!diag_had_errors(&diagnostics));
  } else if (diag_had_errors(&diagnostics)) {
    diag_print_all(&diagnostics, stderr);
    result = PARSER_SINGLE_ERROR;
  } else {
    assert(code != NULL);
    if (!write_module_to_file(out_filename, code)) {
      result = PARSER_SINGLE_ERROR;
    }
  }

  scanner_free(&parser.scanner);
  parser_free(&parser);
  cg_free(&cg);
  object_intern_free(&objects);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  diag_free(&diagnostics);
  return result;
}

static int run_interactive_test_mode(const char *out_prefix,
                                     int         optimize_level)
{
  char   line[4096];
  char  *source = NULL;
  size_t source_len = 0;
  size_t source_capacity = 0;
  /* True when fgets returned data without a trailing newline, meaning we are
     mid-line at EOF and the accumulated source has not yet been compiled. */
  bool     unterminated_line = false;
  unsigned output_index = 1;

  while (fgets(line, sizeof(line), stdin) != NULL) {
    size_t line_len = strlen(line);
    if (!append_bytes(&source, &source_len, &source_capacity, line,
                      line_len)) {
      fprintf(stderr, "out of memory\n");
      free(source);
      return 1;
    }
    if (line_len == 0 || line[line_len - 1] != '\n') {
      unterminated_line = true;
      continue;
    }
    unterminated_line = false;

    char output_path[1024];
    int n = snprintf(output_path, sizeof(output_path), "%s.%u.pyc", out_prefix,
                     output_index);
    if (n < 0 || (size_t)n >= sizeof(output_path)) {
      fprintf(stderr, "interactive output path too long\n");
      free(source);
      return 1;
    }

    enum parser_single_result result = compile_single_buffer(
        source, source_len, "<stdin>", output_path, optimize_level);
    if (result == PARSER_SINGLE_INCOMPLETE) {
      puts("incomplete");
      continue;
    }
    if (result == PARSER_SINGLE_ERROR) {
      puts("error");
      source_len = 0;
      if (source != NULL) source[0] = '\0';
      continue;
    }

    printf("ok: %s\n", output_path);
    source_len = 0;
    if (source != NULL) source[0] = '\0';
    ++output_index;
  }

  if (ferror(stdin)) {
    fprintf(stderr, "Failed to read stdin: %s\n", strerror(errno));
    free(source);
    return 1;
  }
  if (unterminated_line) {
    char output_path[1024];
    int n = snprintf(output_path, sizeof(output_path), "%s.%u.pyc", out_prefix,
                     output_index);
    if (n < 0 || (size_t)n >= sizeof(output_path)) {
      fprintf(stderr, "interactive output path too long\n");
      free(source);
      return 1;
    }

    enum parser_single_result result = compile_single_buffer(
        source, source_len, "<stdin>", output_path, optimize_level);
    if (result == PARSER_SINGLE_INCOMPLETE) {
      puts("incomplete");
    } else if (result == PARSER_SINGLE_ERROR) {
      puts("error");
      source_len = 0;
    } else {
      printf("ok: %s\n", output_path);
      source_len = 0;
    }
  }
  free(source);
  return source_len == 0 ? 0 : 1;
}

int main(int argc, char **argv)
{
  const char *input_filename = NULL;
  const char *out_filename = NULL;
  const char *interactive_out_prefix = "/tmp/pycomparse-interactive";
  bool        interactive_test_mode = false;
  int         optimize_level = 0;

  for (int i = 1; i < argc; ++i) {
    const char *arg = argv[i];

    if (strcmp(arg, "-h") == 0 || strcmp(arg, "--help") == 0) {
      print_usage(stdout, argv[0]);
      return 0;
    }

    if (strcmp(arg, "-OO") == 0) {
      optimize_level = 2;
      continue;
    }

    if (strcmp(arg, "-O") == 0) {
      optimize_level = 1;
      continue;
    }

    if (strcmp(arg, "--interactive-test") == 0) {
      interactive_test_mode = true;
      continue;
    }

    if (strcmp(arg, "--interactive-out-prefix") == 0) {
      if (i + 1 >= argc) {
        fprintf(stderr, "Missing value for %s\n", arg);
        print_usage(stderr, argv[0]);
        return 1;
      }
      interactive_out_prefix = argv[++i];
      continue;
    }

    if (strncmp(arg, "--interactive-out-prefix=",
                strlen("--interactive-out-prefix="))
        == 0) {
      interactive_out_prefix = arg + strlen("--interactive-out-prefix=");
      if (*interactive_out_prefix == '\0') {
        fprintf(stderr, "Missing value for --interactive-out-prefix\n");
        print_usage(stderr, argv[0]);
        return 1;
      }
      continue;
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

  if (interactive_test_mode) {
    if (input_filename != NULL || out_filename != NULL) {
      fprintf(stderr, "--interactive-test does not accept input/out files\n");
      print_usage(stderr, argv[0]);
      return 1;
    }
    return run_interactive_test_mode(interactive_out_prefix, optimize_level);
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
  diag_init(&diagnostics, input_filename);

  struct object_intern objects;
  object_intern_init(&objects);

  struct cg_state cg;
  cg_init(&cg, &objects, &symbol_table, input_filename, &diagnostics);
  cg.optimize_no_assertions = (optimize_level >= 1);
  cg.optimize_no_docstrings = (optimize_level >= 2);

  struct parser_state parser;
  parser_init(&parser, &objects, &diagnostics);
  scanner_init(&parser.scanner, input, input_filename, &symbol_table, &objects,
               &strings, &diagnostics);

  struct ast_module *module = parse_module(&parser);

  fclose(input);

  union object *code = NULL;
  if (!diag_had_errors(&diagnostics)) {
    ast_fold_constants(&objects, &parser.ast, module);
    code = emit_module(&cg, module);
  }

  diag_print_all(&diagnostics, stderr);

  bool io_error = false;
  if (!diag_had_errors(&diagnostics)) {
    bool  write_to_stdout = strcmp(out_filename, "-") == 0;
    FILE *out = write_to_stdout ? stdout : fopen(out_filename, "wb");
    if (out == NULL) {
      fprintf(stderr, "Failed to open '%s': %s\n", out_filename,
              strerror(errno));
      io_error = true;
    } else {
      write_module(out, code);
      bool write_failed = ferror(out);
      if (write_to_stdout) {
        write_failed |= fflush(out) != 0;
      } else {
        write_failed |= fclose(out) != 0;
      }
      if (write_failed) {
        fprintf(stderr, "Failed to write '%s': %s\n", out_filename,
                strerror(errno));
        io_error = true;
      }
    }
  }

  bool had_errors = diag_had_errors(&diagnostics);
  scanner_free(&parser.scanner);
  parser_free(&parser);
  cg_free(&cg);
  object_intern_free(&objects);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  diag_free(&diagnostics);
  return (had_errors || io_error) ? 1 : 0;
}

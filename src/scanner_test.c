#include <errno.h>
#include <stdlib.h>
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

static char *read_file_into_string(FILE *input)
{
  char  *buf = NULL;
  size_t buf_size = 0;
  size_t buf_len = 0;
  char   tmp[4096];
  size_t n;

  while ((n = fread(tmp, 1, sizeof(tmp), input)) > 0) {
    if (buf_len + n + 1 > buf_size) {
      buf_size = (buf_len + n + 1) * 2;
      char *newbuf = realloc(buf, buf_size);
      if (newbuf == NULL) {
        free(buf);
        return NULL;
      }
      buf = newbuf;
    }
    memcpy(buf + buf_len, tmp, n);
    buf_len += n;
  }

  if (buf == NULL) {
    buf = malloc(1);
    if (buf == NULL) return NULL;
  }
  buf[buf_len] = '\0';
  return buf;
}

int main(int argc, char **argv)
{
  bool        from_string = false;
  FILE       *input = stdin;
  const char *filename = "-";

  for (int i = 1; i < argc; ++i) {
    if (strcmp(argv[i], "--from-string") == 0) {
      from_string = true;
    } else {
      filename = argv[i];
      input = fopen(filename, "r");
      if (input == NULL) {
        fprintf(stderr, "Failed to open '%s': %s\n", filename,
                strerror(errno));
        return 1;
      }
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

  char *buf = NULL;
  if (from_string) {
    buf = read_file_into_string(input);
    if (input != stdin) fclose(input);
    if (buf == NULL) {
      fprintf(stderr, "Failed to read '%s': out of memory\n", filename);
      return 1;
    }
    scanner_init_from_string(&s, buf, filename, &symbol_table, &objects,
                             &strings, &diagnostics,
                             /*is_utf8=*/false);
  } else {
    scanner_init(&s, input, filename, &symbol_table, &objects, &strings,
                 &diagnostics);
  }

  const struct token *token = &s.token;
  do {
    scanner_next_token(&s);
    struct location location = scanner_location(&s);
    printf("%u: ", location.line);
    print_token(stdout, token);
    fputc('\n', stdout);
  } while (token->kind != T_EOF);

  if (!from_string && input != stdin) {
    fclose(input);
  }

  scanner_free(&s);
  free(buf);
  object_intern_free(&objects);
  arena_free(&strings);
  symbol_table_free(&symbol_table);
  return diag_had_errors(&diagnostics) ? 1 : 0;
}

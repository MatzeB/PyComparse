#include "util.h"

#include <stdio.h>
#include <stdlib.h>

noreturn void unimplemented(const char *what)
{
  fprintf(stderr, "unimplemented: %s\n", what);
  abort();
}

noreturn void internal_error(const char *what)
{
  fprintf(stderr, "internal compiler error: %s\n", what);
  abort();
}

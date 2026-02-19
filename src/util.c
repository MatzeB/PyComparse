#include "pycomparse/util.h"

#include <stdio.h>
#include <stdlib.h>

noreturn void internal_error(const char *what)
{
  fprintf(stderr, "internal compiler error: %s\n", what);
  abort();
}

#include "util.h"

#include <stdio.h>
#include <stdlib.h>

noreturn void unimplemented(void)
{
  fprintf(stderr, "unimplemented\n");
  abort();
}

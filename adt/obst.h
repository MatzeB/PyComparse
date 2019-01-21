#include <stdlib.h>
#include "obstack/obstack.h"

#define obstack_chunk_alloc malloc
#define obstack_chunk_free  free

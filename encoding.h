#pragma once

#include <stdio.h>

#ifndef PYCOMPARSE_NO_ICONV
void encoding_maybe_transcode_to_utf8(FILE **input_io, FILE **owned_input_out,
                                      char **owned_source_out);
#endif

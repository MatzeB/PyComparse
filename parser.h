#pragma once

#include <stdbool.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

struct diagnostics_state;
union object;
struct parser_state;

void parser_init(struct parser_state      *s,
                 struct diagnostics_state *diagnostics);
void parser_free(struct parser_state *s);

union object *parse(struct parser_state *s, const char *filename);
bool          parser_had_errors(struct parser_state *s);

ASSUME_NONNULL_END

#pragma once

#include <stdbool.h>

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct ast_module;
struct diagnostics_state;
struct object_intern;
struct parser_state;

void parser_init(struct parser_state *s, struct object_intern *objects,
                 struct diagnostics_state *diagnostics);
void parser_free(struct parser_state *s);

struct ast_module *parse(struct parser_state *s);
bool               parser_had_errors(struct parser_state *s);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

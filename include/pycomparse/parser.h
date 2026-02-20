#pragma once

#include <stdbool.h>
#include <stdint.h>

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

void parser_set_flags(struct parser_state *s, uint32_t flags);

struct ast_module *parse_module(struct parser_state *s);
struct ast_module *parse_single_statement(struct parser_state *s);
struct ast_module *parse_single_expression(struct parser_state *s);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

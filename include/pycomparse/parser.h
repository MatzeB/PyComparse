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

enum parser_single_result {
  PARSER_SINGLE_OK = 0,
  PARSER_SINGLE_INCOMPLETE,
  PARSER_SINGLE_ERROR,
};

void parser_init(struct parser_state *s, struct object_intern *objects,
                 struct diagnostics_state *diagnostics, uint32_t flags);
void parser_free(struct parser_state *s);

struct ast_module        *parse_module(struct parser_state *s);
struct ast_module        *parse_single_statement(struct parser_state *s);
struct ast_module        *parse_single_expression(struct parser_state *s);
enum parser_single_result parse_single_statement_with_status(
    struct parser_state *s, struct ast_module * nullable * nonnull out_module);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

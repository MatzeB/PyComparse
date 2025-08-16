#pragma once

#include <stdbool.h>

union object;
struct parser_state;

void parser_init(struct parser_state *s);

void parser_free(struct parser_state *s);

union object *parse(struct parser_state *s, const char *filename);

bool parser_had_errors(struct parser_state *s);

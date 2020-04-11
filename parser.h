#pragma once

#include "adt/arena.h"
#include "scanner.h"
#include "token_kinds.h"
#include "codegen.h"
#include "writer.h"

struct parser_state {
  struct scanner_state scanner;
  struct arena ast;
  struct cg_state cg;
  bool error;
  unsigned short anchor_set[NUM_TOKENS];
};

void parser_init(struct parser_state *s);

void parser_free(struct parser_state *s);

struct object_code *parse(struct parser_state *s);

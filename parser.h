#ifndef PARSER_H
#define PARSER_H

#include "adt/arena.h"
#include "scanner.h"
#include "token_kinds.h"
#include "writer.h"

struct parser_state {
  struct scanner_state scanner;
  bool error;
  unsigned short anchor_set[NUM_TOKENS];

  struct bytecode_writer_state writer;

  struct object_code *code;
  struct arena objects;
  struct arena opcodes;
  bool had_return;
  unsigned stacksize;
};

void parser_init(struct parser_state *s);

void parser_free(struct parser_state *s);

void parse(struct parser_state *s);

#endif

#pragma once

#include <stdint.h>

union object;
struct argument;
struct parameter;

struct ast_node_base {
  uint8_t type;
};

struct ast_identifier {
  struct ast_node_base base;
  struct symbol       *symbol;
};

struct ast_const {
  struct ast_node_base base;
  uint16_t             index;
};

struct ast_call {
  struct ast_node_base base;
  union ast_node      *callee;
  struct argument     *arguments;
};

struct ast_binexpr {
  struct ast_node_base base;
  union ast_node      *left;
  union ast_node      *right;
};

struct ast_unexpr {
  struct ast_node_base base;
  union ast_node      *op;
};

union ast_node {
  uint8_t              type;
  struct ast_node_base base;

  struct ast_binexpr    binexpr;
  struct ast_call       call;
  struct ast_const      cnst;
  struct ast_identifier identifier;
  struct ast_unexpr     unexpr;
};


struct argument {
  union ast_node  *expression;
  struct argument *next;
};

struct dotted_name {
  unsigned num_symbols;
  struct symbol *symbols[];
};

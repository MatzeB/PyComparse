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
  struct ast_node_base  base;
  union ast_expression *callee;
  struct argument      *arguments;
};

struct ast_binexpr {
  struct ast_node_base  base;
  union ast_expression *left;
  union ast_expression *right;
};

struct ast_unexpr {
  struct ast_node_base  base;
  union ast_expression *op;
};

struct ast_tuple_form {
  struct ast_node_base base;
  struct argument     *arguments;
};

union ast_expression {
  uint8_t              type;
  struct ast_node_base base;

  struct ast_binexpr    binexpr;
  struct ast_call       call;
  struct ast_const      cnst;
  struct ast_identifier identifier;
  struct ast_tuple_form tuple_form;
  struct ast_unexpr     unexpr;
};


struct argument {
  union ast_expression *expression;
  struct argument      *next;
};

struct dotted_name {
  unsigned       num_symbols;
  struct symbol *symbols[];
};

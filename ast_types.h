#pragma once

#include <stdint.h>

union object;
struct argument;
struct parameter;

struct ast_node_base {
  uint8_t type;
};

struct ast_attr {
  struct ast_node_base  base;
  union ast_expression *expression;
  struct symbol        *attr;
};

struct ast_identifier {
  struct ast_node_base base;
  struct symbol       *symbol;
};

struct ast_const {
  struct ast_node_base base;
  union object        *object;
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

struct ast_expression_list {
  struct ast_node_base  base;
  union object         *as_constant;
  unsigned              num_expressions;
  union ast_expression *expressions[];
};

struct dict_item {
  union ast_expression *key;
  union ast_expression *value;
};

struct ast_dict_item_list {
  struct ast_node_base base;
  unsigned             num_items;
  struct dict_item     items[];
};

enum generator_expression_part_type {
  GENERATOR_EXPRESSION_PART_FOR,
  GENERATOR_EXPRESSION_PART_IF,
};

struct generator_expression_part {
  enum generator_expression_part_type type;
  union ast_expression               *target;
  union ast_expression               *expression;
  struct generator_expression_part   *next;
};

struct ast_generator_expression {
  struct ast_node_base              base;
  union ast_expression             *expression;
  struct generator_expression_part *parts;
};

union ast_expression {
  uint8_t              type;
  struct ast_node_base base;

  struct ast_attr                 attr;
  struct ast_binexpr              binexpr;
  struct ast_call                 call;
  struct ast_const                cnst;
  struct ast_generator_expression generator_expression;
  struct ast_identifier           identifier;
  struct ast_expression_list      expression_list;
  struct ast_dict_item_list       dict_item_list;
  struct ast_unexpr               unexpr;
};

struct argument {
  union ast_expression *expression;
  struct argument      *next;
};

struct dotted_name {
  unsigned       num_symbols;
  struct symbol *symbols[];
};

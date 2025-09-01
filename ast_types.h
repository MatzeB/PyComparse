#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "nullable.h"

ASSUME_NONNULL_BEGIN

union object;
struct argument;

struct ast_node_base {
  uint8_t type;
};

struct ast_attr {
  struct ast_node_base  base;
  union ast_expression *expression;
  struct symbol        *symbol;
};

struct ast_binexpr {
  struct ast_node_base  base;
  union ast_expression *left;
  union ast_expression *right;
};

struct comparison_op {
  uint8_t               op;
  union ast_expression *operand;
};

struct ast_comparison {
  struct ast_node_base  base;
  unsigned              num_operands;
  union ast_expression *left;
  struct comparison_op  operands[];
};

struct argument {
  union ast_expression   *expression;
  struct symbol *nullable name;
};

struct ast_call {
  struct ast_node_base           base;
  unsigned                       num_arguments;
  bool                           has_star_argument;
  bool                           has_kw_argument;
  union ast_expression *nullable callee;
  struct argument                arguments[];
};

struct ast_conditional {
  struct ast_node_base  base;
  union ast_expression *condition;
  union ast_expression *true_expression;
  union ast_expression *false_expression;
};

struct ast_const {
  struct ast_node_base base;
  union object        *object;
};

struct dict_item {
  union ast_expression *key;
  union ast_expression *expression;
};

struct ast_dict_item_list {
  struct ast_node_base base;
  unsigned             num_items;
  struct dict_item     items[];
};

struct ast_expression_list {
  struct ast_node_base          base;
  unsigned                      num_expressions;
  bool                          has_star_expression;
  union object *nullable        as_constant;
  union ast_expression *nonnull expressions[];
};

enum generator_expression_part_type {
  GENERATOR_EXPRESSION_PART_FOR,
  GENERATOR_EXPRESSION_PART_IF,
};

struct generator_expression_part {
  enum generator_expression_part_type type;
  union ast_expression *nullable      targets;
  union ast_expression               *expression;
};

struct ast_generator_expression {
  struct ast_node_base             base;
  unsigned                         num_parts;
  union ast_expression            *expression;
  struct generator_expression_part parts[];
};

struct ast_identifier {
  struct ast_node_base base;
  struct symbol       *symbol;
};

struct ast_slice {
  struct ast_node_base           base;
  union ast_expression *nullable start;
  union ast_expression *nullable stop;
  union ast_expression *nullable step;
};

struct ast_unexpr {
  struct ast_node_base  base;
  union ast_expression *op;
};

union ast_expression {
  uint8_t              type;
  struct ast_node_base base;

  struct ast_attr                 attr;
  struct ast_binexpr              binexpr;
  struct ast_call                 call;
  struct ast_comparison           comparison;
  struct ast_conditional          conditional;
  struct ast_const                cnst;
  struct ast_dict_item_list       dict_item_list;
  struct ast_expression_list      expression_list;
  struct ast_generator_expression generator_expression;
  struct ast_identifier           identifier;
  struct ast_slice                slice;
  struct ast_unexpr               unexpr;
};

ASSUME_NONNULL_END

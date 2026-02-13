#pragma once

#include "ast_types.h"
#include "nullable.h"

ASSUME_NONNULL_BEGIN

union object;
union ast_expression;
struct symbol;

struct argument {
  union ast_expression   *expression;
  struct symbol *nullable name;
};

enum parameter_variant {
  PARAMETER_NORMAL,
  PARAMETER_STAR,
  PARAMETER_STAR_STAR,
};

struct parameter {
  struct symbol                 *name;
  union ast_expression *nullable type;
  union ast_expression *nullable initializer;
  enum parameter_variant         variant;
};

struct fstring_element {
  union {
    union ast_expression *expression;
    union object         *string;
  } u;
  union ast_expression *nullable format_spec;
  uint8_t                        is_expression : 1;
  uint8_t                        conversion : 3;
};

enum generator_expression_part_type {
  GENERATOR_EXPRESSION_PART_FOR,
  GENERATOR_EXPRESSION_PART_IF,
};

struct generator_expression_part {
  enum generator_expression_part_type type;
  bool                                async;
  union ast_expression *nullable      targets;
  union ast_expression               *expression;
};

struct ast_attr {
  struct ast_expression_base base;
  union ast_expression      *expression;
  struct symbol             *symbol;
};

struct ast_binexpr {
  struct ast_expression_base base;
  union ast_expression      *left;
  union ast_expression      *right;
};

struct comparison_op {
  uint8_t               op;
  union ast_expression *operand;
};

struct ast_comparison {
  struct ast_expression_base base;
  unsigned                   num_operands;
  union ast_expression      *left;
  struct comparison_op       operands[];
};

struct ast_call {
  struct ast_expression_base     base;
  unsigned                       num_arguments;
  bool                           has_star_argument;
  bool                           has_kw_argument;
  union ast_expression *nullable callee;
  struct argument                arguments[];
};

struct ast_conditional {
  struct ast_expression_base base;
  union ast_expression      *condition;
  union ast_expression      *true_expression;
  union ast_expression      *false_expression;
};

struct ast_const {
  struct ast_expression_base base;
  union object              *object;
};

struct ast_fstring {
  struct ast_expression_base base;
  unsigned                   num_elements;
  struct fstring_element     elements[];
};

struct dict_item {
  union ast_expression *nullable key;
  union ast_expression          *value;
};

struct ast_dict_item_list {
  struct ast_expression_base base;
  unsigned                   num_items;
  struct dict_item           items[];
};

struct ast_expression_list {
  struct ast_expression_base    base;
  unsigned                      num_expressions;
  bool                          has_star_expression;
  union object *nullable        as_constant;
  union ast_expression *nonnull expressions[];
};

struct ast_generator_expression {
  struct ast_expression_base       base;
  unsigned                         num_parts;
  bool                             is_async;
  union ast_expression            *expression;
  union ast_expression *nullable   item_value;
  struct generator_expression_part parts[];
};

struct ast_identifier {
  struct ast_expression_base base;
  struct symbol             *symbol;
};

struct ast_lambda {
  struct ast_expression_base            base;
  union ast_expression                 *expression;
  struct parameter_shape                parameter_shape;
  bool                                  scope_bindings_ready;
  unsigned                              num_scope_globals;
  struct symbol * nonnull * nullable    scope_globals;
  unsigned                              num_scope_locals;
  struct symbol * nonnull * nullable    scope_locals;
  unsigned                              num_scope_cellvars;
  struct symbol * nonnull * nullable    scope_cellvars;
  unsigned                              num_scope_freevars;
  struct symbol * nonnull * nullable    scope_freevars;
  struct parameter                      parameters[];
};

struct ast_slice {
  struct ast_expression_base     base;
  union ast_expression *nullable start;
  union ast_expression *nullable stop;
  union ast_expression *nullable step;
};

struct ast_unexpr {
  struct ast_expression_base base;
  union ast_expression      *op;
};

struct ast_expression_yield {
  struct ast_expression_base     base;
  union ast_expression *nullable value;
};

union ast_expression {
  uint8_t                    type;
  struct ast_expression_base base;

  struct ast_attr                 attr;
  struct ast_binexpr              binexpr;
  struct ast_call                 call;
  struct ast_comparison           comparison;
  struct ast_conditional          conditional;
  struct ast_const                cnst;
  struct ast_dict_item_list       dict_item_list;
  struct ast_expression_list      expression_list;
  struct ast_fstring              fstring;
  struct ast_generator_expression generator_expression;
  struct ast_identifier           identifier;
  struct ast_lambda               lambda;
  struct ast_slice                slice;
  struct ast_unexpr               unexpr;
  struct ast_expression_yield     yield;
};

ASSUME_NONNULL_END

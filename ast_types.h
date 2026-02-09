#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "nullable.h"
#include "scanner_location.h"

ASSUME_NONNULL_BEGIN

union object;
union ast_expression;
union ast_statement;
struct symbol;

struct argument {
  union ast_expression   *expression;
  struct symbol *nullable name;
};

struct dotted_name {
  unsigned               num_symbols;
  struct symbol *nonnull symbols[];
};

struct from_import_item {
  struct symbol          *name;
  struct symbol *nullable as;
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
  union ast_expression *nullable      targets;
  union ast_expression               *expression;
};

struct ast_node_base {
  uint8_t type;
};

struct ast_statement_base {
  uint8_t         type;
  struct location location;
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

struct ast_fstring {
  struct ast_node_base   base;
  unsigned               num_elements;
  struct fstring_element elements[];
};

struct dict_item {
  union ast_expression *nullable key;
  union ast_expression          *value;
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

struct ast_generator_expression {
  struct ast_node_base             base;
  unsigned                         num_parts;
  union ast_expression            *expression;
  union ast_expression *nullable   item_value;
  struct generator_expression_part parts[];
};

struct ast_identifier {
  struct ast_node_base base;
  struct symbol       *symbol;
};

struct ast_lambda {
  struct ast_node_base  base;
  union ast_expression *expression;
  unsigned              positional_only_argcount;
  unsigned              num_parameters;
  struct parameter      parameters[];
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

struct ast_yield {
  struct ast_node_base           base;
  union ast_expression *nullable value;
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
  struct ast_fstring              fstring;
  struct ast_generator_expression generator_expression;
  struct ast_identifier           identifier;
  struct ast_lambda               lambda;
  struct ast_slice                slice;
  struct ast_unexpr               unexpr;
  struct ast_yield                yield;
};

struct ast_statement_list {
  unsigned                     num_statements;
  union ast_statement *nonnull statements[];
};

struct ast_statement_annotation {
  struct ast_statement_base base;
  union ast_expression     *target;
  union ast_expression     *annotation;
  union ast_expression     *nullable value;
};

struct ast_statement_assert {
  struct ast_statement_base base;
  union ast_expression     *expression;
  union ast_expression     *nullable message;
};

struct ast_statement_assign {
  struct ast_statement_base       base;
  unsigned                        num_targets;
  union ast_expression *nonnull   value;
  union ast_expression *nonnull *nonnull targets;
};

struct ast_statement_augassign {
  struct ast_statement_base base;
  union ast_expression     *expression;
};

struct ast_statement_break {
  struct ast_statement_base base;
};

struct ast_statement_class {
  struct ast_statement_base       base;
  struct symbol                  *name;
  struct ast_call                *call;
  struct ast_statement_list      *body;
  unsigned                        num_decorators;
  union ast_expression *nonnull *nullable decorators;
  bool                            scope_bindings_ready;
  unsigned                        num_scope_globals;
  struct symbol *nonnull *nullable scope_globals;
  unsigned                        num_scope_locals;
  struct symbol *nonnull *nullable scope_locals;
  unsigned                        num_scope_freevars;
  struct symbol *nonnull *nullable scope_freevars;
};

struct ast_statement_continue {
  struct ast_statement_base base;
};

struct ast_statement_def {
  struct ast_statement_base       base;
  struct symbol                  *name;
  bool                            async;
  bool                            has_yield;
  bool                            scope_bindings_ready;
  unsigned                        positional_only_argcount;
  unsigned                        num_parameters;
  struct parameter *nullable      parameters;
  union ast_expression *nullable  return_type;
  struct ast_statement_list      *body;
  unsigned                        num_decorators;
  union ast_expression *nonnull *nullable decorators;
  unsigned                        num_scope_globals;
  struct symbol *nonnull *nullable scope_globals;
  unsigned                        num_scope_locals;
  struct symbol *nonnull *nullable scope_locals;
  unsigned                        num_scope_cellvars;
  struct symbol *nonnull *nullable scope_cellvars;
  unsigned                        num_scope_freevars;
  struct symbol *nonnull *nullable scope_freevars;
};

struct ast_statement_del {
  struct ast_statement_base base;
  union ast_expression     *targets;
};

struct ast_statement_expression {
  struct ast_statement_base base;
  union ast_expression     *expression;
};

struct ast_statement_for {
  struct ast_statement_base  base;
  union ast_expression      *targets;
  union ast_expression      *expression;
  struct ast_statement_list *body;
  struct ast_statement_list *nullable else_body;
};

struct ast_statement_from_import {
  struct ast_statement_base          base;
  unsigned                           num_prefix_dots;
  struct dotted_name *nullable       module;
  bool                               import_star;
  unsigned                           num_items;
  struct from_import_item *nullable  items;
};

struct ast_statement_global {
  struct ast_statement_base base;
  unsigned                  num_names;
  struct symbol *nonnull *nonnull names;
};

struct ast_if_elif {
  union ast_expression      *condition;
  struct ast_statement_list *body;
};

struct ast_statement_if {
  struct ast_statement_base        base;
  union ast_expression            *condition;
  struct ast_statement_list       *body;
  unsigned                         num_elifs;
  struct ast_if_elif *nullable     elifs;
  struct ast_statement_list *nullable else_body;
};

struct ast_import_item {
  struct dotted_name *module;
  struct symbol *nullable as;
};

struct ast_statement_import {
  struct ast_statement_base      base;
  unsigned                       num_items;
  struct ast_import_item *nullable items;
};

struct ast_statement_nonlocal {
  struct ast_statement_base base;
  unsigned                  num_names;
  struct symbol *nonnull *nonnull names;
};

struct ast_statement_pass {
  struct ast_statement_base base;
};

struct ast_statement_raise {
  struct ast_statement_base       base;
  union ast_expression *nullable  expression;
  union ast_expression *nullable  from;
};

struct ast_statement_return {
  struct ast_statement_base       base;
  union ast_expression *nullable  expression;
};

struct ast_try_except {
  union ast_expression *nullable  match;
  struct symbol *nullable         as;
  struct ast_statement_list      *body;
};

struct ast_statement_try {
  struct ast_statement_base          base;
  struct ast_statement_list         *body;
  unsigned                           num_excepts;
  struct ast_try_except *nullable    excepts;
  struct ast_statement_list *nullable else_body;
  struct ast_statement_list *nullable finally_body;
};

struct ast_statement_while {
  struct ast_statement_base  base;
  union ast_expression      *condition;
  struct ast_statement_list *body;
  struct ast_statement_list *nullable else_body;
};

struct ast_with_item {
  union ast_expression      *expression;
  union ast_expression *nullable targets;
};

struct ast_statement_with {
  struct ast_statement_base      base;
  unsigned                       num_items;
  struct ast_with_item *nullable items;
  struct ast_statement_list     *body;
};

struct ast_statement_yield {
  struct ast_statement_base       base;
  union ast_expression *nullable  expression;
};

union ast_statement {
  uint8_t                    type;
  struct ast_statement_base  base;

  struct ast_statement_annotation annotation;
  struct ast_statement_assert     assertion;
  struct ast_statement_assign     assign;
  struct ast_statement_augassign  augassign;
  struct ast_statement_break      break_stmt;
  struct ast_statement_class      class_stmt;
  struct ast_statement_continue   continue_stmt;
  struct ast_statement_def        def_stmt;
  struct ast_statement_del        del_stmt;
  struct ast_statement_expression expression_stmt;
  struct ast_statement_for        for_stmt;
  struct ast_statement_from_import from_import_stmt;
  struct ast_statement_global      global_stmt;
  struct ast_statement_if          if_stmt;
  struct ast_statement_import      import_stmt;
  struct ast_statement_nonlocal    nonlocal_stmt;
  struct ast_statement_pass        pass_stmt;
  struct ast_statement_raise       raise_stmt;
  struct ast_statement_return      return_stmt;
  struct ast_statement_try         try_stmt;
  struct ast_statement_while       while_stmt;
  struct ast_statement_with        with_stmt;
  struct ast_statement_yield       yield_stmt;
};

ASSUME_NONNULL_END

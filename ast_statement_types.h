#pragma once

#include "ast_types.h"
#include "nullable.h"

ASSUME_NONNULL_BEGIN

union ast_expression;
union ast_statement;
struct symbol;
struct ast_call;
struct parameter;

struct dotted_name {
  unsigned               num_symbols;
  struct symbol *nonnull symbols[];
};

struct from_import_item {
  struct symbol          *name;
  struct symbol *nullable as;
};

struct ast_statement_list {
  unsigned                     num_statements;
  union ast_statement *nonnull statements[];
};

struct ast_statement_annotation {
  struct ast_statement_base      base;
  union ast_expression          *target;
  bool                           simple;
  union ast_expression          *annotation;
  union ast_expression *nullable value;
};

struct ast_assert {
  struct ast_statement_base      base;
  union ast_expression          *expression;
  union ast_expression *nullable message;
};

struct ast_assignment {
  struct ast_statement_base                base;
  unsigned                                 num_targets;
  union ast_expression *nonnull            value;
  union ast_expression * nonnull * nonnull targets;
};

struct ast_augassign {
  struct ast_statement_base base;
  union ast_expression     *expression;
};

struct ast_break {
  struct ast_statement_base base;
};

struct ast_class {
  struct ast_statement_base                 base;
  struct symbol                            *name;
  struct ast_call                          *call;
  struct ast_statement_list                *body;
  unsigned                                  num_decorators;
  union ast_expression * nonnull * nullable decorators;
  bool                                      needs_class_cell;
  struct ast_scope_bindings *nullable       scope;
};

struct ast_continue {
  struct ast_statement_base base;
};

struct ast_def {
  struct ast_statement_base                 base;
  struct symbol                            *name;
  bool                                      async;
  bool                                      has_yield;
  struct parameter_shape                    parameter_shape;
  union ast_expression *nullable            return_type;
  struct ast_statement_list                *body;
  unsigned                                  num_decorators;
  union ast_expression * nonnull * nullable decorators;
  struct ast_scope_bindings *nullable       scope;
  struct parameter                          parameters[];
};

struct ast_del {
  struct ast_statement_base base;
  union ast_expression     *targets;
};

struct ast_expression_statement {
  struct ast_statement_base base;
  union ast_expression     *expression;
};

struct ast_for {
  struct ast_statement_base           base;
  bool                                async;
  union ast_expression               *targets;
  union ast_expression               *expression;
  struct ast_statement_list          *body;
  struct ast_statement_list *nullable else_body;
};

struct ast_from_import {
  struct ast_statement_base         base;
  unsigned                          num_prefix_dots;
  struct dotted_name *nullable      module;
  bool                              import_star;
  unsigned                          num_items;
  struct from_import_item *nullable items;
};

struct ast_global {
  struct ast_statement_base         base;
  unsigned                          num_names;
  struct symbol * nonnull * nonnull names;
};

struct ast_if_elif {
  union ast_expression      *condition;
  struct ast_statement_list *body;
};

struct ast_if {
  struct ast_statement_base           base;
  union ast_expression               *condition;
  struct ast_statement_list          *body;
  unsigned                            num_elifs;
  struct ast_if_elif *nullable        elifs;
  struct ast_statement_list *nullable else_body;
};

struct ast_import_item {
  struct dotted_name     *module;
  struct symbol *nullable as;
};

struct ast_import {
  struct ast_statement_base        base;
  unsigned                         num_items;
  struct ast_import_item *nullable items;
};

struct ast_statement_nonlocal {
  struct ast_statement_base         base;
  unsigned                          num_names;
  struct symbol * nonnull * nonnull names;
};

struct ast_pass {
  struct ast_statement_base base;
};

struct ast_raise {
  struct ast_statement_base      base;
  union ast_expression *nullable expression;
  union ast_expression *nullable from;
};

struct ast_return {
  struct ast_statement_base      base;
  union ast_expression *nullable expression;
};

struct ast_try_except {
  struct location                location;
  union ast_expression *nullable match;
  struct symbol *nullable        as;
  struct ast_statement_list     *body;
};

struct ast_try {
  struct ast_statement_base           base;
  struct ast_statement_list          *body;
  unsigned                            num_excepts;
  struct ast_try_except *nullable     excepts;
  struct ast_statement_list *nullable else_body;
  struct ast_statement_list *nullable finally_body;
};

struct ast_while {
  struct ast_statement_base           base;
  union ast_expression               *condition;
  struct ast_statement_list          *body;
  struct ast_statement_list *nullable else_body;
};

struct ast_with_item {
  union ast_expression          *expression;
  struct location                as_location;
  union ast_expression *nullable targets;
};

struct ast_with {
  struct ast_statement_base      base;
  bool                           async;
  unsigned                       num_items;
  struct ast_with_item *nullable items;
  struct ast_statement_list     *body;
};

struct ast_yield {
  struct ast_statement_base      base;
  union ast_expression *nullable expression;
};

union ast_statement {
  uint8_t                   type;
  struct ast_statement_base base;

  struct ast_statement_annotation annotation;
  struct ast_assert               assert;
  struct ast_assignment           assign;
  struct ast_augassign            augassign;
  struct ast_break                break_;
  struct ast_class                class_;
  struct ast_continue             continue_;
  struct ast_def                  def;
  struct ast_del                  del;
  struct ast_expression_statement expression;
  struct ast_for                  for_;
  struct ast_from_import          from_import;
  struct ast_global               global;
  struct ast_if                   if_;
  struct ast_import               import;
  struct ast_statement_nonlocal   nonlocal;
  struct ast_pass                 pass;
  struct ast_raise                raise;
  struct ast_return               return_;
  struct ast_try                  try_;
  struct ast_while                while_;
  struct ast_with                 with;
  struct ast_yield                yield;
};

ASSUME_NONNULL_END

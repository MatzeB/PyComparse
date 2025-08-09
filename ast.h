#pragma once

union ast_expression;
union object;
struct ast_tuple_list_form;
struct object_intern;

enum ast_node_type {
  AST_ATTR,
  AST_BINEXPR_ADD,
  AST_BINEXPR_AND,
  AST_BINEXPR_ASSIGN,
  AST_BINEXPR_EQUAL,
  AST_BINEXPR_FLOORDIV,
  AST_BINEXPR_GREATER,
  AST_BINEXPR_GREATER_EQUAL,
  AST_BINEXPR_IN,
  AST_BINEXPR_IS,
  AST_BINEXPR_IS_NOT,
  AST_BINEXPR_LESS,
  AST_BINEXPR_LESS_EQUAL,
  AST_BINEXPR_LOGICAL_AND,
  AST_BINEXPR_LOGICAL_OR,
  AST_BINEXPR_MATMUL,
  AST_BINEXPR_MOD,
  AST_BINEXPR_MUL,
  AST_BINEXPR_NOT_IN,
  AST_BINEXPR_OR,
  AST_BINEXPR_SUB,
  AST_BINEXPR_SUBSCRIPT,
  AST_BINEXPR_TRUEDIV,
  AST_BINEXPR_UNEQUAL,
  AST_BINEXPR_XOR,
  AST_CALL,
  AST_CONST,
  AST_GENERATOR_EXPRESSION,
  AST_IDENTIFIER,
  AST_LIST_FORM,
  AST_TUPLE_FORM,
  AST_UNEXPR_INVERT,
  AST_UNEXPR_NEGATIVE,
  AST_UNEXPR_NOT,
  AST_UNEXPR_PLUS,
};

union object *ast_expression_as_constant(union ast_expression *expression);

union object *ast_tuple_compute_constant(struct object_intern *intern,
                                         struct ast_tuple_list_form *tuple);

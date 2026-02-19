#include "pycomparse/ast_fold_constants.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "pycomparse/adt/arena.h"
#include "pycomparse/ast.h"
#include "pycomparse/ast_expression_types.h"
#include "pycomparse/ast_statement_types.h"
#include "pycomparse/object.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/object_types.h"
#include "pycomparse/opcodes.h"
#include "pycomparse/util.h"

/* Returns the logically negated comparison op for `not (a OP b) --> a NEG_OP
 * b`. Only call this for ops that have a logical negation (not
 * COMPARE_OP_EXC_MATCH). */
static uint8_t get_compare_op_negated(uint8_t op)
{
  switch (op) {
  case COMPARE_OP_LT:
    return COMPARE_OP_GE;
  case COMPARE_OP_LE:
    return COMPARE_OP_GT;
  case COMPARE_OP_EQ:
    return COMPARE_OP_NE;
  case COMPARE_OP_NE:
    return COMPARE_OP_EQ;
  case COMPARE_OP_GT:
    return COMPARE_OP_LE;
  case COMPARE_OP_GE:
    return COMPARE_OP_LT;
  case COMPARE_OP_IN:
    return COMPARE_OP_NOT_IN;
  case COMPARE_OP_NOT_IN:
    return COMPARE_OP_IN;
  case COMPARE_OP_IS:
    return COMPARE_OP_IS_NOT;
  case COMPARE_OP_IS_NOT:
    return COMPARE_OP_IS;
  default:
    abort();
  }
}

struct constant_fold_state {
  struct object_intern *intern;
  struct arena         *ast_arena;
};

#define CONSTANT_FOLD_MAX_COLLECTION_SIZE 256u
#define CONSTANT_FOLD_MAX_STR_SIZE        4096u
#define CONSTANT_FOLD_MAX_TOTAL_ITEMS     1024u

static void fold_statement_list(struct constant_fold_state *s,
                                struct ast_statement_list  *statements);

static union ast_expression *fold_expression(struct constant_fold_state *s,
                                             union ast_expression *expression);
static union ast_expression *
new_const_expression(struct constant_fold_state *s, union object *object,
                     struct location location);

static inline union ast_expression *nullable fold_expression_nullable(
    struct constant_fold_state *s, union ast_expression *nullable expression)
{
  if (expression == NULL) {
    return NULL;
  }
  return fold_expression(s, expression);
}

static bool object_constant_truth_value(const union object *object,
                                        bool               *truth)
{
  switch (object_type(object)) {
  case OBJECT_FALSE:
  case OBJECT_NONE:
    *truth = false;
    return true;
  case OBJECT_TRUE:
  case OBJECT_ELLIPSIS:
  case OBJECT_CODE:
    *truth = true;
    return true;
  case OBJECT_INT:
    *truth = object_int_value(object) != 0;
    return true;
  case OBJECT_BIG_INT:
    *truth = object->big_int.num_pydigits != 0;
    return true;
  case OBJECT_FLOAT:
    *truth = object_float_value(object) != 0.0;
    return true;
  case OBJECT_COMPLEX:
    *truth = object_complex_real(object) != 0.0
             || object_complex_imag(object) != 0.0;
    return true;
  case OBJECT_STRING:
  case OBJECT_BYTES:
    *truth = object_string_length(object) != 0;
    return true;
  case OBJECT_TUPLE:
  case OBJECT_FROZENSET:
    *truth = object_tuple_length(object) != 0;
    return true;
  default:
    return false;
  }
}

static union ast_expression *
canonicalize_condition_constant(struct constant_fold_state *s,
                                union ast_expression       *condition)
{
  union object *constant = ast_expression_as_constant(condition);
  if (constant == NULL) {
    return condition;
  }
  enum object_type constant_type = object_type(constant);
  if (constant_type == OBJECT_TRUE || constant_type == OBJECT_FALSE) {
    return condition;
  }
  bool truth = false;
  if (!object_constant_truth_value(constant, &truth)) {
    return condition;
  }
  return new_const_expression(
      s,
      object_intern_singleton(s->intern, truth ? OBJECT_TRUE : OBJECT_FALSE),
      get_expression_location(condition));
}

static inline bool object_as_fast_int(const union object *object,
                                      int64_t            *value)
{
  if (object_type(object) != OBJECT_INT) {
    return false;
  }
  *value = object_int_value(object);
  return true;
}

static inline union object *nullable
fast_int_constant_object(struct constant_fold_state *s, int64_t value)
{
  if (value == INT64_MIN) {
    return NULL;
  }
  return object_intern_int(s->intern, value);
}

static void int64_floor_divmod(int64_t left, int64_t right, int64_t *quotient,
                               int64_t *remainder)
{
  int64_t q = left / right;
  int64_t r = left % right;
  if (r != 0 && ((r < 0) != (right < 0))) {
    q -= 1;
    r += right;
  }
  *quotient = q;
  *remainder = r;
}

static inline bool object_is_string_like(enum object_type type)
{
  return type == OBJECT_STRING || type == OBJECT_BYTES;
}

static union object *nullable fold_string_like_add(
    struct constant_fold_state *s, union object *left, union object *right)
{
  enum object_type left_type = object_type(left);
  enum object_type right_type = object_type(right);
  if (left_type != right_type || !object_is_string_like(left_type)) {
    return NULL;
  }
  uint64_t left_length = object_string_length(left);
  uint64_t right_length = object_string_length(right);
  uint64_t combined_length = left_length + right_length;
  if (combined_length > UINT32_MAX) {
    return NULL;
  }
  if (combined_length == 0) {
    return object_intern_string(s->intern, left_type, 0, "");
  }
  char *chars = arena_allocate(s->ast_arena, (size_t)combined_length, 1);
  memcpy(chars, left->string.chars, (size_t)left_length);
  memcpy(chars + left_length, right->string.chars, (size_t)right_length);
  return object_intern_string(s->intern, left_type, (uint32_t)combined_length,
                              chars);
}

static union object *nullable fold_tuple_add(struct constant_fold_state *s,
                                             union object               *left,
                                             union object               *right)
{
  if (object_type(left) != OBJECT_TUPLE
      || object_type(right) != OBJECT_TUPLE) {
    return NULL;
  }

  uint64_t left_length = object_tuple_length(left);
  uint64_t right_length = object_tuple_length(right);
  uint64_t combined_length = left_length + right_length;
  if (combined_length > UINT32_MAX) {
    return NULL;
  }

  struct tuple_prep *tuple
      = object_intern_tuple_begin(s->intern, (uint32_t)combined_length);
  for (uint64_t i = 0; i < left_length; ++i) {
    object_new_tuple_set_at(tuple, (uint32_t)i,
                            object_tuple_at(left, (uint32_t)i));
  }
  for (uint64_t i = 0; i < right_length; ++i) {
    object_new_tuple_set_at(tuple, (uint32_t)(left_length + i),
                            object_tuple_at(right, (uint32_t)i));
  }
  return object_intern_tuple_end(s->intern, tuple, /*may_free_arena=*/true);
}

static int64_t check_tuple_complexity(union object *tuple, int64_t limit)
{
  if (object_type(tuple) != OBJECT_TUPLE) {
    return limit;
  }
  uint32_t tuple_length = object_tuple_length(tuple);
  limit -= (int64_t)tuple_length;
  if (limit < 0) {
    return limit;
  }
  for (uint32_t i = 0; i < tuple_length && limit >= 0; ++i) {
    limit = check_tuple_complexity(object_tuple_at(tuple, i), limit);
  }
  return limit;
}

static bool try_sequence_repeat_operands(union object  *left,
                                         union object  *right,
                                         union object **sequence,
                                         uint32_t      *repeat_count)
{
  int64_t count = 0;
  if (object_as_fast_int(left, &count)) {
    if (count < 0 || count > UINT32_MAX) {
      return false;
    }
    *sequence = right;
    *repeat_count = (uint32_t)count;
    return true;
  }
  if (object_as_fast_int(right, &count)) {
    if (count < 0 || count > UINT32_MAX) {
      return false;
    }
    *sequence = left;
    *repeat_count = (uint32_t)count;
    return true;
  }
  return false;
}

static union object *nullable
fold_string_like_repeat(struct constant_fold_state *s, union object *sequence,
                        uint32_t repeat_count)
{
  enum object_type sequence_type = object_type(sequence);
  if (!object_is_string_like(sequence_type)) {
    return NULL;
  }

  uint32_t sequence_length = object_string_length(sequence);
  if (sequence_length != 0
      && repeat_count > CONSTANT_FOLD_MAX_STR_SIZE / sequence_length) {
    return NULL;
  }

  uint64_t total_length = (uint64_t)sequence_length * repeat_count;
  if (total_length > UINT32_MAX) {
    return NULL;
  }
  if (total_length == 0) {
    return object_intern_string(s->intern, sequence_type, 0, "");
  }

  char *chars = arena_allocate(s->ast_arena, (size_t)total_length, 1);
  for (uint32_t i = 0; i < repeat_count; ++i) {
    memcpy(chars + (size_t)i * sequence_length, sequence->string.chars,
           sequence_length);
  }
  return object_intern_string(s->intern, sequence_type, (uint32_t)total_length,
                              chars);
}

static union object *nullable fold_tuple_repeat(struct constant_fold_state *s,
                                                union object *sequence,
                                                uint32_t      repeat_count)
{
  if (object_type(sequence) != OBJECT_TUPLE) {
    return NULL;
  }

  uint32_t sequence_length = object_tuple_length(sequence);
  if (sequence_length != 0
      && repeat_count > CONSTANT_FOLD_MAX_COLLECTION_SIZE / sequence_length) {
    return NULL;
  }
  if (repeat_count != 0
      && check_tuple_complexity(
             sequence, (int64_t)(CONSTANT_FOLD_MAX_TOTAL_ITEMS / repeat_count))
             < 0) {
    return NULL;
  }

  uint64_t total_length = (uint64_t)sequence_length * repeat_count;
  if (total_length > UINT32_MAX) {
    return NULL;
  }

  struct tuple_prep *tuple
      = object_intern_tuple_begin(s->intern, (uint32_t)total_length);
  uint32_t write_index = 0;
  for (uint32_t i = 0; i < repeat_count; ++i) {
    for (uint32_t j = 0; j < sequence_length; ++j) {
      object_new_tuple_set_at(tuple, write_index++,
                              object_tuple_at(sequence, j));
    }
  }
  return object_intern_tuple_end(s->intern, tuple, /*may_free_arena=*/true);
}

static union object *nullable try_fold_sequence_repeat(
    struct constant_fold_state *s, union object *left, union object *right)
{
  union object *sequence = NULL;
  uint32_t      repeat_count = 0;
  if (!try_sequence_repeat_operands(left, right, &sequence, &repeat_count)) {
    return NULL;
  }
  union object *folded = fold_string_like_repeat(s, sequence, repeat_count);
  if (folded != NULL) {
    return folded;
  }
  return fold_tuple_repeat(s, sequence, repeat_count);
}

static size_t ast_expression_size(union ast_expression *expression)
{
  switch (ast_expression_type(expression)) {
  case AST_ATTR:
    return sizeof(struct ast_attr);
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_ADD_ASSIGN:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_AND_ASSIGN:
  case AST_BINEXPR_ASSIGN:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_FLOORDIV_ASSIGN:
  case AST_BINEXPR_LOGICAL_AND:
  case AST_BINEXPR_LOGICAL_OR:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MATMUL_ASSIGN:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MOD_ASSIGN:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_MUL_ASSIGN:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_OR_ASSIGN:
  case AST_BINEXPR_POWER:
  case AST_BINEXPR_POWER_ASSIGN:
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_LEFT_ASSIGN:
  case AST_BINEXPR_SHIFT_RIGHT:
  case AST_BINEXPR_SHIFT_RIGHT_ASSIGN:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_SUB_ASSIGN:
  case AST_BINEXPR_SUBSCRIPT:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_TRUEDIV_ASSIGN:
  case AST_BINEXPR_XOR:
  case AST_BINEXPR_XOR_ASSIGN:
    return sizeof(struct ast_binexpr);
  case AST_CALL:
    return sizeof(struct ast_call)
           + (size_t)expression->call.num_arguments
                 * sizeof(expression->call.arguments[0]);
  case AST_COMPARISON:
    return sizeof(struct ast_comparison)
           + (size_t)expression->comparison.num_operands
                 * sizeof(expression->comparison.operands[0]);
  case AST_CONDITIONAL:
    return sizeof(struct ast_conditional);
  case AST_CONST:
  case AST_INVALID:
    return sizeof(struct ast_const);
  case AST_DICT_DISPLAY:
    return sizeof(struct ast_dict_item_list)
           + (size_t)expression->dict_item_list.num_items
                 * sizeof(expression->dict_item_list.items[0]);
  case AST_DICT_COMPREHENSION:
  case AST_GENERATOR_EXPRESSION:
  case AST_LIST_COMPREHENSION:
  case AST_SET_COMPREHENSION:
    return sizeof(struct ast_generator_expression)
           + (size_t)expression->generator_expression.num_parts
                 * sizeof(expression->generator_expression.parts[0]);
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY:
  case AST_SET_DISPLAY:
    return sizeof(struct ast_expression_list)
           + (size_t)expression->expression_list.num_expressions
                 * sizeof(expression->expression_list.expressions[0]);
  case AST_FSTRING:
    return sizeof(struct ast_fstring)
           + (size_t)expression->fstring.num_elements
                 * sizeof(expression->fstring.elements[0]);
  case AST_IDENTIFIER:
    return sizeof(struct ast_identifier);
  case AST_LAMBDA:
    return sizeof(struct ast_lambda)
           + (size_t)expression->lambda.parameter_shape.num_parameters
                 * sizeof(expression->lambda.parameters[0]);
  case AST_SLICE:
    return sizeof(struct ast_slice);
  case AST_UNEXPR_AWAIT:
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_NOT:
  case AST_UNEXPR_PLUS:
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
    return sizeof(struct ast_unexpr);
  case AST_YIELD:
  case AST_YIELD_FROM:
    return sizeof(struct ast_expression_yield);
  }
}

static union ast_expression *clone_expression(struct constant_fold_state *s,
                                              union ast_expression *expression)
{
  size_t                size = ast_expression_size(expression);
  union ast_expression *result = (union ast_expression *)arena_allocate(
      s->ast_arena, size, alignof(union ast_expression));
  memcpy(result, expression, size);
  return result;
}

static union ast_expression *
new_const_expression(struct constant_fold_state *s, union object *object,
                     struct location location)
{
  union ast_expression *result = (union ast_expression *)arena_allocate(
      s->ast_arena, sizeof(struct ast_const), alignof(union ast_expression));
  memset(result, 0, sizeof(struct ast_const));
  result->type = AST_CONST;
  result->cnst.base.location = location;
  result->cnst.object = object;
  return result;
}

static union object *nullable try_fold_unexpr(struct constant_fold_state *s,
                                              union ast_expression *expression)
{
  enum ast_expression_type type = ast_expression_type(expression);
  switch (type) {
  case AST_UNEXPR_PLUS:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NOT:
    break;
  default:
    return NULL;
  }

  union object *object = ast_expression_as_constant(expression->unexpr.op);
  if (object == NULL) {
    return NULL;
  }
  enum object_type object_kind = object_type(object);
  switch (type) {
  case AST_UNEXPR_PLUS:
    if (object_kind == OBJECT_INT || object_kind == OBJECT_BIG_INT
        || object_kind == OBJECT_FLOAT || object_kind == OBJECT_COMPLEX) {
      return object;
    }
    return NULL;
  case AST_UNEXPR_NEGATIVE:
    if (object_kind == OBJECT_INT) {
      int64_t value = object_int_value(object);
      assert(value != INT64_MIN);
      return object_intern_int(s->intern, -value);
    }
    if (object_kind == OBJECT_FLOAT) {
      return object_intern_float(s->intern, -object_float_value(object));
    }
    if (object_kind == OBJECT_COMPLEX) {
      return object_intern_complex(s->intern, -object_complex_real(object),
                                   -object_complex_imag(object));
    }
    return NULL;
  case AST_UNEXPR_INVERT:
    if (object_kind != OBJECT_INT) {
      return NULL;
    }
    int64_t value = object_int_value(object);
    if (value == INT64_MAX) {
      return NULL;
    }
    return object_intern_int(s->intern, ~value);
  case AST_UNEXPR_NOT: {
    bool truth = false;
    if (!object_constant_truth_value(object, &truth)) {
      return NULL;
    }
    return object_intern_singleton(s->intern,
                                   truth ? OBJECT_FALSE : OBJECT_TRUE);
  }
  default:
    return NULL;
  }
}

static union object *nullable try_fold_binexpr(
    struct constant_fold_state *s, union ast_expression *expression)
{
  enum ast_expression_type type = ast_expression_type(expression);
  switch (type) {
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_POWER:
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_RIGHT:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_XOR:
    break;
  default:
    return NULL;
  }

  union object *left_object
      = ast_expression_as_constant(expression->binexpr.left);
  union object *right_object
      = ast_expression_as_constant(expression->binexpr.right);
  if (left_object == NULL || right_object == NULL) {
    return NULL;
  }

  if (type == AST_BINEXPR_ADD) {
    union object *folded = fold_string_like_add(s, left_object, right_object);
    if (folded != NULL) {
      return folded;
    }
    folded = fold_tuple_add(s, left_object, right_object);
    if (folded != NULL) {
      return folded;
    }
  } else if (type == AST_BINEXPR_MUL) {
    union object *folded
        = try_fold_sequence_repeat(s, left_object, right_object);
    if (folded != NULL) {
      return folded;
    }
  }

  int64_t left_value;
  int64_t right_value;
  if (!object_as_fast_int(left_object, &left_value)
      || !object_as_fast_int(right_object, &right_value)) {
    return NULL;
  }

  int64_t result;
  switch (type) {
  case AST_BINEXPR_ADD:
    if (__builtin_add_overflow(left_value, right_value, &result)) {
      return NULL;
    }
    return fast_int_constant_object(s, result);
  case AST_BINEXPR_SUB:
    if (__builtin_sub_overflow(left_value, right_value, &result)) {
      return NULL;
    }
    return fast_int_constant_object(s, result);
  case AST_BINEXPR_MUL:
    if (__builtin_mul_overflow(left_value, right_value, &result)) {
      return NULL;
    }
    return fast_int_constant_object(s, result);
  case AST_BINEXPR_AND:
    return fast_int_constant_object(s, left_value & right_value);
  case AST_BINEXPR_OR:
    return fast_int_constant_object(s, left_value | right_value);
  case AST_BINEXPR_XOR:
    return fast_int_constant_object(s, left_value ^ right_value);
  case AST_BINEXPR_SHIFT_LEFT: {
    if (right_value < 0 || right_value >= 63) return NULL;
    if (left_value == 0) {
      return fast_int_constant_object(s, 0);
    }
    int64_t factor = INT64_C(1) << right_value;
    if (__builtin_mul_overflow(left_value, factor, &result)) {
      return NULL;
    }
    return fast_int_constant_object(s, result);
  }
  case AST_BINEXPR_SHIFT_RIGHT: {
    if (right_value < 0) return NULL;
    if (right_value >= 63) {
      return fast_int_constant_object(s, left_value < 0 ? -1 : 0);
    }

    int64_t quotient;
    int64_t remainder;
    int64_floor_divmod(left_value, INT64_C(1) << right_value, &quotient,
                       &remainder);
    (void)remainder;
    return fast_int_constant_object(s, quotient);
  }
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_MOD: {
    if (right_value == 0) return NULL;

    int64_t quotient;
    int64_t remainder;
    int64_floor_divmod(left_value, right_value, &quotient, &remainder);
    if (type == AST_BINEXPR_FLOORDIV) {
      return fast_int_constant_object(s, quotient);
    }
    return fast_int_constant_object(s, remainder);
  }
  case AST_BINEXPR_POWER: {
    if (right_value < 0) return NULL;

    uint64_t exponent = (uint64_t)right_value;
    int64_t  base = left_value;
    int64_t  power_result = 1;
    while (exponent != 0) {
      if ((exponent & 1u) != 0u) {
        if (__builtin_mul_overflow(power_result, base, &power_result)) {
          return NULL;
        }
        if (power_result == INT64_MIN) {
          return NULL;
        }
      }
      exponent >>= 1;
      if (exponent == 0) break;
      if (__builtin_mul_overflow(base, base, &base)) {
        return NULL;
      }
      if (base == INT64_MIN) {
        return NULL;
      }
    }
    return fast_int_constant_object(s, power_result);
  }
  default:
    return NULL;
  }
}

static union ast_expression *fold_expression(struct constant_fold_state *s,
                                             union ast_expression *expression)
{
  enum ast_expression_type type = ast_expression_type(expression);
  union ast_expression    *result = clone_expression(s, expression);

  switch (type) {
  case AST_ATTR:
    result->attr.expression = fold_expression(s, expression->attr.expression);
    break;
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_ADD_ASSIGN:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_AND_ASSIGN:
  case AST_BINEXPR_ASSIGN:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_FLOORDIV_ASSIGN:
  case AST_BINEXPR_LOGICAL_AND:
  case AST_BINEXPR_LOGICAL_OR:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MATMUL_ASSIGN:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MOD_ASSIGN:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_MUL_ASSIGN:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_OR_ASSIGN:
  case AST_BINEXPR_POWER:
  case AST_BINEXPR_POWER_ASSIGN:
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_LEFT_ASSIGN:
  case AST_BINEXPR_SHIFT_RIGHT:
  case AST_BINEXPR_SHIFT_RIGHT_ASSIGN:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_SUB_ASSIGN:
  case AST_BINEXPR_SUBSCRIPT:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_TRUEDIV_ASSIGN:
  case AST_BINEXPR_XOR:
  case AST_BINEXPR_XOR_ASSIGN:
    result->binexpr.left = fold_expression(s, expression->binexpr.left);
    result->binexpr.right = fold_expression(s, expression->binexpr.right);
    {
      union object *folded = try_fold_binexpr(s, result);
      if (folded != NULL) {
        return new_const_expression(s, folded,
                                    get_expression_location(result));
      }
    }
    break;
  case AST_CALL:
    result->call.callee = fold_expression_nullable(s, expression->call.callee);
    for (unsigned i = 0; i < expression->call.num_arguments; ++i) {
      result->call.arguments[i].expression
          = fold_expression(s, expression->call.arguments[i].expression);
    }
    break;
  case AST_COMPARISON:
    result->comparison.left = fold_expression(s, expression->comparison.left);
    for (unsigned i = 0; i < expression->comparison.num_operands; ++i) {
      result->comparison.operands[i].operand
          = fold_expression(s, expression->comparison.operands[i].operand);
    }
    /* x in {const, ...}  -->  x in frozenset({const, ...}) */
    if (result->comparison.num_operands == 1) {
      uint8_t               op = result->comparison.operands[0].op;
      union ast_expression *rhs = result->comparison.operands[0].operand;
      if ((op == COMPARE_OP_IN || op == COMPARE_OP_NOT_IN)
          && ast_expression_type(rhs) == AST_SET_DISPLAY
          && !rhs->expression_list.has_star_expression) {
        struct ast_expression_list *set = &rhs->expression_list;
        bool                        all_const = true;
        for (unsigned i = 0; i < set->num_expressions; ++i) {
          if (ast_expression_as_constant(set->expressions[i]) == NULL) {
            all_const = false;
            break;
          }
        }
        if (all_const) {
          struct tuple_prep *prep
              = object_intern_tuple_begin(s->intern, set->num_expressions);
          for (unsigned i = 0; i < set->num_expressions; ++i) {
            object_new_tuple_set_at(
                prep, i, ast_expression_as_constant(set->expressions[i]));
          }
          union object *frozenset
              = object_intern_tuple_end_as_frozenset(s->intern, prep, true);
          result->comparison.operands[0].operand = new_const_expression(
              s, frozenset, get_expression_location(rhs));
        }
      }
    }
    break;
  case AST_CONDITIONAL:
    result->conditional.condition
        = fold_expression(s, expression->conditional.condition);
    result->conditional.true_expression
        = fold_expression(s, expression->conditional.true_expression);
    result->conditional.false_expression
        = fold_expression(s, expression->conditional.false_expression);
    break;
  case AST_CONST:
  case AST_IDENTIFIER:
  case AST_INVALID:
    break;
  case AST_DICT_COMPREHENSION:
  case AST_GENERATOR_EXPRESSION:
  case AST_LIST_COMPREHENSION:
  case AST_SET_COMPREHENSION:
    result->generator_expression.expression
        = fold_expression(s, expression->generator_expression.expression);
    result->generator_expression.item_value = fold_expression_nullable(
        s, expression->generator_expression.item_value);
    for (unsigned i = 0; i < expression->generator_expression.num_parts; ++i) {
      result->generator_expression.parts[i].targets = fold_expression_nullable(
          s, expression->generator_expression.parts[i].targets);
      result->generator_expression.parts[i].expression = fold_expression(
          s, expression->generator_expression.parts[i].expression);
    }
    break;
  case AST_DICT_DISPLAY:
    for (unsigned i = 0; i < expression->dict_item_list.num_items; ++i) {
      result->dict_item_list.items[i].key = fold_expression_nullable(
          s, expression->dict_item_list.items[i].key);
      result->dict_item_list.items[i].value
          = fold_expression(s, expression->dict_item_list.items[i].value);
    }
    break;
  case AST_EXPRESSION_LIST:
  case AST_LIST_DISPLAY:
  case AST_SET_DISPLAY:
    for (unsigned i = 0; i < expression->expression_list.num_expressions;
         ++i) {
      result->expression_list.expressions[i]
          = fold_expression(s, expression->expression_list.expressions[i]);
    }
    if (type == AST_EXPRESSION_LIST) {
      if (result->expression_list.has_star_expression) {
        result->expression_list.as_constant = NULL;
      } else {
        result->expression_list.as_constant
            = ast_tuple_compute_constant(s->intern, &result->expression_list);
      }
    }
    break;
  case AST_FSTRING:
    for (unsigned i = 0; i < expression->fstring.num_elements; ++i) {
      if (expression->fstring.elements[i].is_expression) {
        result->fstring.elements[i].u.expression
            = fold_expression(s, expression->fstring.elements[i].u.expression);
      }
      result->fstring.elements[i].format_spec = fold_expression_nullable(
          s, expression->fstring.elements[i].format_spec);
    }
    break;
  case AST_LAMBDA:
    for (unsigned i = 0; i < expression->lambda.parameter_shape.num_parameters;
         ++i) {
      result->lambda.parameters[i].type
          = fold_expression_nullable(s, expression->lambda.parameters[i].type);
      result->lambda.parameters[i].initializer = fold_expression_nullable(
          s, expression->lambda.parameters[i].initializer);
    }
    result->lambda.expression
        = fold_expression(s, expression->lambda.expression);
    break;
  case AST_SLICE:
    result->slice.start = fold_expression_nullable(s, expression->slice.start);
    result->slice.stop = fold_expression_nullable(s, expression->slice.stop);
    result->slice.step = fold_expression_nullable(s, expression->slice.step);
    break;
  case AST_UNEXPR_AWAIT:
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_NOT:
  case AST_UNEXPR_PLUS:
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
    result->unexpr.op = fold_expression(s, expression->unexpr.op);
    {
      union object *folded = try_fold_unexpr(s, result);
      if (folded != NULL) {
        return new_const_expression(s, folded,
                                    get_expression_location(result));
      }
    }
    if (type == AST_UNEXPR_NOT) {
      /* not (a OP b)  -->  a NEG_OP b  for negatable single comparisons */
      union ast_expression *inner = result->unexpr.op;
      if (ast_expression_type(inner) == AST_COMPARISON
          && inner->comparison.num_operands == 1) {
        uint8_t op = inner->comparison.operands[0].op;
        if (op != COMPARE_OP_EXC_MATCH) {
          inner->comparison.operands[0].op = get_compare_op_negated(op);
          return inner;
        }
      }
    }
    break;
  case AST_YIELD:
  case AST_YIELD_FROM:
    result->yield.value = fold_expression_nullable(s, expression->yield.value);
    break;
  }

  return result;
}

static void
fold_expression_array_inplace(struct constant_fold_state     *s,
                              union ast_expression **nullable expressions,
                              unsigned                        num_expressions)
{
  if (expressions == NULL || num_expressions == 0) {
    return;
  }
  for (unsigned i = 0; i < num_expressions; ++i) {
    expressions[i] = fold_expression(s, expressions[i]);
  }
}

static void fold_parameters_inplace(struct constant_fold_state *s,
                                    struct parameter *nullable  parameters,
                                    unsigned                    num_parameters)
{
  if (parameters == NULL || num_parameters == 0) {
    return;
  }
  for (unsigned i = 0; i < num_parameters; ++i) {
    parameters[i].type = fold_expression_nullable(s, parameters[i].type);
    parameters[i].initializer
        = fold_expression_nullable(s, parameters[i].initializer);
  }
}

static void fold_if_elifs_inplace(struct constant_fold_state  *s,
                                  struct ast_if_elif *nullable elifs,
                                  unsigned                     num_elifs)
{
  if (elifs == NULL || num_elifs == 0) {
    return;
  }
  for (unsigned i = 0; i < num_elifs; ++i) {
    elifs[i].condition = fold_expression(s, elifs[i].condition);
    elifs[i].condition
        = canonicalize_condition_constant(s, elifs[i].condition);
    fold_statement_list(s, elifs[i].body);
  }
}

static void fold_try_excepts_inplace(struct constant_fold_state     *s,
                                     struct ast_try_except *nullable excepts,
                                     unsigned num_excepts)
{
  if (excepts == NULL || num_excepts == 0) {
    return;
  }
  for (unsigned i = 0; i < num_excepts; ++i) {
    excepts[i].match = fold_expression_nullable(s, excepts[i].match);
    fold_statement_list(s, excepts[i].body);
  }
}

static void fold_with_items_inplace(struct constant_fold_state    *s,
                                    struct ast_with_item *nullable items,
                                    unsigned                       num_items)
{
  if (items == NULL || num_items == 0) {
    return;
  }
  for (unsigned i = 0; i < num_items; ++i) {
    items[i].expression = fold_expression(s, items[i].expression);
    items[i].targets = fold_expression_nullable(s, items[i].targets);
  }
}

static void fold_statement(struct constant_fold_state *s,
                           union ast_statement        *statement)
{
  switch (ast_statement_type(statement)) {
  case AST_STATEMENT_ANNOTATION:
    statement->annotation.target
        = fold_expression(s, statement->annotation.target);
    statement->annotation.annotation
        = fold_expression(s, statement->annotation.annotation);
    statement->annotation.value
        = fold_expression_nullable(s, statement->annotation.value);
    break;
  case AST_STATEMENT_ASSERT:
    statement->assert.expression
        = fold_expression(s, statement->assert.expression);
    statement->assert.message
        = fold_expression_nullable(s, statement->assert.message);
    break;
  case AST_STATEMENT_ASSIGN:
    fold_expression_array_inplace(s, statement->assign.targets,
                                  statement->assign.num_targets);
    statement->assign.value = fold_expression(s, statement->assign.value);
    break;
  case AST_STATEMENT_AUGASSIGN:
    statement->augassign.expression
        = fold_expression(s, statement->augassign.expression);
    break;
  case AST_STATEMENT_BREAK:
    break;
  case AST_STATEMENT_CLASS: {
    union ast_expression *call_expr
        = (union ast_expression *)statement->class_.call;
    union ast_expression *folded_call = fold_expression(s, call_expr);
    if (ast_expression_type(folded_call) != AST_CALL) {
      internal_error("class call fold changed node type unexpectedly");
    }
    statement->class_.call = &folded_call->call;
    fold_expression_array_inplace(s, statement->class_.decorators,
                                  statement->class_.num_decorators);
    fold_statement_list(s, statement->class_.body);
    break;
  }
  case AST_STATEMENT_CONTINUE:
    break;
  case AST_STATEMENT_DEF:
    fold_parameters_inplace(s, statement->def.parameters,
                            statement->def.parameter_shape.num_parameters);
    statement->def.return_type
        = fold_expression_nullable(s, statement->def.return_type);
    fold_expression_array_inplace(s, statement->def.decorators,
                                  statement->def.num_decorators);
    fold_statement_list(s, statement->def.body);
    break;
  case AST_STATEMENT_DEL:
    statement->del.targets = fold_expression(s, statement->del.targets);
    break;
  case AST_STATEMENT_EXPRESSION:
    statement->expression.expression
        = fold_expression(s, statement->expression.expression);
    break;
  case AST_STATEMENT_FOR:
    statement->for_.targets = fold_expression(s, statement->for_.targets);
    statement->for_.expression
        = fold_expression(s, statement->for_.expression);
    fold_statement_list(s, statement->for_.body);
    if (statement->for_.else_body != NULL) {
      fold_statement_list(s, statement->for_.else_body);
    }
    break;
  case AST_STATEMENT_FROM_IMPORT:
    break;
  case AST_STATEMENT_GLOBAL:
    break;
  case AST_STATEMENT_IF:
    statement->if_.condition = fold_expression(s, statement->if_.condition);
    statement->if_.condition
        = canonicalize_condition_constant(s, statement->if_.condition);
    fold_statement_list(s, statement->if_.body);
    fold_if_elifs_inplace(s, statement->if_.elifs, statement->if_.num_elifs);
    if (statement->if_.else_body != NULL) {
      fold_statement_list(s, statement->if_.else_body);
    }
    break;
  case AST_STATEMENT_IMPORT:
    break;
  case AST_STATEMENT_NONLOCAL:
    break;
  case AST_STATEMENT_PASS:
    break;
  case AST_STATEMENT_RAISE:
    statement->raise.expression
        = fold_expression_nullable(s, statement->raise.expression);
    statement->raise.from = fold_expression_nullable(s, statement->raise.from);
    break;
  case AST_STATEMENT_RETURN:
    statement->return_.expression
        = fold_expression_nullable(s, statement->return_.expression);
    break;
  case AST_STATEMENT_TRY:
    fold_statement_list(s, statement->try_.body);
    fold_try_excepts_inplace(s, statement->try_.excepts,
                             statement->try_.num_excepts);
    if (statement->try_.else_body != NULL) {
      fold_statement_list(s, statement->try_.else_body);
    }
    if (statement->try_.finally_body != NULL) {
      fold_statement_list(s, statement->try_.finally_body);
    }
    break;
  case AST_STATEMENT_WHILE:
    statement->while_.condition
        = fold_expression(s, statement->while_.condition);
    statement->while_.condition
        = canonicalize_condition_constant(s, statement->while_.condition);
    fold_statement_list(s, statement->while_.body);
    if (statement->while_.else_body != NULL) {
      fold_statement_list(s, statement->while_.else_body);
    }
    break;
  case AST_STATEMENT_WITH:
    fold_with_items_inplace(s, statement->with.items,
                            statement->with.num_items);
    fold_statement_list(s, statement->with.body);
    break;
  case AST_STATEMENT_YIELD:
  case AST_STATEMENT_YIELD_FROM:
    statement->yield.expression
        = fold_expression_nullable(s, statement->yield.expression);
    break;
  }
}

static void fold_statement_list(struct constant_fold_state *s,
                                struct ast_statement_list  *statements)
{
  for (unsigned i = 0; i < statements->num_statements; ++i) {
    fold_statement(s, statements->statements[i]);
  }
}

void ast_fold_constants(struct object_intern *intern, struct arena *ast_arena,
                        struct ast_module *module)
{
  struct constant_fold_state s = {
    .intern = intern,
    .ast_arena = ast_arena,
  };
  fold_statement_list(&s, module->body);
}

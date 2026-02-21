#include "pycomparse/ast_unparse.h"

#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "pycomparse/adt/arena.h"
#include "pycomparse/ast.h"
#include "pycomparse/ast_expression_types.h"
#include "pycomparse/object.h"
#include "pycomparse/object_intern.h"
#include "pycomparse/object_types.h"
#include "pycomparse/opcodes.h"
#include "pycomparse/symbol_types.h"
#include "pycomparse/util.h"

enum unparse_precedence {
  UNPARSE_PREC_ROOT = 0,
  UNPARSE_PREC_NAMED,
  UNPARSE_PREC_LAMBDA,
  UNPARSE_PREC_CONDITIONAL,
  UNPARSE_PREC_LOGICAL_OR,
  UNPARSE_PREC_LOGICAL_AND,
  UNPARSE_PREC_LOGICAL_NOT,
  UNPARSE_PREC_COMPARISON,
  UNPARSE_PREC_BIT_OR,
  UNPARSE_PREC_BIT_XOR,
  UNPARSE_PREC_BIT_AND,
  UNPARSE_PREC_SHIFT,
  UNPARSE_PREC_ADD,
  UNPARSE_PREC_MUL,
  UNPARSE_PREC_UNARY,
  UNPARSE_PREC_POWER,
  UNPARSE_PREC_AWAIT,
  UNPARSE_PREC_ATOM,
};

struct unparse_state {
  struct object_intern *intern;
  struct arena         *arena;
};

static void unparse_expression_prec(struct unparse_state   *s,
                                    union ast_expression   *expression,
                                    enum unparse_precedence parent_precedence);

static inline void append_char(struct unparse_state *s, char c)
{
  arena_grow_char(s->arena, c);
}

static void append_mem(struct unparse_state *s, const char *chars,
                       size_t length)
{
  if (length == 0) return;
  memcpy(arena_grow(s->arena, (unsigned)length), chars, length);
}

static void append_cstring(struct unparse_state *s, const char *cstring)
{
  append_mem(s, cstring, strlen(cstring));
}

static void append_int64(struct unparse_state *s, int64_t value)
{
  char buffer[32];
  int  length = snprintf(buffer, sizeof(buffer), "%" PRId64, value);
  assert(length > 0);
  assert((size_t)length < sizeof(buffer));
  append_mem(s, buffer, (size_t)length);
}

static void append_float(struct unparse_state      *s,
                         const struct object_float *float_obj)
{
  char buffer[128];
  int  length = snprintf(buffer, sizeof(buffer), "%.17g", float_obj->value);
  if (length <= 0 || (size_t)length >= sizeof(buffer)) {
    internal_error("failed to format float");
  }
  append_mem(s, buffer, (size_t)length);
  if (strchr(buffer, '.') == NULL && strchr(buffer, 'e') == NULL
      && strchr(buffer, 'E') == NULL && strcmp(buffer, "inf") != 0
      && strcmp(buffer, "-inf") != 0 && strcmp(buffer, "nan") != 0) {
    append_cstring(s, ".0");
  }
}

static void append_double_for_complex(struct unparse_state *s, double value)
{
  char buffer[128];
  int  length = snprintf(buffer, sizeof(buffer), "%.17g", value);
  if (length <= 0 || (size_t)length >= sizeof(buffer)) {
    internal_error("failed to format complex");
  }
  append_mem(s, buffer, (size_t)length);
}

static void append_escaped_char(struct unparse_state *s, char c)
{
  unsigned char uc = (unsigned char)c;
  switch (uc) {
  case '\\':
    append_cstring(s, "\\\\");
    return;
  case '\'':
    append_cstring(s, "\\'");
    return;
  case '\n':
    append_cstring(s, "\\n");
    return;
  case '\r':
    append_cstring(s, "\\r");
    return;
  case '\t':
    append_cstring(s, "\\t");
    return;
  default:
    break;
  }

  if (uc < 0x20 || uc >= 0x7f) {
    static const char hexdigits[] = "0123456789abcdef";
    append_cstring(s, "\\x");
    append_char(s, hexdigits[(uc >> 4) & 0xf]);
    append_char(s, hexdigits[uc & 0xf]);
    return;
  }
  append_char(s, (char)c);
}

static void append_escaped_string_literal(struct unparse_state       *s,
                                          const struct object_string *string)
{
  append_char(s, '\'');
  for (uint32_t i = 0; i < string->length; ++i) {
    append_escaped_char(s, string->chars[i]);
  }
  append_char(s, '\'');
}

static void append_escaped_fstring_fragment(struct unparse_state *s,
                                            const char *chars, uint32_t length)
{
  for (uint32_t i = 0; i < length; ++i) {
    char c = chars[i];
    if (c == '{' || c == '}') {
      append_char(s, c);
      append_char(s, c);
      continue;
    }
    append_escaped_char(s, c);
  }
}

static void append_big_int_pydigits(struct unparse_state        *s,
                                    const struct object_big_int *big_int)
{
  uint32_t num_pydigits = big_int->num_pydigits;
  assert(num_pydigits > 0);
  size_t max_decimal_digits = (size_t)num_pydigits * 5u;
  size_t digits_size = (size_t)num_pydigits * sizeof(uint16_t);
  size_t alignment_padding = _Alignof(uint16_t) - 1u;
  if (max_decimal_digits > SIZE_MAX - alignment_padding) {
    internal_error("integer too large to unparse");
  }
  size_t scratch_size = max_decimal_digits + alignment_padding;
  if (scratch_size > SIZE_MAX - digits_size) {
    internal_error("integer too large to unparse");
  }
  scratch_size += digits_size;
  if (scratch_size > UINT_MAX || max_decimal_digits > UINT_MAX) {
    internal_error("integer too large to unparse");
  }

  unsigned  output_offset = arena_grow_current_size(s->arena);
  char     *scratch = (char *)arena_grow(s->arena, (unsigned)scratch_size);
  char     *decimal_rev = scratch;
  uintptr_t digits_unaligned = (uintptr_t)(scratch + max_decimal_digits);
  uintptr_t digits_aligned
      = (digits_unaligned + alignment_padding) & ~(uintptr_t)alignment_padding;
  uint16_t *digits = (uint16_t *)digits_aligned;
  memcpy(digits, big_int->pydigits, digits_size);

  unsigned decimal_length = 0;
  while (num_pydigits > 0) {
    uint32_t carry = 0;
    for (uint32_t i = num_pydigits; i-- > 0;) {
      uint32_t current = carry * 32768u + digits[i];
      digits[i] = (uint16_t)(current / 10u);
      carry = current % 10u;
    }
    decimal_rev[decimal_length++] = (char)('0' + carry);
    while (num_pydigits > 0 && digits[num_pydigits - 1] == 0) {
      --num_pydigits;
    }
  }
  assert(decimal_length > 0);

  for (unsigned i = 0, e = decimal_length / 2; i < e; ++i) {
    char tmp = decimal_rev[i];
    decimal_rev[i] = decimal_rev[decimal_length - 1 - i];
    decimal_rev[decimal_length - 1 - i] = tmp;
  }

  arena_grow_truncate(s->arena, output_offset + decimal_length);
}

static enum unparse_precedence
precedence_of_expression(union ast_expression *expression)
{
  switch (ast_expression_type(expression)) {
  case AST_BINEXPR_ASSIGN:
    return UNPARSE_PREC_NAMED;
  case AST_LAMBDA:
    return UNPARSE_PREC_LAMBDA;
  case AST_CONDITIONAL:
    return UNPARSE_PREC_CONDITIONAL;
  case AST_BINEXPR_LOGICAL_OR:
    return UNPARSE_PREC_LOGICAL_OR;
  case AST_BINEXPR_LOGICAL_AND:
    return UNPARSE_PREC_LOGICAL_AND;
  case AST_UNEXPR_NOT:
    return UNPARSE_PREC_LOGICAL_NOT;
  case AST_COMPARISON:
    return UNPARSE_PREC_COMPARISON;
  case AST_BINEXPR_OR:
    return UNPARSE_PREC_BIT_OR;
  case AST_BINEXPR_XOR:
    return UNPARSE_PREC_BIT_XOR;
  case AST_BINEXPR_AND:
    return UNPARSE_PREC_BIT_AND;
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_RIGHT:
    return UNPARSE_PREC_SHIFT;
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_SUB:
    return UNPARSE_PREC_ADD;
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_TRUEDIV:
    return UNPARSE_PREC_MUL;
  case AST_UNEXPR_INVERT:
  case AST_UNEXPR_NEGATIVE:
  case AST_UNEXPR_PLUS:
  case AST_UNEXPR_STAR:
  case AST_UNEXPR_STAR_STAR:
    return UNPARSE_PREC_UNARY;
  case AST_BINEXPR_POWER:
    return UNPARSE_PREC_POWER;
  case AST_UNEXPR_AWAIT:
    return UNPARSE_PREC_AWAIT;
  case AST_ATTR:
  case AST_BINEXPR_SUBSCRIPT:
  case AST_CALL:
  case AST_CONST:
  case AST_DICT_COMPREHENSION:
  case AST_DICT_DISPLAY:
  case AST_EXPRESSION_LIST:
  case AST_FSTRING:
  case AST_GENERATOR_EXPRESSION:
  case AST_IDENTIFIER:
  case AST_LIST_COMPREHENSION:
  case AST_LIST_DISPLAY:
  case AST_SET_COMPREHENSION:
  case AST_SET_DISPLAY:
  case AST_SLICE:
  case AST_YIELD:
  case AST_YIELD_FROM:
    return UNPARSE_PREC_ATOM;
  case AST_INVALID:
  case AST_BINEXPR_ADD_ASSIGN:
  case AST_BINEXPR_AND_ASSIGN:
  case AST_BINEXPR_FLOORDIV_ASSIGN:
  case AST_BINEXPR_MATMUL_ASSIGN:
  case AST_BINEXPR_MOD_ASSIGN:
  case AST_BINEXPR_MUL_ASSIGN:
  case AST_BINEXPR_OR_ASSIGN:
  case AST_BINEXPR_POWER_ASSIGN:
  case AST_BINEXPR_SHIFT_LEFT_ASSIGN:
  case AST_BINEXPR_SHIFT_RIGHT_ASSIGN:
  case AST_BINEXPR_SUB_ASSIGN:
  case AST_BINEXPR_TRUEDIV_ASSIGN:
  case AST_BINEXPR_XOR_ASSIGN:
    break;
  }
  internal_error("invalid expression for unparse precedence");
}

static bool needs_parens(enum unparse_precedence expression_precedence,
                         enum unparse_precedence parent_precedence)
{
  return expression_precedence < parent_precedence;
}

static void unparse_tuple_items(struct unparse_state       *s,
                                struct ast_expression_list *tuple,
                                bool                        wrap_parentheses)
{
  unsigned num_expressions = tuple->num_expressions;
  if (wrap_parentheses) append_char(s, '(');
  for (unsigned i = 0; i < num_expressions; ++i) {
    if (i > 0) append_cstring(s, ", ");
    union ast_expression *expression = tuple->expressions[i];
    if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
      append_char(s, '*');
      expression = expression->unexpr.op;
    }
    unparse_expression_prec(s, expression, UNPARSE_PREC_ROOT);
  }
  if (num_expressions == 1) {
    append_char(s, ',');
  }
  if (wrap_parentheses) append_char(s, ')');
}

static void
unparse_expression_list(struct unparse_state       *s,
                        struct ast_expression_list *expression_list,
                        char open_char, char close_char)
{
  append_char(s, open_char);
  for (unsigned i = 0; i < expression_list->num_expressions; ++i) {
    if (i > 0) append_cstring(s, ", ");
    union ast_expression *expression = expression_list->expressions[i];
    if (ast_expression_type(expression) == AST_UNEXPR_STAR) {
      append_char(s, '*');
      expression = expression->unexpr.op;
    }
    unparse_expression_prec(s, expression, UNPARSE_PREC_ROOT);
  }
  append_char(s, close_char);
}

static const char *comparison_op_name(uint8_t op)
{
  switch (op) {
  case COMPARE_OP_LT:
    return "<";
  case COMPARE_OP_LE:
    return "<=";
  case COMPARE_OP_EQ:
    return "==";
  case COMPARE_OP_NE:
    return "!=";
  case COMPARE_OP_GT:
    return ">";
  case COMPARE_OP_GE:
    return ">=";
  case COMPARE_OP_IN:
    return "in";
  case COMPARE_OP_NOT_IN:
    return "not in";
  case COMPARE_OP_IS:
    return "is";
  case COMPARE_OP_IS_NOT:
    return "is not";
  default:
    break;
  }
  internal_error("invalid comparison operator");
}

static const char *binop_name(enum ast_expression_type type)
{
  switch (type) {
  case AST_BINEXPR_ADD:
    return "+";
  case AST_BINEXPR_AND:
    return "&";
  case AST_BINEXPR_FLOORDIV:
    return "//";
  case AST_BINEXPR_MATMUL:
    return "@";
  case AST_BINEXPR_MOD:
    return "%";
  case AST_BINEXPR_MUL:
    return "*";
  case AST_BINEXPR_OR:
    return "|";
  case AST_BINEXPR_POWER:
    return "**";
  case AST_BINEXPR_SHIFT_LEFT:
    return "<<";
  case AST_BINEXPR_SHIFT_RIGHT:
    return ">>";
  case AST_BINEXPR_SUB:
    return "-";
  case AST_BINEXPR_TRUEDIV:
    return "/";
  case AST_BINEXPR_XOR:
    return "^";
  default:
    break;
  }
  internal_error("invalid binary operator");
}

static void unparse_parameter(struct unparse_state *s,
                              struct parameter     *parameter)
{
  switch (parameter->variant) {
  case PARAMETER_NORMAL:
    append_cstring(s, parameter->name->string);
    break;
  case PARAMETER_STAR:
    append_char(s, '*');
    append_cstring(s, parameter->name->string);
    break;
  case PARAMETER_STAR_STAR:
    append_cstring(s, "**");
    append_cstring(s, parameter->name->string);
    break;
  default:
    internal_error("invalid parameter variant");
  }

  if (parameter->initializer != NULL) {
    append_char(s, '=');
    unparse_expression_prec(s, parameter->initializer, UNPARSE_PREC_ROOT);
  }
}

static void unparse_slice(struct unparse_state *s, struct ast_slice *slice)
{
  if (slice->start != NULL) {
    unparse_expression_prec(s, slice->start, UNPARSE_PREC_ROOT);
  }
  append_char(s, ':');
  if (slice->stop != NULL) {
    unparse_expression_prec(s, slice->stop, UNPARSE_PREC_ROOT);
  }
  if (slice->step != NULL) {
    append_char(s, ':');
    unparse_expression_prec(s, slice->step, UNPARSE_PREC_ROOT);
  }
}

static void unparse_subscript_item(struct unparse_state *s,
                                   union ast_expression *expression)
{
  if (ast_expression_type(expression) == AST_SLICE) {
    unparse_slice(s, &expression->slice);
    return;
  }
  unparse_expression_prec(s, expression, UNPARSE_PREC_ROOT);
}

static void unparse_fstring_format_spec(struct unparse_state *s,
                                        union ast_expression *format_spec)
{
  enum ast_expression_type type = ast_expression_type(format_spec);
  if (type == AST_CONST) {
    union object *object = format_spec->cnst.object;
    if (object->type == OBJECT_STRING) {
      append_escaped_fstring_fragment(s, object->string.chars,
                                      object->string.length);
      return;
    }
  }
  if (type == AST_FSTRING) {
    struct ast_fstring *fstring = &format_spec->fstring;
    for (unsigned i = 0; i < fstring->num_elements; ++i) {
      struct fstring_element *element = &fstring->elements[i];
      if (!element->is_expression) {
        union object *string = element->u.string;
        append_escaped_fstring_fragment(s, string->string.chars,
                                        string->string.length);
        continue;
      }
      append_char(s, '{');
      unparse_expression_prec(s, element->u.expression, UNPARSE_PREC_ROOT);
      switch (element->conversion) {
      case FORMAT_VALUE_NONE:
        break;
      case FORMAT_VALUE_STR:
        append_cstring(s, "!s");
        break;
      case FORMAT_VALUE_REPR:
        append_cstring(s, "!r");
        break;
      case FORMAT_VALUE_ASCII:
        append_cstring(s, "!a");
        break;
      default:
        internal_error("invalid f-string conversion");
      }
      if (element->format_spec != NULL) {
        append_char(s, ':');
        unparse_fstring_format_spec(s, element->format_spec);
      }
      append_char(s, '}');
    }
    return;
  }
  unparse_expression_prec(s, format_spec, UNPARSE_PREC_ROOT);
}

static void unparse_generator_parts(struct unparse_state            *s,
                                    struct ast_generator_expression *generator)
{
  for (unsigned i = 0; i < generator->num_parts; ++i) {
    struct generator_expression_part *part = &generator->parts[i];
    if (part->type == GENERATOR_EXPRESSION_PART_FOR) {
      if (part->async) {
        append_cstring(s, " async for ");
      } else {
        append_cstring(s, " for ");
      }
      union ast_expression *targets = part->targets;
      if (targets != NULL
          && ast_expression_type(targets) == AST_EXPRESSION_LIST) {
        unparse_tuple_items(s, &targets->expression_list,
                            /*wrap_parentheses=*/false);
      } else if (targets != NULL) {
        unparse_expression_prec(s, targets, UNPARSE_PREC_ROOT);
      }
      append_cstring(s, " in ");
      unparse_expression_prec(s, part->expression, UNPARSE_PREC_ROOT);
      continue;
    }
    if (part->type == GENERATOR_EXPRESSION_PART_IF) {
      append_cstring(s, " if ");
      unparse_expression_prec(s, part->expression, UNPARSE_PREC_ROOT);
      continue;
    }
    internal_error("invalid generator part");
  }
}

static void unparse_constant_object(struct unparse_state *s,
                                    union object         *object)
{
  switch ((enum object_type)object->type) {
  case OBJECT_NONE:
    append_cstring(s, "None");
    break;
  case OBJECT_TRUE:
    append_cstring(s, "True");
    break;
  case OBJECT_FALSE:
    append_cstring(s, "False");
    break;
  case OBJECT_ELLIPSIS:
    append_cstring(s, "...");
    break;
  case OBJECT_INT:
    append_int64(s, object->int_obj.value);
    break;
  case OBJECT_BIG_INT:
    append_big_int_pydigits(s, &object->big_int);
    break;
  case OBJECT_FLOAT:
    append_float(s, &object->float_obj);
    break;
  case OBJECT_COMPLEX: {
    double real = object->complex.real;
    double imag = object->complex.imag;
    if (real == 0.0) {
      append_double_for_complex(s, imag);
      append_char(s, 'j');
      break;
    }
    append_char(s, '(');
    append_double_for_complex(s, real);
    if (signbit(imag)) {
      append_char(s, '-');
    } else {
      append_char(s, '+');
    }
    append_double_for_complex(s, fabs(imag));
    append_char(s, 'j');
    append_char(s, ')');
    break;
  }
  case OBJECT_TUPLE: {
    uint32_t length = object_tuple_length(object);
    append_char(s, '(');
    for (uint32_t i = 0; i < length; ++i) {
      if (i > 0) {
        append_cstring(s, ", ");
      }
      unparse_constant_object(s, object_tuple_at(object, i));
    }
    if (length == 1) {
      append_char(s, ',');
    }
    append_char(s, ')');
    break;
  }
  case OBJECT_FROZENSET: {
    uint32_t length = object_frozenset_length(object);
    if (length == 0) {
      append_cstring(s, "frozenset()");
      break;
    }
    append_cstring(s, "frozenset({");
    for (uint32_t i = 0; i < length; ++i) {
      if (i > 0) {
        append_cstring(s, ", ");
      }
      unparse_constant_object(s, object_frozenset_at(object, i));
    }
    append_cstring(s, "})");
    break;
  }
  case OBJECT_STRING:
    append_escaped_string_literal(s, &object->string);
    break;
  case OBJECT_BYTES:
    append_char(s, 'b');
    append_escaped_string_literal(s, &object->string);
    break;
  default:
    internal_error("invalid constant type for unparse");
  }
}

static void unparse_subscript_expression(struct unparse_state *s,
                                         struct ast_binexpr   *subscript)
{
  unparse_expression_prec(s, subscript->left, UNPARSE_PREC_ATOM);
  append_char(s, '[');
  if (ast_expression_type(subscript->right) == AST_EXPRESSION_LIST) {
    struct ast_expression_list *items = &subscript->right->expression_list;
    for (unsigned i = 0; i < items->num_expressions; ++i) {
      if (i > 0) append_cstring(s, ", ");
      unparse_subscript_item(s, items->expressions[i]);
    }
    if (items->num_expressions == 1) {
      append_char(s, ',');
    }
  } else {
    unparse_subscript_item(s, subscript->right);
  }
  append_char(s, ']');
}

static void unparse_call_expression(struct unparse_state *s,
                                    struct ast_call      *call)
{
  assert(call->callee != NULL);
  unparse_expression_prec(s, call->callee, UNPARSE_PREC_ATOM);
  append_char(s, '(');
  for (unsigned i = 0; i < call->num_arguments; ++i) {
    if (i > 0) append_cstring(s, ", ");
    struct argument      *argument = &call->arguments[i];
    union ast_expression *arg_expression = argument->expression;
    if (argument->name != NULL) {
      append_cstring(s, argument->name->string);
      append_char(s, '=');
      unparse_expression_prec(s, arg_expression, UNPARSE_PREC_ROOT);
      continue;
    }
    if (ast_expression_type(arg_expression) == AST_UNEXPR_STAR) {
      append_char(s, '*');
      unparse_expression_prec(s, arg_expression->unexpr.op, UNPARSE_PREC_ROOT);
      continue;
    }
    if (ast_expression_type(arg_expression) == AST_UNEXPR_STAR_STAR) {
      append_cstring(s, "**");
      unparse_expression_prec(s, arg_expression->unexpr.op, UNPARSE_PREC_ROOT);
      continue;
    }
    unparse_expression_prec(s, arg_expression, UNPARSE_PREC_ROOT);
  }
  append_char(s, ')');
}

static void unparse_comparison_expression(struct unparse_state  *s,
                                          struct ast_comparison *comparison)
{
  unparse_expression_prec(s, comparison->left, UNPARSE_PREC_COMPARISON + 1);
  for (unsigned i = 0; i < comparison->num_operands; ++i) {
    struct comparison_op *operand = &comparison->operands[i];
    append_char(s, ' ');
    append_cstring(s, comparison_op_name(operand->op));
    append_char(s, ' ');
    unparse_expression_prec(s, operand->operand, UNPARSE_PREC_COMPARISON + 1);
  }
}

static void unparse_conditional_expression(struct unparse_state   *s,
                                           struct ast_conditional *conditional)
{
  unparse_expression_prec(s, conditional->true_expression,
                          UNPARSE_PREC_CONDITIONAL + 1);
  append_cstring(s, " if ");
  unparse_expression_prec(s, conditional->condition, UNPARSE_PREC_LOGICAL_OR);
  append_cstring(s, " else ");
  unparse_expression_prec(s, conditional->false_expression,
                          UNPARSE_PREC_CONDITIONAL);
}

static void unparse_lambda_expression(struct unparse_state *s,
                                      struct ast_lambda    *lambda)
{
  const struct parameter_shape *shape = &lambda->parameter_shape;

  append_cstring(s, "lambda");
  if (shape->num_parameters > 0) {
    struct parameter *parameters = lambda->parameters;
    append_char(s, ' ');
    for (unsigned i = 0; i < shape->num_parameters; ++i) {
      if (i > 0) append_cstring(s, ", ");
      if (i == shape->keyword_only_begin
          && (i == 0 || parameters[i - 1].variant != PARAMETER_STAR)) {
        append_cstring(s, "*, ");
      }
      unparse_parameter(s, &parameters[i]);
      if (i + 1 == shape->positional_only_argcount) {
        append_cstring(s, ", /");
      }
    }
  }
  append_cstring(s, ": ");
  unparse_expression_prec(s, lambda->expression, UNPARSE_PREC_ROOT);
}

static void unparse_dict_display_expression(struct unparse_state      *s,
                                            struct ast_dict_item_list *dict)
{
  append_char(s, '{');
  for (unsigned i = 0; i < dict->num_items; ++i) {
    if (i > 0) append_cstring(s, ", ");
    struct dict_item *item = &dict->items[i];
    if (item->key == NULL) {
      append_cstring(s, "**");
      union ast_expression *value = item->value;
      if (ast_expression_type(value) == AST_UNEXPR_STAR_STAR) {
        value = value->unexpr.op;
      }
      unparse_expression_prec(s, value, UNPARSE_PREC_ROOT);
    } else {
      unparse_expression_prec(s, item->key, UNPARSE_PREC_ROOT);
      append_cstring(s, ": ");
      unparse_expression_prec(s, item->value, UNPARSE_PREC_ROOT);
    }
  }
  append_char(s, '}');
}

static void unparse_fstring_expression(struct unparse_state *s,
                                       struct ast_fstring   *fstring)
{
  append_cstring(s, "f'");
  for (unsigned i = 0; i < fstring->num_elements; ++i) {
    struct fstring_element *element = &fstring->elements[i];
    if (!element->is_expression) {
      union object *string = element->u.string;
      append_escaped_fstring_fragment(s, string->string.chars,
                                      string->string.length);
      continue;
    }
    append_char(s, '{');
    unparse_expression_prec(s, element->u.expression, UNPARSE_PREC_ROOT);
    switch (element->conversion) {
    case FORMAT_VALUE_NONE:
      break;
    case FORMAT_VALUE_STR:
      append_cstring(s, "!s");
      break;
    case FORMAT_VALUE_REPR:
      append_cstring(s, "!r");
      break;
    case FORMAT_VALUE_ASCII:
      append_cstring(s, "!a");
      break;
    default:
      internal_error("invalid f-string conversion");
    }
    if (element->format_spec != NULL) {
      append_char(s, ':');
      unparse_fstring_format_spec(s, element->format_spec);
    }
    append_char(s, '}');
  }
  append_char(s, '\'');
}

static void unparse_expression_prec(struct unparse_state   *s,
                                    union ast_expression   *expression,
                                    enum unparse_precedence parent_precedence)
{
  enum ast_expression_type type = ast_expression_type(expression);
  enum unparse_precedence  precedence = precedence_of_expression(expression);
  bool                     wrap = (type == AST_BINEXPR_ASSIGN)
              || needs_parens(precedence, parent_precedence);
  if (wrap) append_char(s, '(');

  switch (type) {
  case AST_IDENTIFIER:
    append_cstring(s, expression->identifier.symbol->string);
    break;
  case AST_CONST:
    unparse_constant_object(s, expression->cnst.object);
    break;
  case AST_ATTR:
    unparse_expression_prec(s, expression->attr.expression, UNPARSE_PREC_ATOM);
    append_char(s, '.');
    append_cstring(s, expression->attr.symbol->string);
    break;
  case AST_BINEXPR_SUBSCRIPT:
    unparse_subscript_expression(s, &expression->binexpr);
    break;
  case AST_CALL:
    unparse_call_expression(s, &expression->call);
    break;
  case AST_COMPARISON:
    unparse_comparison_expression(s, &expression->comparison);
    break;
  case AST_CONDITIONAL:
    unparse_conditional_expression(s, &expression->conditional);
    break;
  case AST_LAMBDA:
    unparse_lambda_expression(s, &expression->lambda);
    break;
  case AST_EXPRESSION_LIST:
    unparse_tuple_items(s, &expression->expression_list,
                        /*wrap_parentheses=*/true);
    break;
  case AST_LIST_DISPLAY:
    unparse_expression_list(s, &expression->expression_list, '[', ']');
    break;
  case AST_SET_DISPLAY:
    unparse_expression_list(s, &expression->expression_list, '{', '}');
    break;
  case AST_DICT_DISPLAY:
    unparse_dict_display_expression(s, &expression->dict_item_list);
    break;
  case AST_GENERATOR_EXPRESSION:
    append_char(s, '(');
    unparse_expression_prec(s, expression->generator_expression.expression,
                            UNPARSE_PREC_ROOT);
    unparse_generator_parts(s, &expression->generator_expression);
    append_char(s, ')');
    break;
  case AST_LIST_COMPREHENSION:
    append_char(s, '[');
    unparse_expression_prec(s, expression->generator_expression.expression,
                            UNPARSE_PREC_ROOT);
    unparse_generator_parts(s, &expression->generator_expression);
    append_char(s, ']');
    break;
  case AST_SET_COMPREHENSION:
    append_char(s, '{');
    unparse_expression_prec(s, expression->generator_expression.expression,
                            UNPARSE_PREC_ROOT);
    unparse_generator_parts(s, &expression->generator_expression);
    append_char(s, '}');
    break;
  case AST_DICT_COMPREHENSION:
    append_char(s, '{');
    unparse_expression_prec(s, expression->generator_expression.expression,
                            UNPARSE_PREC_ROOT);
    append_cstring(s, ": ");
    unparse_expression_prec(s, expression->generator_expression.item_value,
                            UNPARSE_PREC_ROOT);
    unparse_generator_parts(s, &expression->generator_expression);
    append_char(s, '}');
    break;
  case AST_FSTRING:
    unparse_fstring_expression(s, &expression->fstring);
    break;
  case AST_UNEXPR_AWAIT:
    append_cstring(s, "await ");
    unparse_expression_prec(s, expression->unexpr.op, UNPARSE_PREC_AWAIT);
    break;
  case AST_UNEXPR_NOT:
    append_cstring(s, "not ");
    unparse_expression_prec(s, expression->unexpr.op,
                            UNPARSE_PREC_LOGICAL_NOT);
    break;
  case AST_UNEXPR_INVERT:
    append_char(s, '~');
    unparse_expression_prec(s, expression->unexpr.op, UNPARSE_PREC_UNARY);
    break;
  case AST_UNEXPR_NEGATIVE:
    append_char(s, '-');
    unparse_expression_prec(s, expression->unexpr.op, UNPARSE_PREC_UNARY);
    break;
  case AST_UNEXPR_PLUS:
    append_char(s, '+');
    unparse_expression_prec(s, expression->unexpr.op, UNPARSE_PREC_UNARY);
    break;
  case AST_UNEXPR_STAR:
    append_char(s, '*');
    unparse_expression_prec(s, expression->unexpr.op, UNPARSE_PREC_UNARY);
    break;
  case AST_UNEXPR_STAR_STAR:
    append_cstring(s, "**");
    unparse_expression_prec(s, expression->unexpr.op, UNPARSE_PREC_UNARY);
    break;
  case AST_BINEXPR_ASSIGN:
    unparse_expression_prec(s, expression->binexpr.left,
                            UNPARSE_PREC_NAMED + 1);
    append_cstring(s, ":=");
    unparse_expression_prec(s, expression->binexpr.right, UNPARSE_PREC_NAMED);
    break;
  case AST_BINEXPR_LOGICAL_AND:
    unparse_expression_prec(s, expression->binexpr.left,
                            UNPARSE_PREC_LOGICAL_AND);
    append_cstring(s, " and ");
    unparse_expression_prec(s, expression->binexpr.right,
                            UNPARSE_PREC_LOGICAL_AND + 1);
    break;
  case AST_BINEXPR_LOGICAL_OR:
    unparse_expression_prec(s, expression->binexpr.left,
                            UNPARSE_PREC_LOGICAL_OR);
    append_cstring(s, " or ");
    unparse_expression_prec(s, expression->binexpr.right,
                            UNPARSE_PREC_LOGICAL_OR + 1);
    break;
  case AST_BINEXPR_ADD:
  case AST_BINEXPR_AND:
  case AST_BINEXPR_FLOORDIV:
  case AST_BINEXPR_MATMUL:
  case AST_BINEXPR_MOD:
  case AST_BINEXPR_MUL:
  case AST_BINEXPR_OR:
  case AST_BINEXPR_SHIFT_LEFT:
  case AST_BINEXPR_SHIFT_RIGHT:
  case AST_BINEXPR_SUB:
  case AST_BINEXPR_TRUEDIV:
  case AST_BINEXPR_XOR:
    unparse_expression_prec(s, expression->binexpr.left, precedence);
    append_char(s, ' ');
    append_cstring(s, binop_name(type));
    append_char(s, ' ');
    unparse_expression_prec(s, expression->binexpr.right, precedence + 1);
    break;
  case AST_BINEXPR_POWER:
    unparse_expression_prec(s, expression->binexpr.left,
                            UNPARSE_PREC_POWER + 1);
    append_cstring(s, " ** ");
    unparse_expression_prec(s, expression->binexpr.right, UNPARSE_PREC_POWER);
    break;
  case AST_SLICE:
    unparse_slice(s, &expression->slice);
    break;
  case AST_YIELD:
    append_cstring(s, "yield");
    if (expression->yield.value != NULL) {
      append_char(s, ' ');
      unparse_expression_prec(s, expression->yield.value, UNPARSE_PREC_ROOT);
    }
    break;
  case AST_YIELD_FROM:
    append_cstring(s, "yield from ");
    unparse_expression_prec(s, expression->yield.value, UNPARSE_PREC_ROOT);
    break;
  case AST_INVALID:
  case AST_BINEXPR_ADD_ASSIGN:
  case AST_BINEXPR_AND_ASSIGN:
  case AST_BINEXPR_FLOORDIV_ASSIGN:
  case AST_BINEXPR_MATMUL_ASSIGN:
  case AST_BINEXPR_MOD_ASSIGN:
  case AST_BINEXPR_MUL_ASSIGN:
  case AST_BINEXPR_OR_ASSIGN:
  case AST_BINEXPR_POWER_ASSIGN:
  case AST_BINEXPR_SHIFT_LEFT_ASSIGN:
  case AST_BINEXPR_SHIFT_RIGHT_ASSIGN:
  case AST_BINEXPR_SUB_ASSIGN:
  case AST_BINEXPR_TRUEDIV_ASSIGN:
  case AST_BINEXPR_XOR_ASSIGN:
    internal_error("invalid expression for unparse");
  }

  if (wrap) append_char(s, ')');
}

union object *ast_unparse_expression(struct object_intern *intern,
                                     union ast_expression *expression)
{
  struct unparse_state state = {
    .intern = intern,
    .arena = object_intern_arena(intern),
  };

  arena_grow_begin(state.arena, /*alignment=*/1);
  unparse_expression_prec(&state, expression, UNPARSE_PREC_ROOT);
  unsigned length = arena_grow_current_size(state.arena);
  char    *chars = arena_grow_finish(state.arena);
  return object_intern_string(intern, OBJECT_STRING, length, chars);
}

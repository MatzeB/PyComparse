#ifndef OBJECTS_H
#define OBJECTS_H

struct arena;

enum object_type {
  TYPE_NULL    = '0',
  TYPE_NONE    = 'N',
  TYPE_TRUE    = 'T',
  TYPE_FALSE   = 'F',
  TYPE_STRING  = 's',
  TYPE_UNICODE = 'u',
  TYPE_CODE    = 'c',
  TYPE_TUPLE   = '(',
  TYPE_LIST    = '[',

  TYPE_ASCII   = 'a',
  TYPE_INT     = 'i',

  TYPE_SHORT_ASCII = 'z',
  TYPE_SMALL_TUPLE = ')',

  TYPE_AST_CALL = -100,
  TYPE_AST_BINEXPR_ADD,
  TYPE_AST_BINEXPR_FLOORDIV,
  TYPE_AST_BINEXPR_MATMUL,
  TYPE_AST_BINEXPR_MUL,
  TYPE_AST_BINEXPR_SUB,
  TYPE_AST_BINEXPR_TRUEDIV,
  TYPE_AST_CONST,
  TYPE_AST_NAME,
  TYPE_AST_UNEXPR_INVERT,
  TYPE_AST_UNEXPR_NEGATIVE,
  TYPE_AST_UNEXPR_NOT,
  TYPE_AST_UNEXPR_PLUS,
};

union object;

struct object_base {
  char type;
};

struct object_string {
  struct object_base base;
  uint32_t length;
  const char *chars;
};

struct object_list {
  struct object_base base;
  uint32_t length;
  uint32_t capacity;
  union object **items;
};

struct object_tuple {
  struct object_base base;
  uint32_t length;
  union object *items[];
};

struct object_int {
  struct object_base base;
  int32_t value;
};

struct object_code {
  struct object_base base;
  uint32_t argcount;
  uint32_t kwonlyargcount;
  uint32_t nlocals;
  uint32_t stacksize;
  uint32_t flags;
  uint32_t firstlineno;
  union object *code;
  union object *consts;
  union object *names;
  union object *varnames;
  union object *freevars;
  union object *cellvars;
  union object *filename;
  union object *name;
  union object *lnotab;
};

struct object_ast_name {
  struct object_base base;
  uint16_t index;
};

struct object_ast_const {
  struct object_base base;
  uint16_t index;
};

struct object_ast_call {
  struct object_base base;
  union object       *callee;
  struct object_list *arguments;
};

struct object_ast_binexpr {
  struct object_base base;
  union object       *left;
  union object       *right;
};

struct object_ast_unexpr {
  struct object_base base;
  union object       *op;
};

union object {
  char               type;
  struct object_base base;

  struct object_ast_binexpr ast_binexpr;
  struct object_ast_call    ast_call;
  struct object_ast_const   ast_const;
  struct object_ast_name    ast_name;
  struct object_ast_unexpr  ast_unexpr;
  struct object_code        code;
  struct object_int         int_obj;
  struct object_list        list;
  struct object_string      string;
  struct object_tuple       tuple;
};

bool objects_equal(const union object *object0, const union object *object1);

void object_list_append(struct object_list *list, union object *object);

struct object_list *object_new_list(struct arena *arena);

#endif

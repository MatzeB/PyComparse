cmake_minimum_required(VERSION 3.10)
project(pyparse)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGUREATION_TYPES)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
endif()
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wcomma -Wall -Wextra -Wmissing-prototypes -pedantic -Wno-gnu-folding-constant")
# glibc 2.40 keeps warning about _FORTIFY_SOURCE in -O0 but I somehow cannot
# remove the define from clang either...
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-#warnings")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror")

add_executable(arena_test
  arena_test.c
  adt/arena.h
)
add_executable(scanner_test
  scanner.c
  scanner.h
  scanner_test.c
  scanner_types.h
  symbol_table.c
  symbol_table.h
  symbol_table_types.h
  symbol_types.h
)
target_include_directories(scanner_test PRIVATE "${CMAKE_SOURCE_DIR}")
add_executable(parser_test
  adt/arena.h
  adt/dynmemory.h
  adt/stack.h
  ast.c
  ast.h
  ast_types.h
  codegen.c
  codegen.h
  codegen_expression.c
  codegen_expression.h
  codegen_statement.c
  codegen_statement.h
  codegen_types.h
  object_intern.c
  object_intern.h
  object_intern_types.h
  objects.c
  objects.h
  objects_types.h
  parser.c
  parser.h
  parser_test.c
  parser_types.h
  scanner.c
  scanner.h
  scanner_types.h
  symbol_info_types.h
  symbol_table.c
  symbol_table.h
  symbol_table_types.h
  symbol_types.h
  util.c
  util.h
  writer.c
  writer.h
)
target_include_directories(parser_test PRIVATE "${CMAKE_SOURCE_DIR}")

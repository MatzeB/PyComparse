cmake_minimum_required(VERSION 3.10)
project(pyparse)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGUREATION_TYPES)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
endif()
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wcomma -Wall -Wextra -Wmissing-prototypes -pedantic -Wno-gnu-folding-constant")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Weverything -Wno-c++98-compat -Wno-shorten-64-to-32 -Wno-padded")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-missing-noreturn")

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
  codegen.c
  codegen.h
  codegen_ast.c
  codegen_ast.h
  codegen_types.h
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

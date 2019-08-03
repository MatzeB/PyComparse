cmake_minimum_required(VERSION 3.10)
project(pyparse)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGUREATION_TYPES)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
endif()
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wmissing-prototypes -pedantic -Wno-gnu-folding-constant")

add_executable(scanner_test
  scanner.c
  scanner.h
  scanner_test.c
  symbol_table.c
  symbol_table.h
)
target_include_directories(scanner_test PRIVATE "${CMAKE_SOURCE_DIR}")
add_executable(parser_test
  codegen.c
  codegen.h
  objects.c
  objects.h
  parser.c
  parser.h
  parser_test.c
  scanner.c
  scanner.h
  symbol_table.c
  symbol_table.h
  writer.c
  writer.h
)
target_include_directories(parser_test PRIVATE "${CMAKE_SOURCE_DIR}")

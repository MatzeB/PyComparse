cmake_minimum_required(VERSION 3.10)
project(pyparse)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGUREATION_TYPES)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
endif()

option(PYPARSE_ENABLE_LTO "Enable link-time optimization (IPO/LTO)" OFF)
set(PYPARSE_PGO_MODE "off" CACHE STRING "PGO mode: off, generate, use")
set_property(CACHE PYPARSE_PGO_MODE PROPERTY STRINGS off generate use)
set(PYPARSE_PGO_DATA "" CACHE FILEPATH "Path to PGO profile data for use-mode")

string(TOLOWER "${PYPARSE_PGO_MODE}" PYPARSE_PGO_MODE_LOWER)
if (NOT PYPARSE_PGO_MODE_LOWER MATCHES "^(off|generate|use)$")
  message(FATAL_ERROR "Invalid PYPARSE_PGO_MODE='${PYPARSE_PGO_MODE}'. "
                      "Expected one of: off, generate, use.")
endif()

if (PYPARSE_ENABLE_LTO)
  include(CheckIPOSupported)
  check_ipo_supported(RESULT PYPARSE_IPO_SUPPORTED
                      OUTPUT PYPARSE_IPO_ERROR)
  if (NOT PYPARSE_IPO_SUPPORTED)
    message(FATAL_ERROR "LTO requested but not supported: ${PYPARSE_IPO_ERROR}")
  endif()
endif()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wcomma -Wall -Wextra -Wmissing-prototypes -pedantic -Wno-gnu-folding-constant")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-nullability-extension -Wnullability-completeness")
# nullable-to-nonnull-conversion warning is annoying as it is not
# flow-sensitive. Rather use clang-static-analyzer to check...
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-nullable-to-nonnull-conversion")
# glibc 2.40 keeps warning about _FORTIFY_SOURCE in -O0 but I somehow cannot
# remove the define from clang either...
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-#warnings")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror")

set(PYPARSE_PGO_COMPILE_FLAGS "")
set(PYPARSE_PGO_LINK_FLAGS "")
if (PYPARSE_PGO_MODE_LOWER STREQUAL "generate")
  if (CMAKE_C_COMPILER_ID MATCHES "Clang")
    set(PYPARSE_PGO_COMPILE_FLAGS "-fprofile-instr-generate")
    set(PYPARSE_PGO_LINK_FLAGS "-fprofile-instr-generate")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(PYPARSE_PGO_COMPILE_FLAGS "-fprofile-generate")
    set(PYPARSE_PGO_LINK_FLAGS "-fprofile-generate")
  else()
    message(FATAL_ERROR "PGO generate mode is unsupported for compiler "
                        "${CMAKE_C_COMPILER_ID}")
  endif()
elseif (PYPARSE_PGO_MODE_LOWER STREQUAL "use")
  if (PYPARSE_PGO_DATA STREQUAL "")
    message(FATAL_ERROR "PYPARSE_PGO_MODE=use requires PYPARSE_PGO_DATA")
  endif()
  if (CMAKE_C_COMPILER_ID MATCHES "Clang")
    set(PYPARSE_PGO_COMPILE_FLAGS
        "-fprofile-instr-use=${PYPARSE_PGO_DATA} -Wno-profile-instr-unprofiled -Wno-profile-instr-out-of-date")
    set(PYPARSE_PGO_LINK_FLAGS "-fprofile-instr-use=${PYPARSE_PGO_DATA}")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(PYPARSE_PGO_COMPILE_FLAGS
        "-fprofile-use=${PYPARSE_PGO_DATA} -fprofile-correction -Wno-missing-profile")
    set(PYPARSE_PGO_LINK_FLAGS "-fprofile-use=${PYPARSE_PGO_DATA}")
  else()
    message(FATAL_ERROR "PGO use mode is unsupported for compiler "
                        "${CMAKE_C_COMPILER_ID}")
  endif()
endif()

if (NOT PYPARSE_PGO_COMPILE_FLAGS STREQUAL "")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${PYPARSE_PGO_COMPILE_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${PYPARSE_PGO_LINK_FLAGS}")
endif()

add_executable(arena_test
  arena_test.c
  adt/arena.h
)
add_executable(scanner_test
  ast.c
  ast.h
  ast_types.h
  diagnostics.c
  diagnostics.h
  diagnostics_types.h
  object.c
  object.h
  object_intern.c
  object_intern.h
  scanner.c
  scanner.h
  scanner_test.c
  scanner_types.h
  symbol_table.c
  symbol_table.h
  symbol_table_types.h
  symbol_types.h
  util.c
  util.h
)
target_include_directories(scanner_test PRIVATE "${CMAKE_SOURCE_DIR}")
if (PYPARSE_ENABLE_LTO)
  set_property(TARGET scanner_test PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
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
  diagnostics.h
  diagnostics.c
  diagnostics_types.h
  object.c
  object.h
  object_intern.c
  object_intern.h
  object_intern_types.h
  object_types.h
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
if (PYPARSE_ENABLE_LTO)
  set_property(TARGET parser_test PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
if (PYPARSE_ENABLE_LTO)
  set_property(TARGET arena_test PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

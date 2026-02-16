cmake_minimum_required(VERSION 3.11)
project(pycomparse)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "" FORCE)
endif()

option(PYCOMPARSE_ENABLE_LTO "Enable link-time optimization (IPO/LTO)" OFF)
set(PYCOMPARSE_PGO_MODE "off" CACHE STRING "PGO mode: off, generate, use")
set_property(CACHE PYCOMPARSE_PGO_MODE PROPERTY STRINGS off generate use)
set(PYCOMPARSE_PGO_DATA "" CACHE FILEPATH "Path to PGO profile data for use-mode")

string(TOLOWER "${PYCOMPARSE_PGO_MODE}" PYCOMPARSE_PGO_MODE_LOWER)
if (NOT PYCOMPARSE_PGO_MODE_LOWER MATCHES "^(off|generate|use)$")
  message(FATAL_ERROR "Invalid PYCOMPARSE_PGO_MODE='${PYCOMPARSE_PGO_MODE}'. "
                      "Expected one of: off, generate, use.")
endif()

if (PYCOMPARSE_ENABLE_LTO)
  include(CheckIPOSupported)
  check_ipo_supported(RESULT PYCOMPARSE_IPO_SUPPORTED
                      OUTPUT PYCOMPARSE_IPO_ERROR)
  if (NOT PYCOMPARSE_IPO_SUPPORTED)
    message(FATAL_ERROR "LTO requested but not supported: ${PYCOMPARSE_IPO_ERROR}")
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

set(PYCOMPARSE_PGO_COMPILE_FLAGS "")
set(PYCOMPARSE_PGO_LINK_FLAGS "")
if (PYCOMPARSE_PGO_MODE_LOWER STREQUAL "generate")
  if (CMAKE_C_COMPILER_ID MATCHES "Clang")
    set(PYCOMPARSE_PGO_COMPILE_FLAGS "-fprofile-instr-generate")
    set(PYCOMPARSE_PGO_LINK_FLAGS "-fprofile-instr-generate")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(PYCOMPARSE_PGO_COMPILE_FLAGS "-fprofile-generate")
    set(PYCOMPARSE_PGO_LINK_FLAGS "-fprofile-generate")
  else()
    message(FATAL_ERROR "PGO generate mode is unsupported for compiler "
                        "${CMAKE_C_COMPILER_ID}")
  endif()
elseif (PYCOMPARSE_PGO_MODE_LOWER STREQUAL "use")
  if (PYCOMPARSE_PGO_DATA STREQUAL "")
    message(FATAL_ERROR "PYCOMPARSE_PGO_MODE=use requires PYCOMPARSE_PGO_DATA")
  endif()
  if (CMAKE_C_COMPILER_ID MATCHES "Clang")
    set(PYCOMPARSE_PGO_COMPILE_FLAGS
        "-fprofile-instr-use=${PYCOMPARSE_PGO_DATA} -Wno-profile-instr-unprofiled -Wno-profile-instr-out-of-date")
    set(PYCOMPARSE_PGO_LINK_FLAGS "-fprofile-instr-use=${PYCOMPARSE_PGO_DATA}")
  elseif (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    set(PYCOMPARSE_PGO_COMPILE_FLAGS
        "-fprofile-use=${PYCOMPARSE_PGO_DATA} -fprofile-correction -Wno-missing-profile")
    set(PYCOMPARSE_PGO_LINK_FLAGS "-fprofile-use=${PYCOMPARSE_PGO_DATA}")
  else()
    message(FATAL_ERROR "PGO use mode is unsupported for compiler "
                        "${CMAKE_C_COMPILER_ID}")
  endif()
endif()

if (NOT PYCOMPARSE_PGO_COMPILE_FLAGS STREQUAL "")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${PYCOMPARSE_PGO_COMPILE_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${PYCOMPARSE_PGO_LINK_FLAGS}")
endif()

set(PYCOMPARSE_ICONV_FOUND OFF)
set(PYCOMPARSE_ICONV_LIBRARIES "")
set(PYCOMPARSE_ICONV_TARGET "")

find_package(Iconv QUIET)
if (Iconv_FOUND)
  set(PYCOMPARSE_ICONV_FOUND ON)
  if (TARGET Iconv::Iconv)
    set(PYCOMPARSE_ICONV_TARGET Iconv::Iconv)
  elseif (NOT Iconv_LIBRARIES STREQUAL "")
    set(PYCOMPARSE_ICONV_LIBRARIES "${Iconv_LIBRARIES}")
  endif()
endif()

if (NOT PYCOMPARSE_ICONV_FOUND)
  message(WARNING "iconv not found; building with PYCOMPARSE_NO_ICONV")
  add_compile_definitions(PYCOMPARSE_NO_ICONV)
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
  encoding.c
  encoding.h
  object.c
  object.h
  object_intern.c
  object_intern.h
  scanner.c
  scanner.h
  scanner_test.c
  scanner_types.h
  unicode_name_lookup.c
  unicode_name_lookup.h
  unicode_name_table.c
  unicode_name_table.h
  symbol_table.c
  symbol_table.h
  symbol_table_types.h
  symbol_types.h
  unicode_nfkc_table.c
  unicode_xid_table.c
  util.c
  util.h
)
target_include_directories(scanner_test PRIVATE "${CMAKE_SOURCE_DIR}")
target_link_libraries(scanner_test PRIVATE ${PYCOMPARSE_ICONV_TARGET}
                                           ${PYCOMPARSE_ICONV_LIBRARIES})
if (PYCOMPARSE_ENABLE_LTO)
  set_property(TARGET scanner_test PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
add_executable(pycomparse
  adt/arena.h
  adt/dynmemory.h
  adt/stack.h
  ast.c
  ast.h
  ast_fold_constants.c
  ast_fold_constants.h
  ast_unparse.c
  ast_unparse.h
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
  encoding.c
  encoding.h
  object.c
  object.h
  object_intern.c
  object_intern.h
  object_intern_types.h
  object_types.h
  parser.c
  parser.h
  pycomparse.c
  parser_types.h
  scanner.c
  scanner.h
  scanner_types.h
  unicode_name_lookup.c
  unicode_name_lookup.h
  unicode_name_table.c
  unicode_name_table.h
  symbol_info_types.h
  symbol_table.c
  symbol_table.h
  symbol_table_types.h
  symbol_types.h
  unicode_nfkc_table.c
  unicode_xid_table.c
  util.c
  util.h
  writer.c
  writer.h
)
target_include_directories(pycomparse PRIVATE "${CMAKE_SOURCE_DIR}")
target_link_libraries(pycomparse PRIVATE ${PYCOMPARSE_ICONV_TARGET}
                                          ${PYCOMPARSE_ICONV_LIBRARIES})
if (PYCOMPARSE_ENABLE_LTO)
  set_property(TARGET pycomparse PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()
if (PYCOMPARSE_ENABLE_LTO)
  set_property(TARGET arena_test PROPERTY INTERPROCEDURAL_OPTIMIZATION TRUE)
endif()

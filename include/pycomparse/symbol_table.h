#pragma once

#include "pycomparse/nullable.h"

#ifdef __cplusplus
extern "C" {
#endif

ASSUME_NONNULL_BEGIN

struct symbol;
struct symbol_table;

struct symbol *symbol_table_get_or_insert(struct symbol_table *symbol_table,
                                          const char          *string);

void symbol_table_init(struct symbol_table *symbol_table);
void symbol_table_free(struct symbol_table *symbol_table);

ASSUME_NONNULL_END

#ifdef __cplusplus
}
#endif

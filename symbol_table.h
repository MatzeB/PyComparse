#pragma once

struct symbol;
struct symbol_table;

struct symbol *symbol_table_get_or_insert(struct symbol_table *symbol_table,
                                          const char          *string);

void init_symbol_table(struct symbol_table *symbol_table);

void exit_symbol_table(struct symbol_table *symbol_table);

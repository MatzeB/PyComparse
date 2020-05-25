#pragma once

enum symbol_info_type {
  SYMBOL_GLOBAL,
  SYMBOL_LOCAL,
  SYMBOL_NAME,
};

struct symbol_info {
  char type;
  uint16_t scope_id;
  unsigned index;
};

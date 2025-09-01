#pragma once

enum token_kind {
#define TCHAR(val, name, desc)    name = val,
#define TDES_VAL(name, desc, val) name = val,
#define TDES(name, desc)          name,
#define TID(id, name)             name,
#include "tokens.h"
#undef TID
#undef TDES
#undef TDES_VAL
#undef TCHAR
  NUM_TOKENS,
};

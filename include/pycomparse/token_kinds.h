#pragma once

#ifdef __cplusplus
extern "C" {
#endif

enum token_kind {
#define TCHAR(val, name, desc)    name = val,
#define TDES_VAL(name, desc, val) name = val,
#define TDES(name, desc)          name,
#define TID(id, name)             name,
#include "pycomparse/tokens.h"
#undef TID
#undef TDES
#undef TDES_VAL
#undef TCHAR
  NUM_TOKENS,
};

#ifdef __cplusplus
}
#endif

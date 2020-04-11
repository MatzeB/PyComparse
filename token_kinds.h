#pragma once

enum token_kind {
#define TCHAR(val, name, desc)  name = val,
#define TDES(name, desc)        name,
#define TID(id, name)           name,
#include "tokens.h"
#undef TID
#undef TDES
#undef TCHAR
};

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

struct location {
  unsigned line;
  /* unsigned column; */
};

#define INVALID_LOCATION ((struct location){ 0 })

#ifdef __cplusplus
}
#endif

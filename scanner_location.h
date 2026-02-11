#pragma once

struct location {
  unsigned line;
  /* unsigned column; */
};

#define INVALID_LOCATION ((struct location){ 0 })

#include "pycomparse/unicode_name_lookup.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "pycomparse/unicode_name_table.h"

static uint32_t unicode_name_codepoint_at(size_t index)
{
  size_t   bit_index = index * UNICODE_NAME_CODEPOINT_BITS;
  size_t   byte_index = bit_index >> 3;
  unsigned shift = (unsigned)(bit_index & 7u);

  uint32_t chunk = 0;
  for (unsigned i = 0; i < 4; ++i) {
    size_t b = byte_index + i;
    if (b < UNICODE_NAME_CODEPOINT_BYTES) {
      chunk |= (uint32_t)unicode_name_codepoints[b] << (8u * i);
    }
  }
  return (chunk >> shift) & UNICODE_NAME_CODEPOINT_MASK;
}

static bool is_unified_ideograph(uint32_t code)
{
  return (0x3400 <= code && code <= 0x4DB5)
         || (0x4E00 <= code && code <= 0x9FEF)
         || (0x20000 <= code && code <= 0x2A6D6)
         || (0x2A700 <= code && code <= 0x2B734)
         || (0x2B740 <= code && code <= 0x2B81D)
         || (0x2B820 <= code && code <= 0x2CEA1)
         || (0x2CEB0 <= code && code <= 0x2EBEF);
}

static bool lookup_cjk_unified_ideograph(const char *name, size_t len,
                                         uint32_t *out)
{
  static const char prefix[] = "CJK UNIFIED IDEOGRAPH-";
  const size_t      prefix_len = sizeof(prefix) - 1;

  if (len <= prefix_len || memcmp(name, prefix, prefix_len) != 0) {
    return false;
  }

  const char *hex = name + prefix_len;
  size_t      hex_len = len - prefix_len;
  if (hex_len != 4 && hex_len != 5) {
    return false;
  }

  uint32_t value = 0;
  for (size_t i = 0; i < hex_len; ++i) {
    char c = hex[i];
    int  digit = 0;
    if ('0' <= c && c <= '9') {
      digit = c - '0';
    } else if ('A' <= c && c <= 'F') {
      digit = 10 + (c - 'A');
    } else {
      return false;
    }
    value = (value << 4) | (uint32_t)digit;
  }

  if (!is_unified_ideograph(value)) {
    return false;
  }

  *out = value;
  return true;
}

static void hangul_find_syllable(const char *str, size_t len,
                                 const char *const *syllables, int count,
                                 int *index, size_t *match_len)
{
  *index = -1;
  *match_len = 0;
  for (int i = 0; i < count; ++i) {
    size_t cur_len = strlen(syllables[i]);
    if (cur_len <= *match_len || cur_len > len) {
      continue;
    }
    if (memcmp(str, syllables[i], cur_len) == 0) {
      *index = i;
      *match_len = cur_len;
    }
  }
}

static bool lookup_hangul_syllable(const char *name, size_t len, uint32_t *out)
{
  static const char prefix[] = "HANGUL SYLLABLE ";
  const size_t      prefix_len = sizeof(prefix) - 1;
  if (len <= prefix_len || memcmp(name, prefix, prefix_len) != 0) {
    return false;
  }

  static const char *const hangul_l[] = {
    "G",  "GG", "N", "D",  "DD", "R", "M", "B", "BB", "S",
    "SS", "",   "J", "JJ", "C",  "K", "T", "P", "H",
  };
  static const char *const hangul_v[] = {
    "A",  "AE", "YA", "YAE", "EO", "E",  "YEO", "YE", "O",  "WA", "WAE",
    "OE", "YO", "U",  "WEO", "WE", "WI", "YU",  "EU", "YI", "I",
  };
  static const char *const hangul_t[] = {
    "",   "G",  "GG", "GS", "N",  "NJ", "NH", "D", "L",  "LG",
    "LM", "LB", "LS", "LT", "LP", "LH", "M",  "B", "BS", "S",
    "SS", "NG", "J",  "C",  "K",  "T",  "P",  "H",
  };

  static const int l_count = (int)(sizeof(hangul_l) / sizeof(hangul_l[0]));
  static const int v_count = (int)(sizeof(hangul_v) / sizeof(hangul_v[0]));
  static const int t_count = (int)(sizeof(hangul_t) / sizeof(hangul_t[0]));

  static const uint32_t s_base = 0xAC00;

  const char *p = name + prefix_len;
  size_t      rem = len - prefix_len;

  int    l_index = -1;
  size_t l_len = 0;
  hangul_find_syllable(p, rem, hangul_l, l_count, &l_index, &l_len);
  if (l_index < 0 || l_len == 0) {
    return false;
  }
  p += l_len;
  rem -= l_len;

  int    v_index = -1;
  size_t v_len = 0;
  hangul_find_syllable(p, rem, hangul_v, v_count, &v_index, &v_len);
  if (v_index < 0 || v_len == 0) {
    return false;
  }
  p += v_len;
  rem -= v_len;

  int    t_index = 0;
  size_t t_len = 0;
  hangul_find_syllable(p, rem, hangul_t, t_count, &t_index, &t_len);
  if (t_index < 0) {
    t_index = 0;
    t_len = 0;
  }
  p += t_len;
  rem -= t_len;

  if (rem != 0) {
    return false;
  }

  *out
      = s_base + (uint32_t)((l_index * v_count + v_index) * t_count + t_index);
  return true;
}

static bool unicode_name_lookup_table(const char *name, size_t len,
                                      uint32_t *out)
{
  if (len == 0 || len > UNICODE_NAME_MAX_LENGTH) {
    return false;
  }

  char query[UNICODE_NAME_MAX_LENGTH + 1];
  for (size_t i = 0; i < len; ++i) {
    unsigned char c = (unsigned char)name[i];
    if (c >= 0x80) {
      return false;
    }
    if ('a' <= c && c <= 'z') {
      c = (unsigned char)(c - ('a' - 'A'));
    }
    query[i] = (char)c;
  }
  query[len] = '\0';

  size_t lo = 0;
  size_t hi = UNICODE_NAME_BLOCK_COUNT;
  while (lo < hi) {
    size_t      mid = lo + (hi - lo) / 2;
    const char *mid_name = (const char *)unicode_name_block_first_names
                           + unicode_name_block_first_offsets[mid];
    int cmp = strcmp(query, mid_name);
    if (cmp < 0) {
      hi = mid;
    } else {
      lo = mid + 1;
    }
  }

  if (lo == 0) {
    return false;
  }

  size_t block = lo - 1;
  size_t index_base = block * UNICODE_NAME_BLOCK_SIZE;
  size_t count = UNICODE_NAME_ENTRY_COUNT - index_base;
  if (count > UNICODE_NAME_BLOCK_SIZE) {
    count = UNICODE_NAME_BLOCK_SIZE;
  }

  const char *first_name = (const char *)unicode_name_block_first_names
                           + unicode_name_block_first_offsets[block];
  int cmp = strcmp(query, first_name);
  if (cmp < 0) {
    return false;
  }
  if (cmp == 0) {
    *out = unicode_name_codepoint_at(index_base);
    return true;
  }

  char   prev[UNICODE_NAME_MAX_LENGTH + 1];
  size_t prev_len = strlen(first_name);
  memcpy(prev, first_name, prev_len + 1);

  const uint8_t *data
      = unicode_name_block_data + unicode_name_block_data_offsets[block];
  const uint8_t *end
      = unicode_name_block_data + unicode_name_block_data_offsets[block + 1];
  for (size_t i = 1; i < count; ++i) {
    if (data + 2 > end) {
      return false;
    }
    uint8_t prefix = data[0];
    uint8_t suffix_len = data[1];
    data += 2;
    if (prefix > prev_len || data + suffix_len > end) {
      return false;
    }

    size_t cur_len = (size_t)prefix + (size_t)suffix_len;
    if (cur_len > UNICODE_NAME_MAX_LENGTH) {
      return false;
    }
    memcpy(prev + prefix, data, suffix_len);
    prev[cur_len] = '\0';
    prev_len = cur_len;
    data += suffix_len;

    cmp = strcmp(query, prev);
    if (cmp < 0) {
      return false;
    }
    if (cmp == 0) {
      *out = unicode_name_codepoint_at(index_base + i);
      return true;
    }
  }

  return false;
}

bool unicode_name_lookup(const char *name, size_t name_len, uint32_t *out)
{
  if (lookup_hangul_syllable(name, name_len, out)) {
    return true;
  }
  if (lookup_cjk_unified_ideograph(name, name_len, out)) {
    return true;
  }
  return unicode_name_lookup_table(name, name_len, out);
}

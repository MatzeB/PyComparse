#!/usr/bin/env python3
"""Generate nfkc_table.h — NFKC normalization tables for Unicode identifiers.

Run with: uv run scripts/gen_nfkc_table.py > unicode_nfkc_table.h

Generates three data tables using binary search on sorted arrays:
1. NFKD decomposition mapping (for codepoints that change under NFKC)
2. Canonical Combining Class (CCC) table
3. NFC canonical composition pairs

Plus Hangul algorithmic composition constants and a top-level
nfkc_normalize() function implementing the full NFKC algorithm:
  NFKC = NFC(NFKD(input))

Uses Python 3.8's unicodedata module (Unicode 12.1.0) to match CPython 3.8.
"""

import sys
import unicodedata

MAX_CP = 0x110000


def is_xid(cp):
    """Check if a codepoint is XID_Start or XID_Continue."""
    try:
        c = chr(cp)
    except (ValueError, OverflowError):
        return False
    return ("_" + c).isidentifier()


def get_nfkc_mappings():
    """Get per-codepoint NFKC mappings for XID codepoints that change.
    Only includes codepoints whose NFKC form differs from the codepoint itself.
    Returns dict of {codepoint: [nfkc_codepoints]}."""
    mappings = {}
    for cp in range(MAX_CP):
        if not is_xid(cp):
            continue
        c = chr(cp)
        nfkc = unicodedata.normalize("NFKC", c)
        if nfkc != c:
            mappings[cp] = [ord(ch) for ch in nfkc]
    return mappings


def get_ccc_table():
    """Get all codepoints with non-zero Canonical Combining Class.
    Returns sorted list of (codepoint, ccc)."""
    entries = []
    for cp in range(MAX_CP):
        try:
            ccc = unicodedata.combining(chr(cp))
        except (ValueError, OverflowError):
            continue
        if ccc != 0:
            entries.append((cp, ccc))
    return entries


def get_composition_pairs():
    """Get all canonical composition pairs (starter + combining -> composite).
    These are pairs that compose under NFC.

    A pair (A, B) -> C exists when:
    - C has a canonical decomposition to exactly A + B
    - C is not in the composition exclusion table
    - Neither A nor B is a Hangul jamo (handled algorithmically)
    """
    pairs = {}
    for cp in range(MAX_CP):
        try:
            c = chr(cp)
        except (ValueError, OverflowError):
            continue
        # Get canonical decomposition (not compatibility)
        decomp = unicodedata.decomposition(c)
        if not decomp or decomp.startswith("<"):
            continue
        parts = decomp.split()
        if len(parts) != 2:
            continue
        a, b = int(parts[0], 16), int(parts[1], 16)

        # Skip Hangul jamo (handled algorithmically)
        if 0xAC00 <= cp <= 0xD7A3:
            continue

        # Check composition exclusion: try composing and see if we get back cp
        composed = unicodedata.normalize("NFC", chr(a) + chr(b))
        if len(composed) == 1 and ord(composed) == cp:
            pairs[(a, b)] = cp
    return pairs


def encode_utf8(cp):
    """Encode a single codepoint to UTF-8 bytes."""
    return chr(cp).encode("utf-8")


def encode_codepoints_utf8(cps):
    """Encode a list of codepoints to UTF-8 bytes."""
    return "".join(chr(cp) for cp in cps).encode("utf-8")


def generate():
    print("Generating NFKC mappings...", file=sys.stderr)
    nfkc_mappings = get_nfkc_mappings()

    print("Generating CCC table...", file=sys.stderr)
    ccc_table = get_ccc_table()

    print("Generating composition pairs...", file=sys.stderr)
    composition_pairs = get_composition_pairs()

    # Build NFKC per-codepoint table: sorted by codepoint
    # Pack as: keys[] (uint32_t), offsets[] (uint16_t), data[] (length-prefixed UTF-8)
    sorted_nfkc = sorted(nfkc_mappings.items())
    nfkc_keys = []
    nfkc_data = bytearray()
    nfkc_offsets = []
    for cp, nfkc_cps in sorted_nfkc:
        utf8 = encode_codepoints_utf8(nfkc_cps)
        assert len(utf8) <= 255
        nfkc_keys.append(cp)
        offset = len(nfkc_data)
        assert offset <= 0xFFFF, f"offset {offset} too large for uint16_t"
        nfkc_offsets.append(offset)
        nfkc_data.append(len(utf8))
        nfkc_data.extend(utf8)

    # CCC table: sorted by codepoint
    ccc_keys = [cp for cp, _ in ccc_table]
    ccc_values = [ccc for _, ccc in ccc_table]

    # Composition table: sorted by packed (starter << 21 | combining) for binary search
    # We pack starter and combining into a uint64_t as (starter << 32 | combining)
    sorted_comp = sorted(composition_pairs.items())
    comp_packed = [(a << 32) | b for (a, b), _ in sorted_comp]
    comp_results = [c for _, c in sorted_comp]

    # Print stats
    nfkc_size = len(nfkc_keys) * 4 + len(nfkc_offsets) * 2 + len(nfkc_data)
    ccc_size = len(ccc_keys) * 4 + len(ccc_values)
    comp_size = len(comp_packed) * 8 + len(comp_results) * 4
    print(
        (
            f"NFKC mappings: {len(sorted_nfkc)} entries, {len(nfkc_data)} bytes data\n"
            f"CCC entries: {len(ccc_table)}\n"
            f"Composition pairs: {len(composition_pairs)}\n"
            f"NFKC table: {nfkc_size} bytes\n"
            f"CCC table: {ccc_size} bytes\n"
            f"Composition table: {comp_size} bytes\n"
            f"Total: {nfkc_size + ccc_size + comp_size} bytes"
        ),
        file=sys.stderr,
    )

    # Generate .c file (data tables only)
    with open("unicode_nfkc_table.c", "w") as f:

        def p(*a, **kw):
            print(*a, file=f, **kw)

        p(
            """\
/* Generated by scripts/gen_nfkc_table.py — do not edit. */

#include "unicode_nfkc_table.h"
"""
        )

        # --- Per-codepoint NFKC mapping ---
        p(f"const uint32_t nfkc_map_keys[{len(nfkc_keys)}] = {{")
        for i in range(0, len(nfkc_keys), 8):
            chunk = nfkc_keys[i : i + 8]
            p("  " + ", ".join(f"0x{v:04X}" for v in chunk) + ",")
        p("};")
        p()
        p(f"const uint16_t nfkc_map_offsets[{len(nfkc_offsets)}] = {{")
        for i in range(0, len(nfkc_offsets), 8):
            chunk = nfkc_offsets[i : i + 8]
            p("  " + ", ".join(f"{v}" for v in chunk) + ",")
        p("};")
        p()
        p(f"const uint8_t nfkc_map_data[{len(nfkc_data)}] = {{")
        for i in range(0, len(nfkc_data), 16):
            chunk = nfkc_data[i : i + 16]
            p("  " + ", ".join(f"0x{b:02x}" for b in chunk) + ",")
        p("};")
        p()

        # --- CCC table ---
        p(f"const uint32_t ccc_keys[{len(ccc_keys)}] = {{")
        for i in range(0, len(ccc_keys), 8):
            chunk = ccc_keys[i : i + 8]
            p("  " + ", ".join(f"0x{v:04X}" for v in chunk) + ",")
        p("};")
        p()
        p(f"const uint8_t ccc_values[{len(ccc_values)}] = {{")
        for i in range(0, len(ccc_values), 16):
            chunk = ccc_values[i : i + 16]
            p("  " + ", ".join(f"{v:3d}" for v in chunk) + ",")
        p("};")
        p()

        # --- NFC composition ---
        p(f"const uint64_t comp_keys[{len(comp_packed)}] = {{")
        for i in range(0, len(comp_packed), 4):
            chunk = comp_packed[i : i + 4]
            p("  " + ", ".join(f"0x{v:012X}ULL" for v in chunk) + ",")
        p("};")
        p()
        p(f"const uint32_t comp_values[{len(comp_results)}] = {{")
        for i in range(0, len(comp_results), 8):
            chunk = comp_results[i : i + 8]
            p("  " + ", ".join(f"0x{v:04X}" for v in chunk) + ",")
        p("};")

    # Generate .h file (extern declarations, inline functions, nfkc_normalize)
    with open("unicode_nfkc_table.h", "w") as f:

        def p(*a, **kw):
            print(*a, file=f, **kw)

        p(
            """\
/* Generated by scripts/gen_nfkc_table.py — do not edit. */
/* NFKC normalization tables (Unicode 12.1.0 / Python 3.8) */

#pragma once

#include <stdint.h>
#include <string.h>
"""
        )

        p(
            f"""\
#define NFKC_MAP_COUNT {len(nfkc_keys)}
extern const uint32_t nfkc_map_keys[{len(nfkc_keys)}];
extern const uint16_t nfkc_map_offsets[{len(nfkc_offsets)}];
extern const uint8_t nfkc_map_data[{len(nfkc_data)}];

#define CCC_COUNT {len(ccc_keys)}
extern const uint32_t ccc_keys[{len(ccc_keys)}];
extern const uint8_t ccc_values[{len(ccc_values)}];

#define COMP_COUNT {len(comp_packed)}
extern const uint64_t comp_keys[{len(comp_packed)}];
extern const uint32_t comp_values[{len(comp_results)}];

/* Hangul algorithmic composition constants */
#define HANGUL_SBASE  0xAC00
#define HANGUL_LBASE  0x1100
#define HANGUL_VBASE  0x1161
#define HANGUL_TBASE  0x11A7
#define HANGUL_LCOUNT 19
#define HANGUL_VCOUNT 21
#define HANGUL_TCOUNT 28
#define HANGUL_NCOUNT (HANGUL_VCOUNT * HANGUL_TCOUNT)  /* 588 */
#define HANGUL_SCOUNT (HANGUL_LCOUNT * HANGUL_NCOUNT)  /* 11172 */

/* Binary search for uint32_t key in sorted array. */
static inline int32_t nfkc_bsearch32(const uint32_t *keys, int32_t count,
                                      uint32_t key)
{{
  int32_t lo = 0, hi = count;
  while (lo < hi) {{
    int32_t mid = lo + (hi - lo) / 2;
    if (keys[mid] < key) lo = mid + 1;
    else hi = mid;
  }}
  return (lo < count && keys[lo] == key) ? lo : -1;
}}

/* Binary search for uint64_t key in sorted array. */
static inline int32_t nfkc_bsearch64(const uint64_t *keys, int32_t count,
                                      uint64_t key)
{{
  int32_t lo = 0, hi = count;
  while (lo < hi) {{
    int32_t mid = lo + (hi - lo) / 2;
    if (keys[mid] < key) lo = mid + 1;
    else hi = mid;
  }}
  return (lo < count && keys[lo] == key) ? lo : -1;
}}

/* Look up per-codepoint NFKC mapping.
 * Returns pointer to length-prefixed UTF-8 data, or NULL if unchanged. */
static inline const uint8_t *nfkc_map_lookup(uint32_t cp)
{{
  int32_t idx = nfkc_bsearch32(nfkc_map_keys, NFKC_MAP_COUNT, cp);
  if (idx < 0) return NULL;
  return &nfkc_map_data[nfkc_map_offsets[idx]];
}}

/* Look up Canonical Combining Class for a codepoint. */
static inline uint8_t ccc_lookup(uint32_t cp)
{{
  int32_t idx = nfkc_bsearch32(ccc_keys, CCC_COUNT, cp);
  return idx >= 0 ? ccc_values[idx] : 0;
}}

/* Look up NFC canonical composition. Returns 0 if no composition. */
static inline uint32_t nfc_compose(uint32_t starter, uint32_t combining)
{{
  /* Hangul LV composition */
  if (starter >= HANGUL_LBASE && starter < HANGUL_LBASE + HANGUL_LCOUNT) {{
    if (combining >= HANGUL_VBASE && combining < HANGUL_VBASE + HANGUL_VCOUNT) {{
      return HANGUL_SBASE
             + (starter - HANGUL_LBASE) * HANGUL_NCOUNT
             + (combining - HANGUL_VBASE) * HANGUL_TCOUNT;
    }}
    return 0;
  }}
  /* Hangul LVT composition */
  if (starter >= HANGUL_SBASE && starter < HANGUL_SBASE + HANGUL_SCOUNT
      && ((starter - HANGUL_SBASE) % HANGUL_TCOUNT) == 0) {{
    if (combining > HANGUL_TBASE && combining < HANGUL_TBASE + HANGUL_TCOUNT) {{
      return starter + (combining - HANGUL_TBASE);
    }}
    return 0;
  }}
  uint64_t key = ((uint64_t)starter << 32) | combining;
  int32_t idx = nfkc_bsearch64(comp_keys, COMP_COUNT, key);
  return idx >= 0 ? comp_values[idx] : 0;
}}

"""
        )

        p(
            """\
/* Encode a codepoint as UTF-8. Returns number of bytes written. */
static inline int nfkc_encode_utf8(uint32_t cp, uint8_t *out)
{
  if (cp <= 0x7F) {
    out[0] = (uint8_t)cp;
    return 1;
  } else if (cp <= 0x7FF) {
    out[0] = (uint8_t)(0xC0 | (cp >> 6));
    out[1] = (uint8_t)(0x80 | (cp & 0x3F));
    return 2;
  } else if (cp <= 0xFFFF) {
    out[0] = (uint8_t)(0xE0 | (cp >> 12));
    out[1] = (uint8_t)(0x80 | ((cp >> 6) & 0x3F));
    out[2] = (uint8_t)(0x80 | (cp & 0x3F));
    return 3;
  } else {
    out[0] = (uint8_t)(0xF0 | (cp >> 18));
    out[1] = (uint8_t)(0x80 | ((cp >> 12) & 0x3F));
    out[2] = (uint8_t)(0x80 | ((cp >> 6) & 0x3F));
    out[3] = (uint8_t)(0x80 | (cp & 0x3F));
    return 4;
  }
}

/* Decode one UTF-8 codepoint from a byte string.
 * Returns number of bytes consumed, or 0 on error. */
static inline int nfkc_decode_utf8(const uint8_t *s, const uint8_t *end,
                                   uint32_t *out)
{
  if (s >= end) return 0;
  uint8_t lead = s[0];
  if (lead < 0x80) { *out = lead; return 1; }
  if (lead < 0xC2) return 0;
  if (lead < 0xE0) {
    if (s + 2 > end) return 0;
    if ((s[1] & 0xC0) != 0x80) return 0;
    *out = ((uint32_t)(lead & 0x1F) << 6) | (s[1] & 0x3F);
    return 2;
  }
  if (lead < 0xF0) {
    if (s + 3 > end) return 0;
    if ((s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80) return 0;
    *out = ((uint32_t)(lead & 0x0F) << 12)
         | ((uint32_t)(s[1] & 0x3F) << 6) | (s[2] & 0x3F);
    if (*out < 0x800) return 0;
    return 3;
  }
  if (lead <= 0xF4) {
    if (s + 4 > end) return 0;
    if ((s[1] & 0xC0) != 0x80 || (s[2] & 0xC0) != 0x80
        || (s[3] & 0xC0) != 0x80)
      return 0;
    *out = ((uint32_t)(lead & 0x07) << 18)
         | ((uint32_t)(s[1] & 0x3F) << 12)
         | ((uint32_t)(s[2] & 0x3F) << 6) | (s[3] & 0x3F);
    if (*out < 0x10000 || *out > 0x10FFFF) return 0;
    return 4;
  }
  return 0;
}

/* NFKC-normalize a UTF-8 string in-place.
 * `buf` must contain valid UTF-8 of length `len` bytes.
 * `buf_capacity` is the total writable size of `buf`.
 * Returns the new length in bytes, or -1 if the buffer is too small.
 * The result is NOT null-terminated by this function. */
static inline int nfkc_normalize(uint8_t *buf, int len, int buf_capacity)
{
  /* Temporary buffer for expanded codepoints. Identifiers are short;
   * 1024 codepoints handles identifiers up to ~1KB. */
  uint32_t expanded[1024];
  int exp_len = 0;

  /* Step 1: Per-codepoint NFKC expansion. */
  const uint8_t *s = buf;
  const uint8_t *end = buf + len;
  while (s < end) {
    uint32_t cp;
    int n = nfkc_decode_utf8(s, end, &cp);
    if (n == 0) return -1;
    s += n;

    const uint8_t *mapping = nfkc_map_lookup(cp);
    if (mapping) {
      /* Per-codepoint NFKC result (already in NFC form). */
      uint8_t mlen = mapping[0];
      const uint8_t *mp = mapping + 1;
      const uint8_t *mp_end = mp + mlen;
      while (mp < mp_end) {
        uint32_t mcp;
        int mn = nfkc_decode_utf8(mp, mp_end, &mcp);
        if (mn == 0) return -1;
        mp += mn;
        if (exp_len >= 1024) return -1;
        expanded[exp_len++] = mcp;
      }
    } else {
      if (exp_len >= 1024) return -1;
      expanded[exp_len++] = cp;
    }
  }

  /* Step 2: Canonical ordering — sort combining marks by CCC. */
  for (int i = 1; i < exp_len; i++) {
    uint8_t ccc = ccc_lookup(expanded[i]);
    if (ccc == 0) continue;
    int j = i;
    while (j > 0) {
      uint8_t prev_ccc = ccc_lookup(expanded[j - 1]);
      if (prev_ccc == 0 || prev_ccc <= ccc) break;
      uint32_t tmp = expanded[j];
      expanded[j] = expanded[j - 1];
      expanded[j - 1] = tmp;
      j--;
    }
  }

  /* Step 3: NFC compose — in-place, standard algorithm. */
  if (exp_len > 0) {
    int w = 1;  /* write index (first codepoint always kept) */
    int starter_idx = 0;
    uint8_t last_ccc = ccc_lookup(expanded[0]) != 0 ? 255 : 0;
    for (int r = 1; r < exp_len; r++) {
      uint8_t ccc = ccc_lookup(expanded[r]);
      uint32_t composed = 0;
      if ((last_ccc < ccc || last_ccc == 0)
          && (composed = nfc_compose(expanded[starter_idx],
                                     expanded[r])) != 0) {
        expanded[starter_idx] = composed;
        continue;  /* consumed — don't write, don't update last_ccc */
      }
      if (ccc == 0) {
        starter_idx = w;
        last_ccc = 0;
      } else {
        last_ccc = ccc;
      }
      expanded[w++] = expanded[r];
    }
    exp_len = w;
  }

  /* Step 4: Encode back to UTF-8. */
  int out_len = 0;
  for (int i = 0; i < exp_len; i++) {
    uint8_t tmp[4];
    int n = nfkc_encode_utf8(expanded[i], tmp);
    if (out_len + n > buf_capacity) return -1;
    memcpy(buf + out_len, tmp, n);
    out_len += n;
  }
  return out_len;
}
"""
        )


if __name__ == "__main__":
    generate()

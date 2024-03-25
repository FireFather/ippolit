#pragma once

#if defined(__arm__) || defined(__linux__) || defined(__GNUC__)
#define popcnt(x) __builtin_popcountll(x)
#define bsf(v)    __builtin_ctzll(v)
#define bsr(v)    (63 - __builtin_clzll(v))
#endif

#if defined(_WIN64) && !defined(__GNUC__)
__inline int popcnt(const uint64_t b) { return static_cast<int>(__popcnt64(b)); }

__inline unsigned long bsf(const uint64_t x) {
  unsigned long y;
  _BitScanForward64(&y, x);
  return y;
}

__inline unsigned long bsr(const uint64_t x) {
  unsigned long y;
  _BitScanReverse64(&y, x);
  return y;
}
#endif

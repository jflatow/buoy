/* MurmurHash3 */

#ifndef HASH_H
#define HASH_H

#include <stdint.h>

#define BIGNUM(x)   (x##LLU)
#define ROTL64(x,r) (x << r) | (x >> (64 - r))

static inline uint64_t fmix64(uint64_t k) {
  k ^= k >> 33;
  k *= BIGNUM(0xff51afd7ed558ccd);
  k ^= k >> 33;
  k *= BIGNUM(0xc4ceb9fe1a85ec53);
  k ^= k >> 33;
  return k;
}

static inline uint64_t hash(const void *key, const int len) {
  const uint32_t seed = 0xf1a70111;
  const uint8_t *data = (const uint8_t *) key;
  const int nblocks = len / 16;
  int i;

  uint64_t h1 = seed;
  uint64_t h2 = seed;

  uint64_t c1 = BIGNUM(0x87c37b91114253d5);
  uint64_t c2 = BIGNUM(0x4cf5ad432745937f);

  /* body */

  const uint64_t *blocks = (const uint64_t *) (data);

  for (i = 0; i < nblocks; i++) {
    uint64_t k1 = blocks[i*2+0];
    uint64_t k2 = blocks[i*2+1];

    k1 *= c1;
    k1  = ROTL64(k1,31);
    k1 *= c2; h1 ^= k1;
    h1  = ROTL64(h1,27);
    h1 += h2;
    h1  = h1*5+0x52dce729;

    k2 *= c2;
    k2  = ROTL64(k2,33);
    k2 *= c1; h2 ^= k2;
    h2  = ROTL64(h2,31);
    h2 += h1;
    h2  = h2*5+0x38495ab5;
  }

  /* tail */

  const uint8_t *tail = (const uint8_t *) (data + nblocks*16);

  uint64_t k1 = 0;
  uint64_t k2 = 0;

  switch (len & 15) {
    case 15: k2 ^= (uint64_t) (tail[14]) << 48;
    case 14: k2 ^= (uint64_t) (tail[13]) << 40;
    case 13: k2 ^= (uint64_t) (tail[12]) << 32;
    case 12: k2 ^= (uint64_t) (tail[11]) << 24;
    case 11: k2 ^= (uint64_t) (tail[10]) << 16;
    case 10: k2 ^= (uint64_t) (tail[ 9]) << 8;
    case  9: k2 ^= (uint64_t) (tail[ 8]) << 0;
      k2 *= c2;
      k2  = ROTL64(k2,33);
      k2 *= c1;
      h2 ^= k2;
    case  8: k1 ^= (uint64_t) (tail[ 7]) << 56;
    case  7: k1 ^= (uint64_t) (tail[ 6]) << 48;
    case  6: k1 ^= (uint64_t) (tail[ 5]) << 40;
    case  5: k1 ^= (uint64_t) (tail[ 4]) << 32;
    case  4: k1 ^= (uint64_t) (tail[ 3]) << 24;
    case  3: k1 ^= (uint64_t) (tail[ 2]) << 16;
    case  2: k1 ^= (uint64_t) (tail[ 1]) << 8;
    case  1: k1 ^= (uint64_t) (tail[ 0]) << 0;
      k1 *= c1;
      k1  = ROTL64(k1,31);
      k1 *= c2;
      h1 ^= k1;
  }

  /* final */

  h1 ^= len;
  h2 ^= len;

  h1 += h2;
  h2 += h1;

  h1 = fmix64(h1);
  h2 = fmix64(h2);

  h1 += h2;
  h2 += h1;

  return h1; // XXX: just return the low 64 bits
}

#endif /* HASH_H */

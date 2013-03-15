/* A data structure for fast learning
   Copyright (C) Jared Flatow, 2012 */

#ifndef BUOY_H
#define BUOY_H

#include <stdint.h>

typedef uint8_t  byte;
typedef uint64_t Size;
typedef double   Real;

typedef struct {
  byte length;
  byte *bytes;
} Attr;

typedef struct {
  Attr attr;
  Real weight;
} Item;

typedef struct {
  Size size, capacity;
  Item *items;
} Buoy;

Buoy *buoy_new(void);

Buoy *buoy_copy(Buoy *buoy);
Buoy *buoy_free(Buoy *buoy);
Buoy *buoy_grow(Buoy *buoy);

Buoy *buoy_dump(Buoy *buoy, int fd);
Buoy *buoy_load(int fd);

Item *buoy_find(Buoy *buoy, Attr *attr);

Real buoy_fetch(Buoy *buoy, Attr *attr);
Real buoy_store(Buoy *buoy, Attr *attr, Real weight);

Real buoy_learn(Buoy *buoy, Attr **attrs, Size nattrs, Real score);
Real buoy_score(Buoy *buoy, Attr **attrs, Size nattrs);

#endif /* BUOY_H */

/* A buoy is (currently) implemented as a hash table.
   We use simple open-addressing with linear probing.
   We let our sophisticated hash function do the rest. */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "buoy.h"
#include "hash.h"

Buoy *buoy_new() {
  return buoy_grow(calloc(sizeof(Buoy), 1));
}

Buoy *buoy_copy(Buoy *buoy) {
  Buoy *copy = buoy_new();
  for (Size i = 0; i < buoy->capacity; i++) {
    Item *item = buoy->items + i;
    if (item->attr.bytes)
      buoy_store(copy, &item->attr, item->weight);
  }
  return copy;
}

Buoy *buoy_free(Buoy *buoy) {
  for (Size i = 0; i < buoy->capacity; i++)
    free(buoy->items[i].attr.bytes);
  free(buoy->items);
  free(buoy);
  return NULL;
}

Buoy *buoy_grow(Buoy *buoy) {
  Size capacity = buoy->capacity;
  Item *items = buoy->items;

  buoy->capacity = capacity ? capacity * 2 : 128;
  buoy->items = calloc(sizeof(Item), buoy->capacity);

  for (Size i = 0; i < capacity; i++) {
    if (items[i].attr.bytes) {
      Item *item = buoy_find(buoy, &items[i].attr);
      item->weight = items[i].weight;
      item->attr.bytes = items[i].attr.bytes;
      item->attr.length = items[i].attr.length;
    }
  }
  free(items);

  return buoy;
}

Buoy *buoy_dump(Buoy *buoy, int fd) {
  Size magic = BUOY_MAGIC;
  if (write(fd, &magic, sizeof(magic)) < 0)
    return NULL;
  if (write(fd, &buoy->size, sizeof(Size)) < 0)
    return NULL;
  if (write(fd, &buoy->capacity, sizeof(Size)) < 0)
    return NULL;

  for (Size i = 0; i < buoy->capacity; i++) {
    Item item = buoy->items[i];
    if (item.attr.bytes) {
      if (write(fd, &item.weight, sizeof(Real)) < 0)
        return NULL;
      if (write(fd, &item.attr.length, sizeof(byte)) < 0)
        return NULL;
      if (write(fd, item.attr.bytes, item.attr.length) < 0)
        return NULL;
    }
  }

  return buoy;
}

Buoy *buoy_load(int fd) {
  Buoy *buoy = calloc(sizeof(Buoy), 1);
  Size magic = 0;
  if (read(fd, &magic, sizeof(magic)) < 0 || magic != BUOY_MAGIC)
    goto eprem;
  if (read(fd, &buoy->size, sizeof(Size)) < 0)
    goto eprem;
  if (read(fd, &buoy->capacity, sizeof(Size)) < 0)
    goto eprem;
  if (buoy->capacity < 1)
    goto eprem;

  Size size = buoy->size;
  buoy->size = 0;
  buoy->items = calloc(sizeof(Item), buoy->capacity);

  for (Size i = 0; i < size; i++) {
    Real weight;
    Attr attr;
    byte bytes[255];
    if (read(fd, &weight, sizeof(Real)) < 0)
      goto error;
    if (read(fd, &attr.length, sizeof(byte)) < 0)
      goto error;
    if (read(fd, bytes, attr.length) < 0)
      goto error;
    attr.bytes = bytes;
    buoy_store(buoy, &attr, weight);
  }
  return buoy;

 eprem:
  free(buoy);
  return NULL;

 error:
  return buoy_free(buoy);
}

Item *buoy_find(Buoy *buoy, Attr *attr) {
  Size capacity = buoy->capacity,
    index = hash(attr->bytes, attr->length) % capacity;
  Item *items = buoy->items,
    *item = NULL;
  /* search until we find an empty item,
     or the one we are looking for */
  while ((item = items + index)->attr.bytes &&
         (item->attr.length != attr->length || memcmp(item->attr.bytes, attr->bytes, attr->length)))
    if (++index == capacity)
      index = 0;
  return item;
}

Real buoy_fetch(Buoy *buoy, Attr *attr) {
  Item *item = buoy_find(buoy, attr);
  return item->weight;
}

Real buoy_store(Buoy *buoy, Attr *attr, Real weight) {
  Item *item = buoy_find(buoy, attr);
  item->weight = weight;

  if (item->attr.bytes == NULL) {
    item->attr.length = attr->length;
    item->attr.bytes = malloc(sizeof(byte) * attr->length);
    memcpy(item->attr.bytes, attr->bytes, attr->length);
    if (3 * ++buoy->size >= 2 * buoy->capacity)
      buoy_grow(buoy);
  }

  return weight;
}

Real buoy_learn(Buoy *buoy, Attr **attrs, Size nattrs, Real score) {
  Real delta = score - buoy_score(buoy, attrs, nattrs);
  Real alpha = delta / nattrs;
  for (Size i = 0; i < nattrs; i++)
    buoy_store(buoy, attrs[i], buoy_fetch(buoy, attrs[i]) + alpha);
  return delta;
}

Real buoy_score(Buoy *buoy, Attr **attrs, Size nattrs) {
  Real score = 0;
  for (Size i = 0; i < nattrs; i++)
    score += buoy_fetch(buoy, attrs[i]);
  return score;
}

Real buoy_dot(Buoy *a, Buoy *b) {
  Real ab = 0;
  Buoy *s = a->capacity < b->capacity ? a : b, *t = s == a ? b : a;
  for (Size i = 0; i < s->capacity; i++) {
    Item item = s->items[i];
    if (item.attr.bytes)
      ab += buoy_fetch(t, &item.attr) * item.weight;
  }
  return ab;
}
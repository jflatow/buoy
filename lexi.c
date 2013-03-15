/* Lexi provides a low-level bridge from doc => buoy.
   Given a doc, it can turn tokens into attr lists,
   and hide the details for learning and scoring. */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "lexi.h"

Doc *lexi_read(Doc *doc, int fd) {
  const size_t B = 10 << 20;
  int n = 0;
  doc->length = 0;
  doc->bytes = malloc(B);
  for (int i = 0; (n = read(fd, &doc->bytes[i], B)) > 0; doc->length += n)
    doc->bytes = realloc(doc->bytes, ++i * B);
  if (n < 0)
    return NULL;
  return doc;
}

Buoy *lexi_keys(Doc *doc, Tok *tok) {
  const byte K = 6, N = 1 << K;
  const byte *data = doc->bytes;
  Pos k, n,
    start = tok->start,
    end = tok->end,
    len = end - start,
    pre = start > N ? start - N : 0,
    suf = end + N > doc->length ? doc->length : end + N;
  Buoy *keys = buoy_new();
  buoy_store(keys, &(Attr) {.bytes = (byte *) "bias", .length = 4}, 1);

  do {
    byte bytes[4 + sizeof(Pos)] = "len:";
    memcpy(&bytes[4], &len, sizeof(Pos));
    buoy_store(keys, &(Attr) {.bytes = bytes, .length = 2 + sizeof(Pos)}, 1);
  } while (0);

  for (n = pre; n < suf; n++) {
    for (k = 0; k < K; k++) {
      int size = 1 << k;
      if (n < start) {
        if (n + size <= start) {
          byte *bytes = malloc(sizeof(byte) * (2 + size));
          bytes[0] = '-';
          bytes[1] = (start - n) >> k;
          memcpy(&bytes[2], &data[n], size);
          buoy_store(keys, &(Attr) {.bytes = bytes, .length = 2 + size}, 1);
          free(bytes);
        }
      } else if (n < end) {
        if (n + size <= end) {
          byte *bytes = malloc(sizeof(byte) * (2 + size));
          bytes[0] = '=';
          bytes[1] = (n - start) >> k;
          memcpy(&bytes[2], &data[n], size);
          buoy_store(keys, &(Attr) {.bytes = bytes, .length = 2 + size}, 1);
          free(bytes);
        }
      } else {
        if (n + size <= doc->length) {
          byte *bytes = malloc(sizeof(byte) * (2 + size));
          bytes[0] = '+';
          bytes[1] = (n - end) >> k;
          memcpy(&bytes[2], &data[n], size);
          buoy_store(keys, &(Attr) {.bytes = bytes, .length = 2 + size}, 1);
          free(bytes);
        }
      }
    }
  }
  return keys;
}

Real lexi_learn(Buoy *buoy, Doc *doc, Tok *tok, Real score) {
  Buoy *keys = lexi_keys(doc, tok);
  Attr *attrs[keys->size];
  for (Size k = 0, i = 0; i < keys->capacity; i++)
    if (keys->items[i].attr.bytes)
      attrs[k++] = &keys->items[i].attr;
  Real delta = buoy_learn(buoy, attrs, keys->size, score);
  buoy_free(keys);
  return delta;
}

Real lexi_score(Buoy *buoy, Doc *doc, Tok *tok) {
  Buoy *keys = lexi_keys(doc, tok);
  Attr *attrs[keys->size];
  for (Size k = 0, i = 0; i < keys->capacity; i++)
    if (keys->items[i].attr.bytes)
      attrs[k++] = &keys->items[i].attr;
  Real score = buoy_score(buoy, attrs, keys->size);
  buoy_free(keys);
  return score;
}

/* Doxi functions create global features for a document.
   We don't waste resources building contextual clues,
   we instead cover a larger region with lower resolution. */

Buoy *doxi_keys(Doc *doc) {
  const byte *data = doc->bytes;
  Pos n;
  Buoy *keys = buoy_new();
  buoy_store(keys, &(Attr) {.bytes = (byte *) "bias", .length = 4}, 1);

  for (byte i = 1; i <= 16; i++) {
    Pos len = doc->length >> i;
    byte bytes[2 + sizeof(Pos)] = {'~', i};
    memcpy(&bytes[2], &len, sizeof(Pos));
    buoy_store(keys, &(Attr) {.bytes = bytes, .length = 2 + sizeof(Pos)}, 1);
  }

  for (n = 0; n < doc->length; n++) {
    if (n + 8 <= doc->length) {
      Pos pos = n >> 8;
      byte bytes[1 + sizeof(Pos) + 8] = {'@'};
      memcpy(&bytes[1], &pos, sizeof(Pos));
      memcpy(&bytes[1 + sizeof(Pos)], &data[n], 8);
      buoy_store(keys, &(Attr) {.bytes = bytes, .length = 1 + sizeof(Pos) + 8}, 1);
    }
    if (n + 16 <= doc->length) {
      byte bytes[1 + 16] = {'?'};
      memcpy(&bytes[1], &data[n], 16);
      buoy_store(keys, &(Attr) {.bytes = bytes, .length = 1 + 16}, 1);
    }
  }
  return keys;
}

Real doxi_learn(Buoy *buoy, Doc *doc, Real score) {
  Buoy *keys = doxi_keys(doc);
  Attr *attrs[keys->size];
  for (Size k = 0, i = 0; i < keys->capacity; i++)
    if (keys->items[i].attr.bytes)
      attrs[k++] = &keys->items[i].attr;
  Real delta = buoy_learn(buoy, attrs, keys->size, score);
  buoy_free(keys);
  return delta;
}

Real doxi_score(Buoy *buoy, Doc *doc) {
  Buoy *keys = doxi_keys(doc);
  Attr *attrs[keys->size];
  for (Size k = 0, i = 0; i < keys->capacity; i++)
    if (keys->items[i].attr.bytes)
      attrs[k++] = &keys->items[i].attr;
  Real score = buoy_score(buoy, attrs, keys->size);
  buoy_free(keys);
  return score;
}

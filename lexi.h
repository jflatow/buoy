/* A library for tokenizing documents
   Copyright (C) Jared Flatow, 2012 */

#ifndef LEXI_H
#define LEXI_H

#include "buoy.h"

typedef Size Pos;

typedef struct {
  Size length;
  byte *bytes;
} Doc;

typedef struct {
  Pos start;
  Pos end;
} Tok;

Doc *lexi_read(Doc *doc, int fd);

Buoy *lexi_keys(Doc *doc, Tok *tok);
Real lexi_learn(Buoy *buoy, Doc *doc, Tok *tok, Real score);
Real lexi_score(Buoy *buoy, Doc *doc, Tok *tok);

Buoy *doxi_keys(Doc *doc);
Real doxi_learn(Buoy *buoy, Doc *doc, Real score);
Real doxi_score(Buoy *buoy, Doc *doc);

#endif /* LEXI_H */

#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "buoy.h"
#include "lexi.h"

int buoy_info(Buoy *buoy) {
  fprintf(stderr, "size:    \t%8llu\n", buoy->size);
  fprintf(stderr, "capacity:\t%8llu\n", buoy->capacity);
  fprintf(stderr, "\e[?25l");
  for (Size i = 0; i < buoy->capacity; i++)
    fprintf(stderr, "\ritem:    \t%8llu\t%3d\t%.3f", i, buoy->items[i].attr.length, buoy->items[i].weight);
  fprintf(stderr, "\e[?25h\n");
  return 0;
}

int copy(int argc, char **argv) {
    Buoy *buoy = buoy_new();

    for (int i = 0; i < 1e6; i++) {
      Attr *attr[] = {&(Attr) {.bytes = (byte *) &i, .length = 8}};
      double delta = buoy_learn(buoy, attr, 1, i);
    }

    for (int k = 0; k < 1e2; k++) {
      Buoy *copy = buoy_copy(buoy);
      buoy_free(buoy);
      buoy = copy;
    }

    buoy_free(buoy);
    return 0;
}

int load(int argc, char **argv) {
  return buoy_info(buoy_load(0));
}

int dump(int argc, char **argv) {
  for (int k = 0; k < 1; k++) {
    Buoy *buoy = buoy_new();

    for (int i = 0; i < 1e3; i++) {
      Attr *attr[] = {&(Attr) {.bytes = (byte *) &i, .length = 8}};
      double delta = buoy_learn(buoy, attr, 1, i);
      double score = buoy_score(buoy, attr, 1);
    }

    buoy_dump(buoy, 1);
    buoy_free(buoy);
  }
  return 0;
}

int lexi(int argc, char **argv) {
  Doc doc;
  if (!lexi_read(&doc, 0))
    return 1;
  Buoy *keys = lexi_keys(&doc, &(Tok) {.start = 0, .end = 10});
  for (Size i = 0; i < keys->capacity; i++) {
    Attr *attr = &keys->items[i].attr;
    if (attr->bytes) {
      for (byte b = 0; b < attr->length; b++) {
        byte byte = attr->bytes[b];
        if (' ' < byte && byte <= '~')
          printf("%c", byte);
        else
          printf("(%d)", byte);
      }
      printf("\n");
    }
  }
  buoy_free(keys);
  free(doc.bytes);
  return 0;
}

int doxi(int argc, char **argv) {
  Doc doc;
  if (!lexi_read(&doc, 0))
    return 1;
  Buoy *keys = doxi_keys(&doc);
  buoy_info(keys);
  buoy_free(keys);
  free(doc.bytes);
  return 0;
}

int spin(int argc, char **argv) {
  int min = argc > 2 ? atoi(argv[2]) : 0,
    max = argc > 3 ? atoi(argv[3]) : 10;
  Buoy *buoy = buoy_new();
  Doc doc;
  if (!lexi_read(&doc, 0))
    return 1;
  fprintf(stderr, "document length: %llu\n", doc.length);
  fprintf(stderr, "\e[?25l");
  for (Pos start = 0; start < doc.length; start++) {
    for (int size = min; size < max; size++) {
      Pos end = start + size;
      Tok tok = {.start = start, .end = end > doc.length ? doc.length : end};
      double delta = lexi_learn(buoy, &doc, &tok, size);
      fprintf(stderr, "\rlearn: %8llu\t%.3f", start, delta);
    }
  }
  fprintf(stderr, "\n");
  for (Pos start = 0; start < doc.length; start++) {
    for (int size = min; size < max; size++) {
      Pos end = start + size;
      Tok tok = {.start = start, .end = end > doc.length ? doc.length : end};
      double score = lexi_score(buoy, &doc, &tok);
      fprintf(stderr, "\rscore: %8llu\t%.3f\t(%d)", start, score, size);
    }
  }
  fprintf(stderr, "\e[?25h\n");
  buoy_info(buoy);
  buoy_free(buoy);
  free(doc.bytes);
  return 0;
}

int norm(int argc, char **argv) {
  int n = argc > 2 ? atoi(argv[2]) : 1000;
  for (int k = 0; k < 1; k++) {
    Buoy *buoy = buoy_new();

    for (int i = 0; i < n; i++) {
      Attr *attr[] = {&(Attr) {.bytes = (byte *) &i, .length = 8}};
      double delta = buoy_learn(buoy, attr, 1, i);
      double score = buoy_score(buoy, attr, 1);
    }

    printf("%d: %lf\n", n, buoy_dot(buoy, buoy));
    buoy_free(buoy);
  }
  return 0;
}

int main(int argc, char **argv) {
  if (argc > 1) {
    if (strncmp(argv[1], "copy", 4) == 0)
      return copy(argc, argv);
    if (strncmp(argv[1], "dump", 4) == 0)
      return dump(argc, argv);
    if (strncmp(argv[1], "load", 4) == 0)
      return load(argc, argv);
    if (strncmp(argv[1], "lexi", 4) == 0)
      return lexi(argc, argv);
    if (strncmp(argv[1], "doxi", 4) == 0)
      return doxi(argc, argv);
    if (strncmp(argv[1], "spin", 4) == 0)
      return spin(argc, argv);
    if (strncmp(argv[1], "norm", 4) == 0)
      return norm(argc, argv);
  }
  return printf("usage: %s [copy|dump|load|lexi|doxi|spin|norm]\n", argv[0]);
}

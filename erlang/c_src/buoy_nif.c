#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include "erl_nif.h"
#include "buoy.h"
#include "lexi.h"
#include "queue.h"

/* Static Erlang Terms */

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
#define TERM_EQ(lhs, rhs) (enif_compare(lhs, rhs) == 0)

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_BADARG;
static ERL_NIF_TERM ATOM_EALLOC;
static ERL_NIF_TERM ATOM_ECREAT;
static ERL_NIF_TERM ATOM_ERROR;

static ERL_NIF_TERM ATOM_BUOY_NEW;
static ERL_NIF_TERM ATOM_BUOY_COPY;
static ERL_NIF_TERM ATOM_BUOY_OPEN;
static ERL_NIF_TERM ATOM_BUOY_SAVE;
static ERL_NIF_TERM ATOM_BUOY_FETCH;
static ERL_NIF_TERM ATOM_BUOY_STORE;
static ERL_NIF_TERM ATOM_DOXI_LEARN;
static ERL_NIF_TERM ATOM_DOXI_SCORE;
static ERL_NIF_TERM ATOM_LEXI_LEARN;
static ERL_NIF_TERM ATOM_LEXI_SCORE;

/* Buoy Types */

#define AttrMaxLen ((1 << (8 * sizeof(byte))) - 1)

typedef struct {
  Buoy *buoy;
  ErlNifTid tid;
  ErlNifThreadOpts *opts;
  queue *msgs;
} ErlBuoy;

static ErlNifResourceType *ErlBuoyType;

typedef struct Message Message;
typedef ERL_NIF_TERM (*ErlBuoyFn)(ErlBuoy *, Message *);

struct Message {
  ErlNifEnv *env;
  ErlNifPid from;
  ErlBuoyFn func;
  ERL_NIF_TERM term;
};

/* Support Functions */

static char *
errno_id(int error) {
  switch (error) {
    case EACCES: return "eacces";
    case EAGAIN: return "eagain";
    case EEXIST: return "eexist";
    case EINVAL: return "einval";
    case EISDIR: return "eisdir";
    case EMFILE: return "emfile";
    case ENFILE: return "enfile";
    case ENOENT: return "enoent";
    case ENOMEM: return "enomem";
    case ENOSPC: return "enospc";
  }
  return "unknown";
}

static ERL_NIF_TERM
make_reference(ErlNifEnv *env, void *res) {
  ERL_NIF_TERM ref = enif_make_resource(env, res);
  enif_release_resource(res);
  return ref;
}

/* Buoy Implementation */

static void
Message_free(Message *msg) {
  if (msg->env)
    enif_free_env(msg->env);
  enif_free(msg);
}

static Message *
Message_new(ErlNifEnv *env, ErlBuoyFn func, ERL_NIF_TERM term) {
  Message *msg;
  if (!(msg = (Message *)enif_alloc(sizeof(Message))))
    return NULL;

  if (!(msg->env = enif_alloc_env())) {
    Message_free(msg);
    return NULL;
  }

  if (env)
    enif_self(env, &msg->from);

  msg->func = func;
  msg->term = term ? enif_make_copy(msg->env, term) : 0;
  return msg;
}

static ERL_NIF_TERM
ErlDoxi_learn_async(ErlBuoy *buoy, Message *msg) {
  int arity;
  const ERL_NIF_TERM *args;
  ErlNifBinary docbin;
  Doc doc;
  Real score;
  if (!enif_get_tuple(msg->env, msg->term, &arity, &args) || arity != 2)
    goto badarg;
  if (!enif_inspect_iolist_as_binary(msg->env, args[0], &docbin))
    goto badarg;
  if (!enif_get_double(msg->env, args[1], &score))
    goto badarg;
  doc.bytes = docbin.data;
  doc.length = docbin.size;
  return enif_make_double(msg->env, doxi_learn(buoy->buoy, &doc, score));

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlDoxi_score_async(ErlBuoy *buoy, Message *msg) {
  ErlNifBinary docbin;
  Doc doc;
  if (!enif_inspect_iolist_as_binary(msg->env, msg->term, &docbin))
    goto badarg;
  doc.bytes = docbin.data;
  doc.length = docbin.size;
  return enif_make_double(msg->env, doxi_score(buoy->buoy, &doc));

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlLexi_learn_async(ErlBuoy *buoy, Message *msg) {
  int arity;
  const ERL_NIF_TERM *args;
  ErlNifBinary docbin;
  Doc doc;
  Tok tok;
  Real score;
  if (!enif_get_tuple(msg->env, msg->term, &arity, &args) || arity != 4)
    goto badarg;
  if (!enif_inspect_iolist_as_binary(msg->env, args[0], &docbin))
    goto badarg;
  if (!enif_get_uint64(msg->env, args[1], &tok.start))
    goto badarg;
  if (!enif_get_uint64(msg->env, args[2], &tok.end))
    goto badarg;
  if (!enif_get_double(msg->env, args[3], &score))
    goto badarg;
  doc.bytes = docbin.data;
  doc.length = docbin.size;
  return enif_make_double(msg->env, lexi_learn(buoy->buoy, &doc, &tok, score));

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlLexi_score_async(ErlBuoy *buoy, Message *msg) {
  int arity;
  const ERL_NIF_TERM *args;
  ErlNifBinary docbin;
  Doc doc;
  Tok tok;
  if (!enif_get_tuple(msg->env, msg->term, &arity, &args) || arity != 3)
    goto badarg;
  if (!enif_inspect_iolist_as_binary(msg->env, args[0], &docbin))
    goto badarg;
  if (!enif_get_uint64(msg->env, args[1], &tok.start))
    goto badarg;
  if (!enif_get_uint64(msg->env, args[2], &tok.end))
    goto badarg;
  doc.bytes = docbin.data;
  doc.length = docbin.size;
  return enif_make_double(msg->env, lexi_score(buoy->buoy, &doc, &tok));

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlBuoy_new_async(ErlBuoy *buoy, Message *msg) {
  if (!(buoy->buoy = buoy_new()))
    return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_ECREAT);
  return ATOM_OK;
}

static ERL_NIF_TERM
ErlBuoy_copy_async(ErlBuoy *buoy, Message *msg) {
  ErlBuoy *orig;
  if (!enif_get_resource(msg->env, msg->term, ErlBuoyType, (void **)&orig) || !orig->buoy)
    return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
  if (!(buoy->buoy = buoy_copy(orig->buoy)))
    return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_ECREAT);
  return ATOM_OK;
}

static ERL_NIF_TERM
ErlBuoy_open_async(ErlBuoy *buoy, Message *msg) {
  unsigned size;
  if (enif_get_list_length(msg->env, msg->term, &size)) {
    char buf[size + 1];
    if (!enif_get_string(msg->env, msg->term, buf, size + 1, ERL_NIF_LATIN1))
      goto badarg;
    int fd = open(buf, O_RDONLY);
    if (fd < 0)
      return enif_make_tuple2(msg->env, ATOM_ERROR, enif_make_atom(msg->env, errno_id(errno)));
    if (!(buoy->buoy = buoy_load(fd))) {
      close(fd);
      return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_ECREAT);
    }
    close(fd);
    return ATOM_OK;
  }

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlBuoy_save_async(ErlBuoy *buoy, Message *msg) {
  unsigned size;
  if (enif_get_list_length(msg->env, msg->term, &size)) {
    char buf[size + 1];
    if (!enif_get_string(msg->env, msg->term, buf, size + 1, ERL_NIF_LATIN1))
      goto badarg;
    int fd = open(buf, O_WRONLY | O_CREAT, 0644);
    if (fd < 0)
      return enif_make_tuple2(msg->env, ATOM_ERROR, enif_make_atom(msg->env, errno_id(errno)));
    if (!(buoy_dump(buoy->buoy, fd))) {
      close(fd);
      return enif_make_tuple2(msg->env, ATOM_ERROR, enif_make_atom(msg->env, errno_id(errno)));
    }
    close(fd);
    return ATOM_OK;
  }

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlBuoy_fetch_async(ErlBuoy *buoy, Message *msg) {
  ErlNifBinary attrbin;
  Attr attr;
  if (!enif_inspect_iolist_as_binary(msg->env, msg->term, &attrbin))
    goto badarg;
  if (attrbin.size > AttrMaxLen)
    goto badarg;
  attr.bytes = attrbin.data;
  attr.length = attrbin.size;
  return enif_make_double(msg->env, buoy_fetch(buoy->buoy, &attr));

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static ERL_NIF_TERM
ErlBuoy_store_async(ErlBuoy *buoy, Message *msg) {
  int arity;
  const ERL_NIF_TERM *args;
  ErlNifBinary attrbin;
  Attr attr;
  Real weight;
  if (!enif_get_tuple(msg->env, msg->term, &arity, &args) || arity != 2)
    goto badarg;
  if (!enif_inspect_iolist_as_binary(msg->env, args[0], &attrbin))
    goto badarg;
  if (!enif_get_double(msg->env, args[1], &weight))
    goto badarg;
  if (attrbin.size > AttrMaxLen)
    goto badarg;
  attr.bytes = attrbin.data;
  attr.length = attrbin.size;
  return enif_make_double(msg->env, buoy_store(buoy->buoy, &attr, weight));

 badarg:
  return enif_make_tuple2(msg->env, ATOM_ERROR, ATOM_BADARG);
}

static void *
ErlBuoy_run(void *arg) {
  ErlBuoy *buoy = (ErlBuoy *)arg;
  int done = 0;

  while (!done) {
    Message *msg = queue_pop(buoy->msgs);
    if (msg->func)
      enif_send(NULL, &msg->from, msg->env, msg->func(buoy, msg));
    else
      done = 1;
    Message_free(msg);
  }

  return NULL;
}

static ErlBuoy *
ErlBuoy_start(ErlNifEnv *env) {
  ErlBuoy *buoy;
  if (!(buoy = enif_alloc_resource(ErlBuoyType, sizeof(ErlBuoy))))
    goto error;
  if (!(buoy = memset(buoy, 0, sizeof(ErlBuoy))))
    goto error;
  if (!(buoy->msgs = queue_new()))
    goto error;
  if (!(buoy->opts = enif_thread_opts_create("buoy_opts")))
    goto error;
  if (enif_thread_create("buoy", &buoy->tid, &ErlBuoy_run, buoy, buoy->opts))
    goto error;
  return buoy;

 error:
  if (buoy)
    enif_release_resource(buoy);
  return NULL;
}

static ERL_NIF_TERM
ErlBuoy_init(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlBuoy *buoy;
  if (!(buoy = ErlBuoy_start(env)))
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_EALLOC);
  if (TERM_EQ(argv[0], ATOM_BUOY_NEW))
    queue_push(buoy->msgs, Message_new(env, &ErlBuoy_new_async, argv[1]));
  else if (TERM_EQ(argv[0], ATOM_BUOY_COPY))
    queue_push(buoy->msgs, Message_new(env, &ErlBuoy_copy_async, argv[1]));
  else if (TERM_EQ(argv[0], ATOM_BUOY_OPEN))
    queue_push(buoy->msgs, Message_new(env, &ErlBuoy_open_async, argv[1]));
  else {
    enif_release_resource(buoy);
    return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);
  }
  return make_reference(env, buoy);
}

static ERL_NIF_TERM
ErlBuoy_call(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlBuoy *buoy;
  if (!enif_get_resource(env, argv[0], ErlBuoyType, (void **)&buoy))
    goto badarg;
  if (TERM_EQ(argv[1], ATOM_BUOY_SAVE))
    queue_push(buoy->msgs, Message_new(env, &ErlBuoy_save_async, argv[2]));
  else if (TERM_EQ(argv[1], ATOM_BUOY_FETCH))
    queue_push(buoy->msgs, Message_new(env, &ErlBuoy_fetch_async, argv[2]));
  else if (TERM_EQ(argv[1], ATOM_BUOY_STORE))
    queue_push(buoy->msgs, Message_new(env, &ErlBuoy_store_async, argv[2]));
  else if (TERM_EQ(argv[1], ATOM_DOXI_LEARN))
    queue_push(buoy->msgs, Message_new(env, &ErlDoxi_learn_async, argv[2]));
  else if (TERM_EQ(argv[1], ATOM_DOXI_SCORE))
    queue_push(buoy->msgs, Message_new(env, &ErlDoxi_score_async, argv[2]));
  else if (TERM_EQ(argv[1], ATOM_LEXI_LEARN))
    queue_push(buoy->msgs, Message_new(env, &ErlLexi_learn_async, argv[2]));
  else if (TERM_EQ(argv[1], ATOM_LEXI_SCORE))
    queue_push(buoy->msgs, Message_new(env, &ErlLexi_score_async, argv[2]));
  else
    goto badarg;
  return argv[0];

 badarg:
  return enif_make_tuple2(env, ATOM_ERROR, ATOM_BADARG);
}

static void
ErlBuoy_free(ErlNifEnv *env, void *res) {
  ErlBuoy *buoy = (ErlBuoy *)res;
  Message *stop = Message_new(NULL, NULL, 0);

  queue_push(buoy->msgs, stop);

  enif_thread_join(buoy->tid, NULL);
  enif_thread_opts_destroy(buoy->opts);

  queue_free(buoy->msgs);

  if (buoy->buoy)
    buoy_free(buoy->buoy);
}

/* NIF Initialization */

static ErlNifFunc nif_funcs[] =
  {
    {"init", 2, ErlBuoy_init},
    {"call", 3, ErlBuoy_call},
  };

static int
on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  ErlBuoyType = enif_open_resource_type(env, NULL, "buoy", &ErlBuoy_free, flags, NULL);
  if (ErlBuoyType == NULL)
    return -1;

  ATOM(ATOM_OK, "ok");
  ATOM(ATOM_BADARG, "badarg");
  ATOM(ATOM_EALLOC, "ealloc");
  ATOM(ATOM_ECREAT, "ecreat");
  ATOM(ATOM_ERROR, "error");

  ATOM(ATOM_BUOY_NEW, "buoy_new");
  ATOM(ATOM_BUOY_COPY, "buoy_copy");
  ATOM(ATOM_BUOY_OPEN, "buoy_open");
  ATOM(ATOM_BUOY_SAVE, "buoy_save");
  ATOM(ATOM_BUOY_FETCH, "buoy_fetch");
  ATOM(ATOM_BUOY_STORE, "buoy_store");
  ATOM(ATOM_DOXI_LEARN, "doxi_learn");
  ATOM(ATOM_DOXI_SCORE, "doxi_score");
  ATOM(ATOM_LEXI_LEARN, "lexi_learn");
  ATOM(ATOM_LEXI_SCORE, "lexi_score");

  return 0;
}

ERL_NIF_INIT(buoy_nif, nif_funcs, &on_load, NULL, NULL, NULL);

#include "buoy.h"
#include "lexi.h"

typedef struct {
    PyObject_HEAD
    Buoy *buoy;
} PyBuoy;

typedef struct {
    PyObject_HEAD
    PyBuoy *owner;
    Size cursor;
} PyBuoyIter;

/* Module Methods */

static PyObject * Lexi_learn(PyObject *, PyObject *);
static PyObject * Lexi_score(PyObject *, PyObject *);

/* PyBuoy Object Protocol */

static PyTypeObject PyBuoyType;

static PyObject * PyBuoy_new(PyTypeObject *, PyObject *, PyObject *);
static void       PyBuoy_dealloc(PyBuoy *);

/* Mapping Formal / Informal Protocol */

static Py_ssize_t PyBuoy_length(PyBuoy *);
static PyObject * PyBuoy_getitem(PyBuoy *, PyObject *);
static int        PyBuoy_setitem(PyBuoy *, PyObject *, PyObject *);
static PyObject * PyBuoy_keys(PyBuoy *);
static PyObject * PyBuoy_dot(PyBuoy *, PyObject *);

/* Serialization / Deserialization Informal Protocol */

static PyObject * PyBuoy_dump(PyBuoy *, PyObject *);
static PyObject * PyBuoy_load(PyTypeObject *, PyObject *);

/* PyBuoy Iterator Types */

static PyTypeObject PyBuoyIterType;

static PyObject * PyBuoyIter_new(PyTypeObject *, PyBuoy *);
static void       PyBuoyIter_dealloc(PyBuoyIter *);
static PyObject * PyBuoyIter_iternext(PyBuoyIter *);


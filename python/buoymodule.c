#include <Python.h>
#include "structmember.h"
#include "buoymodule.h"

static PyObject *BuoyError;

/* Module Methods */

static PyMethodDef buoy_methods[] = {
  {"lexi_learn", (PyCFunction)Lexi_learn, METH_VARARGS,
   "d.learn(buoy, doc, start, end, score) -> learn a token from doc into buoy."},
  {"lexi_score", (PyCFunction)Lexi_score, METH_VARARGS,
   "d.score(buoy, doc, start, end) -> score a token from doc using buoy."},
  {NULL}                         /* Sentinel          */
};

static PyObject *
Lexi_learn(PyObject *self, PyObject *args)
{
  PyBuoy *pybuoy = NULL;
  Doc doc = {0, 0};
  Tok tok = {0, 0};
  Real delta, score;

  if (!PyArg_ParseTuple(args, "Os#kkd", &pybuoy, &doc.bytes, &doc.length, &tok.start, &tok.end, &score))
    goto Done;

  if (!PyObject_IsInstance((PyObject *)pybuoy, (PyObject *)&PyBuoyType)) {
    PyErr_SetString(BuoyError, "Object is not a Buoy");
    goto Done;
  }

  delta = lexi_learn(pybuoy->buoy, &doc, &tok, score);

 Done:
  if (PyErr_Occurred())
    return NULL;
  return PyFloat_FromDouble(delta);
}

static PyObject *
Lexi_score(PyObject *self, PyObject *args)
{
  PyBuoy *pybuoy = NULL;
  Doc doc = {0, 0};
  Tok tok = {0, 0};
  Real score;

  if (!PyArg_ParseTuple(args, "Os#kk", &pybuoy, &doc.bytes, &doc.length, &tok.start, &tok.end))
    goto Done;

  score = lexi_score(pybuoy->buoy, &doc, &tok);

 Done:
  if (PyErr_Occurred())
    return NULL;
  return PyFloat_FromDouble(score);
}

/* PyBuoy Object Definition */

static PyMappingMethods PyBuoy_as_mapping = {
  (lenfunc)PyBuoy_length,        /* mp_length         */
  (binaryfunc)PyBuoy_getitem,    /* mp_subscript      */
  (objobjargproc)PyBuoy_setitem, /* mp_ass_subscript  */
};

static PyMethodDef PyBuoy_methods[] = {
  {"keys", (PyCFunction)PyBuoy_keys, METH_NOARGS,
   "d.keys() -> an iterator over the keys of d."},
  {"dump", (PyCFunction)PyBuoy_dump, METH_O,
   "d.dump(o) -> write serialization of d to file object o."},
  {"load", (PyCFunction)PyBuoy_load, METH_CLASS | METH_O,
   "D.load(o) -> a deserialized instance of D from file object o."},
  {NULL}                         /* Sentinel          */
};

static PyMemberDef PyBuoy_members[] = {
  {NULL}                         /* Sentinel          */
};

static PyTypeObject PyBuoyType = {
  PyObject_HEAD_INIT(&PyType_Type)
  0,                             /* ob_size */
  "Buoy",                        /* tp_name           */
  sizeof(PyBuoy),                /* tp_basicsize      */
  0,                             /* tp_itemsize       */
  (destructor)PyBuoy_dealloc,    /* tp_dealloc        */
  0,                             /* tp_print          */
  0,                             /* tp_getattr        */
  0,                             /* tp_setattr        */
  0,                             /* tp_compare        */
  0,                             /* tp_repr           */
  0,                             /* tp_as_number      */
  0,                             /* tp_as_sequence    */
  &PyBuoy_as_mapping,            /* tp_as_mapping     */
  0,                             /* tp_hash           */
  0,                             /* tp_call           */
  0,                             /* tp_str            */
  0,                             /* tp_getattro       */
  0,                             /* tp_setattro       */
  0,                             /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT |
  Py_TPFLAGS_BASETYPE,           /* tp_flags          */
  0,                             /* tp_doc            */
  0,                             /* tp_traverse       */
  0,                             /* tp_clear          */
  0,                             /* tp_richcompare    */
  0,                             /* tp_weaklistoffset */
  (getiterfunc)PyBuoy_keys,      /* tp_iter           */
  0,                             /* tp_iternext       */
  PyBuoy_methods,                /* tp_methods        */
  PyBuoy_members,                /* tp_members        */
  0,                             /* tp_getset         */
  0,                             /* tp_base           */
  0,                             /* tp_dict           */
  0,                             /* tp_descr_get      */
  0,                             /* tp_descr_set      */
  0,                             /* tp_dictoffset     */
  0,                             /* tp_init           */
  0,                             /* tp_alloc          */
  PyBuoy_new,                    /* tp_new            */
  0,                             /* tp_free           */
};

/* PyBuoy Object Protocol */

static PyObject *
PyBuoy_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  PyBuoy *self;

  if (!(self = (PyBuoy *)PyBuoyType.tp_alloc(type, 0)))
    goto Done;

  if (!(self->buoy = buoy_new())) {
    PyErr_SetString(BuoyError, "Buoy creation failed");
    goto Done;
  }

 Done:
  if (PyErr_Occurred()) {
    Py_CLEAR(self);
    return NULL;
  }
  return (PyObject *)self;
}

static void
PyBuoy_dealloc(PyBuoy *self)
{
  if (self->buoy)
    buoy_free(self->buoy);
  Py_TYPE(self)->tp_free((PyObject *)self);
}


/* Mapping Formal / Informal Protocol */

static Py_ssize_t
PyBuoy_length(PyBuoy *self)
{
  return self->buoy->size;
}

static PyObject *
PyBuoy_getitem(PyBuoy *self, PyObject *key)
{
  PyObject *pack = NULL;
  Attr attr;
  Real weight;

  if (!(pack = Py_BuildValue("(O)", key)))
    goto Done;

  if (!PyArg_ParseTuple(pack, "s#", &attr.bytes, &attr.length))
    goto Done;

  weight = buoy_fetch(self->buoy, &attr);

 Done:
  Py_CLEAR(pack);
  if (PyErr_Occurred())
    return NULL;
  return PyFloat_FromDouble(weight);
}

static int
PyBuoy_setitem(PyBuoy *self, PyObject *key, PyObject *val)
{
  PyObject *pack = NULL;
  Attr attr;

  if (!(pack = Py_BuildValue("(O)", key)))
    goto Done;

  if (!PyArg_ParseTuple(pack, "s#", &attr.bytes, &attr.length))
    goto Done;

  buoy_store(self->buoy, &attr, PyFloat_AsDouble(val));

 Done:
  Py_CLEAR(pack);
  if (PyErr_Occurred())
    return -1;
  return 0;
}

static PyObject *
PyBuoy_keys(PyBuoy *self)
{
  return PyBuoyIter_new(&PyBuoyIterType, self);
}

/* Serialization / Deserialization Informal Protocol */

static PyObject *
PyBuoy_dump(PyBuoy *self, PyObject *file)
{
  PyObject *fileno = NULL;
  int fd;

  if (!(fileno = PyObject_CallMethod(file, "fileno", NULL)))
    goto Done;

  if ((fd = PyLong_AsLong(fileno)) < 0)
    goto Done;

  if (!buoy_dump(self->buoy, fd)) {
    PyErr_SetString(BuoyError, "Buoy dump failed");
    goto Done;
  }

 Done:
  Py_CLEAR(fileno);
  if (PyErr_Occurred())
    return NULL;
  Py_RETURN_NONE;
}

static PyObject *
PyBuoy_load(PyTypeObject *type, PyObject *file)
{
  PyBuoy *self = (PyBuoy *)type->tp_alloc(type, 0);
  PyObject *fileno = NULL;
  int fd;

  if (!self)
    goto Done;

  if (!(fileno = PyObject_CallMethod(file, "fileno", NULL)))
    goto Done;

  if ((fd = PyLong_AsLong(fileno)) < 0)
    goto Done;

  if (!(self->buoy = buoy_load(fd))) {
    PyErr_SetString(BuoyError, "Buoy load failed");
    goto Done;
  }

 Done:
  Py_CLEAR(fileno);
  if (PyErr_Occurred()) {
    Py_CLEAR(self);
    return NULL;
  }
  return (PyObject *)self;
}

/* Module Initialization */

PyMODINIT_FUNC
init_buoy(void)
{
  PyObject *module = Py_InitModule("_buoy", buoy_methods);

  if (PyType_Ready(&PyBuoyType) < 0)
    return;
  Py_INCREF(&PyBuoyType);
  PyModule_AddObject(module, "Buoy", (PyObject *)&PyBuoyType);

  if (PyType_Ready(&PyBuoyIterType) < 0)
    return;
  Py_INCREF(&PyBuoyIterType);
  PyModule_AddObject(module, "PyBuoyIter", (PyObject *)&PyBuoyIterType);

  BuoyError = PyErr_NewException("buoy.BuoyError", NULL, NULL);
  Py_INCREF(BuoyError);
  PyModule_AddObject(module, "BuoyError", BuoyError);
}

/* PyBuoy Iterator Types */

static PyMethodDef PyBuoyIter_methods[] = {
  {NULL}                                   /* Sentinel          */
};

static PyTypeObject PyBuoyIterType = {
  PyObject_HEAD_INIT(&PyType_Type)
  0          ,                             /* ob_size */
  "Buoy-iterator",                         /* tp_name           */
  sizeof(PyBuoyIter),                      /* tp_basicsize      */
  0,                                       /* tp_itemsize       */
  (destructor)PyBuoyIter_dealloc,          /* tp_dealloc        */
  0,                                       /* tp_print          */
  0,                                       /* tp_getattr        */
  0,                                       /* tp_setattr        */
  0,                                       /* tp_compare        */
  0,                                       /* tp_repr           */
  0,                                       /* tp_as_number      */
  0,                                       /* tp_as_sequence    */
  0,                                       /* tp_as_mapping     */
  0,                                       /* tp_hash           */
  0,                                       /* tp_call           */
  0,                                       /* tp_str            */
  PyObject_GenericGetAttr,                 /* tp_getattro       */
  0,                                       /* tp_setattro       */
  0,                                       /* tp_as_buffer      */
  Py_TPFLAGS_DEFAULT |
  Py_TPFLAGS_BASETYPE,                     /* tp_flags          */
  0,                                       /* tp_doc            */
  0,                                       /* tp_traverse       */
  0,                                       /* tp_clear          */
  0,                                       /* tp_richcompare    */
  0,                                       /* tp_weaklistoffset */
  PyObject_SelfIter,                       /* tp_iter           */
  (iternextfunc)PyBuoyIter_iternext,       /* tp_iternext       */
  PyBuoyIter_methods,                      /* tp_methods        */
};

static PyObject *
PyBuoyIter_new(PyTypeObject *type, PyBuoy *owner)
{
  PyBuoyIter *self = PyObject_New(PyBuoyIter, type);
  if (self != NULL) {
    Py_INCREF(owner);
    self->owner  = owner;
    self->cursor = 0;
  }
  return (PyObject *)self;
}

static void
PyBuoyIter_dealloc(PyBuoyIter *self)
{
  Py_CLEAR(self->owner);
  PyObject_Del(self);
}

static PyObject *
PyBuoyIter_iternext(PyBuoyIter *self)
{
  Item *item;
  do {
    if (self->cursor >= self->owner->buoy->capacity)
      return NULL;
    item = self->owner->buoy->items + self->cursor++;
  } while (!item->attr.bytes);
  return Py_BuildValue("s#", item->attr.bytes, item->attr.length);
}

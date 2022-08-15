.. _library_metagol:

``metagol``
===========

This folder contains a Logtalk port of ``metagol``, an inductive logic
programming (ILP) system based on meta-interpretive learning available
from:

::

   https://github.com/metagol/metagol

See the original code git repo for details and bibliography on Metagol
and ILP.

The port allows any number of datasets to be loaded simultaneously with
per-dataset learning options. A dataset is simply wrapped in an object
that extends and is expanded by the ``metagol`` object as illustrated by
the ported examples.

Both the original code and the port requires the coroutining ``when/2``
predicate, which is only available in some backend Prolog systems. The
port currently supports ECLiPSe, LVM, SICStus Prolog, SWI-Prolog, and
YAP. It can be used on both POSIX and Windows operating-systems.

The examples are ported from the original Metagol distribution. Some of
the examples are taken from the following paper (with the original
Prolog examples source code files made available by MystikNinja):

::

   @article{DBLP:journals/jair/EvansG18,
       author    = {Richard Evans and Edward Grefenstette},
       title     = {Learning Explanatory Rules from Noisy Data},
       journal   = {J. Artif. Intell. Res.},
       volume    = {61},
       pages     = {1--64},
       year      = {2018},
       url       = {https://doi.org/10.1613/jair.5714},
       doi       = {10.1613/jair.5714},
       timestamp = {Mon, 21 Jan 2019 15:01:17 +0100},
       biburl    = {https://dblp.org/rec/bib/journals/jair/EvansG18},
       bibsource = {dblp computer science bibliography, https://dblp.org}
   }

The paper can be downloaded at

::

   https://arxiv.org/pdf/1711.04574.pdf

For sample queries, please see the ``SCRIPT.txt`` file.

API documentation
-----------------

Open the
`../../docs/library_index.html#metagol <../../docs/library_index.html#metagol>`__
link in a web browser.

Loading
-------

To load all entities in this port, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(metagol(loader)).

Testing
-------

To test this port predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(metagol(tester)).

There are three lengthy tests that only run when the tests are being run
manually instead of automatically.

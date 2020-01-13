``assignvars``
==============

The ``assignvarsp`` protocol declares the predicates used for logical
assignment of Prolog terms developed by Nobukuni Kino.

The ``assignvars`` object provides a declarative implementation of the
``assignvarsp`` protocol. It can be used with any backend Prolog
compiler.

The ``nd_assignvars`` object provides a non-declarative but faster
implementation of the ``assignvarsp`` protocol. It can be used with the
following backend Prolog compilers: B-Prolog, CxProlog, ECLiPSe, GNU
Prolog, Qu-Prolog, SICStus Prolog, SWI-Prolog, and YAP.

For more information on assignvars, please consult the URL:

::

   http://www.kprolog.com/en/logical_assignment/

API documentation
-----------------

Open the
`../../docs/library_index.html#assignvars <../../docs/library_index.html#assignvars>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(assignvars(loader)).


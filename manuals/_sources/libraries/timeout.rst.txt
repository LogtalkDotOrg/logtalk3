.. _library_timeout:

``timeout``
===========

The ``timeout`` object provides a portable abstraction over calling a
goal deterministically with a time limit as made available in some form
by some of the supported backend Prolog systems (B-Prolog, ECLiPSe, XVM,
SICStus Prolog, SWI-Prolog, Trealla Prolog, XSB, and YAP).

For better performance, compile calls to this library meta-predicates
with the ``optimize`` flag turned on so that the meta-arguments, i.e.
the goals that you are timing, are also compiled.

API documentation
-----------------

Open the
`../../docs/library_index.html#timeout <../../docs/library_index.html#timeout>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(timeout(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(timeout(tester)).

Known issues
------------

Two tests are currently skipped when using the SWI-Prolog backend as
they cannot be interrupted and generate the expected timeout exceptions
due to tests being run from an ``initialization/1`` directive goal that,
in the SWI-Prolog implementation of this directive, ignores signals. The
test goals do generate the expected timeout exception when not called
from an ``initialization/1`` directive.

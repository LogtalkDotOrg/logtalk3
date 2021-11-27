``dif``
=======

The ``dif`` object provides a portable abstraction over how the
``dif/2`` predicate is made available by the supported backend Prolog
systems that implement it (B-Prolog, ECLiPSe, Scryer Prolog, SICStus
Prolog, SWI-Prolog, XSB, and YAP).

Calls to the library predicates are inlined when compiled with the
``optimize`` flag turned on. In this case, there is no overhead compared
with calling the abstracted predicates directly.

API documentation
-----------------

Open the
`../../docs/library_index.html#dif <../../docs/library_index.html#dif>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(dif(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(dif(tester)).

``coroutining``
===============

The ``coroutining`` object provides a portable abstraction over how
common coroutining predicates are made available by the supported
backend Prolog systems (ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP).

Calls to the library predicates are inlined when compiled with the
``optimize`` flag turned on. In this case, there is no overhead compared
with calling the abstracted predicates directly.

API documentation
-----------------

Open the
`../../docs/library_index.html#coroutining <../../docs/library_index.html#coroutining>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(coroutining(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(coroutining(tester)).


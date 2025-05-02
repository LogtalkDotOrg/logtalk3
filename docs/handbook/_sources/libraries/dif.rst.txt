.. _library_dif:

``dif``
=======

The ``dif`` object provides a portable abstraction over how the
``dif/2`` predicate is made available by the supported backend Prolog
systems that implement it (B-Prolog, ECLiPSe, XVM, SICStus Prolog,
SWI-Prolog, Trealla Prolog, XSB, and YAP).

Calls to the library predicates are inlined when compiled with the
``optimize`` flag turned on. In this case, there is no overhead compared
with calling the abstracted predicate directly.

See also the ``coroutining`` library.

API documentation
-----------------

Open the
`../../apis/library_index.html#dif <../../apis/library_index.html#dif>`__
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

Usage
-----

Load this library from your application loader file. To call the
``dif/1-2`` predicates using implicit message-sending, add the following
directive to any object or category calling the predicates:

::

   :- uses(dif, [
       dif/2, dif/1
   ]).

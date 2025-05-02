.. _library_coroutining:

``coroutining``
===============

The ``coroutining`` object provides a portable abstraction over how
common coroutining predicates are made available by the supported
backend Prolog systems (ECLiPSe, XVM, SICStus Prolog, SWI-Prolog,
Trealla Prolog, and YAP) that provide them. Partial support for XSB is
provided (the predicate ``frozen/2`` is not available and calls to it
fail).

Calls to the library predicates are inlined when compiled with the
``optimize`` flag turned on. In this case, there is no overhead compared
with calling the abstracted predicates directly.

See also the ``dif`` library.

API documentation
-----------------

Open the
`../../apis/library_index.html#coroutining <../../apis/library_index.html#coroutining>`__
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

Usage
-----

Load this library from your application loader file. To call the
coroutining predicates using implicit message-sending, add the following
directive to any object or category calling the predicates (adjust the
list to the predicates actually called):

::

   :- uses(coroutining, [
       dif/2, dif/1, freeze/2, frozen/2, when/2
   ]).

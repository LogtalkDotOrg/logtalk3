``dictionaries``
================

This library provides a dictionary (also know as associative array, map,
or symbol table) protocol and binary tree, AVL tree, and Red–Black tree
implementations.

API documentation
-----------------

Open the
`../../docs/library_index.html#dictionaries <../../docs/library_index.html#dictionaries>`__
file in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(dictionaries(loader)).

Credits
-------

The AVL tree implementation is an adaptation to Logtalk of the ``assoc``
SWI-Prolog library authored by R.A.O'Keefe, L.Damas, V.S.Costa, Glenn
Burgess, Jiri Spitz, and Jan Wielemaker. Additional predicates authored
by Paulo Moura.

The Red–Black tree implementation is an adaptation to Logtalk of the
``rbtrees`` Prolog library authored by Vitor Santos Costa.

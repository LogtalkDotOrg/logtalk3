``dictionaries``
================

This library provides a dictionary (also know as associative array, map,
or symbol table) protocol and binary tree, AVL tree, and Red–Black tree
implementations. The different representations of a dictionary should be
regarded as opaque terms and only accessed using the library predicates.

API documentation
-----------------

Open the
`../../docs/library_index.html#dictionaries <../../docs/library_index.html#dictionaries>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(dictionaries(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(dictionaries(tester)).

Usage
-----

First, select the dictionary implementation that you want to use. For
cases where the number of elements is relatively small and performance
is not critical, ``bintree`` can be a good choice. For other cases,
``avltree`` or ``rbtree`` are likely better choices. If you want to
compare the performance of the implementations, either define an object
alias or use an ``uses/2`` directive so that you can switch between
implementations by simply changing the alias definition or the first
argument of the directive. Note that you can switch between
implementations at runtime without code changes by using a parameter
variable in the first argument of an ``uses/2`` directive.

To create a new dictionary, you can use the ``new/1`` predicate. For
example:

::

   | ?- avltree::new(Dictionary).
   Dictionary = ...
   yes

You can also create a new dictionary from a list of key-value pairs by
using the ``as_dictionary/2`` predicate. For example:

::

   | ?- avltree::as_dictionary([a-1,c-3,b-2], Dictionary).
   Dictionary = ...
   yes

Several predicates are provided for inserting key-value pairs, lookup
key-value pairs updating the value associated with a key, and deleting
key-value pairs. For example:

::

   | ?- avltree::(
           new(Dictionary0),
           insert(Dictionary0, a, 1, Dictionary1),
           update(Dictionary1, a, 2, Dictionary2),
           lookup(a, Value, Dictionary2)
       ).
   Dictionary0 = ...,
   Dictionary1 = ...,
   Dictionary2 = ...,
   Value = 2
   yes

For details on these and other provided predicates, consult the library
API documentation.

Credits
-------

The AVL tree implementation is an adaptation to Logtalk of the ``assoc``
SWI-Prolog library authored by R.A.O'Keefe, L.Damas, V.S.Costa, Glenn
Burgess, Jiri Spitz, and Jan Wielemaker. Additional predicates authored
by Paulo Moura.

The Red–Black tree implementation is an adaptation to Logtalk of the
``rbtrees`` Prolog library authored by Vitor Santos Costa.

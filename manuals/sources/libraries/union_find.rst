``union_find``
==============

This library implements a disjoint-set data structure, aka a union-find
structure. For a discussion on this data structure, see e.g.

https://en.wikipedia.org/wiki/Disjoint-set_data_structure

API documentation
-----------------

Open the
`../../docs/library_index.html#union-find <../../docs/library_index.html#union-find>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(union_find(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(union_find(tester)).

Usage
-----

To create a new union-find structure, use the ``new/2`` predicate:

::

   | ?- union_find::new(UnionFind).
   UnionFind = ...
   yes

For details on these and other provided predicates, consult the library
API documentation.

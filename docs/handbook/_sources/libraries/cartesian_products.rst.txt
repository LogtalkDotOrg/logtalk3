.. _library_cartesian_products:

``cartesian_products``
======================

This library provides predicates for generating and querying Cartesian
products over lists. The following categories of predicates are
provided:

- **Generation operations** - Predicates for generating Cartesian
  products and tuples.
- **Ordering variants** - Predicates that support an additional order
  argument (``default`` or ``lexicographic``) for controlling output
  order.
- **Distinct-value generation** - Predicates for generating tuples while
  deduplicating repeated values in factor lists.
- **Indexed access** - Predicates for direct access to tuples at
  specific positions.
- **Lexicographic stepping** - Predicates for navigating tuples in
  lexicographic order.
- **Counting operations** - Predicates for counting tuples without
  generating them.
- **Random selection** - Predicates for randomly selecting and sampling
  tuples.

Dedicated ``arrangements``, ``permutations``, ``combinations``,
``multisets``, ``derangements``, ``partitions``, and ``subsequences``
libraries are also available for focused APIs on related operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#cartesian_products <../../apis/library_index.html#cartesian_products>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(cartesian_products(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(cartesian_products(tester)).

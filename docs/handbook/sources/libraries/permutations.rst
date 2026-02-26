.. _library_permutations:

``permutations``
================

This library provides predicates for generating and querying
permutations over lists. The following categories of predicates are
provided:

- **Generation operations** - Predicates for generating permutations,
  k-permutations, and Cartesian products.
- **Ordering variants** - Predicates that support an additional order
  argument (``default``, ``lexicographic``, or ``shortlex``) for
  controlling output order.
- **Distinct-value generation** - Predicates for generating permutations
  while deduplicating equal-valued results.
- **Derangement operations** - Predicates for generating and testing
  derangements.
- **Lexicographic stepping** - Predicates for navigating permutations in
  lexicographic order.
- **Indexed access** - Predicates for direct access to permutations at
  specific positions.
- **Counting operations** - Predicates for counting permutations.
- **Random selection** - Predicates for randomly selecting permutations.

Dedicated ``combinations`` and ``subsequences`` libraries are also
available for focused APIs on those operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#permutations <../../apis/library_index.html#permutations>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(permutations(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(permutations(tester)).

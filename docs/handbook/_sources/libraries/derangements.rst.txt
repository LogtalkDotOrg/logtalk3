.. _library_derangements:

``derangements``
================

This library provides predicates for generating and querying
derangements over lists. Derangements are full-length permutations where
no result element is identical to the input element at the same
position. The following categories of predicates are provided:

- **Generation operations** - Predicates for generating derangements.
- **Ordering variants** - Predicates that support an additional order
  argument (``default``, ``lexicographic``, or ``shortlex``) for
  controlling output order.
- **Distinct-value generation** - Predicates for generating derangements
  while deduplicating equal-valued results.
- **Indexed access** - Predicates for direct access to derangements at
  specific positions, including distinct derangements.
- **Lexicographic stepping** - Predicates for navigating derangements in
  lexicographic order.
- **Counting operations** - Predicates for counting derangements and
  distinct derangements.
- **Random selection** - Predicates for randomly selecting and sampling
  derangements and distinct derangements.

Dedicated ``permutations``, ``arrangements``, ``combinations``,
``multisets``, ``partitions``, and ``subsequences`` libraries are also
available for focused APIs on related operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#derangements <../../apis/library_index.html#derangements>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(derangements(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(derangements(tester)).

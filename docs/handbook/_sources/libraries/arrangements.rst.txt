.. _library_arrangements:

``arrangements``
================

This library provides predicates for generating and querying
arrangements over lists. Arrangements are ordered selections of a given
length with element repetition allowed. The following categories of
predicates are provided:

- **Generation operations** - Predicates for generating arrangements.
- **Ordering variants** - Predicates that support an additional order
  argument (``default`` or ``lexicographic``) for controlling output
  order.
- **Distinct-value generation** - Predicates for generating arrangements
  while deduplicating equal-valued results.
- **Indexed access** - Predicates for direct access to arrangements at
  specific positions, including distinct arrangements.
- **Lexicographic stepping** - Predicates for navigating arrangements in
  lexicographic order.
- **Counting operations** - Predicates for counting arrangements and
  distinct arrangements.
- **Random selection** - Predicates for randomly selecting and sampling
  arrangements and distinct arrangements.

Dedicated ``cartesian_products``, ``permutations``, ``combinations``,
``multisets``, ``derangements``, ``partitions``, and ``subsequences``
libraries are also available for focused APIs on related operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#arrangements <../../apis/library_index.html#arrangements>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(arrangements(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(arrangements(tester)).

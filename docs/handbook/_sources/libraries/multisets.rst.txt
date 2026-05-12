.. _library_multisets:

``multisets``
=============

This library provides predicates for generating and querying multisets
over lists. Multisets are unordered selections of a given length with
element repetition allowed. The following categories of predicates are
provided:

- **Generation operations** - Predicates for generating multisets.
- **Ordering variants** - Predicates that support an additional order
  argument (``default`` or ``lexicographic``) for controlling output
  order.
- **Distinct-value generation** - Predicates for generating multisets
  while deduplicating equal-valued results.
- **Indexed access** - Predicates for direct access to multisets at
  specific positions, including order-aware access.
- **Counting operations** - Predicates for counting multisets and
  distinct multisets.
- **Random selection** - Predicates for randomly selecting and sampling
  multisets and distinct multisets.
- **Lexicographic stepping** - Predicates for stepping to the next or
  previous multiset in lexicographic order.

Dedicated ``arrangements``, ``cartesian_products``, ``combinations``,
``permutations``, ``derangements``, ``partitions``, and ``subsequences``
libraries are also available for focused APIs on related operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#multisets <../../apis/library_index.html#multisets>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(multisets(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(multisets(tester)).

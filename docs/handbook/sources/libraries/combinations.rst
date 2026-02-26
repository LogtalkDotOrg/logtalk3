.. _library_combinations:

``combinations``
================

This library provides predicates for generating and querying
combinations over lists. The following categories of predicates are
provided:

- **Generation operations** - Predicates for generating combinations,
  including combinations with replacement.
- **Ordering variants** - Predicates that support an additional order
  argument (``default``, ``lexicographic``, or ``shortlex``) for
  controlling output order.
- **Distinct-value generation** - Predicates for generating combinations
  while deduplicating equal-valued results.
- **Indexed access** - Predicates for direct access to combinations at
  specific positions.
- **Counting operations** - Predicates for counting combinations with
  and without replacement.
- **Random selection** - Predicates for randomly selecting combinations.

Dedicated ``subsequences`` and ``permutations`` libraries are also
available for focused APIs on those operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#combinations <../../apis/library_index.html#combinations>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(combinations(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(combinations(tester)).

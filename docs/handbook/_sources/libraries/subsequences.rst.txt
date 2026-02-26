.. _library_subsequences:

``subsequences``
================

This library provides predicates for working with subsequences
represented using lists, including generation, search, contiguous
subsequence operations, and random selection. The following categories
of predicates are provided:

- **Generation operations** - Predicates for generating subsequences and
  variants thereof.
- **Ordering variants** - Predicates that support an additional order
  argument (``default``, ``lexicographic``, or ``shortlex``) for
  controlling output order.
- **Filtered generation** - Predicates for generating specific types of
  subsequences (combinations, permutations).
- **Indexed access** - Predicates for direct access to subsequences at
  specific positions.
- **Searching and matching** - Predicates for finding specific
  subsequences with desired properties.
- **Prefix and suffix operations** - Predicates for checking and finding
  prefixes and suffixes.
- **Contiguous subsequences** - Predicates for working with contiguous
  subsequences (subslices, sliding windows).
- **Random selection** - Predicates for randomly selecting subsequences.
- **Constrained operations** - Predicates for generating subsequences
  with specific constraints.
- **Utility predicates** - Predicates that support subsequence
  operations.

Dedicated ``combinations`` and ``permutations`` libraries are also
available for focused APIs on those operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#subsequences <../../apis/library_index.html#subsequences>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(subsequences(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(subsequences(tester)).

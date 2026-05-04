.. _library_gsp:

``gsp``
=======

GSP sequential pattern miner for sequence datasets. The library depends
on the ``sequential_pattern_mining_protocols`` support library,
implements the generic ``pattern_miner_protocol`` defined in the
``pattern_mining_protocols`` core library, and mines frequent sequential
patterns using level-wise candidate generation and subsequence support
counting.

API documentation
-----------------

Open the
`../../apis/library_index.html#gsp <../../apis/library_index.html#gsp>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(gsp(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(gsp(tester)).

Features
--------

- **Level-Wise Mining**: Builds longer frequent sequences by extending
  the current frequent level.
- **Join-Based Candidates**: Generates candidate sequences by joining
  compatible frequent patterns from the previous level.
- **Support-Pruned Candidates**: Prunes candidates whose immediate
  subpatterns are not all frequent.
- **Batched Horizontal Support Counting**: Counts singleton and
  candidate supports using horizontal scans over the sequence database
  at each level.
- **Canonical Sequences**: Validates that itemsets are sorted,
  duplicate-free, non-empty, and restricted to declared items.
- **Flexible Support Thresholds**: Supports relative minimum support and
  absolute minimum support count.
- **Model Export**: Mined pattern collections can be exported as
  predicate clauses or written to a file.

Options
-------

The ``mine/3`` predicate accepts the following options:

- ``minimum_support/1``: Relative minimum support threshold in the
  interval ``]0.0, 1.0]``. The default is ``0.5``.
- ``minimum_support_count/1``: Absolute minimum support count. When both
  support options are provided, this option takes precedence.
- ``maximum_pattern_length/1``: Maximum total number of items in a mined
  sequential pattern. The default is ``1000``, effectively capped by the
  longest sequence in the dataset.
- ``minimum_pattern_length/1``: Minimum total number of items retained
  in the mined result. The default is ``1``.

Pattern miner representation
----------------------------

The mined pattern miner result is represented by a compound term with
the functor chosen by the implementation and arity 3. For example:

::

   gsp_pattern_miner(ItemDomain, Patterns, Options)

Where:

- ``ItemDomain``: Canonical sorted list of declared dataset items.
- ``Patterns``: List of ``sequence_pattern(Pattern, SupportCount)``
  terms ordered first by total item count and then lexicographically.
- ``Options``: Effective mining options used to mine the frequent
  sequential patterns.

References
----------

1. Srikant, R. and Agrawal, R. (1996) - "Mining sequential patterns:
   Generalizations and performance improvements".

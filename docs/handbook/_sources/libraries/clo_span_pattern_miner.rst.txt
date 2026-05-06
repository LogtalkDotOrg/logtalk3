.. _library_clo_span_pattern_miner:

``clo_span_pattern_miner``
==========================

CloSpan closed sequential pattern miner for sequence datasets. The
library depends on the ``sequential_pattern_mining_protocols`` support
library, implements the generic ``pattern_miner_protocol`` defined in
the ``pattern_mining_protocols`` core library, and mines closed frequent
sequential patterns directly using closure-aware projected-database
search with explicit projected-database-equivalence backward pruning
that skips equivalent branches before recursion, together with a
same-support closed frontier that merges branch results during the
search without a final post-filter.

Requires a dataset implementing ``sequence_dataset_protocol`` with
sequences represented as ordered lists of canonical sorted itemsets over
a declared item domain.

API documentation
-----------------

Open the
`../../apis/library_index.html#clo_span_pattern_miner <../../apis/library_index.html#clo_span_pattern_miner>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(clo_span_pattern_miner(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(clo_span_pattern_miner(tester)).

Features
--------

- **Closed Pattern Mining**: Retains only frequent sequential patterns
  that have no superpattern with the same support.
- **Backward and Closure Pruning**: Uses projected-database equivalence
  to skip equivalent backward-growth branches before recursion, then
  maintains a same-support closed frontier to suppress dominated
  patterns during the search instead of post-filtering another miner
  output.
- **Canonical Sequences**: Uses the shared sequential validation logic
  from the support library.
- **Flexible Support Thresholds**: Supports relative minimum support and
  absolute minimum support count. When both are given, the
  absolute-count threshold takes precedence.
- **Model Export**: Closed pattern collections can be exported as
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

   clo_span_pattern_miner(ItemDomain, Patterns, Options)

Where:

- ``ItemDomain``: Canonical sorted list of declared dataset items.
- ``Patterns``: List of ``sequence_pattern(Pattern, SupportCount)``
  terms ordered first by total item count and then lexicographically.
- ``Options``: Effective mining options used to mine the closed
  sequential patterns.

References
----------

1. Yan, X., Han, J., and Afshar, R. (2003) - "CloSpan: Mining closed
   sequential patterns in large datasets".

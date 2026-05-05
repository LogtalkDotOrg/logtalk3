.. _library_prefix_span:

``prefix_span``
===============

PrefixSpan sequential pattern miner for sequence datasets. The library
depends on the ``sequential_pattern_mining_protocols`` support library,
implements the generic ``pattern_miner_protocol`` defined in the
``pattern_mining_protocols`` core library, and mines frequent sequential
patterns using recursive projected databases with both same-event and
next-event extensions.

Requires a dataset implementing ``sequence_dataset_protocol`` with
sequences represented as ordered lists of canonical sorted itemsets over
a declared item domain.

API documentation
-----------------

Open the
`../../apis/library_index.html#prefix_span <../../apis/library_index.html#prefix_span>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(prefix_span(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(prefix_span(tester)).

Features
--------

- **Projected Database Mining**: Mines sequential patterns by
  recursively projecting suffix databases.
- **Itemset and Sequence Extensions**: Supports both extending the last
  itemset and appending a new singleton itemset.
- **Canonical Sequences**: Validates that itemsets are sorted,
  duplicate-free, non-empty, and restricted to declared items.
- **Flexible Support Thresholds**: Supports minimum support specified
  either as a relative proportion or as an absolute count.
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

   prefix_span_pattern_miner(ItemDomain, Patterns, Options)

Where:

- ``ItemDomain``: Canonical sorted list of declared dataset items.
- ``Patterns``: List of ``sequence_pattern(Pattern, SupportCount)``
  terms ordered first by total item count and then lexicographically.
- ``Options``: Effective mining options used to mine the frequent
  sequential patterns.

References
----------

1. Pei, J., Han, J., Mortazavi-Asl, B., et al. (2001) - "PrefixSpan:
   Mining sequential patterns efficiently by prefix-projected pattern
   growth".

.. _library_eclat_pattern_miner:

``eclat_pattern_miner``
=======================

Eclat frequent itemset miner for transaction datasets. Normalizes
dataset transaction identifiers to internal ascending ordinals, builds
vertical tidsets for frequent singleton items, and recursively extends
them by lexicographic suffix joins and tidset intersections.

This library depends on the ``frequent_pattern_mining_protocols``
support library, implements the generic ``pattern_miner_protocol``
defined in the ``pattern_mining_protocols`` core library.

Requires a dataset implementing ``transaction_dataset_protocol`` with
transactions represented as canonical sorted lists of unique declared
items. External transaction identifiers are treated as opaque metadata
and normalized to internal ordinals before tidset construction.

API documentation
-----------------

Open the
`../../apis/library_index.html#eclat_pattern_miner <../../apis/library_index.html#eclat_pattern_miner>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(eclat_pattern_miner(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(eclat_pattern_miner(tester)).

Features
--------

- **Vertical Tidsets**: Represents each frequent extension by the sorted
  list of transaction identifiers containing it.
- **Transaction Ordinal Normalization**: Treats external transaction
  identifiers as opaque metadata and normalizes them to internal
  ascending ordinals before building vertical tidsets.
- **Depth-First Mining**: Extends frequent prefixes recursively using
  tidset intersections.
- **Canonical Transactions**: Validates that transactions are sorted,
  duplicate-free, and restricted to declared items.
- **Flexible Support Thresholds**: Supports relative minimum support and
  absolute minimum support count. When both are given, the
  absolute-count threshold takes precedence.
- **Model Export**: Mined pattern collections can be exported as
  predicate clauses or written to a file.

Options
-------

The ``mine/3`` predicate accepts the following options:

- ``minimum_support/1``: Relative minimum support threshold in the
  interval ``]0.0, 1.0]``. The default is ``0.5``.
- ``minimum_support_count/1``: Absolute minimum support count. When both
  support options are provided, this option takes precedence.
- ``maximum_pattern_length/1``: Maximum itemset length to mine. The
  default is ``1000``, which is effectively capped by the longest
  transaction in the dataset.
- ``minimum_pattern_length/1``: Minimum itemset length retained in the
  mined result. The default is ``1``.

Pattern miner representation
----------------------------

The mined pattern miner result is represented by a compound term with
the functor chosen by the implementation and arity 3. For example:

::

   eclat_pattern_miner(ItemDomain, Patterns, Options)

Where:

- ``ItemDomain``: Canonical sorted list of declared dataset items.
- ``Patterns``: List of ``itemset(Items, SupportCount)`` terms ordered
  first by pattern length and then lexicographically.
- ``Options``: Effective mining options used to mine the frequent
  itemsets.

References
----------

1. Zaki, M. J. (2000) - "Scalable algorithms for association mining".

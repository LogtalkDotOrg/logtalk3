.. _library_sequential_pattern_mining_protocols:

``sequential_pattern_mining_protocols``
=======================================

This library provides support entities for sequential pattern mining
algorithms. Ordered sequence datasets are represented as objects
implementing the ``sequence_dataset_protocol`` protocol. The generic
``pattern_miner_protocol`` protocol and the ``pattern_miner_common``
category used by concrete miners are loaded from the
``pattern_mining_protocols`` core library.

The ``sequential_pattern_mining_common`` category builds on that generic
core with sequential-specific helpers for dataset validation, support
count accumulation, and sequential pattern ordering/filtering.

This library also provides reusable sequence smoke-test datasets and a
small smoke-test suite.

API documentation
-----------------

Open the
`../../apis/library_index.html#sequential_pattern_mining_protocols <../../apis/library_index.html#sequential_pattern_mining_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(sequential_pattern_mining_protocols(loader)).

Testing
-------

To run the library smoke tests, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(sequential_pattern_mining_protocols(tester)).

Test datasets
-------------

The ``test_datasets`` directory includes the following sample sequence
datasets:

- ``clickstream_sequences.lgt``: A compact sequence dataset with
  repeated prefixes intended for sequential-pattern smoke tests.

- ``prefix_ladder_sequences.lgt``: A small ladder-shaped dataset of
  singleton events intended for exact baseline checks across sequential
  mining algorithms.

- ``same_event_vs_next_event_sequences.lgt``: A compact dataset intended
  to distinguish same-event extensions from next-event extensions.

- ``repeated_embedding_sequences.lgt``: A dataset where the same
  subsequence admits multiple embeddings inside a single sequence,
  intended for support-count semantics checks.

- ``border_threshold_sequences.lgt``: A compact dataset with patterns
  just above and below typical support thresholds, intended for pruning
  and threshold regression tests.

- ``closure_sequences.lgt``: A compact dataset intended for
  closed-pattern tests where some frequent patterns share the same
  support as one of their supersequences.

- ``dense_overlap_sequences.lgt``: A denser dataset with overlapping
  subsequences and mixed singleton and multi-item events, intended for
  overlap-heavy mining scenarios.

- ``branching_sequences.lgt``: A dataset with a common prefix and
  several competing branches, intended for candidate-generation and
  branching coverage.

The directory also includes invalid fixtures useful for validation and
error-handling tests:

- ``invalid_undeclared_item_sequences.lgt``: Uses an item not listed in
  the declared item domain.

- ``invalid_unsorted_itemset_sequences.lgt``: Uses an event with items
  not in canonical sorted order.

- ``invalid_duplicate_item_in_event_sequences.lgt``: Uses an event with
  a duplicate item.

- ``invalid_empty_event_sequences.lgt``: Uses an empty event inside a
  sequence.

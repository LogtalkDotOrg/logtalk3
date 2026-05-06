.. _library_pattern_mining_protocols:

``pattern_mining_protocols``
============================

This library provides the generic core entities used in the
implementation of machine learning pattern-finding algorithms.
Pattern-mining algorithm implementations typically import the
``pattern_miner_common`` category, which implements the
``pattern_miner_protocol`` protocol and provides shared defaults, option
handling, diagnostics accessors, and export helpers.

The family-specific dataset protocols and bundled smoke-test datasets
are provided by the ``frequent_pattern_mining_protocols`` and
``sequential_pattern_mining_protocols`` support libraries.

API documentation
-----------------

Open the
`../../apis/library_index.html#pattern_mining_protocols <../../apis/library_index.html#pattern_mining_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(pattern_mining_protocols(loader)).

Testing
-------

To run the library smoke tests, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(pattern_mining_protocols(tester)).

Common options
--------------

The ``pattern_miner_common`` category supports the following options
used by importing pattern miners to control support thresholds and
pattern length filtering:

- ``minimum_support(0.5)`` sets the relative minimum support threshold
  as a proportion in the interval ``]0.0, 1.0]``.

- ``minimum_support_count(N)`` sets the absolute minimum support count.
  If both support options are provided, this option takes precedence.

- ``maximum_pattern_length(1000)`` sets the maximum pattern length to
  mine. The effective value is capped by the longest transaction or
  sequence in the dataset.

- ``minimum_pattern_length(1)`` sets the minimum pattern length retained
  in the mined result.

The current ``apriori_pattern_miner``, ``eclat_pattern_miner``,
``fp_growth_pattern_miner``, and ``prefix_span_pattern_miner`` libraries
all use these shared defaults.

Diagnostics
-----------

The ``pattern_miner_protocol`` protocol also defines the
``diagnostics/2``, ``diagnostic/2``, and ``pattern_miner_options/2``
predicates. These expose representation-independent metadata about mined
results.

All pattern miners now provide at least the following generic
diagnostics terms:

- ``model(Model)``

- ``options(Options)``

- ``item_domain_size(Size)``

- ``pattern_count(Count)``

- ``pattern_length_histogram(Histogram)``

- ``support_range(MinimumSupport, MaximumSupport)``

Each miner also provides algorithm-specific diagnostics terms describing
its mining strategy and support representation, such as
candidate-generation style, projected-database growth, closure
filtering, or vertical support layout.

Related support libraries
-------------------------

- ``frequent_pattern_mining_protocols``: Provides the
  ``transaction_dataset_protocol`` protocol plus bundled transaction
  smoke-test datasets for frequent itemset miners.

- ``sequential_pattern_mining_protocols``: Provides the
  ``sequence_dataset_protocol`` protocol plus bundled sequence
  smoke-test datasets for sequential pattern miners.

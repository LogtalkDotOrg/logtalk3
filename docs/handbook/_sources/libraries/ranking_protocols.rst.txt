.. _library_ranking_protocols:

``ranking_protocols``
=====================

This library provides protocols used in the implementation of machine
learning ranking algorithms. Rankers are represented as objects
implementing the ``ranker_protocol`` protocol. Datasets are represented
as objects implementing either the ``pairwise_ranking_dataset_protocol``
protocol or the ``ranking_dataset_protocol`` protocol.

This library also provides reusable test datasets and smoke tests for
the shared ranking-family contracts.

Shared categories
-----------------

The library includes a small family of reusable categories intended to
be imported by ranking algorithm implementations:

- ``ranking_dataset_common`` — dataset collection, summaries, graph
  connectivity, connected-component analysis, and pairwise/grouped
  dataset correctness checks.
- ``ranker_common`` — representation-independent access to
  learned-ranker diagnostics plus reusable helpers for exporting learned
  rankers.

These categories are designed to keep ranking implementations compact
while keeping the shared protocol-facing behavior reusable.

Diagnostics
-----------

The ``ranker_common`` category provides shared accessor predicates such
as ``diagnostics/2``, ``diagnostic/2``, and ``ranker_options/2``,
together with the shared ``ranker_to_file/4`` export helper. These
predicates make it possible to inspect and export learned rankers
without depending on the exact term representation used by a particular
ranking algorithm implementation.

The detailed contents of the diagnostics data are ranking algorithm
implementation dependent. For example, one ranker may report convergence
status, iteration count, and dataset summaries, while another may report
a different set of metadata terms or only a subset of those details.
When using diagnostics in application code, rely on the shared access
predicates and the documentation of the specific ranking algorithm you
are using.

Export header format
--------------------

The shared ranker exporter in the ``ranker_common`` category writes a
header before the exported clauses in the following format:

::

   % exported ranker predicate: Functor/Arity
   % training dataset: Dataset
   % diagnostics: Diagnostics
   % Functor(Ranker)
   Functor(Ranker)

When exporting a serialized ranker term, using a noun such as
``ranker/1`` or ``model/1`` is recommended. The ``Ranker`` argument can
then be passed to the rank predicates.

API documentation
-----------------

Open the
`../../apis/library_index.html#ranking_protocols <../../apis/library_index.html#ranking_protocols>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ranking_protocols(loader)).

Testing
-------

To test this library predicates and datasets, load the ``tester.lgt``
file:

::

   | ?- logtalk_load(ranking_protocols(tester)).

Test datasets
-------------

Several sample datasets are included in the ``test_datasets`` directory:

- ``head_to_head.lgt`` — A compact pairwise-comparison dataset with four
  items and weighted preferences suitable for smoke testing
  deterministic ranking.

- ``search_results.lgt`` — A grouped ranking dataset with two query
  groups, three items per group, and non-negative integer relevance
  judgments.

- ``malformed_pairwise.lgt`` — A negative fixture where a preference
  mentions an undeclared item.

- ``malformed_duplicate_items.lgt`` — A negative pairwise fixture where
  an item is declared more than once.

- ``malformed_self_preference.lgt`` — A negative pairwise fixture where
  an item is preferred over itself.

- ``malformed_non_positive_weight.lgt`` — A negative pairwise fixture
  where a preference weight is not positive.

- ``disconnected_pairwise.lgt`` — A pairwise fixture with more than one
  connected component, useful for testing algorithms that require
  identifiable global scores.

- ``cyclic_pairwise.lgt`` — A connected pairwise fixture with a
  preference cycle, useful for smoke testing algorithms on
  non-transitive data.

- ``malformed_grouped.lgt`` — A negative fixture where a grouped
  relevance value is not a non-negative integer.

- ``sparse_preferences.lgt`` — A sparse pairwise dataset with an
  isolated item, useful for testing dataset summaries and
  disconnected-graph detection.

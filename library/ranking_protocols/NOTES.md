________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`ranking_protocols`
===================

This library provides protocols used in the implementation of machine
learning ranking algorithms. Rankers are represented as objects
implementing the `ranker_protocol` protocol. Datasets are represented as
objects implementing the `pairwise_ranking_dataset_protocol`,
`pairwise_measurement_dataset_protocol`,
`temporal_pairwise_ranking_dataset_protocol`, or the
`ranking_dataset_protocol` protocol.

This library also provides reusable test datasets and smoke tests for the
shared ranking-family contracts.

Protocol requirements
---------------------

The dataset protocols defined by this library impose the following semantic
and validity expectations:

- `pairwise_ranking_dataset_protocol` datasets should declare each item once,
  use only declared items in preferences, assign positive weights to
  preferences between distinct items, and may still need stronger conditions
  such as a strongly connected directed win graph for algorithms like
  Bradley-Terry that require finite maximum-likelihood estimates.
- `pairwise_measurement_dataset_protocol` datasets should declare each item
  once, use only declared items in measurements, assign numeric measurement
  values, and assign positive weights to measurements between distinct items.
  Each `measurement(Item1, Item2, Value, Weight)` fact denotes a weighted
  signed scalar observation on the oriented edge `Item1 -> Item2`, where
  positive values favor `Item1`, negative values favor `Item2`, and zero
  denotes a neutral observation.
- `temporal_pairwise_ranking_dataset_protocol` datasets should declare each
  item and period once, use only declared items and periods in games, keep
  game participants distinct, and restrict scores to the set `{0.0, 0.5,
  1.0}`. A fact `game(Period, Item1, Item2, Score)` records the observed
  result for `Item1` against `Item2`, with the score for `Item2` implicitly
  equal to `1.0 - Score`.

Shared categories
-----------------

The library includes a small family of reusable categories intended to be
imported by ranking algorithm implementations:

- `ranking_dataset_common` — dataset collection, summaries, graph
  connectivity, connected-component analysis, and pairwise/grouped dataset
  correctness checks, including pairwise measurement helpers, temporal
  pairwise rating-period helpers, and grouped tie-block extraction helpers
  for algorithms that consume tied rankings directly.
- `glicko2_common` — shared internal Glicko-2 numeric helpers for scale
  conversions, per-period player updates, and volatility root solving used
  by both the batch and periodic Glicko-2 rankers.
- `condorcet_victory_common` — shared direct-victory preprocessing helpers
  for Condorcet-family rankers that derive dense directed victory strengths
  from aggregated pairwise matchups under the `victory_strength(...)`
  option semantics.
- `ranker_common` — representation-independent access to learned-ranker
  diagnostics plus reusable helpers for exporting learned rankers.
- `grouped_strength_ranker_common` — reusable positive-strength,
  strong-connectivity, and iterative-update helpers for grouped ranking
  models that estimate one latent strength parameter per item.

These categories are designed to keep ranking implementations compact while
keeping the shared protocol-facing behavior reusable.

Diagnostics
-----------

The `ranker_common` category provides shared accessor predicates such as
`diagnostics/2`, `diagnostic/2`, and `ranker_options/2`, together with the
shared `export_to_file/4` export helper. These predicates make it possible to
inspect and export learned rankers without depending on the exact term
representation used by a particular ranking algorithm implementation.

The detailed contents of the diagnostics data are ranking algorithm
implementation dependent. For example, one ranker may report convergence
status, iteration count, and dataset summaries, while another may report a
different set of metadata terms or only a subset of those details. When using
diagnostics in application code, rely on the shared access predicates and the
documentation of the specific ranking algorithm you are using.

Export header format
--------------------

The shared ranker exporter in the `ranker_common` category writes a header
before the exported clauses in the following format:

    % exported ranker predicate: Functor/Arity
    % training dataset: Dataset
    % diagnostics: Diagnostics
    % Functor(Ranker)
    Functor(Ranker)

When exporting a serialized ranker term, using a noun such as `ranker/1`
or `model/1` is recommended. The `Ranker` argument can then be passed to
the rank predicates.

API documentation
-----------------

Open the [../../apis/library_index.html#ranking_protocols](../../apis/library_index.html#ranking_protocols)
link in a web browser.

Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(ranking_protocols(loader)).


Testing
-------

To test this library predicates and datasets, load the `tester.lgt` file:

	| ?- logtalk_load(ranking_protocols(tester)).


Test datasets
-------------

Several sample datasets are included in the `test_datasets` directory:

- `head_to_head.lgt` — A compact pairwise-comparison dataset with four items
  and weighted preferences suitable for smoke testing deterministic ranking.

- `search_results.lgt` — A grouped ranking dataset with two query groups,
  three items per group, and non-negative integer relevance judgments.

- `malformed_pairwise.lgt` — A negative fixture where a preference mentions an
  undeclared item.

- `malformed_duplicate_items.lgt` — A negative pairwise fixture where an item is
  declared more than once.

- `malformed_self_preference.lgt` — A negative pairwise fixture where an item is
  preferred over itself.

- `malformed_non_positive_weight.lgt` — A negative pairwise fixture where a
  preference weight is not positive.

- `disconnected_pairwise.lgt` — A pairwise fixture with more than one connected
  component, useful for testing algorithms that require identifiable global
  scores.

- `cyclic_pairwise.lgt` — A connected pairwise fixture with a preference cycle,
  useful for smoke testing algorithms on non-transitive data.

- `condorcet_divergence_pairwise.lgt` — A compact connected pairwise fixture
  where the current `schulze_ranker` and `ranked_pairs` implementations produce
  different rankings, useful for cross-method regression tests.

- `malformed_grouped.lgt` — A negative fixture where a grouped relevance value
  is not a non-negative integer.

- `sparse_preferences.lgt` — A sparse pairwise dataset with an isolated item,
  useful for testing dataset summaries and disconnected-graph detection.

- `two_item_measurements.lgt` — A compact pairwise measurement dataset with
  two items and one signed measurement, useful for smoke testing exact
  two-item least-squares fits.

- `regular_measurements.lgt` — A compact connected pairwise measurement
  dataset whose measurements are perfectly explained by a zero-sum score
  potential, useful for regression tests with zero residuals.

- `cyclic_measurements.lgt` — A compact connected pairwise measurement dataset
  with cyclic inconsistency, useful for smoke testing zero global scores with
  non-zero residuals.

- `disconnected_measurements.lgt` — A pairwise measurement fixture with more
  than one connected component, useful for testing identifiable-score checks.

- `malformed_measurement_unknown_item.lgt` — A negative measurement fixture
  where a measurement mentions an undeclared item.

- `malformed_measurement_duplicate_items.lgt` — A negative measurement fixture
  where an item is declared more than once.

- `malformed_measurement_self.lgt` — A negative measurement fixture where an
  item is measured against itself.

- `malformed_measurement_non_numeric.lgt` — A negative measurement fixture
  where a measurement value is not numeric.

- `malformed_measurement_non_positive_weight.lgt` — A negative measurement
  fixture where a measurement weight is not positive.

- `temporal_two_period_chain.lgt` — A compact temporal pairwise dataset with
  two rating periods and three players, useful for smoke testing periodic
  rating carryover.

- `temporal_draws.lgt` — A compact temporal pairwise dataset with a draw,
  useful for testing score handling in temporal game results.

- `temporal_idle_periods.lgt` — A temporal pairwise dataset with an empty
  period between two played periods, useful for testing inactivity handling.

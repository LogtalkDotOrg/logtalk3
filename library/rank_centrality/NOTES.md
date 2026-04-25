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


`rank_centrality`
=================

Rank Centrality pairwise preference ranker.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preferences, using it to order candidate items, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#rank_centrality](../../apis/library_index.html#rank_centrality)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(rank_centrality(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(rank_centrality(tester)).


Features
--------

- **Pairwise Preference Learning**: Learns one stationary probability score
  per item from weighted head-to-head outcomes.
- **Original Rank Centrality Transition Rule**: Builds the Markov chain from
  empirical pairwise win probabilities scaled by the maximum comparison
  degree, then estimates the stationary distribution by deterministic power
  iteration.
- **Deterministic Ranking**: Orders candidate items by learned stationary
  probability with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items,
  self-preferences, non-positive weights, disconnected undirected comparison
  graphs, and pairwise datasets whose directed transition graph is not
  strongly connected.
- **Training Diagnostics**: Learned rankers include convergence, iteration,
  maximum-degree, and dataset summary metadata that can be accessed using the
  `diagnostics/2` predicate.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Sparse Transition Processing**: Training aggregates observed pairwise
  comparisons into sparse incoming transition lists so each power-iteration
  step scales with observed matchups instead of a dense item cross-product.


Dataset requirements
--------------------

This implementation requires more than undirected connectedness. In order to
guarantee a unique stationary distribution, the directed transition graph
induced by the aggregated pairwise outcomes must be strongly connected.
Datasets that create one-way dominance sinks or other disconnected directed
transition components are rejected instead of producing ambiguous stationary
scores.


Usage
-----

### Learning a ranker

	% Learn from a pairwise ranking dataset object
	| ?- rank_centrality::learn(my_dataset, Ranker).
	...

	% Learn with custom iteration and convergence options
	| ?- rank_centrality::learn(my_dataset, Ranker, [maximum_iterations(500), tolerance(1.0e-9)]).
	...

### Inspecting diagnostics

	% Inspect convergence and dataset summary metadata
	| ?- rank_centrality::learn(my_dataset, Ranker),
	     rank_centrality::diagnostics(Ranker, Diagnostics).
	Diagnostics = [...]
	...


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

	[
	    model(rank_centrality),
	    options(Options),
	    convergence(Status),
	    iterations(Iterations),
	    final_delta(FinalDelta),
	    maximum_degree(MaximumDegree),
	    dataset_summary(DatasetSummary)
	]

Where:

- `model(rank_centrality)` identifies the learning algorithm that produced
  the ranker.
- `options(Options)` stores the effective learning options after merging the
  user options with the library defaults.
- `convergence(Status)` records the training stop condition. The current
  values are `converged` and `maximum_iterations_exhausted`.
- `iterations(Iterations)` stores the number of power-iteration steps that
  were executed.
- `final_delta(FinalDelta)` stores the maximum absolute score update in the
  last iteration.
- `maximum_degree(MaximumDegree)` stores the maximum number of observed
  opponents for any single item in the training dataset.
- `dataset_summary(DatasetSummary)` stores a summary list describing the
  validated training dataset.

The current `dataset_summary/1` payload has the form:

	[
	    items(NumberOfItems),
	    preferences(NumberOfPreferences),
	    connected_components(NumberOfComponents),
	    isolated_items(IsolatedItems)
	]

Use the `ranking_protocols` `diagnostic/2` and `ranker_options/2` helper
predicates when you only need a single metadata term or the effective
options.

### Ranking candidate items

	% Rank a candidate set from most preferred to least preferred
	| ?- rank_centrality::learn(my_dataset, Ranker),
	     rank_centrality::rank(Ranker, [item_a, item_b, item_c], Ranking).
	Ranking = [...]
	...

Candidate lists must be proper lists of unique, ground items declared by the
training dataset. Invalid ranker terms, duplicate candidates, and candidates
containing variables are rejected with errors instead of being silently
accepted.

### Exporting the ranker

Learned rankers can be exported as a list of clauses or to a file for later
use.

	% Export as predicate clauses
	| ?- rank_centrality::learn(my_dataset, Ranker),
	     rank_centrality::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
	Clauses = [my_ranker(rank_centrality_ranker(...))]
	...

	% Export to a file
	| ?- rank_centrality::learn(my_dataset, Ranker),
	     rank_centrality::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
	...


Options
-------

The following options can be passed to the `learn/3` predicate:

- `maximum_iterations(MaximumIterations)`: Positive integer iteration bound.
- `tolerance(Tolerance)`: Positive convergence tolerance.


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

	rank_centrality_ranker(Items, Scores, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of `Item-Score` pairs.
- `Diagnostics`: List of metadata terms, including the effective options,
  convergence status, iteration count, final update delta, maximum degree,
  and dataset summary.

The `Scores` payload is expected to contain positive stationary probability
values summing to `1.0`. The ranker validation logic enforces this invariant
when consuming serialized or exported ranker terms so that malformed payloads
are rejected instead of silently accepted.

When exported using `export_to_clauses/4` or `export_to_file/4`, this ranker
term is serialized directly as the single argument of the generated predicate
clause so that the exported model can be loaded and reused as-is.


References
----------

1. Negahban, S., Oh, S., and Shah, D. (2012). Rank Centrality: Ranking from pairwise comparisons. *Operations Research*, 65(1), 266-287.

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


`bradley_terry`
===============

Bradley-Terry pairwise preference ranker. Uses a deterministic
minorization-maximization update to estimate one relative strength
parameter per item from weighted pairwise wins and losses.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preferences, using it to order candidate items, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples. The training dataset
must declare each ranked item once, enumerate positive-weight pairwise
preferences between distinct declared items, induce a connected undirected
comparison graph, and induce a strongly connected directed win graph so that
a finite Bradley-Terry maximum-likelihood estimate exists.


API documentation
-----------------

Open the [../../apis/library_index.html#bradley_terry](../../apis/library_index.html#bradley_terry)
link in a web browser.

Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(bradley_terry(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(bradley_terry(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(bradley_terry(tester_performance)).


Features
--------

- **Pairwise Preference Learning**: Learns relative item strengths from
  weighted head-to-head outcomes.
- **Original MM Fidelity**: Rejects datasets without a finite Bradley-Terry
  maximum-likelihood estimate instead of masking them with implicit strength
  flooring or other hidden regularization.
- **Deterministic Ranking**: Orders candidate items by learned strength with
  deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items,
  self-preferences, non-positive weights, disconnected comparison graphs,
  and pairwise datasets that do not admit a finite Bradley-Terry maximum-
  likelihood estimate.
- **Training Diagnostics**: Learned rankers include convergence and dataset
  summary metadata that can be accessed using the `diagnostics/2` predicate.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Sparse Comparison Processing**: Training aggregates weighted comparisons
  into sparse adjacency lists so iteration cost scales with observed pairwise
  comparisons instead of the full dense item cross-product.


Dataset requirements
--------------------

This implementation requires more than undirected connectedness. In order to
admit a finite Bradley-Terry maximum-likelihood estimate, the directed win
graph induced by the preferences must be strongly connected. Datasets that
leave one or more items isolated, split the undirected comparison graph into
multiple components, or create dominance partitions with no directed path back
to stronger items are rejected instead of producing degenerate strengths.

For a related MAP formulation that keeps the same pairwise-preference setting
but uses an explicit Gamma prior to admit connected datasets whose directed
win graph is not strongly connected, see the
`regularized_bradley_terry` library.


Usage
-----

### Learning a ranker

    % Learn from a pairwise ranking dataset object
    | ?- bradley_terry::learn(my_dataset, Ranker).
    ...

    % Learn with custom iteration and convergence options
    | ?- bradley_terry::learn(my_dataset, Ranker, [maximum_iterations(500), tolerance(1.0e-7)]).
    ...

### Inspecting diagnostics

    % Inspect convergence and dataset summary metadata
    | ?- bradley_terry::learn(my_dataset, Ranker),
         bradley_terry::diagnostics(Ranker, Diagnostics).
    Diagnostics = [...]
    ...


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

    [
        model(bradley_terry),
        options(Options),
        convergence(Status),
        iterations(Iterations),
        final_delta(FinalDelta),
        dataset_summary(DatasetSummary)
    ]

Where:

- `model(bradley_terry)` identifies the learning algorithm that produced the
  ranker.
- `options(Options)` stores the effective learning options after merging the
  user options with the library defaults.
- `convergence(Status)` records the training stop condition. The current
  values are `converged` and `maximum_iterations_exhausted`.
- `iterations(Iterations)` stores the number of update iterations that were
  executed.
- `final_delta(FinalDelta)` stores the maximum absolute strength update in the
  last iteration.
- `dataset_summary(DatasetSummary)` stores a summary list describing the
  validated training dataset.

The current `dataset_summary/1` payload has the form:

    [
        items(NumberOfItems),
        preferences(NumberOfPreferences),
        connected_components(NumberOfComponents),
        isolated_items(IsolatedItems)
    ]

Where `IsolatedItems` is the list of declared items that have no comparisons.
For valid Bradley-Terry training datasets this list is expected to be empty,
because disconnected datasets are rejected.

For example, learning from the `head_to_head` test dataset currently returns
diagnostics with the structure:

    [
        model(bradley_terry),
        options([maximum_iterations(5000), tolerance(1.0e-6)]),
        convergence(converged),
        iterations(...),
        final_delta(...),
        dataset_summary([
            items(4),
            preferences(6),
            connected_components(1),
            isolated_items([])
        ])
    ]

Use the `ranking_protocols` `diagnostic/2` and `ranker_options/2` helper
predicates when you only need a single metadata term or the effective options.

### Ranking candidate items

    % Rank a candidate set from most preferred to least preferred
    | ?- bradley_terry::learn(my_dataset, Ranker),
         bradley_terry::rank(Ranker, [item_a, item_b, item_c], Ranking).
    Ranking = [...]
    ...

Candidate lists must be proper lists of unique, ground items declared by the
training dataset. Invalid ranker terms, duplicate candidates, and candidates
containing variables are rejected with errors instead of being silently
accepted.

### Exporting the ranker

Learned rankers can be exported as a list of clauses or to a file for later use.

    % Export as predicate clauses
    | ?- bradley_terry::learn(my_dataset, Ranker),
         bradley_terry::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
    Clauses = [my_ranker(bt_ranker(...))]
    ...

    % Export to a file
    | ?- bradley_terry::learn(my_dataset, Ranker),
         bradley_terry::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
    ...


Options
-------

The following options can be passed to the `learn/3` predicate:

- `maximum_iterations(MaximumIterations)`: Positive integer iteration bound.
- `tolerance(Tolerance)`: Positive convergence tolerance.


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

    bt_ranker(Items, Strengths, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of `Item-Strength` pairs.
- `Diagnostics`: List of metadata terms, including the effective options,
  convergence status, iteration count, final update delta, and dataset summary.

When exported using `export_to_clauses/4` or `export_to_file/4`, this
ranker term is serialized directly as the single argument of the generated
predicate clause so that the exported model can be loaded and reused as-is.


References
----------

1. Bradley, R. A. and Terry, M. E. (1952). Rank analysis of incomplete block designs: I. The method of paired comparisons. *Biometrika*, 39(3/4), 324-345.
2. Hunter, D. R. (2004). MM algorithms for generalized Bradley-Terry models. *The Annals of Statistics*, 32(1), 384-406.

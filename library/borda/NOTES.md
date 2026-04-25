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


`borda`
========

Borda grouped-ranking ranker.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from grouped relevance judgments, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`ranking_dataset_protocol` protocol from the `ranking_protocols` library.
See the `test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#borda](../../apis/library_index.html#borda)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(borda(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(borda(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(borda(tester_performance)).


Features
--------

- **Grouped Relevance Learning**: Learns one deterministic item score from
	grouped ranking or relevance-judgment datasets.
- **Portable Borda Scoring**: Computes scores using only non-negative integer
	grouped relevance judgments and standard Logtalk library predicates.
- **Deterministic Ranking**: Orders candidate items by learned score with
	deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate groups, duplicate items
	within a group, undeclared groups or items in relevance judgments, and
	non-integer or negative relevance values.
- **Explicit Semantics Options**: The `learn/3` predicate exposes the current
	tie and missing-relevance policies using the `tie_scoring/1` and
	`missing_relevance/1` options.
- **Benchmark Coverage**: Includes a dedicated performance suite for a large
	grouped dataset benchmark.
- **Training Diagnostics**: Learned rankers include dataset summary metadata
	that can be accessed using the `diagnostics/2` predicate.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Shared Ranking Infrastructure**: Uses the common `ranking_protocols`
	helper predicates for option processing, dataset validation, diagnostics,
	export, and candidate ranking.


Scoring semantics
-----------------

This implementation uses a grouped Borda count variant over the declared
items of each group. With the default `tie_scoring(standard)` option, an
item receives one point for every same-group item with strictly lower
relevance. Tied items therefore receive the same per-group contribution,
because equal relevance values do not add or subtract points.

With the `tie_scoring(fractional)` option, each tied relevance class receives
the average of the minimum and maximum per-group Borda points available to
that tie block. For example, when two items tie above a single lower-ranked
item, both tied items receive `1.5` points instead of the `1` point assigned
by the default policy.

Missing relevance facts are treated as relevance `0` only for items that are
declared in the group when using the default `missing_relevance(zero)`
option. This allows grouped datasets to omit explicit zero judgments while
keeping the score computation deterministic. Use `missing_relevance(error)`
to reject grouped datasets that omit a declared item relevance.


Usage
-----

### Learning a ranker

		% Learn from a grouped ranking dataset object
		| ?- borda::learn(my_dataset, Ranker).
		...

		% Learn with an explicit empty options list
		| ?- borda::learn(my_dataset, Ranker, []).
		...

		% Learn while requiring every declared group item to have a relevance fact
		| ?- borda::learn(my_dataset, Ranker, [missing_relevance(error)]).
		...

		% Learn using fractional tie scoring for tied relevance levels
		| ?- borda::learn(my_dataset, Ranker, [tie_scoring(fractional)]).
		...

The current implementation accepts the `missing_relevance/1` and
`tie_scoring/1` options described below.

### Inspecting diagnostics

		% Inspect model and dataset summary metadata
		| ?- borda::learn(my_dataset, Ranker),
		     borda::diagnostics(Ranker, Diagnostics).
		Diagnostics = [...]
		...

### Ranking candidate items

		% Rank a candidate set from most preferred to least preferred
		| ?- borda::learn(my_dataset, Ranker),
		     borda::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
		| ?- borda::learn(my_dataset, Ranker),
		     borda::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
		Clauses = [my_ranker(borda_ranker(...))]
		...

		% Export to a file
		| ?- borda::learn(my_dataset, Ranker),
		     borda::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
		...


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

		[
			model(borda),
			options(Options),
			dataset_summary(DatasetSummary)
		]

Where:

- `model(borda)` identifies the learning algorithm that produced the ranker.
- `options(Options)` stores the effective learning options after merging the
	user options with the library defaults.
- `dataset_summary(DatasetSummary)` stores a summary list describing the
	validated training dataset.

The current `dataset_summary/1` payload has the form:

		[
			groups(NumberOfGroups),
			items(NumberOfItems),
			relevance_judgments(NumberOfJudgments)
		]

Use the `ranking_protocols` `diagnostic/2` and `ranker_options/2` helper
predicates when you only need a single metadata term or the effective options.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `missing_relevance(Policy)`: Controls how declared group items without an
	explicit relevance fact are handled. The supported values are `zero`
	(default) and `error`.
- `tie_scoring(Policy)`: Controls the grouped Borda tie semantics. The current
	implementation supports `standard` (minimum tied-block score) and
	`fractional` (average tied-block score).


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

		borda_ranker(Items, Scores, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of `Item-Score` pairs.
- `Diagnostics`: List of metadata terms, including the effective options and
	dataset summary.

When exported using `export_to_clauses/4` or `export_to_file/4`, this ranker
term is serialized directly as the single argument of the generated predicate
clause so that the exported model can be loaded and reused as-is.


References
----------

1. de Borda, J.-C. (1781). Mémoire sur les élections au scrutin. *Histoire de
	 l'Académie Royale des Sciences*.

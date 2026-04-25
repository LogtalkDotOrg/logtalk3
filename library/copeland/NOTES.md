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


`copeland`
===========

Copeland pairwise preference ranker.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preference judgments, using it to order candidate items, and
exporting it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#copeland](../../apis/library_index.html#copeland)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(copeland(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(copeland(tester)).


Features
--------

- **Pairwise Preference Learning**: Learns one deterministic score per item
	from pairwise preference datasets.
- **Portable Copeland Scoring**: Computes Copeland scores from aggregated
	head-to-head outcomes using only standard Logtalk library predicates.
- **Integer Score Semantics**: Learned scores are restricted to integers,
	because each observed aggregated matchup contributes exactly `+1`, `-1`, or
	`0` to an item's total score.
- **Deterministic Ranking**: Orders candidate items by learned score with
	deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items in
	preferences, self-preferences, non-positive weights, and disconnected
	comparison graphs.
- **Training Diagnostics**: Learned rankers include dataset summary metadata
	that can be accessed using the `diagnostics/2` predicate.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Shared Ranking Infrastructure**: Uses the common `ranking_protocols`
	helper predicates for option processing, dataset validation, diagnostics,
	export, and candidate ranking.


Scoring semantics
-----------------

This implementation uses a Copeland score defined over aggregated observed
matchups. For each unordered pair of observed opponents, the preference data
is aggregated into total wins for the left and right item. The item with the
higher aggregated total receives `+1` for that matchup, the item with the
lower total receives `-1`, and both receive `0` when the aggregated totals
tie.

Only observed opponent pairs contribute to the learned scores. Unobserved
pairs are ignored rather than treated as implicit ties. This makes the
implementation a sparse-data Copeland variant suitable for incomplete
pairwise datasets.

Because each observed matchup contributes only `+1`, `-1`, or `0`, every
learned Copeland score is an integer. The ranker validation logic enforces
this invariant when consuming serialized or exported ranker terms so that
malformed non-integer score payloads are rejected instead of silently
accepted.


Usage
-----

### Learning a ranker

		% Learn from a pairwise ranking dataset object
		| ?- copeland::learn(my_dataset, Ranker).
		...

		% Learn with an explicit empty options list
		| ?- copeland::learn(my_dataset, Ranker, []).
		...

The current implementation accepts only the empty options list `[]`.
Any non-empty options list is rejected.

### Inspecting diagnostics

		% Inspect model and dataset summary metadata
		| ?- copeland::learn(my_dataset, Ranker),
		     copeland::diagnostics(Ranker, Diagnostics).
		Diagnostics = [...]
		...

### Ranking candidate items

		% Rank a candidate set from most preferred to least preferred
		| ?- copeland::learn(my_dataset, Ranker),
		     copeland::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
		| ?- copeland::learn(my_dataset, Ranker),
		     copeland::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
		Clauses = [my_ranker(copeland_ranker(...))]
		...

		% Export to a file
		| ?- copeland::learn(my_dataset, Ranker),
		     copeland::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
		...


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

		[
			model(copeland),
			options(Options),
			dataset_summary(DatasetSummary)
		]

Where:

- `model(copeland)` identifies the learning algorithm that produced the
	ranker.
- `options(Options)` stores the effective learning options after merging the
	user options with the library defaults.
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
predicates when you only need a single metadata term or the effective options.


Options
-------

The current `learn/3` implementation does not define any user options beyond
the default empty list. Non-empty options lists are rejected.


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

		copeland_ranker(Items, Scores, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of `Item-Score` pairs.
- `Diagnostics`: List of metadata terms, including the effective options and
	dataset summary.

The `Scores` payload is expected to contain integer Copeland scores only.
This restriction is not just an implementation preference but a direct
consequence of the scoring semantics: each observed aggregated matchup changes
an item's score by one of the three integer values `+1`, `-1`, or `0`.

When exported using `export_to_clauses/4` or `export_to_file/4`, this ranker
term is serialized directly as the single argument of the generated predicate
clause so that the exported model can be loaded and reused as-is.


References
----------

1. Copeland, A. H. (1951). A reasonable social welfare function. *Seminar on
	 Mathematics in the Social Sciences, University of Michigan*.

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


`ranked_pairs`
===============

Ranked Pairs pairwise preference ranker. It builds the direct pairwise
victory graph from aggregated matchups, considers victories in
descending direct-victory strength order, and locks each victory unless
it would create a directed cycle in the accepted lock graph.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preferences, using it to order candidate items, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples. The current
implementation requires a well-formed connected pairwise dataset so that
all ranked items remain part of a single comparison graph.


API documentation
-----------------

Open the [../../apis/library_index.html#ranked_pairs](../../apis/library_index.html#ranked_pairs)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(ranked_pairs(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(ranked_pairs(tester)).


Features
--------

- **Pairwise Preference Learning**: Learns one deterministic score per item
  from aggregated pairwise outcomes.
- **Ranked Pairs Locking**: Processes direct victories in descending
  strength order and accepts each victory unless it would create a directed
  cycle in the current lock graph.
- **Configurable Direct Victory Semantics**: Supports both winning-votes
  and victory-margins victory-strength modes before locking.
- **Configurable Equal-Strength Tie Breaking**: Supports both standard
	term-order and dataset declaration-order resolution when direct
	victories have the same strength.
- **Locked-Pair Access**: Exposes the accepted lock graph in lock order
  using the `locked_pairs/2` predicate.
- **Deterministic Ranking**: Orders candidate items by the number of
  opponents reachable in the final locked relation with deterministic
  tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items,
  self-preferences, non-positive weights, and disconnected comparison
  graphs.
- **Training Diagnostics**: Learned rankers include dataset summary
  metadata, the effective victory-strength mode, and the accepted lock
  graph.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Shared Condorcet Infrastructure**: Reuses the shared direct-victory
  preprocessing helpers from the `ranking_protocols` library.


Usage
-----

### Learning a ranker

	% Learn from a pairwise ranking dataset object
	| ?- ranked_pairs::learn(my_dataset, Ranker).
	...

	% Learn with custom direct-victory semantics
	| ?- ranked_pairs::learn(my_dataset, Ranker, [victory_strength(margins)]).
	...

	% Learn with custom equal-strength tie breaking
	| ?- ranked_pairs::learn(my_dataset, Ranker, [tie_breaking(declaration_order)]).
	...

### Inspecting diagnostics

	% Inspect the effective options and accepted locks
	| ?- ranked_pairs::learn(my_dataset, Ranker),
	     ranked_pairs::diagnostics(Ranker, Diagnostics).
	Diagnostics = [...]
	...

### Ranking candidate items

	% Rank a candidate set from most preferred to least preferred
	| ?- ranked_pairs::learn(my_dataset, Ranker),
	     ranked_pairs::rank(Ranker, [item_a, item_b, item_c], Ranking).
	Ranking = [...]
	...

Candidate lists must be proper lists of unique, ground items declared by the
training dataset. Invalid ranker terms, duplicate candidates, and candidates
containing variables are rejected with errors instead of being silently
accepted.

### Inspecting locked pairs

	% Inspect the accepted lock graph in lock order
	| ?- ranked_pairs::learn(my_dataset, Ranker),
	     ranked_pairs::locked_pairs(Ranker, LockedPairs).
	LockedPairs = [...]
	...

The `locked_pairs/2` predicate returns an ordered list of
`lock(Winner, Loser, Strength)` terms representing the victories that were
accepted during the Ranked Pairs locking phase.

### Exporting the ranker

Learned rankers can be exported as a list of clauses or to a file for later
use.

	% Export as predicate clauses
	| ?- ranked_pairs::learn(my_dataset, Ranker),
	     ranked_pairs::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
	Clauses = [my_ranker(ranked_pairs_ranker(...))]
	...

	% Export to a file
	| ?- ranked_pairs::learn(my_dataset, Ranker),
	     ranked_pairs::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
	...


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

	[
		model(ranked_pairs),
		options(Options),
		locked_pairs(LockedPairs),
		dataset_summary(DatasetSummary)
	]

Where:

- `model(ranked_pairs)` identifies the learning algorithm that produced the
  ranker.
- `options(Options)` stores the effective learning options after merging the
  user options with the library defaults.
- `locked_pairs(LockedPairs)` stores the accepted lock graph in the order in
  which victories were locked.
- `dataset_summary(DatasetSummary)` stores a summary list describing the
  validated training dataset.

Use the `ranking_protocols` `diagnostic/2` and `ranker_options/2` helper
predicates when you only need a single metadata term or the effective options.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `victory_strength(winning_votes)`: Use the winning side's aggregated vote
  total as the direct victory strength.
- `victory_strength(margins)`: Use the victory margin as the direct victory
  strength.
- `tie_breaking(term_order)`: Break equal-strength direct-victory ties using
	the standard term order of the winner and loser item identifiers.
- `tie_breaking(declaration_order)`: Break equal-strength direct-victory ties
	using the training item declaration order preserved by the pairwise dataset
	helpers.

The defaults are `victory_strength(winning_votes)` and
`tie_breaking(term_order)`.


Scoring semantics
-----------------

The learned `scores/2` values count how many opponents are reachable from each
item in the final locked relation. These scores are therefore integers in the
range from `0` to `N-1`, where `N` is the number of learned items.

This choice keeps the `rank/3` behavior aligned with the locked graph that the
algorithm actually accepts: a higher score means that the item precedes more
opponents after all accepted locks are closed transitively.

The effective scores can therefore change when `tie_breaking/1` changes the
order in which equal-strength victories are considered for locking.


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

	ranked_pairs_ranker(Items, Scores, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of integer `Item-Score` pairs.
- `Diagnostics`: List of metadata terms, including the effective options,
  the accepted locked pairs, and the dataset summary.

When exported using `export_to_clauses/4` or `export_to_file/4`, this
ranker term is serialized directly as the single argument of the generated
predicate clause so that the exported model can be loaded and reused as-is.


See also
--------

For the strongest-path Condorcet-family alternative sharing the same
`victory_strength(...)` option, see the `schulze_ranker` library.

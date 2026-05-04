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


`schulze`
==========

Schulze pairwise preference ranker.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preferences, using it to order candidate items, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#schulze](../../apis/library_index.html#schulze)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(schulze(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(schulze(tester)).


Features
--------

- **Pairwise Preference Learning**: Learns one deterministic score per item
	from aggregated pairwise outcomes.
- **Schulze Strongest Paths**: Computes the final ranking relation using the
	Schulze strongest-path dynamic program over aggregated head-to-head data.
- **Configurable Direct Edge Semantics**: Supports both winning-votes and
	victory-margins victory-strength modes.
- **Strongest-Path Access**: Exposes the labeled strongest-path relation for
	ordered item pairs using the `strongest_paths/2` predicate.
- **Deterministic Ranking**: Orders candidate items by final Schulze relation
	win counts with deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items,
	self-preferences, non-positive weights, and disconnected comparison graphs.
- **Training Diagnostics**: Learned rankers include dataset summary metadata
	and the effective victory-strength mode.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `victory_strength(winning_votes)`: Use the winning side's aggregated vote
	total as the direct edge strength.
- `victory_strength(margins)`: Use the victory margin as the direct edge
	strength.

The default is `victory_strength(winning_votes)`.


Strongest paths
---------------

The `strongest_paths/2` predicate returns the learned strongest-path matrix as
an ordered list of `path(Item1, Item2, Strength)` terms for all ordered pairs
of distinct learned items.

This predicate complements `scores/2` and `rank/3` without changing their
semantics. The `scores/2` predicate still returns the number of opponents that
each item beats in the final Schulze relation, while `strongest_paths/2`
exposes the underlying pairwise path strengths used to derive that relation.


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

	[
		model(schulze),
		options(Options),
		strongest_paths(StrongestPaths),
		dataset_summary(DatasetSummary)
	]


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

	schulze_ranker(Items, Scores, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of `Item-Score` pairs.
- `Diagnostics`: List of metadata terms, including the effective
	`victory_strength/1` option, the labeled strongest paths, and the dataset
	summary.

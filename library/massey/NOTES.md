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


`massey`
==========

Massey pairwise preference ranker.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preferences, using it to order candidate items, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#massey](../../apis/library_index.html#massey)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

    | ?- logtalk_load(massey(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(massey(tester)).


Features
--------

- **Pairwise Preference Learning**: Learns one deterministic rating per item
  from aggregated pairwise outcomes.
- **Massey Matrix Fidelity**: Solves the standard Massey linear system with
  diagonal entries `games_i`, off-diagonal entries `-games_ij`, and a final
  anchoring row enforcing `sum(ratings) = 0`.
- **Numerically Hardened Solver**: Uses Gaussian elimination with partial
  pivoting plus residual checks before accepting the learned ratings.
- **Zero-Sum Ratings**: Produces ratings centered at `0.0`, where positive
  values indicate above-average aggregate performance and negative values
  indicate below-average aggregate performance.
- **Deterministic Ranking**: Orders candidate items by learned rating with
  deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items,
  self-preferences, non-positive weights, and disconnected comparison graphs.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Shared Ranking Infrastructure**: Reuses the `ranking_protocols` helpers
  for dataset validation, diagnostics, export, and candidate ranking.


Scoring semantics
-----------------

This implementation aggregates pairwise preferences into matchup totals and
then solves the Massey system

    M r = p

where `M_ii = games_i`, `M_ij = -games_ij` for `i \= j`, and the final row
of `M` is replaced with ones so that the learned ratings satisfy
`sum(ratings) = 0`. The right-hand-side vector `p` is the per-item signed
point-differential total `wins_i - losses_i`.

The linear system is solved using deterministic Gaussian elimination with
partial pivoting. The implementation also verifies that the recovered
solution has a small residual and only clamps negligible floating-point
noise around `0.0`.

The resulting ratings are relative rather than probabilistic: only their
differences and ordering matter. Larger positive values indicate stronger
aggregate pairwise performance against the field.


Usage
-----

### Learning a ranker

    % Learn from a pairwise ranking dataset object
    | ?- massey::learn(my_dataset, Ranker).
    ...

    % Learn with an explicit empty options list
    | ?- massey::learn(my_dataset, Ranker, []).
    ...

The current implementation accepts only the empty options list `[]`.
Any non-empty options list is rejected.

### Inspecting diagnostics

    % Inspect model and dataset summary metadata
    | ?- massey::learn(my_dataset, Ranker),
         massey::diagnostics(Ranker, Diagnostics).
    Diagnostics = [...]
    ...

### Ranking candidate items

    % Rank a candidate set from most preferred to least preferred
    | ?- massey::learn(my_dataset, Ranker),
         massey::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
    | ?- massey::learn(my_dataset, Ranker),
         massey::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
    Clauses = [my_ranker(massey_ranker(...))]
    ...

    % Export to a file
    | ?- massey::learn(my_dataset, Ranker),
         massey::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
    ...


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

    [
        model(massey),
        options(Options),
        dataset_summary(DatasetSummary)
    ]


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

    massey_ranker(Items, Ratings, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Ratings`: List of `Item-Rating` pairs.
- `Diagnostics`: List of metadata terms, including the effective options and
  dataset summary.


References
----------

1. Massey, K. (1997). *Statistical models applied to the rating of sports teams*.

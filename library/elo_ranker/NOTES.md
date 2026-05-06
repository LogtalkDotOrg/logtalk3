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


`elo_ranker`
=======

Elo pairwise preference ranker. Processes the pairwise preference stream
in dataset enumeration order using the standard Elo expected-score formula
and symmetric rating updates after each observed result.

The library implements the `ranker_protocol` defined in the
`ranking_protocols` library. It provides predicates for learning a ranker
from pairwise preferences, using it to order candidate items, and exporting
it as a list of predicate clauses or to a file.

Datasets are represented as objects implementing the
`pairwise_ranking_dataset_protocol` protocol from the `ranking_protocols`
library. See the `test_datasets` directory for examples. The current
implementation requires a well-formed connected pairwise dataset so that
learned ratings remain globally comparable across all ranked items.


API documentation
-----------------

Open the [../../apis/library_index.html#elo_ranker](../../apis/library_index.html#elo_ranker)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

    | ?- logtalk_load(elo_ranker(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

    | ?- logtalk_load(elo_ranker(tester)).


Features
--------

- **Pairwise Preference Learning**: Learns one deterministic rating per item
  from pairwise outcomes.
- **Deterministic Batch Elo Semantics**: Replays the dataset preference stream
  in enumeration order using the standard Elo expected-score formula. Because
  the current pairwise dataset protocol does not record historical timestamps,
  the implementation is a deterministic batch interpretation of that
  enumeration order rather than a reconstruction of a literal chronological
  competition log.
- **Configurable Rating Parameters**: Exposes the initial rating, K-factor,
  and rating scale as user options.
- **Integer Weight Fidelity**: Preference weights must be positive integers
  and are replayed as repeated unit outcomes in dataset enumeration order.
- **Deterministic Ranking**: Orders candidate items by learned rating with
  deterministic tie-breaking.
- **Strict Dataset Validation**: Rejects duplicate items, undeclared items,
  self-preferences, non-positive weights, and disconnected comparison graphs.
- **Ranker Export**: Learned rankers can be exported as self-contained terms.
- **Shared Ranking Infrastructure**: Reuses the `ranking_protocols` helpers
  for option processing, dataset validation, diagnostics, export, and
  candidate ranking.


Rating semantics
----------------

This implementation uses a deterministic batch Elo interpretation over the
pairwise preference stream. Each preference is processed in dataset
enumeration order. For a winner with rating `R_w` and a loser with rating
`R_l`, the expected winner score is computed as

    1 / (1 + 10^((R_l - R_w)/Scale))

and the winner receives a rating update of

    K * (1 - ExpectedWinnerScore)

with the loser receiving the symmetric negative update.

Because the current pairwise dataset protocol does not encode timestamps,
this is a deterministic batch Elo variant rather than a literal historical
competition-log replay.

Positive integer preference weights are replayed as repeated unit outcomes.
Datasets using non-integer preference weights are rejected because they do not
map cleanly to standard Elo update semantics.


Usage
-----

### Learning a ranker

    % Learn from a pairwise ranking dataset object
    | ?- elo_ranker::learn(my_dataset, Ranker).
    ...

    % Learn with custom Elo parameters
    | ?- elo_ranker::learn(my_dataset, Ranker, [initial_rating(1400.0), k_factor(24.0), rating_scale(200.0)]).
    ...

### Inspecting diagnostics

    % Inspect model, options, and dataset summary metadata
    | ?- elo_ranker::learn(my_dataset, Ranker),
         elo_ranker::diagnostics(Ranker, Diagnostics).
    Diagnostics = [...]
    ...

### Ranking candidate items

    % Rank a candidate set from most preferred to least preferred
    | ?- elo_ranker::learn(my_dataset, Ranker),
         elo_ranker::rank(Ranker, [item_a, item_b, item_c], Ranking).
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
    | ?- elo_ranker::learn(my_dataset, Ranker),
         elo_ranker::export_to_clauses(my_dataset, Ranker, my_ranker, Clauses).
    Clauses = [my_ranker(elo_ranker(...))]
    ...

    % Export to a file
    | ?- elo_ranker::learn(my_dataset, Ranker),
         elo_ranker::export_to_file(my_dataset, Ranker, my_ranker, 'ranker.pl').
    ...


Options
-------

The following options can be passed to the `learn/3` predicate:

- `initial_rating(Rating)`: Initial rating assigned to every item.
- `k_factor(KFactor)`: Positive Elo K-factor.
- `rating_scale(Scale)`: Positive rating-scale denominator used in the
  expected-score formula.

Datasets supplied to the ranker must use positive integer preference weights.
Non-integer weights are rejected.


Ranker representation
---------------------

The learned ranker is represented by a compound term of the form:

    elo_ranker(Items, Ratings, Diagnostics)

Where:

- `Items`: List of ranked items.
- `Scores`: List of `Item-Rating` pairs.
- `Diagnostics`: List of metadata terms, including the effective options and
  dataset summary.


References
----------

1. Elo, A. E. (1978). *The Rating of Chessplayers, Past and Present*. Arco.

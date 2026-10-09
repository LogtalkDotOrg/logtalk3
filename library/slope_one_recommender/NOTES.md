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


`slope_one_recommender`
=======================

This library predicts ratings using weighted Slope One collaborative
filtering: average rating differences between items, weighted by the
number of users who rated both. The library object imports the
`recommender_common` category and implements `recommender_protocol`
protocol. The `score/4` predicate returns a predicted rating. Datasets
implement the `rating_dataset_protocol` protocol with atomic identifiers,
unique user/item pairs, numeric ratings, a matching positive count, and
an optional numeric ordered rating scale.


API documentation
-----------------

Open the [../../apis/library_index.html#slope-one-recommender](../../apis/library_index.html#slope-one-recommender)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(slope_one_recommender(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(slope_one_recommender(tester)).


Examples
--------

    | ?- logtalk_load(recommender_protocols('test_datasets/movie_ratings')),
         slope_one_recommender::learn(movie_ratings, Model),
         slope_one_recommender::score(Model, alice, m6, Rating),
         slope_one_recommender::recommend(Model, alice, 3, Recommendations).


Options
-------

The `learn/3` predicate accepts the following options:

- `min_support(Count)` sets the minimum number of users who rated both
    items in a contributing pair to a positive integer. The default is `1`.
- `clip_to_scale(Boolean)` selects `true` (default) or `false`. When enabled,
    it clips estimates and fallbacks to the inclusive declared scale.
    Without a declared scale, it leaves estimates unchanged.

There are no similarity metric or neighborhood-size options.

The learning options can be retrieved from a learned recommender model
using the `recommender_options/2` predicate.


Training and prediction
-----------------------

Each unordered distinct item pair is collected once per co-rating user.
The deviation `d(i,j)` is the mean of `rating(user,i) - rating(user,j)`
over users who rated both; its support `count(i,j)` is their count.
Canonical storage uses standard item order with `ItemLo @< ItemHi`;
reverse lookup negates the stored difference, so `d(j,i) = -d(i,j)`.

For target user `u` and item `i`, each other item `j` rated by `u`
contributes if its pair support meets `min_support`. The estimate is:

    sum(count(i,j) * (rating(u,j) + d(i,j))) / sum(count(i,j))

Counts are rescaled by the maximum contributing count. Weighting is by
support, not equal item weights; the correction is additive, not a product
of deviation and rating. An already-rated target is still estimated and
excluded from its own contributions. Training does not implicitly hold out
observations for evaluation.

Without evidence the fallback is the known user's mean, otherwise the
known item's mean, otherwise the global mean. This also covers unknown
identifiers and catalogs with no co-rated item pairs. Query identifiers
must be instantiated atomic terms. Clipping follows either kind of estimate.

The `recommend/4` predicate returns up to `N` unrated training-catalog items
as `Item-Score` pairs; `N` must be a positive integer. Scores agree with
direct predictions; sorting is descending by score, with descending
standard item order for ties. Every training-catalog item is a candidate
for an unknown user. If no candidates remain, the result is `[]`.

For `u: a=1,b=3` and `v: a=2,b=4,c=5`, the estimate for `u,c` is four.
Adding `w: a=1,c=3` gives `d(c,a)=2.5` with support two and `d(c,b)=1`
with support one. The weighted estimate is `(2*(1+2.5)+(3+1))/3 = 11/3`.


Models, diagnostics, and export
-------------------------------

The `learn/2` and `learn/3` predicates return models using the following
term representation:

	slope_one_model(Ratings, Deviations, GlobalMean, Scale, Diagnostics)

The `Ratings` argument is a list of `rating(User, Item, Rating)` terms.
Sorted canonical deviations are
`deviation(ItemLo, ItemHi, MeanDifference, PositiveCount)` terms. The `Scale`
argument is the atom `none` or a `scale(Min, Max)` term. The `Diagnostics`
argument is a list containing exactly one of each diagnostic term:
`model(slope_one_recommender)`, `rating_count(Count)`, `options(Options)`,
`user_count(Count)`, `item_count(Count)`, and `deviation_pair_count(Count)`.

The `valid_recommender/1` predicate checks records, options, means, scale,
pair keys, orientation, counts, averages, and diagnostic consistency without
binding incomplete models. The `check_recommender/1` predicate throws for
invalid models. The `diagnostics/2` predicate returns the metadata list;
the `diagnostic/2` predicate enumerates it.

The `export_to_clauses(Dataset, Model, Functor, Clauses)` predicate exports
a single `Functor(Model)` fact; the `export_to_file/4` predicate writes it
with the common header. Restore with this library loaded. The
`print_recommender/1` predicate prints the template and complete model,
including effective options and summaries.


Limitations
-----------

Training enumerates a quadratic number of pairs in each user's profile
and sorts the collected contributions. A dense catalog can produce a
quadratic-size deviation table. List-based duplicate checks and pair lookup
also limit large-dataset performance. Public model validation recomputes
the deviation table; recommendation repeats validation for every candidate.
There are no incremental updates, alternative Slope One variants, or
adjustable fallback policies. Arithmetic uses backend numeric precision;
count rescaling does not prevent overflow in extreme rating differences
or sums.

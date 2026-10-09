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


`knn_item_recommender`
======================

This library predicts ratings from similar items using item-based
k-nearest-neighbor collaborative filtering with similarity-weighted raw
ratings. The library object imports the `recommender_common` category and
implements `recommender_protocol`. The `score/4` predicate returns a
predicted rating. Datasets implement `rating_dataset_protocol` with atomic
identifiers, one numeric rating per user/item pair, a matching positive
count, and an optional numeric ordered rating scale. The user-kNN and Slope
One libraries are not dependencies.


API documentation
-----------------

Open the [../../apis/library_index.html#knn-item-recommender](../../apis/library_index.html#knn-item-recommender)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(knn_item_recommender(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(knn_item_recommender(tester)).


Examples
--------

    | ?- logtalk_load(recommender_protocols('test_datasets/movie_ratings')).
         knn_item_recommender::learn(movie_ratings, Model),
         knn_item_recommender::score(Model, alice, m6, Rating),
         knn_item_recommender::recommend(Model, alice, 3, Recommendations).


Options
-------

The `learn/3` predicate accepts the following options:

- `k(K)` sets the maximum neighborhood size to a positive integer.
    The default is `3`.
- `similarity_metric(Metric)` selects a loaded object with a public
    `similarity/3` implementation. Its identifier must be ground; the default
    is `cosine_similarity`. It accepts parametric
  strategies. The dependency supplies cosine, Pearson, Jaccard, inverse MSD,
  and Spearman objects.
- `min_overlap(Count)` sets the minimum number of users who rated both
    items to a positive integer. The default is `1`.
- `min_similarity(Threshold)` sets a non-negative finite numeric threshold.
    The default is `0.0`.
- `clip_to_scale(Boolean)` selects `true` (default) or `false`. When enabled,
    it clips estimates and fallbacks to the inclusive declared scale.
    Without a declared scale, it leaves estimates unchanged.

Metrics receive complete item profiles and retain their own sparse-vector
semantics. A strategy must produce exactly one finite numeric score;
otherwise prediction throws `domain_error(similarity_score, Scores)`.
Provider exceptions propagate. Reloading an exported model also requires
its custom strategy object, if any.

The learning options can be retrieved from a learned recommender model
using the `recommender_options/2` predicate.


Prediction and recommendation
-----------------------------

For user `u` and target item `i`, candidates are other items `j` rated by
`u`. Unrated items cannot consume neighbor slots. Overlap, strictly positive
score, and minimum score are checked before selecting top `k`. The estimate
is the weighted average of the active user's raw ratings:

    sum(s(i,j) * rating(u,j)) / sum(s(i,j))

Weights are rescaled by the maximum selected score. Equal-score neighbors
use descending standard item order, independently of rating enumeration.
This is not adjusted cosine: the two-vector metric interface does not
supply user-mean context. Predicting an already-rated item still estimates
it and excludes that item from neighbors; evaluation must remove held-out
observations from training.

Unknown items, unknown users, or no usable neighbors trigger the known
user's mean, otherwise the known item's mean, otherwise the global mean.
Clipping follows this fallback or the collaborative estimate. Query
identifiers must be instantiated atomic terms, but need not have appeared
in training.

The `recommend/4` predicate returns up to `N` unrated training-catalog
`Item-Score` pairs; `N` must be a positive integer. Results are ordered by decreasing score with
descending standard identifier order for ties. It scores through the same
public prediction path. Every training-catalog item is a candidate for an
unknown user. If no candidates remain, the result is `[]`.

For `u: a=1,b=3` and `v: a=2,b=4,c=5`, cosine scores for `c` against `a`
and `b` are `2/sqrt(5)` and `4/5`. The prediction for `u,c` is:

    (2/sqrt(5) + 3*0.8) / (2/sqrt(5) + 0.8)

With `k(1)` only `a` contributes and the estimate is one.


Models, diagnostics, and export
-------------------------------

The `learn/2` and `learn/3` predicates return models using the following
term representation:

    knn_item_model(Ratings, Profiles, GlobalMean, Scale, Diagnostics)

The `Ratings` argument is a list of `rating(User, Item, Rating)` terms.
Sorted profiles contain `Item-profile(SortedUserRatingPairs, Mean)` entries.
The `Scale` argument is the atom `none` or a `scale(Min, Max)` term.
The `Diagnostics` argument is a list containing exactly one of each diagnostic term:
`model(knn_item_recommender)`,
`rating_count(Count)`, `options(Options)`, `user_count(Count)`,
`item_count(Count)`, and `neighbor_axis(item)`.

The `valid_recommender/1` predicate checks records, effective options, profile coverage,
means, scale, and diagnostics without binding incomplete models.
The `check_recommender/1` predicate throws on invalid models. The `diagnostics/2`
predicate returns the list, and the `diagnostic/2` predicate enumerates entries.

The `export_to_clauses(Dataset, Model, Functor, Clauses)` predicate exports one
`Functor(Model)` fact; the `export_to_file/4` predicate writes it with the common header.
The `print_recommender/1` predicate prints the template and complete model, including
options and counts.


Limitations
-----------

No all-pairs similarity cache, incremental updates, signed weighting, or
adjusted cosine is provided. List-based dataset validation can be quadratic
in rating count. Public prediction recomputes profiles to validate the
model, and recommendation repeats validation for each candidate.
Arithmetic follows backend numeric precision. Rescaled weights avoid
avoidable weight overflow, not overflow in extreme rating sums.


References
----------

- Sarwar, B., Karypis, G., Konstan, J., and Riedl, J. (2001).
    Item-Based Collaborative Filtering Recommendation Algorithms.
    *Proceedings of the 10th International Conference on World Wide Web*,
    285-295.
    https://doi.org/10.1145/371920.372071

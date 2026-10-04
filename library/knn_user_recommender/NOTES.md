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


`knn_user_recommender`
======================

User-based k-nearest-neighbor collaborative filtering with mean-centered
predictions. The object imports `recommender_common` and implements the
`recommender_protocol` contract. The `score/4` predicate returns a predicted
rating. Training datasets implement `rating_dataset_protocol`: atomic
user/item identifiers, one numeric rating per pair, a matching positive
rating count, and an optional numeric ordered rating scale.


API documentation
-----------------

Open the [../../apis/library_index.html#knn-user-recommender](../../apis/library_index.html#knn-user-recommender)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(knn_user_recommender(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(knn_user_recommender(tester)).


Examples
--------

    | ?- logtalk_load(recommender_protocols('test_datasets/movie_ratings')),
         knn_user_recommender::learn(movie_ratings, Model),
         knn_user_recommender::score(Model, alice, m6, Rating),
         knn_user_recommender::recommend(Model, alice, 3, Recommendations).


Options
-------

The `learn/3` predicate accepts the following options:

- `k(3)`: positive maximum number of contributing neighbors.
- `similarity_metric(pearson_similarity)`: ground identifier of a loaded
  object exposing an implemented public `similarity/3`. Parametric objects
  are accepted. Cosine, Pearson, Jaccard, inverse MSD, and Spearman strategies
  are loaded by the dependency loader.
- `min_overlap(1)`: positive minimum number of common observed items.
- `min_similarity(0.0)`: non-negative finite numeric threshold.
- `clip_to_scale(true)`: `true` or `false`; clips both collaborative and
  fallback estimates to the declared inclusive scale. With no scale it is
  a no-op.

Custom metrics receive complete sparse profiles, not an already-trimmed
intersection. They must return exactly one finite numeric score. No result,
multiple results, or invalid scores throw `domain_error(similarity_score, Scores)`;
provider exceptions propagate. The custom object must also be loaded when
restoring an exported model.

The learning options can be retrieved from a learned recommender model
using the `recommender_options/2` predicate.


Prediction and recommendation
-----------------------------

For target user `u` and item `i`, candidates are other users who rated `i`.
Overlap and score filters apply before taking the top `k`. Only strictly
positive scores at least `min_similarity` contribute; anti-correlated
neighbors are excluded. Each user's mean is over their entire training
profile. With selected neighbors `v` and scores `s(u,v)`, the estimate is:

    mean(u) + sum(s(u,v) * (rating(v,i) - mean(v))) / sum(s(u,v))

Weights are rescaled by their maximum before accumulation. Equal-score
neighbors use descending standard identifier order, independently of dataset
enumeration order. Predicting an already-rated item still estimates it;
the target user is excluded from the neighborhood, but training observations
are not implicitly held out. Evaluation must remove held-out ratings.

If the user is unknown or no eligible evidence remains, the fixed fallback
is the known user's mean, otherwise the known item's mean, otherwise the
global training mean. Unknown atomic identifiers are valid queries.
Variables throw `instantiation_error`; compound identifiers throw an atomic
type error.

The `recommend(Model, User, N, Recommendations)` predicate returns up to positive integer
`N` unrated training-catalog items as `Item-Score` pairs. Scores are the
same as direct predictions, including fallbacks and clipping. Unknown users
see the entire catalog; no candidates returns `[]`. Results are ordered by
descending score, with descending standard item order for ties.

For `u: a=1,b=3` and `v: a=2,b=4,c=5`, Pearson gives a neighbor score of
one and the prediction for `u,c` is `2 + (5 - 11/3) = 10/3`.


Models, diagnostics, and export
-------------------------------

The `learn/2` and `learn/3` predicates return the learned recommender model
as a term with the following structure:

    knn_user_model(Ratings, Profiles, GlobalMean, Scale, Diagnostics)

`Ratings` contains `rating(User, Item, Rating)` terms. `Profiles` is sorted
by user and contains `User-profile(SortedItemRatingPairs, Mean)` entries.
`Scale` is `none` or `scale(Min, Max)`. Diagnostics contain exactly one
`model(knn_user_recommender)`, `rating_count(Count)`, `options(Options)`,
`user_count(Count)`, `item_count(Count)`, and `neighbor_axis(user)`.

The `valid_recommender/1` predicate rejects incomplete or inconsistent models without
instantiating them. It recomputes profiles, means, and counts from ratings.
The `check_recommender/1` predicate throws for invalid models. The `diagnostics/2`
predicate returns the metadata list; the `diagnostic/2` predicate enumerates individual terms.

The `export_to_clauses(Dataset, Model, Functor, Clauses)` predicate emits one `Functor(Model)`
fact. The `export_to_file/4` predicate writes the same fact with the shared export header.
The `print_recommender/1` predicate prints the template and complete learned term, including
effective options and summaries.


Limitations
-----------

No similarity cache, incremental learning, signed weighting, or adjustable
fallback policy is provided. Sparse lookup and duplicate validation use
lists; dataset validation can be quadratic in the number of ratings.
Public predictions fully validate the model, recomputing derived profiles.
Recommendations call public prediction for each candidate, repeating this
validation. This favors inspectable correctness over large-catalog speed.
Arithmetic uses backend numeric precision; rescaled weights do not prevent
overflow in arbitrary extreme rating sums or differences.

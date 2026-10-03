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


`recommender_protocols`
=======================

This library provides protocols used in the implementation of recommendation
algorithms. Datasets are represented as objects implementing the
`rating_dataset_protocol` protocol. Recommenders are represented as objects
importing the `recommender_common` category. This category provides shared
helpers for dataset validation, rating-matrix utilities (user and item rating
vectors, mean-rating baselines), similarity metrics, top-k retrieval,
diagnostics metadata, export, and pretty-printing support.

Similarity metrics are also exposed as pluggable strategy objects,
`cosine_similarity`, `pearson_similarity`, `jaccard_similarity`,
`msd_similarity`, and `spearman_similarity`, implementing the
`similarity_metric_protocol` protocol, so that a concrete recommender
can either call the convenience predicates in `recommender_common`
directly or accept a similarity metric as a parameter (for example via
a `similarity_metric(Metric)` option naming one of these objects, or a
user-supplied object implementing the same protocol).

Learned recommenders expose diagnostics using the shared `diagnostics/2`,
`diagnostic/2`, and `recommender_options/2` predicates. Concrete
recommender implementations store effective training options in the
diagnostics metadata under an `options(Options)` term.

Implementations must define the protected `recommender_valid_data/1`
hook to validate model data and its consistency with diagnostics without
instantiating the model. The shared `check_recommender/1` also requires
a proper diagnostics list of compound terms containing exactly one
`model(Atom)`, `rating_count(PositiveInteger)`, and `options(Options)`.
Effective options must be ground compound terms valid for the receiving
implementation. Other diagnostics terms are allowed. Diagnostics are
extracted from the last model argument by default; implementations can
override `recommender_diagnostics_data/2` to use another representation.
A printing template alone does not establish model validity.

Dataset ratings are a sparse ``User``-``Item``-``Rating`` relation: at
most one rating is declared per user-item pair, and the declared
`rating_count/1` must match the number of ratings enumerated by
`rating/3`. Unlike the `time_series_dataset_protocol` datasets, there is
no index-sequence requirement, since users and items are identified by
arbitrary atomic identifiers rather than positions in a sequence.
Identifier validation precedes duplicate checking: variables cause an
instantiation error and non-atomic identifiers cause an atomic type error.

This library also provides a small MovieLens-style test dataset and a
handful of invalid dataset fixtures under the `test_datasets` directory.


API documentation
-----------------

Open the [../../apis/library_index.html#collaborative-filtering-protocols](../../apis/library_index.html#collaborative-filtering-protocols)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(recommender_protocols(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(recommender_protocols(tester)).

The test suite exercises dataset validation, every rating-matrix
utility and baseline, all five similarity metrics (including a pair of
users with high cosine similarity but perfectly inverted, mean-centered
preferences, giving a Pearson correlation of -1.0, illustrating the
difference between the two metrics), top-k retrieval, and a minimal
baseline-predictor recommender (`sample_recommender`, under
`test_objects.lgt`) exercising the full `recommender_protocol` contract
end-to-end. Reference values for the bundled `movie_ratings` dataset
were computed independently in Python.
Regression tests also cover malformed and incomplete models, identifier
validation, constant floating-point vectors, large integer offsets, and
tiny and large numeric magnitudes.
Additional metric tests check key overlap, absolute rating differences,
average ranks for ties, symmetry, key ordering, and common-key subsets.


Test datasets
-------------

`movie_ratings` is a small MovieLens-style dataset: six users rating up
to six movies on a 1-5 scale. Movies `m1`-`m3` are action films and
`m4`-`m6` are romance films; `alice`, `bob`, and `carol` mostly rate
action films highly, while `dave`, `erin`, and `frank` mostly rate
romance films highly, with a few cross-genre ratings, giving two
recognizable but imperfect taste clusters, useful for sanity-checking a
similarity-based recommender.

The following invalid dataset fixtures are also provided, each
exercising one validation error:

- `duplicate_rating.lgt`: the same user-item pair is rated twice.
- `inconsistent_rating_count.lgt`: the declared rating count differs from the number of ratings.
- `no_ratings.lgt`: a positive rating count is declared but no ratings are given.
- `non_numeric_rating.lgt`: a rating value is not a number.
- `out_of_scale_rating.lgt`: a rating value falls outside the declared 1-5 rating scale.

The parametric `identifier_ratings/2` fixture in `test_objects.lgt`
also exercises variable, compound, and valid numeric identifiers.


Rating-matrix utilities
-------------------------

The `recommender_common` category provides, among others, the following predicates:

- `dataset_ratings/2`: collects a dataset's ratings as a list of `rating(User, Item, Rating)` terms, checking atomic identifiers, duplicate user-item pairs, and that the declared rating count matches the observed count.
- `check_ratings/2`: validates that ratings are non-empty, have atomic identifiers and numeric values, and, when a `rating_scale/2` is declared, are within range.
- `users/2` and `items/2`: the sorted list of distinct users or items appearing in a list of ratings.
- `user_vector/3` and `item_vector/3`: the sparse rating vector of a user or an item, as a list of `Item-Rating` or `User-Rating` pairs.
- `global_mean_rating/2`, `user_mean_rating/3`, `item_mean_rating/3`: the classic mean-rating baselines, usable both as standalone baseline predictors and as building blocks (e.g. for a global-plus-bias baseline predictor) in other recommenders.
- `cosine_similarity/3`, `pearson_similarity/3`, `jaccard_similarity/3`, `msd_similarity/3`, and `spearman_similarity/3`: convenience predicates equivalent to sending `similarity/3` to the corresponding metric object.
- `top_k/3`: returns the `K` highest-scoring `Key-Score` pairs from a list of candidates, sorted by decreasing score; returns every pair, still sorted, when fewer than `K` are given.
- `check_top_n/1`: validates a requested recommendation count.
- `check_query_identifiers/2`: checks instantiated atomic user/item identifiers.
- `dataset_rating_scale/2`: returns `none` or `scale(Min, Max)`, checking numeric, ordered bounds.
- `fallback_rating/5`: returns the known user's mean, otherwise the known item's mean, otherwise the supplied global mean.
- `clip_rating/3`: clips to an inclusive validated scale; `none` is a no-op.
- `recommend_from_ratings/5`: validates the query and scores unrated catalog items through self `predict_rating/4`, returning up to `N` results. Equal scores use descending standard item order.

These are declared `protected`, intended to be reused by concrete
recommender libraries such as `knn_user_recommender`,
`knn_item_recommender`, and `slope_one_recommender` that import this category.


Similarity metrics
-------------------

All metrics take two sparse vectors, each a list of `Key-Value` pairs
(`Item-Rating` pairs to compare two users, or `User-Rating` pairs to
compare two items). Keys must be ground and occur at most once per
vector. Values must be finite numbers, except that Jaccard ignores
them entirely:

- `cosine_similarity` computes the cosine of the angle between the two
  vectors. The norms are computed over each vector as a whole; the dot
  product is computed over the keys common to both (a key present in
  only one vector contributes zero, exactly as if the missing entries
  were zero-valued). Two vectors sharing no key, or an all-zero vector,
  give a similarity of `0.0`.
- `pearson_similarity` computes the Pearson correlation coefficient
  over the keys common to both vectors only (the usual pairwise Pearson
  formula used in collaborative filtering: means and norms are computed
  over the co-rated subset, not the whole vector). Fewer than two
  common keys, or a constant (zero-variance) value over the common
  keys, give a similarity of `0.0`.
- `jaccard_similarity` compares the sets of observed keys: the size of
  their intersection divided by the size of their union. Scores range
  from `0.0` to `1.0`; an empty union gives `0.0`. Values are ignored,
  including zeros, so explicit zero-valued entries still count as
  observations. For binary feedback where zero means absence, omit
  those keys from the vectors.
- `msd_similarity` computes `1 / (1 + MSD)`, where `MSD` is the mean
  squared rating difference over common keys. Scores range from `0.0`
  to `1.0`; identical common ratings give `1.0` even for a single
  common key. No common keys gives `0.0`. Unlike correlation, it
  measures agreement in absolute rating values and depends on rating
  units, so the vectors should use the same rating scale. Differences
  are scaled before squaring, avoiding overflow even for large
  opposite-signed ratings; sufficiently small scores may round to zero.
- `spearman_similarity` computes Pearson correlation of the numeric
  ranks within each common-key subset, using average ranks for ties.
  Numeric equals such as `1` and `1.0` share a rank. Scores range from
  `-1.0` to `1.0`; fewer than two common keys or constant common
  values in either vector gives `0.0`. It measures ordinal agreement,
  allowing nonlinear monotonic transformations of ratings.

Cosine and Pearson scale values before unit normalization instead of squaring
their original magnitudes, avoiding norm underflow and overflow for
very small or large finite values. Pearson centers in shifted, scaled
coordinates, preserving differences between large integer ratings.
When values span zero, its reference is zero to avoid overflowing the
difference between large opposite-signed values. Exactly constant values
are detected numerically, without an epsilon. Final scores are bounded
to `[-1.0, 1.0]` to remove endpoint roundoff. These calculations still
use backend floating-point precision and cannot recover distinctions
already lost in floating-point inputs. Spearman uses the same centered
normalization and endpoint bounds after computing ranks.

Cosine similarity does not mean-center the vectors, so it mostly
reflects whether two vectors tend to be large or small together, while
Pearson similarity reflects whether they vary together around their own
means; the two can disagree, sometimes sharply, as the test suite's
`alice`-`bob` example (cosine `0.94`, Pearson `-1.0`) shows.

The metrics reuse helpers in `similarity_metric_common`, including
`common_pairs/3`, `split_pairs/3`, `dot_product/3`, `scale_values/2`,
`normalize_values/2`, `normalize_vector/2`,
`centered_normalized_values/2`, and `bounded_similarity/2`.
The existing `vector_norm/2`, `sum_of_squares_list/2`, `mean_values/2`,
and `center/3` helpers retain their original arithmetic contracts;
custom metrics can reuse the normalized helpers when only a similarity
score, rather than an original-magnitude norm, is needed.


Limitations
-----------

- Adjusted cosine is not provided: it requires user-mean context in
  addition to the two raw item-rating vectors. The two-vector metric
  interface cannot infer those means.
- Spearman ranking uses pairwise numeric comparisons and takes
  `O(c^2)` time for `c` common keys.
- The `check_no_duplicate_ratings/1` predicate (used by the `dataset_ratings/2` predicate) and
  leave-one-out-style validations elsewhere in this family are `O(n)`
  per rating against a growing list, so `O(n^2)` overall; this is only
  a concern for very large rating datasets.
- Concrete recommenders are separate libraries: `knn_user_recommender`,
  `knn_item_recommender`, and `slope_one_recommender`. Matrix factorization
  is not provided.

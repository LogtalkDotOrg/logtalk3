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


`bm25_recommender`
==================

Content-based filtering using positive-feedback raw-count query profiles and
asymmetric Okapi BM25 relevance. The user profile is the weighted query; each
candidate item is the document. Scores are nonnegative floats and can exceed
one. They are neither cosine similarities nor predicted ratings and are not
clipped to a rating scale.

The object imports `recommender_common` and `item_content_dataset_validation`,
implementing `recommender_protocol`. It has no text-vectorizer or cosine
dependency.


API documentation
-----------------

Open the [../../apis/library_index.html#bm25-recommender](../../apis/library_index.html#bm25-recommender)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(bm25_recommender(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(bm25_recommender(tester)).


Datasets and usage
------------------

Training accepts one object implementing both `rating_dataset_protocol` and
`item_content_dataset_protocol`. For example:

    :- object(bm25_article_content,
        implements([rating_dataset_protocol, item_content_dataset_protocol])).

        rating(alice, first, 5).
        rating_count(1).
        rating_scale(1, 5).

        item(first).
        item(second).
        item(third).

        item_content(first, features([science,science])).
        item_content(second, features([science,space,space,space])).
        item_content(third, features([])).

    :- end_object.

With the dataset loaded:

    | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::score(Model, alice, second, Score),
         bm25_recommender::recommend(Model, alice, 3, Recommendations).

The query profile contains `science-2.0`. Recommendations contain `second`
with positive relevance followed by `third` with `0.0`; neither candidate
needs a rating. The catalog includes the rated `first`, but recommendations
exclude every item already rated by the querying user, not just positive ones.
Scores can still be queried for rated items.

The catalog is nonempty with unique atomic item identifiers and exactly one
descriptor per item, without additional content declarations. Every rated
item must belong to the catalog. Ratings are nonempty, finite numeric values
with unique user-item pairs and a matching positive declared count. An optional
rating scale is checked, including its inclusive bounds.

Use one descriptor representation consistently throughout the catalog:

- `features(Occurrences)` accepts a proper list of arbitrary ground terms.
  Repeated occurrences count; terms can be tokens, tags, compounds, or n-grams.
  Tokenization and preprocessing are external.
- `vector(Pairs)` accepts a proper list of `Feature-Count` pairs with unique
  ground keys. These are **raw term counts, not preweighted vectors**.
  Counts must be finite and nonnegative. Exact numeric zeros, including `0.0`,
  are removed; every retained count must be an integer. Positive `1.0` or `1.5`
  therefore raises `type_error(integer, Count)`. Negative, nonnumeric,
  nonfinite, variable, and duplicate-key errors retain shared validation rules.

For example, `features([science,science])` and `vector([science-2])` have the
same BM25 semantics in separately trained, homogeneous catalogs. The shared
dataset protocol has the same shape as TF-IDF, but TF-IDF's preweighted-vector
semantics do not apply here. Feature order and vector key order are canonicalized.

Empty content is allowed in both representations. An entirely empty catalog
feature space is valid, with average length `0.0`, empty statistics, empty item
weights and profiles, and zero relevance.


Options
-------

The `learn/3` predicate accepts:

- `k1(1.2)` controls term-frequency saturation. Any finite nonnegative numeric
  value is accepted. Zero gives the presence-only, IDF-weighted limit.
- `b(0.75)` controls document-length normalization. Accepts finite numeric
  values between zero and one, inclusive. Zero removes length adjustment.
- `positive_threshold(user_mean)` selects ratings at least equal to the user's
  mean over all their ratings. A finite numeric threshold can be supplied instead.
- `profile_weighting(uniform)` averages selected raw-count documents equally.
  The alternative `rating` uses raw ratings as weights; every selected rating
  must be strictly positive, including ratings on empty-content items.
- `query_saturation(none)` uses raw query coefficients directly, preserving
  default BM25 relevance. A finite nonnegative numeric value supplies the
  query-frequency saturation constant `K3`; zero gives presence-only query
  coefficients. Stored profiles remain raw in all modes.

Repeated valid options are accepted and the first occurrence wins. The public
`valid_option/1` and `default_option/1` hooks can be queried;
`recommender_options/2` returns effective options. There are no normalization,
vectorizer, delta, or alternative-IDF options.

    | ?- bm25_recommender::learn(bm25_article_content, Model,
             [k1(2), b(1), positive_threshold(4), profile_weighting(rating)]),
         bm25_recommender::score(Model, alice, second, Score).


Weighting and profiles
---------------------

Let $N$ include **every catalog item**, even unrated and empty ones. The document
frequency $df(t)$ counts documents with positive count of term $t$, not total
occurrences. Raw document length $dl(d)$ is the sum of all term counts and
$avgdl$ is total raw length divided by $N$, including empty documents.

The positive smoothed IDF is:

$$
idf(t)=\log\left(1+\frac{N-df(t)+0.5}{df(t)+0.5}\right)
      =\log\left(\frac{N+1}{df(t)+0.5}\right).
$$

The fitted item weight is:

$$
w(t,d)=idf(t)\frac{tf(t,d)(k_1+1)}
 {tf(t,d)+k_1\left(1-b+b\frac{dl(d)}{avgdl}\right)}.
$$

Zero-frequency terms have no weight. All-empty and absent-term cases are
handled before average-length division; no epsilon, count cap, or fallback
length is introduced. With `k1(0)`, each present term has weight `idf(t)`.

For threshold-selected documents $S_u$, the query coefficients are:

$$
q(u,t)=\frac{\sum_{d\in S_u}\alpha_{u,d}\,tf(t,d)}
             {\sum_{d\in S_u}\alpha_{u,d}},
\qquad score(u,d)=\sum_t q(u,t)\,w(t,d).
$$

The selected item weight $\alpha$ is one in uniform mode or its strictly
positive rating in rating-weighted mode. Empty selected documents contribute
to the denominator. Profiles can contain fractional coefficients despite
integer input counts. They are not IDF-weighted, BM25-weighted, query-saturated,
or length/cosine-normalized. An empty selected set has an empty profile.

With `query_saturation(K3)`, scoring uses the effective coefficients:

$$
q'(u,t)=\frac{q(u,t)(K_3+1)}{q(u,t)+K_3},
\qquad score(u,d)=\sum_t q'(u,t)\,w(t,d).
$$

Only positive raw coefficients are stored. `K3 = 0` gives `1.0` for every
present query term; an empty profile stays empty. Fractional coefficients are
supported. The effective query is derived once per scoring, batch, or
recommendation operation, without changing stored profiles or item weights.
Scaled arithmetic avoids the direct large product in the formula, but remains
subject to backend precision and numeric limits. `query_saturation(none)`
bypasses the transform exactly.

  | ?- bm25_recommender::learn(bm25_article_content, Model,
       [query_saturation(2)]),
     bm25_recommender::score(Model, alice, second, Score).

The example has $N=3$, $avgdl=2$, $df(science)=2$, and $q(alice,science)=2$.
Default scores for `first` and `second` are respectively
$2\log(1.6)(4.4/3.2)$ and $2\log(1.6)(2.2/3.1)$; the first exceeds one.
The defaults and positive IDF follow common BM25 practice, but this is not
bit-for-bit Lucene scoring: there is no quantized document norm or overlap-token
handling, and query aggregation is the explicit recommender policy above.


Scoring APIs
------------

`score/4` requires a catalog item. Unknown users, empty profiles, empty
candidates, and disjoint features score `0.0`. Unknown items raise
`domain_error(catalog_item, Item)`, even for unknown users. `recommend/4` returns
up to a positive integer number of unrated candidates, ordered by decreasing
score, with descending standard item order for ties. Zero-score candidates
are included; an unknown user receives the whole catalog in zero-score tie order.

`score_all/4` validates the model once and returns `Item-Score` pairs in requested
order, preserving duplicates and allowing rated items. An empty batch still
validates the model and atomic user. The query is resolved once; unique atomic
catalog requests are resolved by an ordered merge and duplicate scores are
cached within the batch. Results and identifier errors retain original request
order. All requested identifiers are checked, including for unknown users.

    | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::score_all(Model, alice, [second,first,second,third], Scores).

`score_content/4` scores a supplied descriptor using frozen corpus statistics
and matching the trained representation. It never fits the supplied content
as another corpus document or changes the catalog. Unseen features contribute
no matching weight but **do contribute to full candidate length**, so unseen
padding penalizes known matches when `k1 > 0` and `b > 0`. With `b(0)` or
`k1(0)`, that length penalty disappears. Invalid descriptors are checked even
for unknown users or empty profiles.

    | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::score_content(Model, alice,
             features([science,space,space,space]), SameItemScore),
         bm25_recommender::score_content(Model, alice,
             features([science,space,space,space,unseen,unseen]), PaddedScore).

`PaddedScore` is lower than `SameItemScore` under defaults. An entirely unseen
or empty supplied descriptor scores zero, including after all-empty training.


Immutable feedback changes
--------------------------

`update_ratings/3` accepts a proper list of `rating(User,Item,Value)` records.
It inserts or replaces by user-item key, allowing new users but requiring
catalog items. Values must be finite numbers within any stored rating scale.
Repeated batch keys raise `domain_error(duplicate_rating, User-Item)`.

`remove_ratings/3` accepts a proper list of atomic `User-Item` pairs. Missing
and repeated pairs are accepted, including unknown identifiers. No matching
ratings returns the original validated model. Removing the last global rating
raises `domain_error(non_empty_ratings, [])`. A user with no remaining history
loses their profile and follows unknown-user zero scoring; removed items become
eligible for recommendations with normal BM25 relevance.

Both APIs preserve catalog contents, fitted corpus statistics and cached item
weights. They rebuild only affected users' raw profiles from their complete
resulting history and refresh diagnostics, retaining other users' profiles,
options, scale, permitted metadata extras and their order. Fully validated
identical upserts return the original model without another profile rebuild.
A changed mean can newly select a nonpositive rating in
rating-weighted mode, raising `domain_error(positive_rating_weight, Rating)`
even when that item's content is empty.

    | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::update_ratings(Model,
             [rating(alice,first,1),rating(alice,second,5),rating(bob,first,4)], Rated),
         bm25_recommender::remove_ratings(Rated,
             [alice-second,alice-second,missing-unknown], Removed),
         bm25_recommender::recommend(Removed, alice, 3, Recommendations).

Alice's remaining profile returns to `science-2.0`, and `second` is eligible
again; Bob retains his own profile and rating exclusions.


Immutable catalog changes
-------------------------

`extend_catalog/3` accepts unique atomic **new** item identifiers paired with
descriptors. Existing identifiers raise `domain_error(new_catalog_item, Item)`.
`replace_content/3` accepts unique atomic **existing** identifiers paired with
replacement descriptors; missing items raise `domain_error(catalog_item, Item)`.
Both require proper lists and the trained representation, canonicalize content,
and enforce integer retained counts in vector mode. Repeated identifiers raise
`domain_error(duplicate_item, Item)`.

Both operations refit **the entire corpus and every item weight in both
representations**, refreshing diagnostics while preserving ratings, scale,
effective options, metadata extras and order. Additions reuse all raw profiles;
replacements rebuild only users who rated canonically changed items. Count-vector
mode is not a preweighted bypass. Unrated changes leave raw query profiles
identical but can change every old item's score through $N$, document
frequencies, and average length. Rated replacements can also change profiles.
Empty additions and replacements return the validated original; canonically
identical replacements return the original without another refit.

    | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::extend_catalog(Model,
             [fourth-features([science,space])], Extended),
         bm25_recommender::update_ratings(Extended,
             [rating(alice,fourth,4)], Rated),
         bm25_recommender::replace_content(Rated,
             [fourth-features([space,space])], Replaced),
         bm25_recommender::remove_ratings(Replaced, [alice-first], Final),
         bm25_recommender::recommend(Final, alice, 3, Recommendations).

    `remove_catalog/3` accepts a proper list of atomic item identifiers, ignoring
    missing and repeated identifiers. It removes matching items **and all their
    ratings**, refits the remaining corpus and every item weight, and rebuilds
    affected users' profiles. Users with no remaining history disappear. Removing
    an unrated item still changes corpus statistics but retains raw profiles.
    Empty/no-match requests return the validated original, and retries are safe.
    All identifiers are validated before applying changes or nonempty guards.

    An empty remaining catalog raises `domain_error(non_empty_catalog, [])`;
    an empty global history raises `domain_error(non_empty_ratings, [])`.
    Mean-shift `positive_rating_weight` errors can occur just as for feedback
    removal. The original model is unchanged on success or failure.


    Rating-scale changes
    --------------------

    `set_rating_scale/3` accepts `none` or `scale(Min,Max)`. Numeric bounds must be
    finite and ordered, and every stored rating must lie within the inclusive
    bounds. Removing, widening, or tightening the scale changes only that model
    field; ratings, corpus, item weights, profiles, options, and diagnostics are
    retained exactly. An unchanged scale returns an identical model.

    Variable scales or bounds raise `instantiation_error`; nonnumeric bounds raise
    `type_error(number, Bound)`. Invalid descriptors, nonfinite bounds, and reversed
    bounds raise `domain_error(rating_scale, ...)`. An excluded stored rating raises
    `domain_error(rating_scale(Min,Max), Rating)`. Subsequent rating upserts enforce
    the new scale. This API never clamps or rescales ratings or BM25 relevance.

    For a composed catalog-removal and scale-edit workflow:

      | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::extend_catalog(Model,
           [fourth-features([science,space])], Extended),
         bm25_recommender::update_ratings(Extended,
           [rating(alice,fourth,4)], Rated),
         bm25_recommender::remove_catalog(Rated,
           [first,first,missing], Removed),
         bm25_recommender::set_rating_scale(Removed, scale(4,4), Bounded),
         bm25_recommender::recommend(Bounded, alice, 3, Recommendations).

All update APIs validate the original model once, including for empty/no-op
requests, and return new ground model terms without mutating the original or
    reopening the dataset. Profile rebuilding is selective after that validation;
    validation itself still reconstructs all fitted state. Catalog edits retain
    full corpus/item-weight refitting, not optimized incremental BM25 fitting.
    Unlike catalog changes, supplied-content scoring freezes the trained corpus.


Models, diagnostics, and export
------------------------------

The model is
`bm25_model(Ratings,Contents,ItemWeights,Profiles,Corpus,Scale,Diagnostics)`.
`Corpus` is `bm25_corpus(DocumentCount,AverageLength,FeatureStatistics)` with
sorted `Feature-statistics(DocumentFrequency,IDF)` pairs. Canonical descriptors
retain original counts; document lengths/count vectors are derived rather than
redundantly stored. Cached item weights and raw user profiles are sorted sparse
lists. Treat model terms as opaque inputs to the public API.

`diagnostics/2` reports `model(bm25_recommender)`, `rating_count`, effective
`options`, `user_count`, `item_count`, `content_representation(features|vectors)`,
`feature_count`, `non_empty_profile_count`, and `average_document_length` terms.
Feature count is the union of positive-count corpus keys. Required diagnostics
occur exactly once with exact values; permitted additional compound terms are
retained. `valid_recommender/1` checks groundness and reconstructs corpus, item
weights, profiles, scale, options, and diagnostics before accepting a model.

The common clause/file export APIs serialize a self-contained ground model
fact; no live dataset is needed to restore scoring. `print_recommender/1` prints
the model and diagnostics. For example:

    | ?- bm25_recommender::learn(bm25_article_content, Model),
         bm25_recommender::export_to_clauses(bm25_article_content, Model, saved, Clauses),
         bm25_recommender::export_to_file(bm25_article_content, Model, saved, 'saved.pl').

The clauses contain `saved(Model)`. After loading the exported Prolog file,
retrieve its fact in the backend context and pass the restored term to any
scoring API.


Limitations
-----------

- Learning, profiles, validation and updates are in-memory and list-based.
  Strict validation reconstructs fitted state; repeated single scoring calls
  remain expensive. `score_all/4` amortizes validation, resolves unique catalog
  requests with an ordered merge, and caches duplicate scores. Request-order
  restoration still uses transient lists; there is no persistent lookup index
  or validation bypass.
- Every actual catalog change refits corpus statistics and all item weights.
  Affected profiles are selectively rebuilt after validation, but this is not
  optimized incremental corpus fitting. Item removal and rating-scale editing
  are supported without reopening the training dataset.
- Numeric bounds and floating-point precision depend on the Prolog backend.
  Scaling avoids avoidable large products but does not provide arbitrary-range
  floating-point arithmetic, count clipping, or epsilon-based fallback values.
- Text analysis, fields, BM25+/BM25L, alternative IDF,
  negative-feedback subtraction, popularity/cold-start fallbacks, rating
  calibration, implicit-only datasets, managed persistence/out-of-core
  execution, and cross-model score comparability are not provided. Clause/file
  export already supports serialization of self-contained in-memory models.

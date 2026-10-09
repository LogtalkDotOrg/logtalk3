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


`jaccard_recommender`
=====================

This library recommends items by comparing categorical or binary features
with a user's profile. By default, a user's profile is the union of features
from positively selected items. An optional minimum-support filter removes
features occurring in too few selected items. Candidates are scored using
classical intersection-over-union Jaccard similarity.

The library object imports `item_content_dataset_validation` and
`recommender_common`, implementing `recommender_protocol` and its `score/4`
predicate directly. Scores are relevance values, not predicted ratings.


API documentation
-----------------

Open the [../../apis/library_index.html#jaccard-recommender](../../apis/library_index.html#jaccard-recommender)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

    | ?- logtalk_load(jaccard_recommender(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

    | ?- logtalk_load(jaccard_recommender(tester)).


Datasets and usage
------------------

Training accepts an object implementing both `rating_dataset_protocol` and
`item_content_dataset_protocol`. For example:

    :- object(article_tags,
        implements([rating_dataset_protocol, item_content_dataset_protocol])).

        rating(alice, first, 5).
        rating_count(1).
        rating_scale(1, 5).

        item(first).
        item(second).
        item(third).

        item_content(first, features([science,space,space])).
        item_content(second, features([science,space])).
        item_content(third, features([cooking])).

    :- end_object.

With the dataset loaded:

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::score(Model, alice, second, Score),
         jaccard_recommender::recommend(Model, alice, 3, Recommendations).

The score is `1.0`; recommendations are `[second-1.0,third-0.0]`.
Repeated `space` occurrences do not change the feature set. Neither candidate
has to appear in the training ratings.

The catalog is nonempty, with distinct instantiated atomic item identifiers.
Each item has exactly one content descriptor, and all rated items belong to
the catalog. Ratings are nonempty, finite numbers with distinct user-item
pairs and a matching positive count from the `rating_count/1` predicate.
The optional `rating_scale/2` predicate supplies inclusive bounds that are
checked but never used to clip relevance scores.

Use one descriptor representation consistently throughout the dataset:

- `features(Occurrences)` contains a proper list of arbitrary ground feature
  terms. Duplicates and order do not affect scores. Compound categorical
  features such as `genre(action)` and `language(en)` are supported.
- `vector(Pairs)` contains a proper list of unique ground `Feature-Weight`
  pairs. Weights must be numeric zero or one; both integers and floats are
  accepted. Zero means absence and is removed, while one means presence.
  Fractional and other positive weights raise `domain_error(binary_weight,
  Weight)`; negative and nonfinite weights retain the shared
  `non_negative_finite_weight` domain error.

Empty item content and entirely empty feature spaces are allowed. The
general content protocol also supports weighted vectors for other algorithms;
this library deliberately accepts only binary ones. The reusable
`jaccard_similarity` metric counts every supplied key even when its value is
zero. This recommender removes absent keys before calling that metric.


Options and profiles
--------------------

The `learn/3` predicate accepts the following options:

- `positive_threshold(Threshold)` selects `user_mean` (default) or a finite
    numeric threshold. Ratings at least equal to the threshold are selected;
    `user_mean` uses the mean of all the user's ratings.
- `min_feature_support(Count)` sets the minimum number of distinct selected
    items containing a retained feature to a positive integer. The default
    is `1`. Duplicate occurrences within an item count once, zero vector
    weights do not count, and unselected items contribute no support.
    Filtering does not assign weights to retained features. If no feature
    meets the minimum, the profile is empty.

Repeated valid options are accepted and the first occurrence wins.

The profile contains each retained feature exactly once. Rating magnitudes
and feature frequencies are not weights. "Positive feedback" here
means threshold-selected feedback: zero or negative ratings may be selected
when the dataset's scale and the chosen threshold permit them. No selected
items gives an empty profile.

For example, selected item sets `{a,b}` and `{b,c}` produce `{a,b,c}` under the
defaults. Candidate `{c,d}` receives `1/4`, not the average of its similarities
to the two items. With `min_feature_support(2)`, only `b` is retained, and
`{c,d}` receives `0.0`.

    | ?- jaccard_recommender::learn(article_tags, Model,
             [min_feature_support(2)]),
         jaccard_recommender::score(Model, alice, second, Score).

The score is `0.0`: Alice has only one selected rated item, and repeated
`space` occurrences in that item do not increase support.

Unlike a TF-IDF centroid, the profile gives equal influence to all retained
features. Support filtering can reduce broad profiles, but a broad profile
can still lower the score of a small subset candidate.


Scoring and recommendation
--------------------------

The score is the cardinality of the intersection of the user and item feature
sets divided by the cardinality of their union. It is returned as a float in
`[0.0,1.0]`. Empty unions, empty profiles, empty items, disjoint sets, and unknown
users score `0.0`. Unknown items raise `domain_error(catalog_item, Item)`.
Query identifiers must be instantiated atomic terms. Already-rated items may
be scored normally; their observed rating is not returned directly.

The implementation-specific `score_all/4` predicate accepts a proper list of
catalog identifiers and returns `Item-Score` pairs in input order, preserving
duplicates. Already-rated items are allowed. An empty list returns `[]` after
validating the model and user identifier. Unknown catalog items still raise
`domain_error(catalog_item, Item)`, including when the user is unknown.
The complete model is validated once per batch rather than once per item.

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::score_all(Model, alice,
             [second,first,second,third], Scores).

The result is `[second-1.0,first-1.0,second-1.0,third-0.0]`.

The implementation-specific `score_content/4` predicate scores a supplied
`features(Occurrences)` or binary `vector(Pairs)` descriptor against the
learned user profile. Either kind is accepted regardless of the training
representation. The descriptor obeys the same ground-feature and binary-weight
rules as training content, and is validated even for unknown users or empty
profiles. Novel features contribute to the union denominator; they need not
occur in the learned catalog.

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::score_content(Model, alice,
             features([science,biology]), FeatureScore),
         jaccard_recommender::score_content(Model, alice,
             vector([science-1.0,biology-1,space-0]), VectorScore).

Both scores are `1/3`, returned as floats. No item identifier is assigned and
the model, catalog, and profiles remain unchanged. Supplied content does not
become a recommendation candidate. The `score/4` and `score_all/4` predicates
require identifiers in the supplied model's catalog, including any explicit
catalog extensions.

The `recommend/4` predicate requires a positive integer limit and returns up
to that many `Item-Score` pairs from the complete model catalog. Every item
already rated by the user is excluded, even if its rating did not contribute
to the profile. Zero-score candidates remain eligible. Results are sorted by
descending score, with descending standard item order for ties. Unknown users
receive zero-score catalog candidates in that order; there is no popularity
fallback. With no eligible candidates, the result is `[]`.

There is no TF-IDF fitting, generalized or weighted Jaccard, rating prediction,
clipping, negative-feature subtraction, or raw-text preprocessing. Model
updates rebuild derived profiles rather than using an incremental learning
algorithm.


Catalog extension
-----------------

The implementation-specific `extend_catalog/3` predicate returns a new model
containing additional unrated items, without changing the original model or
retraining its profiles. Supply a proper list of `Item-Descriptor` pairs with
distinct instantiated atomic identifiers absent from the current catalog.
Existing identifiers raise `domain_error(new_catalog_item, Item)`, even when
their supplied content is identical. Duplicate identifiers in the extension
raise `domain_error(duplicate_item, Item)`.

Descriptors obey the same validation rules as training and must use the
existing catalog's representation. Unlike `score_content/4`, extension cannot
mix descriptor kinds or change the representation. New ground features and
empty content are allowed. An empty extension returns the original model
after validation.

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::extend_catalog(Model,
             [fourth-features([science,space,astronomy])], UpdatedModel),
         jaccard_recommender::score(UpdatedModel, alice, fourth, Score),
         jaccard_recommender::recommend(UpdatedModel, alice, 3, Recommendations).

The new item scores `2/3`, returned as a float, and appears between `second`
and `third` in recommendations. It remains unknown to the original model.
Ratings, profiles, scale, effective options, and extra diagnostic terms are
preserved. Catalog and feature counts are updated. Existing-item scores do
not change, including when support filtering is enabled: unrated additions
cannot contribute to profile support. Exported extended models preserve this
behavior.


Content replacement
-------------------

The implementation-specific `replace_content/3` predicate returns a new model
with replacement content for existing catalog items. Supply a proper list of
`Item-Descriptor` pairs with distinct instantiated atomic identifiers already
in the catalog. Unknown identifiers raise `domain_error(catalog_item, Item)`;
duplicate replacement identifiers raise `domain_error(duplicate_item, Item)`.
Use `extend_catalog/3` separately to add items.

Descriptors obey the same validation and strict binary-weight rules as training
and must match the catalog's representation. Empty content and identical
replacements are allowed. An empty replacement list returns the original
model after validation.

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::replace_content(Model,
             [first-features([cooking])], UpdatedModel),
         jaccard_recommender::score(UpdatedModel, alice, second, SecondScore),
         jaccard_recommender::score(UpdatedModel, alice, third, ThirdScore),
         jaccard_recommender::recommend(UpdatedModel, alice, 3, Recommendations).

The scores are `0.0` and `1.0`, respectively; recommendations are
`[third-1.0,second-0.0]`. Alice's selected item now contributes `cooking`
instead of `science` and `space`. The original model still scores `second`
as `1.0`.

Item vectors and all user profiles are rebuilt using the stored ratings and
effective options, including minimum feature support. Replacing selected
rated content can change profiles and other items' scores. Replacing only
unrated content leaves profiles unchanged. Ratings, scale, catalog identifiers,
and effective options are preserved; feature and nonempty-profile counts are
refreshed. Extra diagnostic terms and diagnostic order are retained.


Rating updates
--------------

The implementation-specific `update_ratings/3` predicate returns a new model
with inserted or replaced `rating(User,Item,Value)` records. Supply a proper
list with distinct user-item pairs. Existing pairs have their values replaced;
absent pairs are inserted. New users are allowed, but each item must already
belong to the catalog. Extend the catalog first when necessary. Duplicate
pairs in one batch raise `domain_error(duplicate_rating, User-Item)`, even if
their values are identical. Unknown items raise `domain_error(catalog_item,
Item)`.

User and item identifiers must be instantiated atomic terms. Values must be
finite numbers within the stored inclusive rating scale, when present.
Malformed entries raise `type_error(rating, Entry)`. An empty update list
returns the original model after validation. This operation does not delete
ratings or change the scale.

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::update_ratings(Model,
             [rating(alice,first,1),rating(alice,third,5),rating(bob,second,5)],
             UpdatedModel),
         jaccard_recommender::score(UpdatedModel, alice, second, AliceScore),
         jaccard_recommender::score(UpdatedModel, bob, first, BobScore),
         jaccard_recommender::recommend(UpdatedModel, alice, 3, Recommendations).

The scores are `0.0` and `1.0`, respectively; Alice's recommendations are
`[second-0.0]`. Her mean is now `3`, selecting only `third`; both rated items
are excluded from recommendations. Bob's new profile contains `science` and
`space`. The model contains three ratings and two users.

Profiles are rebuilt from the complete merged rating history. Under
`positive_threshold(user_mean)`, changing one rating can change the selection
of other rated items. Minimum support is recomputed from selected items, and
newly rated candidates are excluded from that user's recommendations regardless
of their value. Content, item vectors, scale, and effective options are
preserved. Rating, user, and nonempty-profile counts are refreshed while
retaining diagnostic order and extra terms.

Rating removal
--------------

The implementation-specific `remove_ratings/3` predicate returns a new model
without the requested `User-Item` ratings. Supply a proper list of pairs with
instantiated atomic identifiers. Missing pairs, including unknown users and
items, are ignored. Repeated pairs are accepted and remove each matching
rating only once. This makes removal idempotent: repeating the same request
on its result leaves the model unchanged.

All pairs are validated, even when none matches. Malformed entries raise
`type_error(pair, Entry)`; variable identifiers raise an instantiation error,
and non-atomic identifiers raise `type_error(atomic, Identifier)`. An empty
list or a request with no matching ratings returns the validated original
model. Removing every stored rating raises `domain_error(non_empty_ratings,
[])`; a model must retain at least one rating globally.

    | ?- jaccard_recommender::learn(article_tags, Model),
         jaccard_recommender::update_ratings(Model,
             [rating(alice,first,1),rating(alice,third,5),rating(bob,second,5)],
             WithRatings),
         jaccard_recommender::remove_ratings(WithRatings,
             [alice-third,alice-third,unknown-missing], UpdatedModel),
         jaccard_recommender::recommend(UpdatedModel, alice, 3, Recommendations),
         jaccard_recommender::remove_ratings(UpdatedModel,
             [alice-third,alice-third,unknown-missing], RetriedModel).

Alice's recommendations are `[second-1.0,third-0.0]`: her remaining rating
selects `first`, restoring the `science` and `space` profile, and `third`
becomes eligible again. Bob's profile is unchanged. There are two ratings
and two users, and `RetriedModel` equals `UpdatedModel`.

Removing a user's last rating is allowed when another user's rating remains.
That user's profile disappears, and subsequent scoring and recommendation
follow the existing unknown-user behavior. Removed ratings no longer exclude
their items from that user's recommendations; scores are computed normally,
not forced to zero. Other users' ratings and exclusions remain intact.

Profiles, user means, feature support, and rating/user/nonempty-profile counts
are rebuilt from the remaining history. Catalog items, content, item vectors,
scale, and effective options are preserved, as are diagnostic order and extra
terms. Rating removal does not delete catalog items.

Content replacement, rating updates, and rating removal validate the original
model once and return portable ground data independent of the training dataset.
The original model remains unchanged. Derived data is rebuilt rather than
updated in constant time; exported updated models preserve scoring and
recommendation behavior.


Models, diagnostics, and export
------------------------------

Learned models use the following term representation:

    jaccard_model(Ratings, Contents, ItemVectors, Profiles, Scale, Diagnostics)

It is portable ground data, independent of the dataset object when scoring.
Ratings and content declarations are canonical; content feature occurrences
are retained in `Contents`, while `ItemVectors` and `Profiles` contain sorted
unique `Feature-1` pairs. Every learned user has a profile, including an empty
one when necessary.

The `Diagnostics` argument is a list of diagnostic terms, including
`model(jaccard_recommender)`, `rating_count/1`, `options/1`,
`user_count/1`, `item_count/1`, `content_representation/1`, `feature_count/1`,
and `non_empty_profile_count/1`. Feature count is the number of distinct present
features across the full catalog. Additional diagnostic terms are allowed;
mandatory diagnostic indicators must be unique and consistent.
Effective options include both the rating threshold and minimum feature
support settings.

The `valid_recommender/1` and `check_recommender/1` predicates revalidate
stored content, ratings, options, and scale, reconstruct vectors and profiles,
and verify diagnostics without instantiating the model. Public scoring
validates the complete model, so validation may cost more than the individual
set comparison. Batch scoring and recommendation validate once before scoring
all requested items or candidates. Supplied-content scoring validates the
model once and canonicalizes its input descriptor separately.

The `export_to_clauses/4` and `export_to_file/4` predicates serialize the
complete model under the chosen predicate functor. The `print_recommender/1`
predicate displays its template and data. Exported models preserve scoring
and recommendation behavior.


Limitations
-----------

- Only categorical feature sets and binary vectors are supported. There
    is no TF-IDF fitting, generalized or weighted Jaccard, or raw-text
    preprocessing.
- Profiles are binary feature sets, optionally filtered by support. Support
    can determine feature retention but does not weight retained features;
    rating magnitudes and within-item frequencies do not increase influence.
    Negative feedback does not subtract features.
- Broad union profiles can lower the relevance of small subset candidates,
    even when those candidates contain only features the user likes. Support
    filtering can reduce broad profiles but does not eliminate this behavior.
- Training requires a nonempty explicit-rating dataset; an implicit-only
    interaction dataset is not supported.
- Scores are not calibrated rating predictions. Unknown users and empty
    profiles receive zero scores, with no popularity or other cold-start
    fallback.
- Catalog additions, existing-content replacement, and rating insertion,
    replacement, or removal are supported as immutable model updates. Item
    removal and rating-scale changes require retraining; removing every stored
    rating is prohibited. Content and feedback updates rebuild derived
    profiles; there is no incremental optimization.
- Public scoring validates the complete model and reconstructs derived data.
    For large catalogs, this can cost more than the individual set comparison;
    `score_all/4` amortizes validation across a batch, and recommendation also
    performs this validation once per call. Repeated separate scoring calls
    still repeat the full validation.

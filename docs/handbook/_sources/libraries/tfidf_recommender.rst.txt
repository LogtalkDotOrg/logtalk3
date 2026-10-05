.. _library_tfidf_recommender:

``tfidf_recommender``
=====================

Classical content-based filtering using sparse item vectors,
positive-feedback Rocchio-style centroid profiles, and cosine
similarity. Feature occurrence lists are weighted by TF-IDF through
``text_vectorization``; externally weighted vectors can also be
supplied. The object imports ``recommender_common`` and the
``item_content_dataset_validation`` and ``similarity_metric_common``
categories, implementing ``recommender_protocol``. The ``score/4``
predicate returns cosine relevance rather than a predicted rating.

API documentation
-----------------

Open the
`../../apis/library_index.html#tfidf-recommender <../../apis/library_index.html#tfidf-recommender>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(tfidf_recommender(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(tfidf_recommender(tester)).

Datasets and usage
------------------

Training accepts one object implementing both
``rating_dataset_protocol`` and ``item_content_dataset_protocol``. For
example:

::

   :- object(article_content,
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

::

   | ?- tfidf_recommender::learn(article_content, Model),
        tfidf_recommender::score(Model, alice, second, Score),
        tfidf_recommender::recommend(Model, alice, 3, Recommendations).

The score is approximately ``0.948683``; recommendations contain
``second`` followed by ``third``, whose score is zero. Neither candidate
needs a rating.

The ``item/1`` predicate declares the complete, nonempty catalog, with
unique atomic identifiers. The ``item_content/2`` predicate declares
exactly one descriptor per catalog item, with no extra items. All rated
items must belong to this catalog. Ratings are nonempty, numeric and
finite, with unique user/item pairs and a matching positive count. An
optional rating scale is validated.

Descriptors use one of these representations consistently throughout a
dataset:

- ``features(Occurrences)`` contains a proper list of arbitrary ground
  feature terms, with repetitions representing term counts. Tokens,
  genre tags, and n-grams are all usable. Text tokenization and
  preprocessing are external.
- ``vector(Pairs)`` contains a proper list of ``Feature-Weight`` pairs
  with unique ground keys and finite nonnegative weights. The library
  sorts keys and removes exact zero weights. No TF-IDF fitting is
  performed in this mode.

Empty item content is allowed. Feature-list mode requires a nonempty
learned vocabulary; preweighted mode permits an entirely empty feature
space.

Options
-------

The ``learn/3`` predicate accepts these options:

- ``positive_threshold(user_mean)`` selects ratings at least equal to
  the user's mean over all their ratings. A finite numeric threshold can
  be supplied instead.
- ``profile_weighting(uniform)`` averages selected item vectors equally.
  The alternative ``rating`` uses raw ratings as weights; every selected
  rating must then be strictly positive, including ratings on
  empty-content items.
- ``normalization(l2)`` unit-normalizes item vectors before averaging
  them. The alternative ``none`` preserves vector magnitude, allowing
  longer documents or externally larger weights to influence profile
  direction more strongly.
- ``vectorizer_options([])`` forwards options to ``text_vectorizer``.
  Defaults are raw TF-IDF and smoothed IDF. Binary/count weighting,
  alternative TF-IDF factors, IDF rules, frequency bounds and vocabulary
  limits are available. Nested ``normalization`` options are rejected;
  use the top-level option instead. Nonempty vectorizer options are
  rejected for preweighted content.

Repeated valid options are accepted; the first occurrence takes
precedence, including inside ``vectorizer_options``. The
``valid_option/1`` and ``default_option/1`` predicates remain public.
The ``recommender_options/2`` predicate returns effective top-level
options; fitted vectorizer diagnostics record its effective options.

Training and scoring
--------------------

Vocabulary and document frequencies are fitted once over ALL catalog
items, including unrated items and empty-content documents. Under
default weighting:

::

   IDF(f) = log((1 + catalog_size) / (1 + document_frequency(f))) + 1
   weight(i,f) = occurrence_count(i,f) * IDF(f)

Let ``x(i)`` be the item vector after the chosen normalization. The
profile for user ``u`` is a positive-feedback centroid:

::

   profile(u) = sum(a(u,i) * x(i)) / sum(a(u,i))

Only threshold-selected ratings contribute. Weights ``a(u,i)`` are one
or the strictly positive raw rating. Empty selected vectors still
contribute to the denominator. No selected items gives an empty profile.
Profiles are not separately unit-normalized; cosine normalization
happens when scoring:

::

   score(u,i) = cosine(profile(u), x(i))

The ``score/4`` predicate returns a relevance value in ``[0,1]``, not a
rating estimate. Missing sparse features contribute zero. Already-rated
items can be scored normally; observed ratings are not returned
directly. Unknown users, empty profiles, empty item vectors, and
disjoint feature sets score zero. An unknown item raises
``domain_error(catalog_item, Item)``. Query identifiers must be
instantiated atomic terms. There is no baseline fallback or clipping to
the rating scale.

The ``recommend/4`` predicate returns up to positive integer ``N``
unrated catalog items as ``Item-Score`` pairs. It excludes ALL
previously rated items, not just positive ones, and includes zero-score
candidates. Results use decreasing score with descending standard item
order for ties. Unknown users therefore receive zero-score catalog
recommendations. No remaining candidates returns ``[]``; fewer than
``N`` candidates returns all of them. Scores agree with direct
``score/4`` calls, without repeating full model validation for each
candidate.

For orthogonal unit vectors ``[x-1]`` and ``[y-1]``, equal selected
ratings produce ``[x-0.5,y-0.5]``, with cosine ``1/sqrt(2)`` to either
axis and one to ``[x-1,y-1]``. Selected ratings two and four with rating
weighting produce ``[x-1/3,y-2/3]``, scoring ``1/sqrt(5)`` and
``2/sqrt(5)`` respectively. Evaluation must remove held-out ratings
explicitly; catalog content may still be available for unrated or
held-out items.

Models, diagnostics, and export
-------------------------------

The ``learn/2`` and ``learn/3`` predicates return a model with the
following structure:

::

   tfidf_model(Ratings, Contents, ItemVectors, Profiles, Vectorizer, Scale, Diagnostics)

``Ratings`` is a sorted list of ``rating(User, Item, Rating)`` terms.
``Contents`` contains sorted ``Item-features(Occurrences)`` or
``Item-vector(Pairs)`` entries; feature occurrences are sorted without
discarding repetitions. ``ItemVectors`` contains sorted ``Item-Vector``
entries. ``Profiles`` contains sorted ``User-Vector`` entries for every
observed user, including users with empty profiles. ``Vectorizer`` is
the fitted ``text_vectorizer_model(Features, Diagnostics)`` term or
``none`` for externally weighted vectors. ``Scale`` is ``none`` or
``scale(Min,Max)`` and validates training ratings only.

Diagnostics contain ``model(tfidf_recommender)``,
``rating_count(Count)``, ``options(Options)``, ``user_count(Count)``,
``item_count(Count)``, ``content_representation(features|vectors)``,
``feature_count(Count)``, and ``non_empty_profile_count(Count)``. Item
count covers the full catalog. Feature count is fitted vocabulary size
in feature mode, including classic-IDF zero-weight features, or the
union of nonzero supplied keys in vector mode. Additional diagnostic
terms are permitted.

The ``check_recommender/1`` and ``valid_recommender/1`` predicates
verify canonical source data, options, scale, catalog coverage, fitted
vectorizer statistics, item vectors, profiles and diagnostic counts
without instantiating the model. The ``diagnostics/2`` predicate returns
metadata; the ``diagnostic/2`` predicate enumerates its terms. The
``export_to_clauses/4`` predicate emits one ``Functor(Model)`` fact. The
``export_to_file/4`` predicate writes this fact with the shared training
header; load the library before restoring it. All model data is
embedded, with no dependency on a live dataset for subsequent scoring.
The ``print_recommender/1`` predicate prints the template and complete
model, including effective options and summary counts.

Validation errors
-----------------

Shared errors cover invalid options, rating identifiers, duplicate
ratings, count mismatches, nonnumeric ratings, and invalid or violated
rating scales. Content errors distinguish empty catalogs, duplicate
items, incomplete/extra content declarations, rated items outside the
catalog, malformed/mixed content descriptors, improper or nonground
lists, duplicate vector keys, nonpair vector entries, and
negative/nonnumeric/nonfinite weights. Empty learned vocabulary and
inconsistent vectorizer frequency bounds propagate vectorizer errors.
Rating-weighted training rejects nonpositive selected ratings with
``domain_error(positive_rating_weight, Rating)``. Query model,
identifier and recommendation-count errors follow the inherited protocol
declarations.

Limitations
-----------

Training and model validation are in-memory. Duplicate checks, sparse
lookup, and existing cosine overlap use lists and can be quadratic.
Public model validation refits vectors and recomputes profiles, favoring
consistency over large-catalog throughput; recommendation validates once
before scoring all candidates. Weight and magnitude rescaling avoid
unnecessary centroid overflow, but vectorizer arithmetic and user means
retain backend numeric limits. There is no epsilon in selection or
sparse zero removal. No raw-text pipeline, negative-feedback Rocchio
subtraction, rating calibration, popularity fallback, implicit-only
training, incremental update, or post-training new-item API is provided.
New catalog content requires refitting the model and IDF statistics.

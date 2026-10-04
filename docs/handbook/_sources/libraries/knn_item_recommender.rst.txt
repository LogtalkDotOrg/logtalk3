.. _library_knn_item_recommender:

``knn_item_recommender``
========================

Item-based k-nearest-neighbor collaborative filtering with
similarity-weighted raw ratings. The object imports
``recommender_common`` and implements ``recommender_protocol``. The
``score/4`` predicate returns a predicted rating. Datasets implement
``rating_dataset_protocol`` with atomic identifiers, one numeric rating
per user/item pair, a matching positive count, and an optional numeric
ordered rating scale. The user-kNN and Slope One libraries are not
dependencies.

API documentation
-----------------

Open the
`../../apis/library_index.html#knn-item-recommender <../../apis/library_index.html#knn-item-recommender>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(knn_item_recommender(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(knn_item_recommender(tester)).

Examples
--------

::

   | ?- logtalk_load(recommender_protocols('test_datasets/movie_ratings')).
        knn_item_recommender::learn(movie_ratings, Model),
        knn_item_recommender::score(Model, alice, m6, Rating),
        knn_item_recommender::recommend(Model, alice, 3, Recommendations).

Options
-------

The ``learn/3`` predicate accepts the following options:

- ``k(3)``: positive maximum neighborhood size.
- ``similarity_metric(cosine_similarity)``: ground identifier of a
  loaded object exposing an implemented public ``similarity/3``,
  including parametric strategies. The dependency supplies cosine,
  Pearson, Jaccard, inverse MSD, and Spearman objects.
- ``min_overlap(1)``: positive minimum co-rater count.
- ``min_similarity(0.0)``: non-negative finite numeric threshold.
- ``clip_to_scale(true)``: ``true`` or ``false``; clips estimates and
  fallbacks to an inclusive declared scale. With no scale it is a no-op.

Metrics receive complete item profiles and retain their own
sparse-vector semantics. A strategy must produce exactly one finite
numeric score; otherwise prediction throws
``domain_error(similarity_score, Scores)``. Provider exceptions
propagate. Reloading an exported model also requires its custom strategy
object, if any.

The learning options can be retrieved from a learned recommender model
using the ``recommender_options/2`` predicate.

Prediction and recommendation
-----------------------------

For user ``u`` and target item ``i``, candidates are other items ``j``
rated by ``u``. Unrated items cannot consume neighbor slots. Overlap,
strictly positive score, and minimum score are checked before selecting
top ``k``. The estimate is the weighted average of the active user's raw
ratings:

::

   sum(s(i,j) * rating(u,j)) / sum(s(i,j))

Weights are rescaled by the maximum selected score. Equal-score
neighbors use descending standard item order, independently of rating
enumeration. This is not adjusted cosine: the two-vector metric
interface does not supply user-mean context. Predicting an already-rated
item still estimates it and excludes that item from neighbors;
evaluation must remove held-out observations from training.

Unknown items, unknown users, or no usable neighbors trigger the known
user's mean, otherwise the known item's mean, otherwise the global mean.
Clipping follows this fallback or the collaborative estimate. Query
identifiers must be instantiated atomic terms, but need not have
appeared in training.

The ``recommend(Model, User, N, Recommendations)`` predicate returns up
to positive integer ``N`` unrated training-catalog ``Item-Score`` pairs,
descending by score with descending standard identifier order for ties.
It scores through the same public prediction path. Unknown users see the
entire catalog; no candidates returns ``[]``.

For ``u: a=1,b=3`` and ``v: a=2,b=4,c=5``, cosine scores for ``c``
against ``a`` and ``b`` are ``2/sqrt(5)`` and ``4/5``. The prediction
for ``u,c`` is:

::

   (2/sqrt(5) + 3*0.8) / (2/sqrt(5) + 0.8)

With ``k(1)`` only ``a`` contributes and the estimate is one.

Models, diagnostics, and export
-------------------------------

The ``learn/2`` and ``learn/3`` predicates return the learned
recommender model as a term with the following structure:

::

   knn_item_model(Ratings, Profiles, GlobalMean, Scale, Diagnostics)

Ratings are ``rating(User, Item, Rating)`` terms. Sorted profiles
contain ``Item-profile(SortedUserRatingPairs, Mean)`` entries. The scale
is ``none`` or ``scale(Min, Max)``. Diagnostics contain exactly one
``model(knn_item_recommender)``, ``rating_count(Count)``,
``options(Options)``, ``user_count(Count)``, ``item_count(Count)``, and
``neighbor_axis(item)``.

The ``valid_recommender/1`` predicate checks records, effective options,
profile coverage, means, scale, and diagnostics without binding
incomplete models. The ``check_recommender/1`` predicate throws on
invalid models. The ``diagnostics/2`` predicate returns the list, and
the ``diagnostic/2`` predicate enumerates entries.

The ``export_to_clauses(Dataset, Model, Functor, Clauses)`` predicate
exports one ``Functor(Model)`` fact; the ``export_to_file/4`` predicate
writes it with the common header. The ``print_recommender/1`` predicate
prints the template and complete model, including options and counts.

Limitations
-----------

No all-pairs similarity cache, incremental updates, signed weighting, or
adjusted cosine is provided. List-based dataset validation can be
quadratic in rating count. Public prediction recomputes profiles to
validate the model, and recommendation repeats validation for each
candidate. Arithmetic follows backend numeric precision. Rescaled
weights avoid avoidable weight overflow, not overflow in extreme rating
sums.

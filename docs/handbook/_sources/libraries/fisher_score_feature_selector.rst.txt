.. _library_fisher_score_feature_selector:

``fisher_score_feature_selector``
=================================

This library implements a univariate filter selector for numeric
features and categorical targets using Fisher scores. The selector
reuses the ``feature_selection_protocols`` library for dataset
validation, stable ranking, selection strategies, diagnostics, model
validation, and export. The ``fisher_score`` object is provided by
``feature_selection_protocols``, implements the shared scoring protocol,
and can be used independently of the selector.

API documentation
-----------------

Open the
`../../apis/library_index.html#fisher-score-feature-selector <../../apis/library_index.html#fisher-score-feature-selector>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(fisher_score_feature_selector(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(fisher_score_feature_selector(tester)).

The tests cover feature selection, unbalanced multiclass scores, missing
sample counts, stable ties, malformed models, options, diagnostics, and
clause and file exports. Tests for the ``fisher_score`` metric itself
are included in the ``feature_selection_protocols`` library.

Fisher scores
-------------

The ``score/3`` predicate computes the ratio ``B/W``, where ``B`` is
``sum(n_c * (mean_c - mean)^2)`` and ``W`` is
``sum(sum((value - mean_c)^2))`` over the observed target classes.
Equivalently, the within-class denominator is ``sum(n_c * variance_c)``
using population class variances. Both sums use the same scaled values;
their common scaling cancels in the ratio.

The input lists must be proper lists of equal length. Complete feature
values must be numeric and complete target labels must be atomic. An
observation with an unbound feature value or target is excluded without
binding that variable. Numeric target labels are categorical class
codes, not regression targets.

Empty samples, single-class samples, and constant features score
``0.0``. Positive between-class scatter with zero within-class scatter
scores ``1.0e10``; finite ratios reaching that cap tie at the same
score. Singleton classes are supported, including perfect separation
when every observed class contains one observation. No p-value is
computed.

The existing ANOVA metric computes ``(B/(c-1))/(W/(n-c))`` instead of
``B/W``. For nondegenerate, uncapped results, its score equals the
Fisher score multiplied by ``(n-c)/(c-1)``. Rankings agree only when
those counts agree across features. Per-feature missing observations can
make the factors differ. ANOVA retains its separate requirement for
positive within-class degrees of freedom.

Selection and options
---------------------

The dataset object must implement ``feature_dataset_protocol`` and
declare each feature with ``attribute_values(Feature, continuous)``.
Categorical feature declarations are rejected with a
``domain_error(feature_type, ...)`` exception, rather than silently
turning category codes into measurements.

The ``learn/2`` predicate uses the default
``selection_strategy(top_k(10))`` option. The ``learn/3`` predicate
accepts the following strategies:

- ``selection_strategy(top_k(K))``: selects up to ``K`` features, where
  ``K`` is a positive integer. Fewer candidates are not an error.
- ``selection_strategy(threshold(T))``: selects every feature scoring at
  least the numeric threshold ``T``.
- ``selection_strategy(all)``: selects every candidate feature.

Repeated options are accepted and the first occurrence takes precedence.
Scores are sorted numerically in decreasing order; ties preserve feature
declaration order. Top-k selection can include zero-scoring features
when the requested count exceeds the number with positive scores.

Models and diagnostics
----------------------

The ``learn/2-3`` predicates return selector terms with the following
shape:

::

   fisher_score_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The ``feature_scores/2`` predicate returns all sorted ``Feature-Score``
pairs. The ``selected_features/2`` predicate returns the selected
feature names. The ``diagnostics/2``, ``diagnostic/2``, and
``selector_options/2`` predicates expose metadata and effective options.
Diagnostics include ``model/1``, ``example_count/1``, ``options/1``,
``candidate_count/1``, ``selected_count/1``,
``scoring_metric(fisher_score)``, and ``complete_cases(FeatureCounts)``.
The latter records a ``Feature-Count`` pair for each declared feature;
subtracting that count from the example count gives excluded
observations.

The ``check_selector/1`` predicate verifies ground model structure,
unique and decreasing scores, the recorded metric and options, selection
consistency, candidate and selected counts, and per-feature sample
counts. The ``valid_selector/1`` predicate succeeds only for valid
models and does not bind malformed partial models. The
``export_to_clauses/4`` and ``export_to_file/4`` predicates preserve the
complete selector term.

Limitations
-----------

Fisher scores assess each feature independently. They do not capture
interaction-only signals or remove redundant features. Scores depend on
the complete-case samples and are capped rather than representing
infinity. Arithmetic remains subject to backend floating-point precision
and range. No automatic choice of feature count or threshold,
regression-target scoring, or fitted preprocessing transform is
provided.

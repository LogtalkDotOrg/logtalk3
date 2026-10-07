.. _library_mrmr_feature_selector:

``mrmr_feature_selector``
=========================

This library implements minimum redundancy maximum relevance feature
selection using the mutual-information difference (MID) criterion. Both
relevance and redundancy are raw empirical mutual information in bits.
MIQ, normalized information measures, thresholds, and alternative
scoring metrics are not supported.

API documentation
-----------------

Open the
`../../apis/library_index.html#mrmr-feature-selector <../../apis/library_index.html#mrmr-feature-selector>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(mrmr_feature_selector(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(mrmr_feature_selector(tester)).

The loader explicitly loads
``feature_selection_protocols(feature_redundancy)`` after the
preparation infrastructure. The unit tests include independent
contingency-table and greedy calculations, redundant signals, stable
ties, negative MID steps, joint missing observations, discretization
provenance, option validation, malformed models, and clause/file
exports.

Preparation and options
-----------------------

The dataset must implement ``feature_dataset_protocol``. Feature
declarations are ``continuous`` or nonempty lists of atomic categorical
labels. Targets are atomic class labels, including numeric class codes,
not regression measurements. Rows missing the target or any candidate
feature are excluded jointly before fitting or scoring. Omitted features
and unbound values are missing observations; learning does not bind
them. At least two usable rows are required, even with an empty feature
vocabulary. Otherwise learning throws
``domain_error(mrmr_usable_examples, Count)``. Empty datasets retain the
shared ``domain_error(non_empty_examples, Dataset)`` exception.

The ``learn/2`` predicate uses these defaults. The ``learn/3`` predicate
accepts:

- ``selection_strategy(top_k(K))``, default ``top_k(10)``, with positive
  integer ``K``. Exactly ``min(K, CandidateCount)`` features are
  selected.
- ``discretization(Specification)``, default ``equal_frequency(10)``,
  for continuous features. Specifications are ``categorical``,
  ``equal_width(B)``, or ``equal_frequency(B)``, with positive integer
  bin counts.
- ``feature_discretization(Feature, Specification)``, overriding the
  global specification for that declared feature.

Categorical declarations preserve their labels unless explicitly
overridden. Binning specifications on categorical declarations require
numeric values. Repeated options are accepted; the first occurrence
takes precedence. All options, including later occurrences, must be
valid. Unknown override features and invalid feature domains are
rejected by shared preparation.

Each feature is discretized once using the common eligible rows. Both
its target relevance and every pairwise redundancy use that prepared
column; bins are never refitted after selecting a feature. Diagnostics
record the specifications and occupied-category counts, not fitted cut
points or a transform for future observations.

Selection
---------

The first feature maximizes ``I(Feature; Target)``. Each subsequent
feature maximizes
``I(Feature; Target) - mean(I(Feature; SelectedFeature))`` over the
remaining candidates. Exact numeric ties preserve feature declaration
order at every step. Zero and negative MID values do not stop top-k
selection. Constant features and constant targets have zero relevance.

Relevance is computed once per candidate. Each remaining candidate
retains a running redundancy sum. Only the pairs between the
just-selected feature and the remaining candidates needed for another
step are evaluated. No full pairwise matrix is built and no unordered
pair is recomputed. Selecting ``s`` out of ``p`` candidates performs
``(s-1)*p - s*(s-1)/2`` redundancy evaluations when ``s >= 1``, and zero
when ``s = 0``. At full selection this is ``p*(p-1)/2``; requesting one
feature performs no pair evaluations.

Models and diagnostics
----------------------

The ``learn/2-3`` predicates return models of the following form:

::

   mrmr_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The ``feature_scores/2`` predicate returns all candidates sorted by
static target relevance, preserving declaration order for ties. The
``selected_features/2`` predicate returns the greedy selection order,
which need not be a prefix of that ranking. The ``diagnostics/2``,
``diagnostic/2``, and ``selector_options/2`` predicates expose metadata
and effective options.

Diagnostics include ``model(mrmr_feature_selector)``,
``example_count/1``, ``options/1``, ``candidate_count/1``,
``selected_count/1``, ``selection_criterion(mid)``,
``scoring_metric(mutual_information)``,
``redundancy_metric(mutual_information)``, ``redundancy_evaluations/1``,
and ``selection_trace/1``. The trace contains these terms in selected
order:

::

   step(Feature, Relevance, MeanRedundancy, MID)

The first step has zero mean redundancy. Preparation diagnostics include
``preparation_mode(joint)``, ``usable_example_count/1``,
``excluded_example_count/1``, ``complete_cases(FeatureCounts)``,
``discretization(FeatureSpecifications)``, and
``occupied_categories(FeatureCategoryCounts)``. These three feature-pair
lists preserve declaration order; every complete-case count equals the
joint usable count.

The ``check_selector/1`` predicate checks ground structure, unique and
sorted scores, candidate membership, selection and trace lengths/order,
relevance identity, MID formulas, first-step selection, effective
options, metrics, preparation provenance/counts, and the exact
evaluation count. A variable model raises an instantiation error;
malformed nonvariable models raise ``domain_error(selector, Model)``.
The ``valid_selector/1`` predicate fails without binding partial models.
Validation checks internal consistency; without training columns it
cannot reconstruct the pairwise information or prove later greedy
winners. It does not impose a relevance-prefix rule.

The ``print_selector/1`` predicate prints the model and its template.
The ``export_to_clauses/4`` and ``export_to_file/4`` predicates export
the complete model, including the selection trace, in a single fact.

Limitations
-----------

The method is a greedy filter, not a globally optimal subset search.
Joint complete-case filtering can discard many rows. Empirical MI
depends on sample size and discretization and can miss interaction-only
signals. Arithmetic and exact tie decisions remain subject to backend
floating-point precision. There is no automatic feature-count selection,
fitted-transform export, regression metric, or additional dependency
beyond the shared feature-selection infrastructure.

.. _library_chi_square_feature_selector:

``chi_square_feature_selector``
===============================

Use this library to select features associated with a categorical
target. It scores categorical and continuous features independently
using chi-square statistics or their documented variants, discretizing
continuous values before scoring. It reuses the typed discretization,
sparse contingency arithmetic, stable ranking, selection, model
validation, and export facilities of ``feature_selection_protocols``.

API documentation
-----------------

Open the
`../../apis/library_index.html#chi-square-feature-selector <../../apis/library_index.html#chi-square-feature-selector>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(chi_square_feature_selector(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(chi_square_feature_selector(tester)).

The tests cover categorical, mixed, and continuous datasets, independent
binary and multiclass numerical references, both binning methods,
missing observations, stable ties, options, malformed inputs and models,
reflection, diagnostics, printing, and clause and file exports.

Scores
------

The default ``score_variant(raw)`` option computes the uncorrected
Pearson chi-square statistic ``sum((Observed-Expected)^2/Expected)``,
where ``Expected = RowCount * TargetCount / CompleteCount``. Empty cells
contribute to the statistic; sparse counting does not omit their
contribution. Only occupied feature categories and target classes define
the table dimensions.

The ``score_variant(normalized)`` option computes Cramer's V:
``sqrt(ChiSquare/(CompleteCount * min(CategoryCount-1, ClassCount-1)))``.
It ranges from zero to one and is not chi-square divided by sample size
alone. The recorded metrics are ``chi_square_score`` and
``cramers_v_score``, respectively. Empty complete-case samples, constant
features, and single-class samples score zero. Numeric target labels are
class codes, not regression targets.

The ``score_variant(yates)`` option uses ``chi_square_yates_score``. For
occupied two-by-two tables it sums
``max(0, abs(Observed-Expected)-0.5)^2/Expected`` over all four cells,
including zero-observation cells. Larger tables retain Pearson scoring
and degenerate tables score zero. This continuity correction does not
provide p-values or validate expected-count assumptions.

The ``score_variant(bias_corrected)`` option uses
``cramers_v_bias_corrected_score``, applying Bergsma's correction to
Pearson chi-square and the occupied table dimensions. With complete-case
count ``n`` and occupied dimensions ``r`` and ``c``, it computes:

::

   PhiSquared = max(0, ChiSquare/n - (r-1)*(c-1)/(n-1))
   CorrectedRows = r - (r-1)^2/(n-1)
   CorrectedColumns = c - (c-1)^2/(n-1)
   V = sqrt(PhiSquared/min(CorrectedRows-1, CorrectedColumns-1))

Scores are clamped to ``[0.0, 1.0]``. A nonpositive corrected
denominator scores zero as an insufficient-sample convention, including
two perfectly separated observations. This is not Yates-corrected V or a
p-value, and does not eliminate every source of sampling bias.

Preparation and options
-----------------------

The dataset object must implement ``feature_dataset_protocol``. It
declares continuous features with
``attribute_values(Feature, continuous)`` and categorical features with
``attribute_values(Feature, Domain)``, where ``Domain`` is a non-empty
list of atomic values. Known continuous values must be numbers; known
categorical values must belong to their declared domains; known targets
must be atomic.

The ``learn/2`` predicate uses the defaults. The ``learn/3`` predicate
accepts an options list as its last argument:

- ``score_variant(Variant)`` selects ``raw`` (default), ``normalized``,
  ``yates``, or ``bias_corrected``.
- ``preparation_mode(Mode)`` selects ``per_feature`` (default) or
  ``joint``.
- ``expected_count_policy(Policy)`` selects ``ignore`` (default) or
  ``minimum(Minimum)``, with a positive numeric minimum.
- ``discretization(Specification)`` selects ``equal_frequency(Bins)``,
  ``equal_width(Bins)``, or ``categorical``, applied only to continuous
  features. The default is ``equal_frequency(10)``; bin counts must be
  positive integers. The ``categorical`` value preserves numeric values
  as category codes.
- ``feature_discretization(Feature, Specification)`` overrides the
  default for one declared feature. The specification can be
  ``categorical``, ``equal_frequency(Bins)``, or ``equal_width(Bins)``.
- ``selection_strategy(top_k(K))``, default ``top_k(10)``, selects up to
  the positive integer ``K`` features.
- ``selection_strategy(all)`` selects every candidate.
- ``selection_strategy(threshold(T))`` selects scores at least the
  numeric ``T``.

Repeated options and repeated overrides are accepted; the first
applicable occurrence wins. All supplied options are validated,
including later occurrences. Unknown override feature names are
rejected.

Default preparation is per feature: observations with an unbound or
absent feature value or an unbound target are excluded from that
feature's score without imputing values. Binning is fitted to its
complete cases only. Categorical features retain their declared
categories unless explicitly overridden. Equal-width bins partition the
observed range; equal-frequency bins use observed quantile cuts without
splitting equal values. Occupied category counts can be smaller than
requested bin counts. Applying numeric binning to an atomic non-numeric
category is an error, not an implicit category encoding.

Joint preparation excludes observations missing any candidate value or
their target before fitting bins. All features are then scored on the
same rows. An empty common sample produces zero scores. Joint deletion
aligns samples; it does not remove missing-data bias and can discard
many observations.

The minimum expected-count policy checks every cell expectation,
including unobserved cells, using occupied marginal categories. Equality
with the minimum is accepted. The ``learn/3`` predicate throws
``domain_error(chi_square_expected_count, Feature-expected(Observed, Required))``
when a nondegenerate feature table has a smaller expectation, aborting
learning rather than omitting that feature. Empty, constant, and
single-class tables retain zero scores. The policy applies to every
score variant and does not assert a universal threshold or provide a
significance test.

Scores are sorted in decreasing order, preserving declaration order on
ties. Top-k selection may include zero-scoring features. Thresholds
should be chosen for the selected score variant.

Models and diagnostics
----------------------

The ``learn/2`` and ``learn/3`` predicates return models using the
following term representation:

::

   chi_square_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The ``feature_scores/2`` predicate returns sorted ``Feature-Score``
pairs. The ``selected_features/2`` predicate returns selected names. The
``diagnostics/2``, ``diagnostic/2``, and ``selector_options/2``
predicates expose metadata and effective options. The ``Diagnostics``
argument is a list of diagnostic terms, including ``model/1``,
``example_count/1``, ``options/1``, ``candidate_count/1``,
``selected_count/1``, ``scoring_metric/1``,
``complete_cases(FeatureCounts)``,
``discretization(FeatureSpecifications)``,
``occupied_categories(FeatureCounts)``, and ``preparation_mode(Mode)``.
Joint preparation also reports ``usable_example_count/1`` and
``excluded_example_count/1``, which partition ``example_count/1``.
Counts and specifications follow declaration order, not score order.

The ``check_selector/1`` predicate checks ground structure, unique
sorted scores, selection consistency, metric identity determined by
options, valid options, candidate and selected counts, complete-case
counts, and preparation-mode consistency. In joint mode all
complete-case counts must equal the usable count. The
``valid_selector/1`` predicate fails for malformed models without
binding partial models. These checks are structural; they do not
recompute scores from a dataset or recheck expected counts without
training tables. The ``export_to_clauses/4`` and ``export_to_file/4``
predicates preserve the complete model. The ``print_selector/1``
predicate prints its template and contents.

Limitations
-----------

Univariate scores do not detect interaction-only signals or remove
redundant features. Raw chi-square depends on sample size and table
dimensions; ordinary normalization does not eliminate sampling bias, and
corrected V does not provide a general bias correction. Default
per-feature missingness can make samples incomparable; joint preparation
trades alignment for fewer observations. Discretization affects the
scores. Binning boundaries are not stored as a transformation for future
examples. No p-values, significance decisions, general small-sample bias
correction, regression scoring, or automatic feature-count selection is
provided. Arithmetic uses backend floating-point precision.

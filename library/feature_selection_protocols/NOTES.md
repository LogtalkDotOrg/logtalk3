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


`feature_selection_protocols`
=============================

Use this library when implementing a feature selector or providing a
dataset for one. It supports filter, nearest-neighbor, and embedded
algorithms that choose the features most relevant to a learning task.
Datasets are represented as objects implementing `feature_dataset_protocol`,
generalizing `classification_protocols::dataset_protocol` (reusing its
`attribute_values/2` predicate) to also support regression targets and
fully unsupervised use. Selectors are represented as objects importing
the `feature_selector_common` category. This category provides shared
auxiliary predicates for dataset validation, feature-matrix utilities,
scoring every candidate feature via a pluggable metric, two selection
strategies (top-k and threshold), diagnostics metadata, export, and
pretty-printing support.

Scoring criteria are exposed as pluggable strategy objects implementing
`feature_scoring_protocol`: `variance_score`, `correlation_score`,
`anova_f_score`, `fisher_score`, `mutual_information_score`, and
`chi_square_score` and `chi_square_yates_score`, plus
`symmetrical_uncertainty_score`, `cramers_v_score`, and
`cramers_v_bias_corrected_score`, are provided. They use shared arithmetic,
discretization, and sparse contingency predicates in the
`feature_scoring_common` category, mirroring how `recommender_protocols`
exposes `cosine_similarity` and `pearson_similarity` via
`similarity_metric_protocol`.

Learned selectors expose diagnostics using the shared `diagnostics/2`,
`diagnostic/2`, and `selector_options/2` predicates. Concrete selector
implementations store effective training options in the diagnostics
metadata under an `options(Options)` term.

The `filter_feature_selector_common` category extends
`feature_selector_common` with production implementations of univariate
filter learning, score and selection access, model validation, and export.
Concrete filters define the protected `filter_model/1` and
`filter_scoring_metric/2` hooks. The `filter_validate_dataset/3` and
`filter_feature_scores/6` hooks allow algorithm-specific validation and
preprocessing. The default scoring hook records supervised per-feature
complete-case counts. The default selection option is `top_k(10)`;
`all` and `threshold(Threshold)` strategies are also supported.
The protected `filter_selection/3` and `filter_validate_diagnostics/2`
hooks allow individual filters to extend selection and diagnostic validation.

Production filters use a `Model(FeatureScores, SelectedFeatures,
Diagnostics)` term representation, where `Model` is the receiving
implementation name. Their validation checks score and selection uniqueness,
decreasing numeric order, effective options, metric identity, selection
consistency, and recorded sample and feature counts. The standalone
`fisher_score_feature_selector`, `mutual_information_feature_selector`, and
`chi_square_feature_selector` libraries implement this contract.

The `feature_discretization` category prepares typed categorical columns
using per-feature or joint complete cases, preserving feature declaration
order and reporting sample sizes, bin specifications, and occupied categories.
The `prepare_feature_columns/7` predicate validates declared domains and
applies the first matching per-feature discretization override. The
`feature_redundancy` category computes pairwise mutual information on
aligned prepared columns without refitting bins.

The `relief_feature_selector_common` category provides joint typed row
preparation, range-normalized differences, stable neighbor ranking,
optional seeded sampling with RNG restoration, and empirical expected
differences for missing values. The standalone `relief_feature_selector`,
`relieff_feature_selector`, and `rrelieff_feature_selector` libraries
implement binary, multiclass, and regression variants. Their signed weights
are not interchangeable with the non-negative univariate scoring criteria.

The standalone `mrmr_feature_selector` library performs greedy MID selection
on a joint complete sample. It exposes static mutual information relevance
scores separately from its greedy selection order and trace. The
`lasso_feature_selector` library groups coefficients from the existing
`lasso_regression` learner by original feature, including missing indicators,
and retains its trained regressor and convergence diagnostics. Fisher and
mRMR offer optional automatic feature-count heuristics; mutual information
and chi-square support joint preparation; chi-square also supports Yates
correction, corrected Cramer's V, and expected-count rejection, while Lasso
offers deterministic holdout regularization search.

Unlike the time series and recommender protocol families, there is no
`update/3-4`-style online update here: feature selection is a one-shot
computation over a fixed dataset, not a model that is incrementally
revised as new data arrives.

This library also provides a small synthetic classification-style and a
small synthetic regression-style test dataset, and a handful of invalid
dataset fixtures, under the `test_datasets` directory.


API documentation
-----------------

Open the [../../apis/library_index.html#feature-selection-protocols](../../apis/library_index.html#feature-selection-protocols)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(feature_selection_protocols(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(feature_selection_protocols(tester)).

The test suite exercises dataset validation, every feature-matrix
utility, all ten scoring criteria (including informative-vs-noise-vs-
near-constant feature discrimination, and edge cases such as too few
groups, perfect class separation, and a constant feature or target), the
top-k and threshold selection strategies, and a minimal filter-based
selector (`sample_selector`, under `test_objects.lgt`) exercising the
full `feature_selector_protocol` contract end-to-end. Reference values
for the bundled `feature_demo` and `regression_demo` datasets were
computed independently in Python.

Regression tests also cover constant decimal values, extreme correlation
scales, ANOVA saturation, duplicate features, stable numeric ties,
missing-value alignment, declaration-only metrics, malformed selector
terms, repeated options, categorical contingency tables, both binning
methods, normalized-score reference values, unequal marginal entropies,
rectangular tables, symmetry and sample-replication invariance, and
configured metric export roundtrips.

Fisher metric tests cover independently calculated binary and unbalanced
multiclass scores, the ANOVA relationship, singleton classes, saturation,
translation and scale invariance, missing observations, and invalid inputs.


Test datasets
-------------

- `feature_demo`: a synthetic 20-example classification-style dataset (fixed seed) with three numeric features and a two-class target: `f1` is strongly informative, `f2` is pure noise, and `f3` is near-constant.
- `regression_demo`: a synthetic 20-example regression-style dataset (fixed seed) with the same three-feature pattern against a numeric target, for exercising `correlation_score`.
- `bad_feature_dataset.lgt`, `inconsistent_example_count.lgt`, `no_examples.lgt`: invalid dataset fixtures, one per validation error (an example naming an undeclared feature, a declared example count that does not match the observed count, and a declared positive count with no examples at all).

Additional invalid dataset and metric fixtures are defined in
`test_objects.lgt`.


Model
-----

The `feature_selector_common::score_features/4` predicate scores every
declared feature (`attribute_values/2`) by sending a `score/3` message
to a pluggable scoring metric object, once per feature, with that feature's
value and the target for every example (in dataset order). The result
is a list of `Feature-Score` pairs sorted by decreasing score, preserving
declaration order when scores are numerically equal. A
selection strategy is then applied to decide the final subset:

- `select_top_k/3` selects the `K` highest-scoring features.
- `select_above_threshold/3` selects every feature scoring at
  least a given threshold.

Both preserve the decreasing-score order and are lenient: `select_top_k/3`
returns every feature, not an error, when fewer than `K` are available
(mirroring `recommender_protocols::top_k/3`), and
`select_above_threshold/3` simply returns an empty list when no feature
reaches the threshold.

The `dataset_examples/2` predicate rejects duplicate feature declarations,
repeated feature names within an example, unknown feature names, malformed
feature lists, and inconsistent example counts. The `dataset_examples/3`
predicate also returns the declared feature names, avoiding a second
enumeration when both names and examples are needed. Feature names must
be atomic and occur once in the declarations and at most once per example.

The `check_scoring_metric/1` predicate requires a public implementation of
`score/3`, whether local, inherited, or category-provided. A protocol
declaration alone is insufficient.

A missing feature value or target (an unbound variable; see
`feature_dataset_protocol::example/3`) is excluded on a per-feature,
per-example basis by casewise deletion inside each scoring criterion
(via the shared `complete_pairs/3` or `complete_values/2` predicates). An
example with one missing feature value can still contribute to the scores
of its other known features, provided any required target is also known.


Scoring criteria
-----------------

All ten scoring criteria return non-negative scores, with higher scores
indicating greater feature relevance. The same selection strategies can
operate on each criterion, but their numeric scales are not interchangeable:
thresholds must be chosen for the selected criterion, and scores from
different criteria should not be compared directly.

- `variance_score`: unsupervised; the population variance of the feature's known values. A near-constant feature scores low regardless of any target. This is the classic "variance threshold" filter. Variance depends on feature units: multiplying values by a factor multiplies their variance by the square of that factor.
- `correlation_score`: supervised, for a numeric (regression-style) target; the squared Pearson correlation coefficient (R-squared) between the feature and the target, over the examples where both are known. Scores lie in `[0.0, 1.0]`. Squaring removes the sign, so a strong negative correlation scores as high as an equally strong positive one.
- `anova_f_score`: supervised, for a categorical (classification-style) target; the one-way ANOVA F-statistic (the ratio of between-group to within-group variance, where groups are the distinct target values), over the examples where both are known. This is the classic `f_classif` filter criterion, capped at `1.0e10`. Perfect separation and finite statistics reaching the cap tie at that score; imperfect separation never outranks perfect separation.
- `fisher_score`: supervised, for numeric features and categorical targets; between-class scatter divided by within-class scatter over complete observations, capped at `1.0e10`. Empty samples, constant features, and single-class samples score zero. Perfect separation, including singleton classes, scores at the cap. This ratio has no ANOVA degrees-of-freedom adjustment and is not a p-value.
- `mutual_information_score`: supervised; empirical mutual information between categorical feature values and target classes, measured in bits. This is not normalized mutual information, gain ratio, or a nearest-neighbor estimator for continuous data.
- `chi_square_score`: supervised; the Pearson independence statistic for a feature-category by target-class contingency table. Zero-observation cells contribute to the statistic. No continuity correction, normalization, or p-value is computed. This is not the nonnegative-feature-sum statistic exposed by scikit-learn's `chi2` function.
- `symmetrical_uncertainty_score`: supervised; empirical mutual information normalized by the mean of the feature and target marginal entropies. The score is `2*I(X;Y)/(H(X)+H(Y))`, with information and entropies measured in bits. This is symmetrical uncertainty, not adjusted mutual information or gain ratio.
- `cramers_v_score`: supervised; uncorrected Cramer's V, computed as `sqrt((ChiSquare/n)/min(r-1,c-1))`, where `n` is the complete-case sample size and `r` and `c` are the numbers of observed feature categories and target classes. The dimension factor is not the chi-square test's degrees of freedom `(r-1)*(c-1)`.

The categorical metrics return `0.0` when fewer than two complete
observations remain, or when only one feature category or target class
is observed. Categorical values and targets must be atomic. Numeric
values are category labels by default, including numeric target class
codes; their presence does not imply continuous features or regression.

The `chi_square_yates_score` object corrects occupied two-by-two tables
using `max(0, abs(Observed-Expected)-0.5)^2/Expected` for each cell. Larger
tables use Pearson chi-square; degenerate tables score zero. Numeric inputs
are categorical labels. The correction does not compute p-values.

Both normalized categorical criteria return scores in `[0.0, 1.0]`.
Independent empirical tables score zero, and perfect bijective category
associations score one. A deterministic many-to-one association can score
one under Cramer's V while scoring below one under symmetrical uncertainty:
the shared range does not imply identical interpretations or interchangeable
thresholds. Both criteria are symmetric under swapping categorical inputs
and invariant to uniformly replicating observations. A zero entropy sum
scores `0.0`; endpoint rounding drift is clamped to the documented range.


The `cramers_v_bias_corrected_score` metric applies Bergsma's correction:
`phi2 = max(0, ChiSquare/n - (r-1)*(c-1)/(n-1))`, with dimensions corrected
to `r - (r-1)^2/(n-1)` and `c - (c-1)^2/(n-1)`. It returns the square root
of the ratio of `phi2` to the smaller corrected dimension minus one, clamped to
`[0.0, 1.0]`. Nonpositive corrected denominators score zero as an
insufficient-sample convention. Unlike uncorrected V, this finite-sample
correction is not invariant to replicating observations. It uses Pearson,
not Yates, chi-square and does not provide p-values or general unbiasedness.
The correction is described by Bergsma (2013), DOI `10.1016/j.jkss.2012.10.002`.


Discretization
--------------

The parametric `mutual_information_score`, `chi_square_score`,
`symmetrical_uncertainty_score`, `cramers_v_score`, and
`cramers_v_bias_corrected_score` metrics accept `categorical`,
`equal_width(Count)`, or `equal_frequency(Count)` as their configuration.
`Count` must be a positive integer. Their non-parametric counterparts use
`categorical`. For example, these options explicitly discretize numeric
features:

  scoring_metric(mutual_information_score(equal_width(10)))
  scoring_metric(chi_square_score(equal_frequency(10)))
  scoring_metric(symmetrical_uncertainty_score(equal_width(10)))
  scoring_metric(cramers_v_score(equal_frequency(10)))

The `categorical_pairs/4` predicate validates aligned input lists and
removes observations with an unbound feature value or target before
fitting bins. Each feature therefore uses its own complete-case sample.
An excluded extreme value does not influence the fitted range or
quantile cutpoints. Targets are never discretized automatically.

Equal-width binning divides the observed range into `Count` intervals.
The minimum belongs to the first bin, the maximum to the last, and an
internal boundary belongs to the higher bin. Constant features form one
bin. Counts larger than the sample size are allowed; empty bins do not
create contingency rows.

Equal-frequency binning sorts the complete numeric values and uses an
effective count no greater than the number of observations. Cutpoints
are values at ranks `ceil(i*n/Count)` for internal divisions. A value
equal to a cutpoint belongs to the lower bin. Identical numeric values
are never split; duplicate cutpoints and cutpoints at the maximum are
removed. Ties can therefore produce fewer occupied bins and unequal
bin populations. Assignments retain the original observation order.

Metrics are stateless. Bins are fitted afresh per scoring call, and
selector export preserves the metric configuration rather than a
learned preprocessing transform. Bin count and method affect the
scores; thresholds must be chosen for that configuration.

Empirical mutual information can favor high-cardinality features,
especially identifiers. Treat genuinely continuous features explicitly
as such rather than treating every distinct numeric value as a category.
Chi-square grows with sample size, so differing complete-case counts
can affect comparisons between features. Sparse tables and small
expected counts limit hypothesis-test interpretations; the raw score
is not a significance guarantee.

Normalization removes raw chi-square's growth under uniform replication,
but does not correct finite-sample or high-cardinality bias in either
normalized criterion. Features with different complete-case samples can
still have different estimation biases. No significance or p-value can
be inferred from a normalized score alone.


Numeric scoring
---------------

For a fixed complete-case count greater than two, squared Pearson
correlation is related to the `f_regression` statistic by a monotonic
transform. Missing values can give different features different
complete-case counts, so their rankings need not match `f_regression`.

Shifted centering preserves exactly constant decimal inputs. Correlation
and ANOVA rescale complete values before computing their statistics;
correlation also rescales centered values to avoid unnecessary overflow
or underflow. This does not make arbitrary precision arithmetic available:
results remain subject to the backend's floating-point precision and range.

`correlation_score` assumes a numeric target and `anova_f_score` assumes
a categorical one; neither checks the target's type, so using the wrong
one for a dataset's target kind (e.g. `correlation_score` against atom
targets) is a user error that is likely to raise an arithmetic type
error from the underlying `is/2` evaluation rather than a clear,
dedicated one.


Selector representation
-------------------------

This library does not mandate a specific term representation for selector
models. Unlike `recommender_protocols`, it does not suggest one in the
protocol documentation. The `sample_selector` object, used by this library's
test suite, uses the following term representation:

	sample_selector(Metric, FeatureScores, SelectedFeatures, Diagnostics)

where `Metric` is the scoring metric object used, `FeatureScores` is
the full list of `Feature-Score` pairs, and `SelectedFeatures` is the
subset chosen by the configured selection strategy. The `Diagnostics`
argument is a list of diagnostic terms: `model/1`, `example_count/1`, and
`options/1`, required for all selectors, plus `selected_count/1`.

The `sample_selector::check_selector/1` predicate requires a ground term
with distinct candidate features, numeric scores in decreasing order,
distinct selected features drawn from those candidates, and valid required
metadata. Stored options are checked using the shared options API. The
recorded metric, selection strategy, and selected count must agree with
the model. Repeated options are accepted, with the first occurrence used
for lookup. Validation does not fill in missing model fields.


Performance
-----------

For `n` examples and `m` candidate features, vocabulary and duplicate
validation use AVL dictionaries rather than repeated feature-list scans.
The training path collects declarations once and indexes each example's
feature values once. Dense validation and indexed scoring take
`O(n*m*log(m+1))` time, excluding the metric's own computation, with
`O(n*m)` storage for the indexed examples. Score sorting takes
`O(m*log(m+1))` time.

The standalone `feature_values/3` predicate still scans each example's
feature list for one requested feature. The `score_features/4` predicate
does not repeat that scan for every candidate.

The `group_by_target/2` predicate sorts complete pairs by target and then
collects adjacent groups in `O(n*log(n+1))` time and `O(n)` space, rather
than searching a growing group list for every observation.

The `class_scatter/5` predicate computes scaled between-class and
within-class scatter from complete numeric-feature and categorical-target
pairs. ANOVA and the standalone Fisher metric share these sums but apply
different degrees-of-freedom factors. The `sort_by_decreasing_score/2`
predicate exposes stable numeric sorting to joint-data and embedded
selector implementations without converting scores to floats.

Equal-width binning takes `O(n)` time; equal-frequency binning takes
`O(n*log(n+1))` time and restores input order without scanning every
cutpoint for every observation. Sparse contingency counts take
`O(n*log(n+1))` time and `O(n)` storage. The statistics process only
observed cells and marginals, avoiding a dense feature-category by
target-class table even for high-cardinality inputs.


Limitations
-----------

- Automatic bin-count tuning, supervised discretization, continuous-
  target mutual-information estimators, bias corrections, and p-values
  are not provided. The categorical criteria require categorical
  targets and explicit binning for continuous features.
- Wrapper methods, such as recursive feature elimination, and embedded
  methods, such as coefficient-based selection, are not implemented in this
  library. Their selectors can implement the protocol; the shared code does
  not assume either approach.

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


`mutual_information_feature_selector`
=====================================

This library implements a univariate filter selector for categorical and
continuous features with categorical targets. It reuses the typed
discretization, sparse contingency arithmetic, stable ranking, selection,
model validation, and export facilities of `feature_selection_protocols`.


API documentation
-----------------

Open the [../../apis/library_index.html#mutual-information-feature-selector](../../apis/library_index.html#mutual-information-feature-selector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(mutual_information_feature_selector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(mutual_information_feature_selector(tester)).

The tests cover categorical, mixed, and continuous datasets, independent
binary and multiclass numerical references, both binning methods, missing
observations, stable ties, options, malformed inputs and models, reflection,
diagnostics, printing, and clause and file exports.


Scores
------

The default `score_variant(raw)` option computes mutual information in bits:
`sum(p(value,target) * log2(p(value,target)/(p(value)*p(target))))` over
occupied contingency cells. The `score_variant(normalized)` option computes
symmetrical uncertainty, `2 * I(feature;target)/(H(feature)+H(target))`.
The latter is between zero and one and is not mutual information divided
by target entropy alone. The recorded metrics are `mutual_information_score`
and `symmetrical_uncertainty_score`, respectively.

Empty complete-case samples, constant features, and single-class samples
score zero. Numeric target labels are class codes, not regression targets.


Preparation and options
-----------------------

The dataset object must implement `feature_dataset_protocol`. It declares
continuous features with `attribute_values(Feature, continuous)` and
categorical features with `attribute_values(Feature, Domain)`, where `Domain`
is a non-empty list of atomic values. Complete continuous values must be
numbers; complete categorical values must belong to their declared domains;
complete targets must be atomic.

The `learn/2` predicate uses the defaults. The `learn/3` predicate accepts an
options list as its last argument:

- `score_variant(raw)` or `score_variant(normalized)`, default `raw`.
- `preparation_mode(per_feature)` is the default. The `joint` mode excludes
  rows missing the target or any candidate feature before fitting bins, so
  all features use the same observations.
- `discretization(equal_frequency(Bins))` or
  `discretization(equal_width(Bins))`, with a positive integer bin count.
  The default is `equal_frequency(10)`, applied only to continuous features.
- `discretization(categorical)` preserves numeric values as category codes.
- `feature_discretization(Feature, Specification)` overrides the default for
  one declared feature. The specification can be `categorical`,
  `equal_frequency(Bins)`, or `equal_width(Bins)`.
- `selection_strategy(top_k(K))`, default `top_k(10)`, selects up to the
  positive integer `K` features.
- `selection_strategy(all)` selects every candidate.
- `selection_strategy(threshold(T))` selects scores at least the numeric `T`.

Repeated options and repeated overrides are accepted; the first applicable
occurrence wins. All supplied options are validated, including later
occurrences. Unknown override feature names are rejected. The
`default_option/1` and `valid_option/1` predicates remain publicly queryable,
including the inherited selection options.

By default, preparation is per feature: observations with an unbound or absent feature
value or an unbound target are excluded from that feature without imputing
values. Binning is fitted to its complete cases only. Categorical features
retain their declared categories unless explicitly overridden. Equal-width
bins partition the observed range; equal-frequency bins use observed
quantile cuts without splitting equal values. Occupied category counts can
be smaller than requested bin counts. Applying numeric binning to an atomic
non-numeric category is an error, not an implicit category encoding.

Scores are sorted in decreasing order, preserving declaration order on
ties. Top-k selection may include zero-scoring features. Thresholds should
be chosen for the selected score variant.


Models and diagnostics
----------------------

The `learn/2-3` predicates return selector terms with this representation:

	mutual_information_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The `feature_scores/2` predicate returns sorted `Feature-Score` pairs. The
`selected_features/2` predicate returns selected names. The `diagnostics/2`,
`diagnostic/2`, and `selector_options/2` predicates expose metadata and
effective options. Diagnostics contain `model/1`, `example_count/1`,
`options/1`, `candidate_count/1`, `selected_count/1`, `scoring_metric/1`,
`complete_cases(FeatureCounts)`, `discretization(FeatureSpecifications)`,
`occupied_categories(FeatureCounts)`, and `preparation_mode(Mode)`. Joint mode
also records `usable_example_count/1` and `excluded_example_count/1`;
every complete-case count equals the usable count.
Counts and specifications follow declaration order, not score order.

The `check_selector/1` predicate checks ground structure, unique sorted
scores, selection consistency, metric identity determined by options,
valid options, candidate and selected counts, and complete-case counts.
The `valid_selector/1` predicate fails for malformed models without binding
partial models. These checks are structural; they do not recompute scores
from a dataset. The `export_to_clauses/4` and `export_to_file/4` predicates
preserve the complete model. The `print_selector/1` predicate prints its
template and contents.


Limitations
-----------

Univariate scores do not detect interaction-only signals or remove redundant
features. Raw information favors high-cardinality features; normalization
does not eliminate sampling bias. Per-feature missingness can make samples
incomparable; joint mode aligns samples but can discard many rows and does
not eliminate missing-data bias. Discretization and sample size affect the
scores. Binning boundaries are not stored as a transformation for future
examples. No regression scoring, p-values, bias correction, or automatic
feature-count selection is provided. Arithmetic uses backend floating-point
precision.

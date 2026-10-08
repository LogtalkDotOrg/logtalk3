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


`rrelieff_feature_selector`
===========================

This library implements RReliefF joint feature selection for numeric
regression targets. Neighbors are determined jointly using all candidate
features. Scores contrast expected feature differences conditional on
different versus similar targets. Signed scores are preserved.


API documentation
-----------------

Open the [../../apis/library_index.html#rrelieff-feature-selector](../../apis/library_index.html#rrelieff-feature-selector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(rrelieff_feature_selector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(rrelieff_feature_selector(tester)).

The loader explicitly loads the shared
`feature_selection_protocols(relief_feature_selector_common)` category and
the existing random library. Tests cover independent exhaustive numeric
references, signed scores, rank normalization, missing probabilities,
target degeneracy, duplicates, sampling, model validation, and exports.


Dataset and distance contract
-----------------------------

The dataset must implement `feature_dataset_protocol`. Feature declarations
are `continuous` or nonempty ground lists of atomic categorical values. Known
numeric feature values and targets must be numbers; categorical values
must be atomic. Unknown targets are excluded without binding them.
Complete-case mode also drops rows missing any candidate feature. At least
two eligible rows are required; otherwise learning raises
`domain_error(relief_population, regression-Count)`. Invalid declarations
raise `domain_error(feature_type, ...)`; invalid values raise type errors.
Observed categorical values must belong to their declared domains; an
unknown category raises `domain_error(feature_value, Feature-Value)`.
Shared dataset validation rejects duplicate or undeclared features and
inconsistent example counts.

Self is excluded by original row position; identical rows and repeated
target values remain eligible neighbors. Neighbor ties prefer the earlier
row position. The distance is the Manhattan sum of normalized continuous
absolute differences and categorical identity mismatches. Continuous
observed ranges and target ranges come from the full eligible pool.
Magnitude scaling avoids overflowing raw endpoint subtraction. Constant
and entirely missing feature columns contribute zero differences.


Regression score
----------------

Each anchor uses up to K closest neighbors. Weights are normalized within
each actual neighbor list, even when fewer than K neighbors exist. With M
anchors, let `d_y` be normalized absolute target difference and `d_f` be a
feature difference. Accumulate `D=sum(w*d_y)`, `A_f=sum(w*d_f)`, and
`DA_f=sum(w*d_y*d_f)` across anchors and neighbors. The score is:

    W_f = DA_f/D - (A_f-DA_f)/(M-D)

M is the number of processed anchors, including repeated sampled anchors,
not the number of neighbor contributions. Scores are not clamped. A
constant target or zero conditioning mass in either denominator produces
all-zero scores and an explicit degeneracy diagnostic. A two-row dataset
with distinct targets has `M-D` equal to zero and is therefore degenerate.
A nonconstant target can also have D equal to zero when every selected
neighbor has the same target as its anchor.


Probabilistic missing values
----------------------------

The probabilistic policy retains missing features without binding or
imputing input variables. Regression uses pooled empirical observed
feature distributions, not target-class distributions. For categorical
features, observed/missing difference is `1-P(v)` and missing/missing
difference is `1-sum(P(v)^2)`. Numeric missing differences are expected
normalized absolute differences over empirical numeric supports. An
entirely missing column contributes zero.

Both neighbor search and score accumulation use these differences. Sorted
numeric supports are indexed in balanced trees with prefix probabilities
and means, avoiding support scans for observed/missing queries. The
missing/missing pooled expectation is cached once per missing column.
Complete columns allocate no empirical support caches.


Options
-------

The `learn/2` predicate uses defaults. The `learn/3` predicate accepts an
options list as its last argument:

- `selection_strategy(top_k(10))` is the default. `top_k(K)` requires a
  positive integer; `all` selects every candidate and `threshold(T)` selects
  scores at least the numeric threshold T.
- `number_of_neighbors(10)` is the default positive integer K. K can exceed
  the available pool size; the actual list is used and normalized.
- `neighbor_weighting(rank(2))` is the default. `rank(Sigma)` requires a
  positive integer and uses zero-based `exp(-(rank/Sigma)^2)` weights.
  `uniform` gives equal weights within each actual list.
- `sample_size(all)` processes eligible rows once in dataset order without
  accessing the RNG. A positive integer M samples M anchors uniformly with
  replacement. Neighbors always come from the full eligible pool.
- `random_seed(1357911)` is the default positive integer seed.
- `missing_values(complete_case)` is the default; `probabilistic` uses the
  empirical expected differences described above.

Repeated options are accepted, with the first occurrence taking precedence.
Incomplete and unknown options are rejected. The inherited
`valid_option/1` and `default_option/1` hooks remain public. Sampling uses
`fast_random(xoshiro128pp)` with portable catch-based seed restoration on
success, failure, and exceptions. Concurrent sampled calls sharing this
generator should be serialized externally.


Models and diagnostics
----------------------

The `learn/2-3` predicates return ground, exportable terms of this shape:

    rrelieff_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The `feature_scores/2` predicate returns all candidates sorted numerically
in decreasing order, preserving declaration order for ties. The
`selected_features/2` predicate returns strategy-selected feature names;
top-k can select zero or negative scores. The `diagnostics/2`,
`diagnostic/2`, and `selector_options/2` predicates expose metadata and
effective options. The `diagnostic/2` predicate enumerates matching terms.

Diagnostics include `model/1`, `example_count/1`, `options/1`,
`variant(regression)`, `candidate_count/1`, `selected_count/1`,
`features(FeatureTypes)`, `eligible_count/1`, `excluded_count/1`,
`eligible_positions/1`, `samples/1`, and `degeneracy/1`. Feature types are
`Feature-numeric` or `Feature-categorical` pairs in declaration order.
Frozen samples contain original row positions, with repetitions allowed.

The `diagnostics/2` and `diagnostic/2` predicates return the regression
population diagnostic with the following representation:

    population(regression(target_range(Minimum, Maximum),
                          conditioning_mass(D, Complement)))

`Complement` is `M-D`. The degeneracy value is `none`, `constant_target`,
or `zero_conditioning_mass`. No training feature vectors, empirical
supports, or fitted preprocessing state are stored in the model.

The `check_selector/1` predicate verifies ground structure, the receiving
model and variant, unique features and scores, stable ordering, selection
consistency, counts, stored options, samples, numeric target bounds,
conditioning masses, and degeneracy consistency. Degenerate models must
contain zero scores. The `valid_selector/1` predicate fails without binding
malformed partial models. The `export_to_clauses/4` and inherited
`export_to_file/4` predicates preserve the complete selector. The
`print_selector/1` predicate prints the shared template and learned model.


Limitations
-----------

Let N be eligible rows, F candidates, M anchors, and S the largest empirical
support. Indexed row preparation costs `O(N F log(F))`; sequential column
extraction and normalization cost `O(N F)`. Complete-case neighbor search and
scoring cost `O(M N (F + log(N)))` beyond preparation. All-row mode uses M
equal to N and is quadratic in N for fixed F.

Probabilistic support sorting and cached pooled expectations add
`O(F N log(N) + F S log(S))` beyond column passes. Observed/missing queries
cost `O(log(S))`; missing/missing queries use a cached value. Working memory
is `O(N F + M)` and does not retain an all-pairs distance matrix. Large
sample sizes still require storing M anchor positions. Arithmetic remains
subject to backend floating-point precision; scaling cannot recover
differences already lost in the input numbers. Small conditioning masses
can amplify rounding effects. The selector neither removes redundant
features nor fits a regression predictor.

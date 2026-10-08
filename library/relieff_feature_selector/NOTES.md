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


`relieff_feature_selector`
==========================

This library implements multiclass ReliefF joint feature selection. All
candidate features determine nearest neighbors using normalized Manhattan
differences; the resulting neighborhood updates score each feature.
Scores are signed and are not clamped.


API documentation
-----------------

Open the [../../apis/library_index.html#relieff-feature-selector](../../apis/library_index.html#relieff-feature-selector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(relieff_feature_selector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(relieff_feature_selector(tester)).

The loader explicitly loads the shared
`feature_selection_protocols(relief_feature_selector_common)` category and
the existing random library. The tests compare independent exhaustive
references, imbalanced class priors, rank weights, binary reduction,
probabilistic missing values, sampling, exports, and malformed models.


Dataset and score definition
----------------------------

The dataset must implement `feature_dataset_protocol`. Feature declarations
are `continuous` or nonempty ground lists of atomic categorical values. Continuous
observations must be numbers; categorical observations and known class
targets must be atomic. Numeric class labels are categorical codes, not
regression measurements. Categorical differences use term identity.

Unknown targets are excluded. The default complete-case policy drops rows
missing any candidate feature. After exclusions, at least two classes are
required, with at least two rows in each class. An insufficient population
raises `domain_error(relief_population, ...)`. Invalid declarations raise
`domain_error(feature_type, ...)`; invalid known values raise type errors.
Observed categorical values must belong to their declared domains; an
unknown category raises `domain_error(feature_value, Feature-Value)`.
Shared dataset validation rejects duplicate or undeclared features and
inconsistent example counts.

Every anchor uses up to K nearest same-class hits, excluding itself by row
position, and up to K nearest misses from each other class. Duplicate rows
remain eligible neighbors; ties prefer earlier row positions. Continuous
differences use the eligible observed range, computed after safe magnitude
scaling. Observed categorical differences are zero for identical values and
one otherwise. The Manhattan distance sums all feature differences.

Each neighbor list is normalized using its actual size and weight total,
not the requested K. The hit contribution is negative. A miss from class C
for an anchor of class A is multiplied by `P(C)/(1-P(A))`, where priors come
from the eligible full pool. The final score is the mean per-anchor update.
For two classes and K equal to one, the scores reduce to binary Relief.


Missing values
--------------

The probabilistic policy retains missing features without binding or
imputing their input variables. Each class uses its empirical observed
feature distribution, with pooled fallback when a class has no observed
value for a column. Constant and entirely missing columns contribute zero.

For categorical features, observed/missing differences are `1-P_C(v)` for
the missing row's class C. Missing/missing differences for classes C and D
are `1-sum(P_C(v)*P_D(v))`. For continuous features, both cases instead use
expected normalized absolute differences over empirical numeric supports.
These differences are used consistently for neighbor search and updates.

Sorted numeric supports and balanced prefix-probability/prefix-mean trees
avoid scanning supports for observed/missing queries. Missing/missing
class-pair differences are cached. Only columns and classes with missing
values allocate the corresponding distributions and caches.


Options
-------

The `learn/2` predicate uses defaults. The `learn/3` predicate accepts an
options list as its last argument:

- `selection_strategy(top_k(10))` is the default. `top_k(K)` requires a
  positive integer; `all` selects all features, and `threshold(T)` selects
  scores at least the numeric threshold T.
- `number_of_neighbors(10)` is the default positive integer K. Larger K
  than an available class neighborhood is valid.
- `neighbor_weighting(uniform)` is the default. `rank(Sigma)` requires a
  positive integer and uses zero-based `exp(-(rank/Sigma)^2)` weights,
  normalized separately for each hit or miss list.
- `sample_size(all)` processes eligible rows once in dataset order without
  accessing the RNG. A positive integer M samples M anchors uniformly with
  replacement. Neighbors always come from the full eligible pool.
- `random_seed(1357911)` is the default positive integer seed.
- `missing_values(complete_case)` is the default; `probabilistic` uses
  empirical expected differences.

Repeated options are accepted and their first occurrence takes precedence.
Incomplete and unknown options are rejected. The inherited
`valid_option/1` and `default_option/1` hooks remain public. Sampling uses
`fast_random(xoshiro128pp)` with catch-based seed restoration on success,
failure, and exceptions. Concurrent sampled calls sharing this RNG should
be serialized externally.


Models and API
--------------

The `learn/2-3` predicates return ground, exportable models of this shape:

    relieff_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The `feature_scores/2` predicate returns every candidate score in numeric
decreasing order, preserving declaration order for ties. The
`selected_features/2` predicate returns strategy-selected names; top-k
selection may include zero or negative scores. The `diagnostics/2`,
`diagnostic/2`, and `selector_options/2` predicates expose metadata and
effective options. The `diagnostic/2` predicate enumerates matching terms.

Diagnostics include `model/1`, `example_count/1`, `options/1`,
`variant(multiclass)`, `candidate_count/1`, `selected_count/1`,
`features(FeatureTypes)`, `eligible_count/1`, `excluded_count/1`,
`eligible_positions/1`, `samples/1`, `population(classes(ClassCounts))`,
and `degeneracy(none)`. Feature types are `Feature-numeric` or
`Feature-categorical` pairs in declaration order. Frozen samples contain
original row positions, with repetitions allowed. Class counts describe
the full eligible pool, not sampled frequencies. No training feature
vectors or empirical supports are retained in the model.

The `check_selector/1` predicate checks ground structure, the receiving
model and variant, unique features and numeric scores, stable ordering,
selection consistency, counts, stored options, samples, and class priors.
The `valid_selector/1` predicate fails without binding malformed partial
models. The `export_to_clauses/4` and inherited `export_to_file/4` predicates
preserve the complete term. The `print_selector/1` predicate prints the
shared template followed by the learned model.


Limitations
-----------

Let N be eligible rows, F candidates, C classes, M anchors, J classes with
missing entries in a column, and S its largest empirical support. Indexed
row preparation costs `O(N F log(F))`; sequential column extraction and
normalization cost `O(N F)`. Without missing values, training costs
`O(M (N (F + log(N) + log(C+1)) + C K F))` beyond preparation, where K is
the requested neighbor count. Sorted neighbors are bucketed once per
anchor in an AVL dictionary; each class retains the original distance and
row-position order. All-row mode uses
M equal to N and is quadratic in N for fixed F and C.

Probabilistic preparation additionally scans class populations and sorts
supports. A conservative bound beyond column passes is
`O(F N C + F N log(N) + F J^2 S log(S))`. Observed/missing distance queries
cost `O(log(S) + log(J+1))`; cached missing/missing queries cost
`O(log(J+1))`. Working memory is `O(N F + F J^2 + M)`, not an all-pairs
distance matrix. Many classes with missing values can therefore make
cache construction expensive. Complete-case learning allocates no
empirical support caches. Floating-point precision remains
backend-dependent. The algorithm neither removes redundant features nor
fits a predictive classifier.

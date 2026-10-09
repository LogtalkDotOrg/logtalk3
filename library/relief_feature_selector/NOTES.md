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


`relief_feature_selector`
=========================

Use this library to select features for binary classification with Relief.
It scores each feature using the mean difference to the nearest observation
in the other class (a miss), minus the mean difference to the nearest
observation in the same class (a hit). Neighbors are determined jointly
from all candidate features, not from independent univariate scores. Signed
scores are preserved; negative scores are not clamped.


API documentation
-----------------

Open the [../../apis/library_index.html#relief-feature-selector](../../apis/library_index.html#relief-feature-selector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(relief_feature_selector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(relief_feature_selector(tester)).

The loader explicitly loads
`feature_selection_protocols(relief_feature_selector_common)` and the
existing random library. The shared category extends
`feature_selector_common`; the concrete object imports that category.
The tests exercise numeric references, empirical missing probabilities,
interaction features, signed scores, duplicates, sampling, options,
malformed models, diagnostics, reflection, printing, and exports.


Datasets and distances
----------------------

The dataset must implement `feature_dataset_protocol`. A continuous feature
is declared with `attribute_values(Feature, continuous)`; a categorical
feature is declared with a nonempty ground list of atomic possible values.
Observed numeric values must be numbers and categorical values must be
atomic members of their declared domains.
Categorical differences use term identity, so integer and floating-point
category codes are not interchangeable. Categorical labels need not be
numeric measurements. Known classification targets must be atomic.

Rows with an unbound target are excluded without binding that variable.
The default complete-case policy also excludes rows with any missing
candidate feature. After these exclusions, exactly two classes, each with
at least two rows, are required. An invalid eligible population raises
`domain_error(relief_population, ...)`. Unsupported declarations raise
`domain_error(feature_type, ...)`; invalid known values raise the
corresponding type error. An observed category outside its declared domain
raises `domain_error(feature_value, Feature-Value)`. Shared validation
rejects undeclared or duplicate features and inconsistent example counts.

Row positions, not dataset IDs or feature-vector identity, exclude self
matches. Duplicate rows remain eligible neighbors. Neighbor ties prefer the
earlier row position. The distance is the Manhattan sum of feature
differences. Categorical observed differences are zero for identical values
and one otherwise. Continuous differences are normalized by the observed
eligible-pool range. Magnitude scaling avoids overflowing a raw subtraction
of opposite large endpoints. Constant and entirely missing columns
contribute zero differences.


Missing values
--------------

The probabilistic policy retains rows with missing features. Each class uses
its empirical distribution of observed feature values in the eligible full
pool, including frequency counts. A class with no observed value for a
column uses the pooled distribution. An entirely missing column contributes
zero. No missing input variable is bound or imputed in the dataset.

For categorical features, a missing class-C value compared with observed
value `v` contributes `1-P_C(v)`. Two missing values from classes C and D
contribute `1-sum(P_C(v)*P_D(v))`. Numeric missing differences are expected
normalized absolute differences over the corresponding empirical numeric
supports, not categorical mismatch probabilities or a worst-case distance.
Both the neighbor search and score updates use these same differences.

Numeric supports are sorted and indexed in balanced trees containing prefix
probabilities and prefix means. Observed/missing numeric queries therefore
avoid scanning the support. Missing/missing class-pair differences are
cached once per column. Distributions are built only for columns with
missing values and classes with missing entries in those columns.


Options and selection
---------------------

The `learn/3` predicate accepts the following options:

- `selection_strategy(Strategy)` selects `top_k(K)` (default: `top_k(10)`),
  `all`, or `threshold(T)`. Top-k selects up to the positive integer `K`
  features; `all` selects all candidates, and `threshold(T)` selects scores
  at least the numeric threshold `T`.
- `sample_size(Size)` selects `all` (default) or a positive integer.
  With `all`, it processes each eligible row once, in dataset order,
  without accessing the random generator. A positive integer M instead
  draws M anchors uniformly with replacement from the eligible pool.
- `random_seed(Seed)` sets a positive integer sampling seed (default: `1357911`).
- `missing_values(Policy)` selects `complete_case` (default) or `probabilistic`.
  Probabilistic handling uses the
  empirical differences described above.
- `neighbor_weighting(Weighting)` selects `uniform` (default) or `rank(Sigma)`.
  The rank scale `Sigma` must be a positive integer. Rank weighting uses
  normalized zero-based weights
  `exp(-(rank/Sigma)^2)`. With one neighbor per list, both schemes agree.

The `learn/2` predicate uses the default option values.

An anchor is an eligible row whose neighbor comparisons contribute to the
feature scores.

The number of neighbors is fixed at one hit and one miss; a
`number_of_neighbors/1` option is not accepted. Every sampled anchor searches
the full eligible pool, never only the sampled anchors. Sampling uses
`fast_random(xoshiro128pp)` and restores its prior seed on success, failure,
and exceptions using a portable catch-based wrapper. Concurrent sampled
training calls sharing this generator should be serialized externally.

Repeated options are accepted, with the first occurrence taking precedence.
Incomplete or unknown options are rejected without filling missing option
parameters. Scores are sorted numerically in decreasing order, with declaration
order preserved for ties. Top-k can select negative or zero scores.


Models and diagnostics
----------------------

The `learn/2` and `learn/3` predicates return ground, exportable models using
the following term representation:

    relief_feature_selector(FeatureScores, SelectedFeatures, Diagnostics)

The `feature_scores/2` predicate returns all sorted candidate scores, not
just the selected scores. The `selected_features/2` predicate returns
selected feature names. The `diagnostics/2`, `diagnostic/2`, and
`selector_options/2` predicates expose metadata and the effective options.
The `diagnostic/2` predicate enumerates matching metadata terms.

The `Diagnostics` argument is a list of diagnostic terms, including
`model/1`, `example_count/1`, `options/1`, `variant(binary)`,
`candidate_count/1`, `selected_count/1`, `features(FeatureTypes)`,
`eligible_count/1`, `excluded_count/1`, `eligible_positions/1`, `samples/1`,
`population(classes(ClassCounts))`, and `degeneracy(none)`. Feature types are
`Feature-numeric` or `Feature-categorical` pairs in declaration order. Sample
positions may repeat when sampling is enabled. Class counts describe the
eligible pool, not the sampled anchors. No training feature vectors,
empirical supports, or fitted preprocessing state are stored in the model.

The `check_selector/1` predicate verifies the receiving model functor and
variant, ground structure, unique feature names and scores, stable numeric
ordering, strategy-consistent selection, counts, stored option domains,
sample membership and length, and eligible class counts. The
`valid_selector/1` predicate fails for malformed models without binding
partial models. The `export_to_clauses/4` and inherited `export_to_file/4`
predicates preserve the complete model. The `print_selector/1` predicate
prints the template and model using the shared printing predicates.


Limitations
-----------

Let N be eligible rows, F candidates, and M anchors (N in all-row mode).
Indexed row preparation costs `O(N F log(F))`; sequential column extraction
and normalization cost `O(N F)` without repeated positional scans.
Complete-case neighbor search and scoring cost `O(M N (F + log(N)))`,
retaining `O(N F + M)` data. There is no retained all-pairs distance matrix.

With probabilistic missing handling, an observed/missing query costs
`O(log(S))` for support size S, in addition to indexed distribution lookup.
Support construction includes sorting and missing/missing pair-cache
construction; its conservative bound is `O(F N log(N) + F S log(S))` for
binary targets, beyond the column passes. Working memory remains
`O(N F + M)` for the two-class case. Floating-point precision is
backend-dependent; magnitude scaling does not recover differences already
lost in the input numbers. Exhaustive nearest-neighbor search is quadratic
in N in all-row mode. The method can detect joint interactions but does not
remove redundant features or fit a predictive model.

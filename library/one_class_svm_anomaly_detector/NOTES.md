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


`one_class_svm_anomaly_detector`
================================

One-class support vector machine anomaly detector for continuous,
categorical, and mixed-feature datasets. The implementation uses the same
linear, polynomial, and radial basis function kernel specifications as the
`kernel_svm_classifier` library.

The library implements the `anomaly_detector_protocol` defined in the
`anomaly_detection_protocols` library. Training examples are selected using
the shared baseline class options. Optimization enforces the one-class SVM
capped-simplex constraints before optional coefficient pruning. Decision
values are converted to bounded anomaly scores using their empirical rank
among the training values.


API documentation
-----------------

Open the [../../apis/library_index.html#one-class-svm-anomaly-detector](../../apis/library_index.html#one-class-svm-anomaly-detector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(one_class_svm_anomaly_detector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(one_class_svm_anomaly_detector(tester)).

The tests train shared Gaussian and mixed-feature fixtures once per test run,
while retaining a separate deterministic learning test. Fixture training takes
place after the coverage counters are reset and contributes to code coverage.
Reported per-test runtime excludes global fixture setup; compare complete test
run times when measuring test performance.


Features
--------

- **One-Class Learning**: Fits a boundary using only selected baseline examples.
- **Multiple Kernels**: Supports `linear`, `polynomial(Degree, Gamma, Coef0)`,
	and `rbf(Gamma)` kernels.
- **Mixed Features**: Encodes continuous and categorical attributes with explicit
	missing-value indicators. Unseen categorical values use the corresponding
	missing-value indicator.
- **Bounded Scores**: Returns empirical anomaly scores in the interval
	`[0.0, 1.0]`.
- **Support-Vector Pruning**: Omits zero and optionally negligible coefficients
	from learned detectors without renormalization. Pruned coefficients are
	replaced by zero in training order; fallback selection retains the first
	largest coefficient when values tie.
- **Model Validation**: Requires the exact ground detector representation,
	consistent dimensions and diagnostics, and nonnegative coefficients within
	their learned bounds. Retained coefficient mass must be positive and at most
	one. Nonground models raise an instantiation error; ground malformed models
	raise an anomaly-detector domain error. Kernel options must be fully specified
	and invalid values use the shared option-domain errors.
- **Numerical Projection**: Uses cap-quantile-relative thresholds and scalar-only
	bisection passes. The threshold bracket is no wider than the effective
	coefficient cap, avoiding loss of precision from distant tails or excessively
	large caps. Termination checks the sum-to-one residual; numerical stagnation
	is corrected at the nearest adjustable boundary while preserving bounds.
- **Efficient Scoring**: Accumulates weighted kernel values directly without
	constructing a temporary kernel vector. RBF squared distances are accumulated
	directly from feature pairs without constructing difference vectors.
- **Symmetric Kernel Matrix**: Computes each diagonal or unordered pair once
	for polynomial and RBF training, requiring `N*(N+1)/2` kernel evaluations and
	quadratic list construction. The full matrix is reused to compute reference
	values for every baseline example using the retained coefficients.
- **Model Export**: Exports learned detectors as predicate clauses or writes
	them to a file.


Options
-------

The `learn/3` predicate supports these options:

- `kernel/1` - kernel function to use (default: `rbf(0.5)`)
- `nu/1` - upper bound on the fraction of training anomalies and lower bound on the fraction of support vectors (default: `0.1`)
- `learning_rate/1` - base projected-gradient learning rate (default: `0.1`)
- `learning_schedule/1` - learning-rate schedule, either `constant` or `inverse_scaling(Power)` (default: `constant`)
- `support_vector_tolerance/1` - coefficients at or below this value are omitted from the learned detector; the largest coefficient is always retained (default: `0.0`)
- `maximum_iterations/1` - maximum number of optimization iterations (default: `100`)
- `tolerance/1` - convergence threshold for the maximum coefficient update (default: `1.0e-6`)
- `optimality_tolerance/1` - positive convergence threshold for the feasible gradient-exchange residual (default: `1.0e-6`)
- `feature_scaling/1` - whether to standardize continuous attributes (default: `true`)
- `anomaly_threshold/1` - score threshold used by prediction (default: `0.95`)
- `baseline_class_values/1` - class values accepted as baseline examples (default: `[normal]`)
- `baseline_selection_policy/1` - either `reject` or `filter` (default: `reject`)


Usage
-----

	| ?- one_class_svm_anomaly_detector::(
			learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter)]),
			predict(Detector, [x-4.5, y-4.2], Prediction)
		 ).

	| ?- one_class_svm_anomaly_detector::(
			learn(mixed_anomalies, Detector, [baseline_selection_policy(filter), kernel(rbf(0.25))]),
			score(Detector, [age-19, income-150000, student-no, credit_rating-excellent], Score)
		 ).


Detector representation
-----------------------

The learned detector is represented by:

	one_class_svm_detector(Encoders, Kernel, SupportVectors, Coefficients, ReferenceScores, Cache, Diagnostics)

Where:

- `Encoders`: feature encoders learned from the training dataset, including
	continuous-attribute scaling parameters and categorical-attribute values
- `Kernel`: kernel specification used for training and prediction
- `SupportVectors`: retained training examples represented as encoded feature
	vectors
- `Coefficients`: learned dual coefficients corresponding to the retained
	support vectors
- `ReferenceScores`: raw decision values for all baseline training examples,
	used to compute empirical anomaly scores
- `Cache`: the serialized `svm_cache(Scorer, ReferenceCount, RankIndex)` term.
	The scorer is `linear_weights(Weights)` for linear models or `kernel_sum`
	for polynomial and RBF models. The reference count and rank index are stored
	with the scorer and validated against the model data.
- `Diagnostics`: learned metadata terms including `model/1`,
	`training_dataset/1`, `kernel/1`, `example_count/1`, `support_vectors/1`,
	`iterations/1`, `final_delta/1`, `decision_geometry/1`, `converged/1`,
	`stopping_reason/1`, `optimality_residual/1`, `step_limit/1`, and `options/1`

When exported using `export_to_clauses/4` or `export_to_file/4`, thw detector
term is serialized directly as the single argument of the generated predicate
clause so that the exported model can be loaded and reused as-is.

The complete seven-argument representation is required. Linear references and
queries use the same cached-weight dot-product path. Reassociating floating-point
operations can change ranks near numerical ties compared with dual kernel sums;
rank comparisons remain strict, without tolerance-based tie grouping.

The rank index is an immutable balanced tree with `rank_empty` leaves and
`rank_node(Score, Multiplicity, SubtreeCount, Left, Right)` nodes. Multiplicities
retain duplicate reference values. A query counts strictly greater reference
scores using subtree counts along one tree path. Explicit model validation
checks the complete canonical index and scorer against the stored model data;
ordinary scoring neither rebuilds nor revalidates these caches.


Benchmarks
----------

To run the opt-in production benchmarks, load the library normally and then
load the local benchmark objects:

	| ?- logtalk_load(one_class_svm_anomaly_detector(loader)),
	     logtalk_load(os(loader)),
	     logtalk_load(one_class_svm_anomaly_detector(benchmarks), [optimize(on)]),
	     svm_benchmarks::run.

The benchmarks report warmed five-run CPU medians for 40, 100, and 250 training
examples, two and ten attributes, and all three kernels. Training uses a fixed
30-iteration budget, unscaled features, and tight stopping criteria. Query
batches contain 1000 fresh instances and include instance preparation and
encoding. Primitive comparisons isolate the original and optimized Gram,
weighted-score, projection, and rank paths. These are local timing probes,
not calibrated throughput or peak-memory measurements; fixed-budget training
does not imply identical convergence quality across kernels.


Limitations
-----------

- Polynomial and RBF training materialize the full kernel Gram matrix. For `N`
	baseline examples and `D` encoded dimensions, matrix construction costs
	`O(N*N*D)` time and `O(N*N)` memory; each optimization iteration costs
	`O(N*N)`. Raw prediction costs `O(S*D)` for `S` retained support vectors.
- Linear training uses a matrix-free weight vector with `O(N*D)` gradient
	evaluation without quadratic matrix storage. The common cap-quantile
	projection adds `O(N*log(N))` work per iteration. Cached linear raw prediction
	costs `O(D)`. Empirical ranking costs `O(log(N))` per query for all kernels.
	Building the rank index costs `O(N*log(N))` time and `O(U)` additional
	storage for `U` distinct reference scores.
- Anomaly scores are empirical ranks of raw kernel decision values. They are
	discrete with resolution `1/N`, are not calibrated probabilities, and should
	not be compared directly across detectors trained on different datasets.
	The `anomaly_threshold/1` option controls prediction independently of the
	`nu/1` constraint.
- The projected-gradient optimizer can stop at `maximum_iterations/1` before
	satisfying the coefficient-update and optimality tolerances. The scheduled
	learning rate is bounded by a safe inverse spectral bound of the kernel
	matrix. Recorded options retain the requested rate; `step_limit/1` reports
	the safeguard. The optimality residual compares the largest removable
	coefficient gradient with the smallest gradient that can receive mass,
	treating coefficients within `1.0e-12` of a bound as active at that bound.
	Inspect the `iterations/1`, `final_delta/1`, `converged/1`,
	`stopping_reason/1`, and `optimality_residual/1` diagnostics when tuning
	difficult datasets to distinguish convergence from iteration-budget exhaustion.
- Centered complete continuous features can make the linear kernel's optimal
	decision function identically zero. Such detectors cannot distinguish central
	and distant observations. The `decision_geometry/1` diagnostic distinguishes
	`zero_linear`, `constant_reference`, and `varying_reference` cases. Constant
	reference scores do not by themselves imply a zero decision function. Use a
	nonlinear kernel when a two-sided boundary is required; uncentered linear
	kernels also cannot express arbitrary two-sided anomaly boundaries.
- The theoretical `nu/1` guarantees concern the constrained SVM optimization,
	not the independent empirical prediction threshold or a pruned approximation.
- Results can be sensitive to the kernel parameters, feature scaling, and
	contamination of the selected baseline examples.


References
----------

1. Scholkopf, B., Platt, J., Shawe-Taylor, J., Smola, A. and Williamson, R. (2001). "Estimating the Support of a High-Dimensional Distribution".

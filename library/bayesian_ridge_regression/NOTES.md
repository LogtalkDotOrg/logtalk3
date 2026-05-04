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


`bayesian_ridge_regression`
===========================

Bayesian ridge regression regressor supporting continuous and mixed-feature
datasets. The library implements the `regressor_protocol` defined in the
`regression_protocols` library and learns a Bayesian linear model using
evidence maximization for the global weight and noise precisions together
with Gamma hyperpriors over both precision terms.


API documentation
-----------------

Open the [../../apis/library_index.html#bayesian_ridge_regression](../../apis/library_index.html#bayesian_ridge_regression)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(bayesian_ridge_regression(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(bayesian_ridge_regression(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(bayesian_ridge_regression(tester_performance)).


Features
--------

- **Continuous and Mixed Features**: Supports continuous attributes and categorical attributes encoded using reference-level dummy coding from the declared dataset attribute values.
- **Automatic Hyperparameter Tuning**: Learns the global coefficient precision and observation-noise precision using MacKay-style evidence maximization with configurable Gamma hyperpriors instead of a user-supplied ridge penalty.
- **Posterior Uncertainty**: Exposes predictive Gaussian distributions using coefficient posterior uncertainty plus observation noise, matching the usual scikit-learn BayesianRidge treatment where the intercept is not probabilistic. Posterior coefficient variances are also exposed.
- **Feature Scaling**: Continuous attributes can be standardized using z-score scaling before fitting and prediction.
- **Stable Posterior Solves**: Evidence-maximization updates clamp the learned weight and noise precisions to configurable `precision_bounds(Min, Max)` to avoid degenerate zero or infinite precision estimates. Posterior solves use Cholesky factorization of positive-definite precision matrices, diagnostics report any diagonal jitter applied when factorization retries are needed, and the evidence-maximization loop computes the effective degrees of freedom from a one-time eigenspectrum of the centered Gram surrogate while still switching to a sample-space solve when the active encoded feature count exceeds the number of training rows.
- **Missing Values**: Missing numeric and categorical values represented using anonymous variables are encoded using explicit missing-value indicator features.
- **Unknown Values**: Prediction requests containing categorical values that are not declared by the dataset raise a domain error.
- **Zero-Variance Features**: Encoded columns with zero variance are excluded from posterior updates and assigned zero mean and zero posterior variance.
- **Diagnostics Metadata**: Learned regressors record model name, target, training example count, Cholesky stabilization attempts and applied jitter, Gamma hyperpriors for both precision terms, effective precision bounds, learned precisions, learned noise variance, final log evidence, the full log-evidence score trace, active feature count, posterior variances, intercept treatment, convergence metric and status, encoded feature count, and effective options.
- **Model Export**: Learned regressors can be exported as predicate clauses or written to a file.


Regressor representation
------------------------

The learned regressor is represented by default as:

- `bayesian_ridge_regressor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics)`

The exported predicate clauses therefore use the shape:

- `Functor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics)`


Diagnostics syntax
------------------

The `diagnostics/2` predicate returns a list of metadata terms with the form:

	[
		model(bayesian_ridge_regression),
		target(Target),
		training_example_count(TrainingExampleCount),
		options(Options),
		solver(cholesky_factorization),
		stabilization_attempts(StabilizationAttempts),
		stabilization_jitter(StabilizationJitter),
		precision_bounds(MinimumPrecision, MaximumPrecision),
		weight_precision_hyperprior(gamma(LambdaShape, LambdaRate)),
		noise_precision_hyperprior(gamma(AlphaShape, AlphaRate)),
		weight_precision(Alpha),
		noise_precision(Beta),
		noise_variance(NoiseVariance),
		log_evidence(LogEvidence),
		scores(Scores),
		active_feature_count(ActiveFeatureCount),
		weight_prior(isotropic_zero_mean_gaussian),
		intercept_treatment(non_probabilistic),
		bias_variance(BiasVariance),
		weight_variances(WeightVariances),
		convergence_metric(coefficient_l1),
		convergence(Convergence),
		iterations(Iterations),
		final_delta(FinalDelta),
		encoded_feature_count(FeatureCount)
	]

The `scores/1` diagnostic is analogous to scikit-learn `scores_`: it stores the log marginal likelihood at the initial hyperparameters followed by the value after each evidence-maximization update. The final element is identical to `log_evidence/1`.

The `bias_variance/1` diagnostic is always `0.0` because the intercept is treated as a deterministic centering adjustment rather than as a probabilistic parameter.

Use the `regression_protocols` `diagnostic/2` and `regressor_options/2` helper predicates when you only need a single metadata term or the effective options.


Options
-------

The `learn/3` predicate accepts the following options:

- `maximum_iterations/1`: Maximum number of evidence-maximization updates. The default is `300`.
- `tolerance/1`: Convergence tolerance on the L1 change between consecutive active coefficient vectors across evidence-maximization updates. The default is `1.0e-6`.
- `initial_weight_precision/1`: Positive initial value for the shared coefficient precision. The default is `1.0`.
- `initial_noise_precision/1`: Positive initial value for the observation-noise precision or `auto` to derive it from the target variance. The default is `auto`.
- `alpha_1/1`: Non-negative shape hyperparameter of the Gamma prior over the learned observation-noise precision. The default is `1.0e-6`.
- `alpha_2/1`: Non-negative rate hyperparameter of the Gamma prior over the learned observation-noise precision. The default is `1.0e-6`.
- `lambda_1/1`: Non-negative shape hyperparameter of the Gamma prior over the learned coefficient precision. The default is `1.0e-6`.
- `lambda_2/1`: Non-negative rate hyperparameter of the Gamma prior over the learned coefficient precision. The default is `1.0e-6`.
- `feature_scaling/1`: Controls z-score standardization of continuous attributes before training and prediction. Accepted values are `true` and `false`. The default is `true`.
- `precision_bounds/2`: Lower and upper positive bounds used to clamp the learned weight and noise precisions during evidence maximization for numerical stability. The default is `precision_bounds(1.0e-12, 1.0e12)`.

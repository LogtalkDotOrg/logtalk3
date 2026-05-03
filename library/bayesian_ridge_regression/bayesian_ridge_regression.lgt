%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(bayesian_ridge_regression,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Bayesian ridge regression regressor supporting continuous and mixed-feature datasets using evidence maximization and posterior uncertainty over coefficients. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction, predictive-distribution queries, and export as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses Bayesian linear regression with a shared scalar coefficient precision, Gamma hyperpriors over the weight and noise precisions, an explicit unpenalized intercept, and MacKay-style fixed-point evidence maximization updates.',
			'Numerical stability' - 'Evidence-maximization updates clamp the learned weight and noise precisions to configurable ``precision_bounds(Min, Max)`` to avoid degenerate zero or infinite precision estimates. Posterior solves use Cholesky factorization of positive-definite precision matrices, diagnostics report any diagonal jitter applied when factorization retries are needed, and the evidence-maximization loop computes the effective degrees of freedom from a one-time eigenspectrum of the centered Gram surrogate while still switching to a sample-space solve when the active encoded feature count exceeds the number of training rows.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are encoded using reference-level dummy coding from the declared dataset attribute values, and zero-variance encoded columns are dropped from posterior updates and assigned zero mean and zero variance.',
			'Uncertainty estimates' - 'In addition to mean predictions, the library can return the posterior predictive Gaussian distribution for a new instance using coefficient posterior uncertainty plus observation noise. The intercept is treated as a deterministic centering adjustment and therefore has no associated posterior variance.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features.',
			'Unknown values' - 'Prediction requests containing categorical values that are not declared by the dataset raise a domain error.',
			'Regressor representation' - 'The learned regressor is represented by default as ``bayesian_ridge_regressor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics)`` where ``Encoders`` stores feature encoding metadata, ``Bias`` stores the posterior mean intercept, ``Weights`` stores one posterior mean coefficient per encoded feature, ``ActiveFlags`` records which encoded features participated in the posterior update, ``PosteriorCovariance`` stores the posterior covariance matrix of the intercept plus active coefficients, ``NoiseVariance`` stores the learned observation noise variance, and ``Diagnostics`` stores training metadata including the effective options and learned precisions.'
		],
		see_also is [
			linear_regression, ridge_regression, lasso_regression, elastic_net_regression, gaussian_process_regression,
			knn_regression, regression_tree, random_forest_regression, gradient_boosting_regression
		]
	]).

	:- public(predict_distribution/3).
	:- mode(predict_distribution(+compound, +list, -compound), one).
	:- info(predict_distribution/3, [
		comment is 'Predicts the posterior predictive Gaussian distribution for a new instance using the learned regressor. The returned term has the shape ``gaussian(Mean, Variance)`` where ``Variance`` includes the learned observation noise variance and coefficient posterior uncertainty only; the intercept is not treated as a probabilistic parameter.',
		argnames is ['Regressor', 'Instance', 'Distribution']
	]).

	:- public(weight_variances/2).
	:- mode(weight_variances(+compound, -list(float)), one).
	:- info(weight_variances/2, [
		comment is 'Returns the posterior marginal variances of the encoded feature coefficients in encoder order. Encoded features dropped from fitting because they had zero variance are reported with posterior variance zero.',
		argnames is ['Regressor', 'Variances']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, nth1/3, reverse/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(Target),
		fit_bayesian_ridge_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, TrainingDiagnostics),
		build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics),
		Regressor = bayesian_ridge_regressor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, Bias, Weights, _ActiveFlags, _PosteriorCovariance, _NoiseVariance, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		linear_response(Weights, Features, Linear),
		Target is Bias + Linear.

	predict_distribution(Regressor, Instance, gaussian(Mean, Variance)) :-
		Regressor =.. [_, Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		linear_response(Weights, Features, Linear),
		Mean is Bias + Linear,
		compress_features(Features, ActiveFlags, ActiveFeatures),
		predictive_posterior_variance(PosteriorCovariance, ActiveFeatures, PosteriorVariance),
		Variance0 is NoiseVariance + PosteriorVariance,
		(   Variance0 >= 0.0 ->
			Variance = Variance0
		;   Variance0 >= -1.0e-10 ->
			Variance = 0.0
		;   domain_error(non_negative_predictive_variance, Variance0)
		).

	weight_variances(Regressor, Variances) :-
		Regressor =.. [_, _Encoders, _Bias, _Weights, ActiveFlags, PosteriorCovariance, _NoiseVariance, _Diagnostics],
		posterior_weight_variances(PosteriorCovariance, ActiveFlags, Variances).

	build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics) :-
		^^encoded_feature_count(Encoders, FeatureCount),
		append(TrainingDiagnostics, [encoded_feature_count(FeatureCount)], ExtraDiagnostics),
		^^base_regressor_diagnostics(bayesian_ridge_regression, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics).

	fit_bayesian_ridge_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, TrainingDiagnostics) :-
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_bayesian_ridge_encoders(Attributes, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		^^encoded_feature_count(Encoders, FeatureCount),
		train_bayesian_ridge_model(Rows, FeatureCount, Options, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, TrainingDiagnostics),
		length(Examples, TrainingExampleCount).

	build_bayesian_ridge_encoders([], _Examples, _Options, []).
	build_bayesian_ridge_encoders([Attribute-Values| Attributes], Examples, Options, [Encoder| Encoders]) :-
		(   Values == continuous ->
			^^continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;   Encoder = categorical(Attribute, Values)
		),
		build_bayesian_ridge_encoders(Attributes, Examples, Options, Encoders).

	train_bayesian_ridge_model(Rows, FeatureCount, Options, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, TrainingDiagnostics) :-
		bayesian_feature_activity(Rows, ActiveFlags),
		compress_rows(Rows, ActiveFlags, ActiveRows),
		training_precision_bounds(Options, MinimumPrecision, MaximumPrecision),
		weight_precision_hyperparameters(Options, LambdaShape, LambdaRate),
		noise_precision_hyperparameters(Options, AlphaShape, AlphaRate),
		(   ActiveRows = [_ActiveFeatures-_| _],
			_ActiveFeatures == [] ->
			train_intercept_only_bayesian_model(Rows, FeatureCount, Options, ActiveFlags, Bias, Weights, PosteriorCovariance, NoiseVariance, TrainingDiagnostics)
		;   initial_precisions(Rows, Options, Alpha0, Beta0),
			prepare_bayesian_training_summary(ActiveRows, Summary),
			Summary = bayesian_training_summary(RowCount, _ActiveFeatureCount, _FeatureMeans, _TargetMean, _CenteredRows, _CenteredTargets, _GramMatrix, _Projection, _TargetSquareNorm, _SampleGram, _Eigenvalues),
			posterior_iteration_summary(Summary, Options, Alpha0, Beta0, InitialWeights, InitialResidualSumOfSquares, InitialGamma, InitialLogEvidence, InitialStabilization),
			updated_weight_precision(InitialWeights, InitialGamma, Alpha0, Options, Alpha1),
			updated_noise_precision(RowCount, InitialGamma, InitialResidualSumOfSquares, Beta0, Options, Beta1),
			optimize_bayesian_ridge_model(Summary, Options, 0, InitialWeights, Alpha1, Beta1, Bias, ActiveWeights, PosteriorCovariance, Alpha, Beta, Convergence, Iterations, FinalDelta, LogEvidence, [InitialLogEvidence], Scores, InitialStabilization, Stabilization),
			expand_weights(ActiveFlags, ActiveWeights, Weights0),
			(   FeatureCount =:= 0 ->
				Weights = []
			;   Weights = Weights0
			),
			NoiseVariance is 1.0 / Beta,
			active_feature_count(ActiveFlags, ActiveFeatureCount),
			posterior_bias_variance(PosteriorCovariance, BiasVariance),
			posterior_weight_variances(PosteriorCovariance, ActiveFlags, WeightVariances),
			stabilization_metrics(Stabilization, StabilizationAttempts, StabilizationJitter),
			TrainingDiagnostics = [
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
				final_delta(FinalDelta)
			]
		).

	train_intercept_only_bayesian_model(Rows, FeatureCount, Options, ActiveFlags, Bias, Weights, PosteriorCovariance, NoiseVariance, TrainingDiagnostics) :-
		rows_targets(Rows, Targets),
		length(Targets, RowCount),
		arithmetic_mean(Targets, Bias),
		zero_vector(FeatureCount, Weights),
		residual_sum_of_squares(Rows, Bias, Weights, ResidualSumOfSquares),
		initial_precisions(Rows, Options, Alpha0, Beta0),
		updated_weight_precision([], 0.0, Alpha0, Options, Alpha),
		updated_noise_precision(RowCount, 0.0, ResidualSumOfSquares, Beta0, Options, Beta),
		NoiseVariance is 1.0 / Beta,
		BiasVariance = 0.0,
		PosteriorCovariance = [[BiasVariance]],
		log_evidence(RowCount, 0, Alpha, Beta, 0.0, ResidualSumOfSquares, 0.0, Options, LogEvidence),
		Scores = [LogEvidence, LogEvidence],
		posterior_weight_variances(PosteriorCovariance, ActiveFlags, WeightVariances),
		TrainingDiagnostics = [
			solver(cholesky_factorization),
			stabilization_attempts(0),
			stabilization_jitter(0.0),
			precision_bounds(MinimumPrecision, MaximumPrecision),
			weight_precision_hyperprior(gamma(LambdaShape, LambdaRate)),
			noise_precision_hyperprior(gamma(AlphaShape, AlphaRate)),
			weight_precision(Alpha),
			noise_precision(Beta),
			noise_variance(NoiseVariance),
			log_evidence(LogEvidence),
			scores(Scores),
			active_feature_count(0),
			weight_prior(isotropic_zero_mean_gaussian),
			intercept_treatment(non_probabilistic),
			bias_variance(BiasVariance),
			weight_variances(WeightVariances),
			convergence_metric(coefficient_l1),
			convergence(tolerance),
			iterations(1),
			final_delta(0.0)
		],
		training_precision_bounds(Options, MinimumPrecision, MaximumPrecision),
		weight_precision_hyperparameters(Options, LambdaShape, LambdaRate),
		noise_precision_hyperparameters(Options, AlphaShape, AlphaRate).

	training_precision_bounds(Options, MinimumPrecision, MaximumPrecision) :-
		^^option(precision_bounds(MinimumPrecision, MaximumPrecision), Options).

	initial_precisions(Rows, Options, Alpha, Beta) :-
		^^option(initial_weight_precision(Alpha), Options),
		^^option(initial_noise_precision(NoisePrecisionOption), Options),
		initial_noise_precision(NoisePrecisionOption, Rows, Beta).

	weight_precision_hyperparameters(Options, Shape, Rate) :-
		^^option(lambda_1(Shape), Options),
		^^option(lambda_2(Rate), Options).

	noise_precision_hyperparameters(Options, Shape, Rate) :-
		^^option(alpha_1(Shape), Options),
		^^option(alpha_2(Rate), Options).

	initial_noise_precision(auto, Rows, Beta) :-
		rows_targets(Rows, Targets),
		length(Targets, Count),
		(   Count > 1 ->
			variance(Targets, TargetVariance0)
		;   TargetVariance0 = 0.0
		),
		(   TargetVariance0 > 1.0e-12 ->
			Beta is 1.0 / TargetVariance0
		;   Beta = 1.0
		),
		!.
	initial_noise_precision(Beta, _Rows, Beta).

	rows_targets([], []).
	rows_targets([_Features-Target| Rows], [Target| Targets]) :-
		rows_targets(Rows, Targets).

	prepare_bayesian_training_summary(Rows, Summary) :-
		Rows = [Features-_Target| _],
		length(Rows, RowCount),
		length(Features, FeatureCount),
		zero_vector(FeatureCount, Zeroes),
		sum_row_features_targets(Rows, Zeroes, 0.0, SumFeatures, SumTargets),
		Scale is 1.0 / RowCount,
		scale_vector(SumFeatures, Scale, FeatureMeans),
		TargetMean is SumTargets / RowCount,
		center_rows(Rows, FeatureMeans, TargetMean, CenteredRows, CenteredTargets),
		zero_matrix(FeatureCount, FeatureCount, ZeroGram),
		zero_vector(FeatureCount, ZeroProjection),
		accumulate_centered_statistics(CenteredRows, CenteredTargets, ZeroGram, ZeroProjection, 0.0, GramMatrix, Projection, TargetSquareNorm),
		(   RowCount < FeatureCount ->
			sample_gram_matrix(CenteredRows, SampleGram),
			SpectralMatrix = SampleGram
		;   SampleGram = [],
			SpectralMatrix = GramMatrix
		),
		extract_positive_eigenvalues(SpectralMatrix, Eigenvalues),
		Summary = bayesian_training_summary(RowCount, FeatureCount, FeatureMeans, TargetMean, CenteredRows, CenteredTargets, GramMatrix, Projection, TargetSquareNorm, SampleGram, Eigenvalues).

	sum_row_features_targets([], SumFeatures, SumTargets, SumFeatures, SumTargets).
	sum_row_features_targets([Features-Target| Rows], SumFeatures0, SumTargets0, SumFeatures, SumTargets) :-
		add_scaled_vector(Features, 1.0, SumFeatures0, SumFeatures1),
		SumTargets1 is SumTargets0 + Target,
		sum_row_features_targets(Rows, SumFeatures1, SumTargets1, SumFeatures, SumTargets).

	center_rows([], _FeatureMeans, _TargetMean, [], []).
	center_rows([Features-Target| Rows], FeatureMeans, TargetMean, [CenteredFeatures| CenteredRows], [CenteredTarget| CenteredTargets]) :-
		subtract_vectors(Features, FeatureMeans, CenteredFeatures),
		CenteredTarget is Target - TargetMean,
		center_rows(Rows, FeatureMeans, TargetMean, CenteredRows, CenteredTargets).

	subtract_vectors([], [], []).
	subtract_vectors([Value| Values], [Mean| Means], [Difference| Differences]) :-
		Difference is Value - Mean,
		subtract_vectors(Values, Means, Differences).

	accumulate_centered_statistics([], [], GramMatrix, Projection, TargetSquareNorm, GramMatrix, Projection, TargetSquareNorm).
	accumulate_centered_statistics([Features| Rows], [Target| Targets], GramMatrix0, Projection0, TargetSquareNorm0, GramMatrix, Projection, TargetSquareNorm) :-
		add_scaled_outer_product(Features, 1.0, GramMatrix0, GramMatrix1),
		add_scaled_vector(Features, Target, Projection0, Projection1),
		TargetSquareNorm1 is TargetSquareNorm0 + Target * Target,
		accumulate_centered_statistics(Rows, Targets, GramMatrix1, Projection1, TargetSquareNorm1, GramMatrix, Projection, TargetSquareNorm).

	sample_gram_matrix([], []) :-
		!.
	sample_gram_matrix(Rows, SampleGram) :-
		sample_gram_rows(Rows, Rows, SampleGram).

	sample_gram_rows([], _AllRows, []).
	sample_gram_rows([Row| Rows], AllRows, [SampleRow| SampleGram]) :-
		row_dot_products(AllRows, Row, SampleRow),
		sample_gram_rows(Rows, AllRows, SampleGram).

	row_dot_products([], _Row, []).
	row_dot_products([OtherRow| OtherRows], Row, [Value| Values]) :-
		dot_product(Row, OtherRow, Value),
		row_dot_products(OtherRows, Row, Values).

	optimize_bayesian_ridge_model(Summary, Options, Iteration, PreviousWeights, Alpha0, Beta0, Bias, Weights, PosteriorCovariance, Alpha, Beta, Convergence, Iterations, FinalDelta, LogEvidence, Scores0, Scores, AccumulatedStabilization, Stabilization) :-
		Summary = bayesian_training_summary(RowCount, _FeatureCount, _FeatureMeans, _TargetMean, _CenteredRows, _CenteredTargets, _GramMatrix, _Projection, _TargetSquareNorm, _SampleGram, _Eigenvalues),
		posterior_iteration_summary(Summary, Options, Alpha0, Beta0, Weights0, ResidualSumOfSquares, Gamma, LogEvidence0, IterationStabilization),
		merge_stabilization(AccumulatedStabilization, IterationStabilization, Stabilization0),
		append(Scores0, [LogEvidence0], Scores1),
		coefficient_delta(PreviousWeights, Weights0, Delta),
		NextIteration is Iteration + 1,
		^^option(tolerance(Tolerance), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Delta =< Tolerance ->
			finalize_bayesian_posterior(Summary, Alpha0, Beta0, Bias, Weights, PosteriorCovariance, FinalStabilization),
			Alpha = Alpha0,
			Beta = Beta0,
			Convergence = tolerance,
			Iterations = NextIteration,
			FinalDelta = Delta,
			LogEvidence = LogEvidence0,
			Scores = Scores1,
			merge_stabilization(Stabilization0, FinalStabilization, Stabilization)
		;   NextIteration >= MaximumIterations ->
			finalize_bayesian_posterior(Summary, Alpha0, Beta0, Bias, Weights, PosteriorCovariance, FinalStabilization),
			Alpha = Alpha0,
			Beta = Beta0,
			Convergence = maximum_iterations_exhausted,
			Iterations = NextIteration,
			FinalDelta = Delta,
			LogEvidence = LogEvidence0,
			Scores = Scores1,
			merge_stabilization(Stabilization0, FinalStabilization, Stabilization)
		;   updated_weight_precision(Weights0, Gamma, Alpha0, Options, Alpha1),
			updated_noise_precision(RowCount, Gamma, ResidualSumOfSquares, Beta0, Options, Beta1),
			optimize_bayesian_ridge_model(Summary, Options, NextIteration, Weights0, Alpha1, Beta1, Bias, Weights, PosteriorCovariance, Alpha, Beta, Convergence, Iterations, FinalDelta, LogEvidence, Scores1, Scores, Stabilization0, Stabilization)
		).

	posterior_iteration_summary(Summary, Options, Alpha, Beta, Weights, ResidualSumOfSquares, Gamma, LogEvidence, Stabilization) :-
		Summary = bayesian_training_summary(RowCount, FeatureCount, _FeatureMeans, _TargetMean, CenteredRows, CenteredTargets, GramMatrix, Projection, TargetSquareNorm, SampleGram, Eigenvalues),
		(   RowCount < FeatureCount ->
			sample_space_iteration_summary(CenteredRows, CenteredTargets, SampleGram, RowCount, FeatureCount, Projection, GramMatrix, TargetSquareNorm, Eigenvalues, Alpha, Beta, Options, Weights, ResidualSumOfSquares, Gamma, LogEvidence, Stabilization)
		;   weight_space_iteration_summary(GramMatrix, Projection, TargetSquareNorm, RowCount, FeatureCount, Eigenvalues, Alpha, Beta, Options, Weights, ResidualSumOfSquares, Gamma, LogEvidence, Stabilization)
		).

	weight_space_iteration_summary(GramMatrix, Projection, TargetSquareNorm, RowCount, FeatureCount, Eigenvalues, Alpha, Beta, Options, Weights, ResidualSumOfSquares, Gamma, LogEvidence, Stabilization) :-
		build_weight_precision_matrix(GramMatrix, Alpha, Beta, Matrix),
		scale_vector(Projection, Beta, Vector),
		factorize_precision_matrix(Matrix, CholeskyFactor, Stabilization),
		solve_cholesky(CholeskyFactor, Vector, Weights),
		gamma_from_eigenvalues(Eigenvalues, Alpha, Beta, Gamma0),
		clamp_gamma(FeatureCount, Gamma0, Gamma),
		residual_sum_of_squares_from_statistics(TargetSquareNorm, Projection, GramMatrix, Weights, ResidualSumOfSquares),
		weight_square_norm(Weights, WeightSquaredNorm),
		log_determinant_sigma_from_cholesky(CholeskyFactor, LogDeterminantSigma),
		log_evidence(RowCount, FeatureCount, Alpha, Beta, LogDeterminantSigma, ResidualSumOfSquares, WeightSquaredNorm, Options, LogEvidence).

	sample_space_iteration_summary(CenteredRows, CenteredTargets, SampleGram, RowCount, FeatureCount, Projection, GramMatrix, TargetSquareNorm, Eigenvalues, Alpha, Beta, Options, Weights, ResidualSumOfSquares, Gamma, LogEvidence, Stabilization) :-
		build_sample_space_matrix(SampleGram, Alpha, Beta, Matrix),
		factorize_precision_matrix(Matrix, CholeskyFactor, Stabilization),
		solve_cholesky(CholeskyFactor, CenteredTargets, DualWeights),
		weighted_feature_projection(CenteredRows, DualWeights, ProjectionFromDual),
		Scale is Beta / Alpha,
		scale_vector(ProjectionFromDual, Scale, Weights),
		gamma_from_eigenvalues(Eigenvalues, Alpha, Beta, Gamma0),
		clamp_gamma(FeatureCount, Gamma0, Gamma),
		residual_sum_of_squares_from_statistics(TargetSquareNorm, Projection, GramMatrix, Weights, ResidualSumOfSquares),
		weight_square_norm(Weights, WeightSquaredNorm),
		log_determinant_sigma_from_sample_space_cholesky(CholeskyFactor, FeatureCount, Alpha, LogDeterminantSigma),
		log_evidence(RowCount, FeatureCount, Alpha, Beta, LogDeterminantSigma, ResidualSumOfSquares, WeightSquaredNorm, Options, LogEvidence).

	finalize_bayesian_posterior(Summary, Alpha, Beta, Bias, Weights, PosteriorCovariance, Stabilization) :-
		Summary = bayesian_training_summary(RowCount, _FeatureCount, FeatureMeans, TargetMean, _CenteredRows, _CenteredTargets, GramMatrix, Projection, _TargetSquareNorm, _SampleGram, _Eigenvalues),
		build_weight_precision_matrix(GramMatrix, Alpha, Beta, Matrix),
		scale_vector(Projection, Beta, Vector),
		factorize_precision_matrix(Matrix, CholeskyFactor, Stabilization),
		solve_cholesky(CholeskyFactor, Vector, Weights),
		invert_from_cholesky(CholeskyFactor, WeightCovariance),
		build_full_posterior_covariance(RowCount, Beta, FeatureMeans, WeightCovariance, PosteriorCovariance),
		linear_response(Weights, FeatureMeans, FeatureMeanResponse),
		Bias is TargetMean - FeatureMeanResponse.

	build_full_posterior_covariance(_RowCount, _Beta, FeatureMeans, WeightCovariance, [[0.0| BiasCovariances]| PosteriorRows]) :-
		zero_vector_like(FeatureMeans, BiasCovariances),
		prepend_bias_covariances(WeightCovariance, BiasCovariances, PosteriorRows).

	prepend_bias_covariances([], [], []).
	prepend_bias_covariances([Row| Rows], [BiasCovariance| BiasCovariances], [[BiasCovariance| Row]| PosteriorRows]) :-
		prepend_bias_covariances(Rows, BiasCovariances, PosteriorRows).

	merge_stabilization(stabilization(Attempts0, Jitter0), stabilization(Attempts1, Jitter1), stabilization(Attempts, Jitter)) :-
		Attempts is max(Attempts0, Attempts1),
		Jitter is max(Jitter0, Jitter1).

	stabilization_metrics(stabilization(Attempts, Jitter), Attempts, Jitter).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Bias', 'Weights', 'ActiveFlags', 'PosteriorCovariance', 'NoiseVariance', 'Diagnostics'].

	regressor_term_template(
		bayesian_ridge_regressor(_Encoders, _Bias, _Weights, _ActiveFlags, _PosteriorCovariance, _NoiseVariance, _Diagnostics),
		bayesian_ridge_regressor('Encoders', 'Bias', 'Weights', 'ActiveFlags', 'PosteriorCovariance', 'NoiseVariance', 'Diagnostics')
	).

	check_regressor(Regressor) :-
		(   Regressor = bayesian_ridge_regressor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics),
			^^valid_regression_encoders(Encoders),
			valid(float, Bias),
			^^encoded_feature_count(Encoders, FeatureCount),
			valid(list(float, FeatureCount), Weights),
			valid_active_flags(ActiveFlags, FeatureCount),
			active_feature_count(ActiveFlags, ActiveFeatureCount),
			CovarianceSize is ActiveFeatureCount + 1,
			valid_square_matrix(PosteriorCovariance, CovarianceSize),
			valid(non_negative_float, NoiseVariance),
			^^valid_regressor_metadata(bayesian_ridge_regression, Diagnostics),
			valid_bayesian_ridge_diagnostics(Diagnostics, FeatureCount, ActiveFeatureCount) ->
			true
		;   domain_error(regressor, Regressor)
		).

	valid_active_flags(ActiveFlags, FeatureCount) :-
		valid(list(one_of(atom, [active, inactive])), ActiveFlags),
		length(ActiveFlags, FeatureCount).

	valid_square_matrix(Matrix, Size) :-
		integer(Size),
		Size > 0,
		length(Matrix, Size),
		valid(list(list(float, Size)), Matrix).

	valid_bayesian_ridge_diagnostics(Diagnostics, FeatureCount, ActiveFeatureCount) :-
		memberchk(solver(cholesky_factorization), Diagnostics),
		memberchk(stabilization_attempts(StabilizationAttempts), Diagnostics),
		integer(StabilizationAttempts),
		StabilizationAttempts >= 0,
		memberchk(stabilization_jitter(StabilizationJitter), Diagnostics),
		valid(non_negative_float, StabilizationJitter),
		memberchk(precision_bounds(MinimumPrecision, MaximumPrecision), Diagnostics),
		valid_option(precision_bounds(MinimumPrecision, MaximumPrecision)),
		memberchk(weight_precision_hyperprior(gamma(LambdaShape, LambdaRate)), Diagnostics),
		valid(non_negative_float, LambdaShape),
		valid(non_negative_float, LambdaRate),
		memberchk(noise_precision_hyperprior(gamma(AlphaShape, AlphaRate)), Diagnostics),
		valid(non_negative_float, AlphaShape),
		valid(non_negative_float, AlphaRate),
		memberchk(weight_precision(Alpha), Diagnostics),
		valid(positive_float, Alpha),
		memberchk(noise_precision(Beta), Diagnostics),
		valid(positive_float, Beta),
		memberchk(noise_variance(NoiseVariance), Diagnostics),
		valid(non_negative_float, NoiseVariance),
		memberchk(log_evidence(LogEvidence), Diagnostics),
		valid(float, LogEvidence),
		memberchk(scores(Scores), Diagnostics),
		valid_score_trace(Scores, Diagnostics, LogEvidence),
		memberchk(active_feature_count(ActiveFeatureCount), Diagnostics),
		integer(ActiveFeatureCount),
		ActiveFeatureCount >= 0,
		memberchk(weight_prior(isotropic_zero_mean_gaussian), Diagnostics),
		memberchk(intercept_treatment(non_probabilistic), Diagnostics),
		memberchk(bias_variance(BiasVariance), Diagnostics),
		valid(non_negative_float, BiasVariance),
		memberchk(weight_variances(WeightVariances), Diagnostics),
		valid(list(non_negative_float, FeatureCount), WeightVariances),
		memberchk(convergence_metric(coefficient_l1), Diagnostics),
		^^valid_linear_model_diagnostics(Diagnostics),
		^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount).

	valid_score_trace(Scores, Diagnostics, LogEvidence) :-
		valid(list(float), Scores),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 1,
		ExpectedCount is Iterations + 1,
		length(Scores, ExpectedCount),
		last_score(Scores, LogEvidence).

	last_score([Score], Score) :-
		!.
	last_score([_| Scores], Score) :-
		last_score(Scores, Score).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = bayesian_ridge_regressor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics),
		Clause =.. [Functor, Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = bayesian_ridge_regressor(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics),
		length(PosteriorCovariance, CovarianceSize),
		format('Bayesian Ridge Regression Regressor~n', []),
		format('===================================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Bias: ~4f~n', [Bias]),
		format('Noise variance: ~12g~n', [NoiseVariance]),
		format('Weights: ~w coefficients~n', [Weights]),
		format('Active flags: ~w~n', [ActiveFlags]),
		format('Posterior covariance size: ~w x ~w~n~n', [CovarianceSize, CovarianceSize]),
		format('Encoders: ~w~n', [Encoders]).

	posterior_bias_variance([[BiasVariance| _Rest]| _Rows], BiasVariance).

	predictive_posterior_variance(_PosteriorCovariance, [], 0.0) :-
		!.
	predictive_posterior_variance(PosteriorCovariance, ActiveFeatures, PosteriorVariance) :-
		posterior_weight_covariance(PosteriorCovariance, WeightCovariance),
		quadratic_form(ActiveFeatures, WeightCovariance, PosteriorVariance0),
		(   PosteriorVariance0 >= 0.0 ->
			PosteriorVariance = PosteriorVariance0
		;   PosteriorVariance0 >= -1.0e-10 ->
			PosteriorVariance = 0.0
		;   domain_error(non_negative_posterior_variance, PosteriorVariance0)
		).

	posterior_weight_covariance([_BiasRow| Rows], WeightCovariance) :-
		remove_bias_column(Rows, WeightCovariance).

	remove_bias_column([], []).
	remove_bias_column([[_BiasCovariance| Row]| Rows], [Row| WeightCovariance]) :-
		remove_bias_column(Rows, WeightCovariance).

	posterior_weight_variances(PosteriorCovariance, ActiveFlags, Variances) :-
		active_diagonal_variances(PosteriorCovariance, 2, ActiveVariances),
		expand_active_variances(ActiveFlags, ActiveVariances, Variances).

	active_diagonal_variances(PosteriorCovariance, Index, ActiveVariances) :-
		length(PosteriorCovariance, Size),
		(   Index > Size ->
			ActiveVariances = []
		;   diagonal_entry(PosteriorCovariance, Index, Variance),
			ActiveVariances = [Variance| Rest],
			NextIndex is Index + 1,
			active_diagonal_variances(PosteriorCovariance, NextIndex, Rest)
		).

	expand_active_variances([], [], []).
	expand_active_variances([active| ActiveFlags], [Variance| ActiveVariances], [Variance| Variances]) :-
		!,
		expand_active_variances(ActiveFlags, ActiveVariances, Variances).
	expand_active_variances([inactive| ActiveFlags], ActiveVariances, [0.0| Variances]) :-
		expand_active_variances(ActiveFlags, ActiveVariances, Variances).

	build_weight_precision_matrix(GramMatrix, Alpha, Beta, Matrix) :-
		build_weight_precision_matrix(GramMatrix, Alpha, Beta, 1, Matrix).

	build_weight_precision_matrix([], _Alpha, _Beta, _Index, []).
	build_weight_precision_matrix([Row0| Rows0], Alpha, Beta, Index, [Row| Rows]) :-
		build_weight_precision_row(Row0, Alpha, Beta, Index, 1, Row),
		NextIndex is Index + 1,
		build_weight_precision_matrix(Rows0, Alpha, Beta, NextIndex, Rows).

	build_weight_precision_row([], _Alpha, _Beta, _RowIndex, _ColumnIndex, []).
	build_weight_precision_row([Value0| Values0], Alpha, Beta, RowIndex, ColumnIndex, [Value| Values]) :-
		ScaledValue is Beta * Value0,
		(   RowIndex =:= ColumnIndex ->
			Value is ScaledValue + Alpha
		;   Value = ScaledValue
		),
		NextColumnIndex is ColumnIndex + 1,
		build_weight_precision_row(Values0, Alpha, Beta, RowIndex, NextColumnIndex, Values).

	build_sample_space_matrix(SampleGram, Alpha, Beta, Matrix) :-
		Ratio is Beta / Alpha,
		build_sample_space_matrix(SampleGram, Ratio, 1, Matrix).

	build_sample_space_matrix([], _Ratio, _Index, []).
	build_sample_space_matrix([Row0| Rows0], Ratio, Index, [Row| Rows]) :-
		build_sample_space_row(Row0, Ratio, Index, 1, Row),
		NextIndex is Index + 1,
		build_sample_space_matrix(Rows0, Ratio, NextIndex, Rows).

	build_sample_space_row([], _Ratio, _RowIndex, _ColumnIndex, []).
	build_sample_space_row([Value0| Values0], Ratio, RowIndex, ColumnIndex, [Value| Values]) :-
		ScaledValue is Ratio * Value0,
		(   RowIndex =:= ColumnIndex ->
			Value is ScaledValue + 1.0
		;   Value = ScaledValue
		),
		NextColumnIndex is ColumnIndex + 1,
		build_sample_space_row(Values0, Ratio, RowIndex, NextColumnIndex, Values).

	updated_weight_precision(Weights, Gamma, _Alpha0, Options, Alpha) :-
		weight_square_norm(Weights, WeightSquaredNorm),
		weight_precision_hyperparameters(Options, LambdaShape, LambdaRate),
		training_precision_bounds(Options, MinimumPrecision, MaximumPrecision),
		Denominator is WeightSquaredNorm + 2.0 * LambdaRate,
		(   Denominator =< MinimumPrecision ->
			Alpha = MaximumPrecision
		;
			Alpha1 is (Gamma + 2.0 * LambdaShape) / Denominator,
			clamp_precision(Alpha1, Options, Alpha)
		).

	updated_noise_precision(RowCount, Gamma, ResidualSumOfSquares, _Beta0, Options, Beta) :-
		noise_precision_hyperparameters(Options, AlphaShape, AlphaRate),
		training_precision_bounds(Options, MinimumPrecision, MaximumPrecision),
		EffectiveDegreesOfFreedom0 is RowCount - Gamma,
		(   EffectiveDegreesOfFreedom0 > MinimumPrecision ->
			EffectiveDegreesOfFreedom = EffectiveDegreesOfFreedom0
		;   EffectiveDegreesOfFreedom = MinimumPrecision
		),
		Denominator is ResidualSumOfSquares + 2.0 * AlphaRate,
		(   Denominator =< MinimumPrecision ->
			Beta = MaximumPrecision
		;   Beta1 is (EffectiveDegreesOfFreedom + 2.0 * AlphaShape) / Denominator,
			clamp_precision(Beta1, Options, Beta)
		).

	gamma_from_eigenvalues(Eigenvalues, Alpha, Beta, Gamma) :-
		gamma_from_eigenvalues(Eigenvalues, Alpha, Beta, 0.0, Gamma).

	gamma_from_eigenvalues([], _Alpha, _Beta, Gamma, Gamma).
	gamma_from_eigenvalues([Eigenvalue0| Eigenvalues], Alpha, Beta, Gamma0, Gamma) :-
		(   Eigenvalue0 > 0.0 ->
			Contribution is (Beta * Eigenvalue0) / (Alpha + Beta * Eigenvalue0)
		;   Contribution = 0.0
		),
		Gamma1 is Gamma0 + Contribution,
		gamma_from_eigenvalues(Eigenvalues, Alpha, Beta, Gamma1, Gamma).

	coefficient_delta(Weights0, Weights1, Delta) :-
		coefficient_delta(Weights0, Weights1, 0.0, Delta).

	coefficient_delta([], [], Delta, Delta).
	coefficient_delta([Weight0| Weights0], [Weight1| Weights1], Delta0, Delta) :-
		Delta1 is Delta0 + abs(Weight1 - Weight0),
		coefficient_delta(Weights0, Weights1, Delta1, Delta).

	clamp_precision(Value0, Options, Value) :-
		training_precision_bounds(Options, MinimumPrecision, MaximumPrecision),
		(   Value0 < MinimumPrecision ->
			Value = MinimumPrecision
		;   Value0 > MaximumPrecision ->
			Value = MaximumPrecision
		;   Value = Value0
		).

	clamp_gamma(FeatureCount, Gamma0, Gamma) :-
		(   Gamma0 < 0.0 ->
			Gamma = 0.0
		;   Gamma0 > FeatureCount ->
			Gamma = FeatureCount
		;   Gamma = Gamma0
		).

	residual_sum_of_squares_from_statistics(TargetSquareNorm, Projection, GramMatrix, Weights, ResidualSumOfSquares) :-
		dot_product(Weights, Projection, ProjectionDot),
		matrix_vector_product(GramMatrix, Weights, GramWeights),
		dot_product(Weights, GramWeights, QuadraticTerm),
		ResidualSumOfSquares0 is TargetSquareNorm - 2.0 * ProjectionDot + QuadraticTerm,
		(   ResidualSumOfSquares0 >= 0.0 ->
			ResidualSumOfSquares = ResidualSumOfSquares0
		;   ResidualSumOfSquares0 >= -1.0e-10 ->
			ResidualSumOfSquares = 0.0
		;   domain_error(non_negative_residual_sum_of_squares, ResidualSumOfSquares0)
		).

	weight_square_norm(Weights, WeightSquaredNorm) :-
		weight_square_norm(Weights, 0.0, WeightSquaredNorm).

	weight_square_norm([], WeightSquaredNorm, WeightSquaredNorm).
	weight_square_norm([Weight| Weights], WeightSquaredNorm0, WeightSquaredNorm) :-
		WeightSquaredNorm1 is WeightSquaredNorm0 + Weight * Weight,
		weight_square_norm(Weights, WeightSquaredNorm1, WeightSquaredNorm).

	log_determinant_sigma_from_cholesky(CholeskyFactor, LogDeterminantSigma) :-
		sum_log_diagonal(CholeskyFactor, 1, 0.0, LogDeterminantHalf),
		LogDeterminantSigma is -2.0 * LogDeterminantHalf.

	log_determinant_sigma_from_sample_space_cholesky(CholeskyFactor, FeatureCount, Alpha, LogDeterminantSigma) :-
		sum_log_diagonal(CholeskyFactor, 1, 0.0, LogDeterminantHalf),
		LogDeterminantB is 2.0 * LogDeterminantHalf,
		LogDeterminantSigma is -FeatureCount * log(Alpha) - LogDeterminantB.

	log_evidence(RowCount, FeatureCount, Alpha, Beta, LogDeterminantSigma, ResidualSumOfSquares, WeightSquaredNorm, Options, LogEvidence) :-
		weight_precision_hyperparameters(Options, LambdaShape, LambdaRate),
		noise_precision_hyperparameters(Options, AlphaShape, AlphaRate),
		LogTwoPi is log(2.0 * pi),
		HyperpriorTerms is LambdaShape * log(Alpha) - LambdaRate * Alpha + AlphaShape * log(Beta) - AlphaRate * Beta,
		LikelihoodTerms is 0.5 * (
			FeatureCount * log(Alpha) +
			RowCount * log(Beta) -
			Alpha * WeightSquaredNorm -
			Beta * ResidualSumOfSquares +
			LogDeterminantSigma -
			RowCount * LogTwoPi
		),
		LogEvidence is HyperpriorTerms + LikelihoodTerms.

	weighted_feature_projection([], [], Projection) :-
		Projection = [].
	weighted_feature_projection([FirstRow| Rows], Coefficients, Projection) :-
		zero_vector_like(FirstRow, Zeroes),
		weighted_feature_projection([FirstRow| Rows], Coefficients, Zeroes, Projection).

	weighted_feature_projection([], [], Projection, Projection).
	weighted_feature_projection([Row| Rows], [Coefficient| Coefficients], Projection0, Projection) :-
		add_scaled_vector(Row, Coefficient, Projection0, Projection1),
		weighted_feature_projection(Rows, Coefficients, Projection1, Projection).

	quadratic_form(Vector, Matrix, Value) :-
		matrix_vector_product(Matrix, Vector, Product),
		dot_product(Vector, Product, Value).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	extract_positive_eigenvalues([], []) :-
		!.
	extract_positive_eigenvalues([[]| _], []) :-
		!.
	extract_positive_eigenvalues(Matrix, Eigenvalues) :-
		length(Matrix, Requested),
		extract_positive_eigenvalues(Matrix, Requested, [], Eigenvalues).

	extract_positive_eigenvalues(_Matrix, 0, EigenvaluesAcc, Eigenvalues) :-
		!,
		reverse(EigenvaluesAcc, Eigenvalues).
	extract_positive_eigenvalues(Matrix, Requested, EigenvaluesAcc, Eigenvalues) :-
		principal_component(Matrix, Eigenvalue, Eigenvector),
		spectral_tolerance(Tolerance),
		(   Eigenvalue =< Tolerance ->
			reverse(EigenvaluesAcc, Eigenvalues)
		;   add_scaled_outer_product(Eigenvector, -Eigenvalue, Matrix, DeflatedMatrix),
			NextRequested is Requested - 1,
			extract_positive_eigenvalues(DeflatedMatrix, NextRequested, [Eigenvalue| EigenvaluesAcc], Eigenvalues)
		).

	principal_component(Matrix, Eigenvalue, Eigenvector) :-
		length(Matrix, Size),
		initial_vectors(Size, InitialVectors),
		zero_vector(Size, ZeroVector),
		principal_component_candidates(Matrix, InitialVectors, 0.0, ZeroVector, Eigenvalue, Eigenvector).

	principal_component_candidates(_Matrix, [], BestEigenvalue, BestEigenvector, BestEigenvalue, BestEigenvector) :-
		!.
	principal_component_candidates(Matrix, [InitialVector| InitialVectors], BestEigenvalue0, BestEigenvector0, BestEigenvalue, BestEigenvector) :-
		normalize_vector(InitialVector, NormalizedInitial),
		iterate_component(Matrix, 0, NormalizedInitial, CandidateEigenvalue, CandidateEigenvector),
		(   CandidateEigenvalue > BestEigenvalue0 ->
			BestEigenvalue1 = CandidateEigenvalue,
			BestEigenvector1 = CandidateEigenvector
		;   BestEigenvalue1 = BestEigenvalue0,
			BestEigenvector1 = BestEigenvector0
		),
		principal_component_candidates(Matrix, InitialVectors, BestEigenvalue1, BestEigenvector1, BestEigenvalue, BestEigenvector).

	iterate_component(Matrix, Iteration, Vector0, Eigenvalue, Eigenvector) :-
		matrix_vector_product(Matrix, Vector0, Product),
		euclidean_norm(Product, Norm),
		spectral_tolerance(Tolerance),
		(   Norm =< Tolerance ->
			Eigenvalue = 0.0,
			Eigenvector = Vector0
		;   scale_vector(Product, 1.0 / Norm, Vector1),
			stabilize_vector_sign(Vector1, StableVector),
			difference_norm(StableVector, Vector0, Delta),
			spectral_maximum_iterations(MaximumIterations),
			(   (Delta =< Tolerance ; Iteration >= MaximumIterations) ->
				rayleigh_quotient(Matrix, StableVector, Eigenvalue),
				Eigenvector = StableVector
			;   NextIteration is Iteration + 1,
				iterate_component(Matrix, NextIteration, StableVector, Eigenvalue, Eigenvector)
			)
		).

	rayleigh_quotient(Matrix, Vector, Eigenvalue) :-
		matrix_vector_product(Matrix, Vector, Product),
		dot_product(Vector, Product, Eigenvalue).

	initial_vectors(Size, [InitialVector| BasisVectors]) :-
		initial_vector(Size, InitialVector),
		basis_initial_vectors(1, Size, BasisVectors).

	initial_vector(0, []) :-
		!.
	initial_vector(Size, [1.0| Vector]) :-
		Size > 0,
		NextSize is Size - 1,
		initial_vector(NextSize, Vector).

	basis_initial_vectors(Index, Size, []) :-
		Index > Size,
		!.
	basis_initial_vectors(Index, Size, [BasisVector| BasisVectors]) :-
		basis_vector(Size, Index, BasisVector),
		NextIndex is Index + 1,
		basis_initial_vectors(NextIndex, Size, BasisVectors).

	basis_vector(Size, Index, Vector) :-
		basis_vector(1, Size, Index, Vector).

	basis_vector(Current, Size, _Index, []) :-
		Current > Size,
		!.
	basis_vector(Index, Size, Index, [1.0| Vector]) :-
		!,
		Next is Index + 1,
		basis_vector(Next, Size, Index, Vector).
	basis_vector(Current, Size, Index, [0.0| Vector]) :-
		Next is Current + 1,
		basis_vector(Next, Size, Index, Vector).

	normalize_vector(Vector, NormalizedVector) :-
		euclidean_norm(Vector, Norm),
		spectral_tolerance(Tolerance),
		(   Norm =< Tolerance ->
			NormalizedVector = Vector
		;   scale_vector(Vector, 1.0 / Norm, NormalizedVector)
		).

	euclidean_norm(Vector, Norm) :-
		dot_product(Vector, Vector, SumSquares),
		Norm is sqrt(SumSquares).

	difference_norm(Vector1, Vector2, Norm) :-
		subtract_vectors(Vector1, Vector2, Difference),
		euclidean_norm(Difference, Norm).

	stabilize_vector_sign(Vector, StableVector) :-
		(   first_significant_component(Vector, First),
			First < 0.0 ->
			scale_vector(Vector, -1.0, StableVector)
		;   StableVector = Vector
		).

	first_significant_component([Value| _Values], Value) :-
		abs(Value) > 1.0e-12,
		!.
	first_significant_component([_Value| Values], First) :-
		first_significant_component(Values, First).
	first_significant_component([], 0.0).

	spectral_tolerance(1.0e-10).

	spectral_maximum_iterations(1000).

	residual_sum_of_squares(Rows, Bias, Weights, ResidualSumOfSquares) :-
		residual_sum_of_squares(Rows, Bias, Weights, 0.0, ResidualSumOfSquares).

	residual_sum_of_squares([], _Bias, _Weights, ResidualSumOfSquares, ResidualSumOfSquares).
	residual_sum_of_squares([Features-Target| Rows], Bias, Weights, ResidualSumOfSquares0, ResidualSumOfSquares) :-
		linear_response(Weights, Features, Linear),
		Residual is Bias + Linear - Target,
		ResidualSumOfSquares1 is ResidualSumOfSquares0 + Residual * Residual,
		residual_sum_of_squares(Rows, Bias, Weights, ResidualSumOfSquares1, ResidualSumOfSquares).

	linear_response([], [], 0.0) :-
		!.
	linear_response(Weights, Features, Linear) :-
		dot_product(Weights, Features, Linear).

	bayesian_feature_activity([Features-_Target| Rows], ActiveFlags) :-
		zero_vector_like(Features, Zeroes),
		accumulate_feature_statistics([Features-_Target| Rows], Zeroes, Zeroes, Sums, SumSquares),
		length([Features-_Target| Rows], Count),
		feature_activity_flags(Sums, SumSquares, Count, ActiveFlags).

	accumulate_feature_statistics([], Sums, SumSquares, Sums, SumSquares).
	accumulate_feature_statistics([Features-_Target| Rows], Sums0, SumSquares0, Sums, SumSquares) :-
		add_scaled_vector(Features, 1.0, Sums0, Sums1),
		add_squared_vector(Features, SumSquares0, SumSquares1),
		accumulate_feature_statistics(Rows, Sums1, SumSquares1, Sums, SumSquares).

	add_squared_vector([], [], []).
	add_squared_vector([Feature| Features], [Square0| Squares0], [Square| Squares]) :-
		Square is Square0 + Feature * Feature,
		add_squared_vector(Features, Squares0, Squares).

	feature_activity_flags([], [], _Count, []).
	feature_activity_flags([Sum| Sums], [SumSquares| Squares], Count, [Active| ActiveFlags]) :-
		Mean is Sum / Count,
		Variance0 is SumSquares / Count - Mean * Mean,
		(   Variance0 > 1.0e-12 ->
			Active = active
		;
			Active = inactive
		),
		feature_activity_flags(Sums, Squares, Count, ActiveFlags).

	active_feature_count([], 0).
	active_feature_count([active| ActiveFlags], Count) :-
		!,
		active_feature_count(ActiveFlags, RestCount),
		Count is RestCount + 1.
	active_feature_count([inactive| ActiveFlags], Count) :-
		active_feature_count(ActiveFlags, Count).

	compress_rows([], _ActiveFlags, []).
	compress_rows([Features-Target| Rows], ActiveFlags, [CompressedFeatures-Target| CompressedRows]) :-
		compress_features(Features, ActiveFlags, CompressedFeatures),
		compress_rows(Rows, ActiveFlags, CompressedRows).

	compress_features([], [], []).
	compress_features([Feature| Features], [active| ActiveFlags], [Feature| Compressed]) :-
		!,
		compress_features(Features, ActiveFlags, Compressed).
	compress_features([_Feature| Features], [inactive| ActiveFlags], Compressed) :-
		compress_features(Features, ActiveFlags, Compressed).

	compress_vector([], [], []).
	compress_vector([Value| Values], [active| ActiveFlags], [Value| Compressed]) :-
		!,
		compress_vector(Values, ActiveFlags, Compressed).
	compress_vector([_Value| Values], [inactive| ActiveFlags], Compressed) :-
		compress_vector(Values, ActiveFlags, Compressed).

	expand_weights([], [], []).
	expand_weights([active| ActiveFlags], [Weight| ActiveWeights], [Weight| Weights]) :-
		!,
		expand_weights(ActiveFlags, ActiveWeights, Weights).
	expand_weights([inactive| ActiveFlags], ActiveWeights, [0.0| Weights]) :-
		expand_weights(ActiveFlags, ActiveWeights, Weights).

	add_scaled_outer_product(Vector, Scale, Rows0, Rows) :-
		add_scaled_outer_product(Vector, Vector, Scale, Rows0, Rows).

	add_scaled_outer_product([], _Vector, _Scale, [], []).
	add_scaled_outer_product([Value| Values], Vector, Scale, [Row0| Rows0], [Row| Rows]) :-
		ScaledValue is Scale * Value,
		add_scaled_vector(Vector, ScaledValue, Row0, Row),
		add_scaled_outer_product(Values, Vector, Scale, Rows0, Rows).

	add_scaled_vector([], _Scale, [], []).
	add_scaled_vector([Value| Values], Scale, [Value0| Values0], [UpdatedValue| UpdatedValues]) :-
		UpdatedValue is Value0 + Scale * Value,
		add_scaled_vector(Values, Scale, Values0, UpdatedValues).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	zero_vector_like([], []).
	zero_vector_like([_| Reference], [0.0| Zeroes]) :-
		zero_vector_like(Reference, Zeroes).

	scale_vector([], _Scale, []).
	scale_vector([Value| Values], Scale, [ScaledValue| ScaledValues]) :-
		ScaledValue is Scale * Value,
		scale_vector(Values, Scale, ScaledValues).

	zero_matrix(0, _Columns, []) :-
		!.
	zero_matrix(Rows, Columns, [Row| Matrix]) :-
		Rows > 0,
		zero_vector(Columns, Row),
		NextRows is Rows - 1,
		zero_matrix(NextRows, Columns, Matrix).

	invert_from_cholesky(CholeskyFactor, Inverse) :-
		length(CholeskyFactor, Size),
		identity_columns(Size, Columns),
		solve_inverse_columns(Columns, CholeskyFactor, InverseColumns),
		columns_to_rows(InverseColumns, Inverse).

	identity_columns(Size, Columns) :-
		identity_columns(1, Size, Columns).

	identity_columns(Index, Size, []) :-
		Index > Size,
		!.
	identity_columns(Index, Size, [Column| Columns]) :-
		identity_column(1, Size, Index, Column),
		NextIndex is Index + 1,
		identity_columns(NextIndex, Size, Columns).

	identity_column(Index, Size, _OneIndex, []) :-
		Index > Size,
		!.
	identity_column(Index, Size, OneIndex, [Value| Column]) :-
		(   Index =:= OneIndex ->
			Value = 1.0
		;   Value = 0.0
		),
		NextIndex is Index + 1,
		identity_column(NextIndex, Size, OneIndex, Column).

	solve_inverse_columns([], _CholeskyFactor, []).
	solve_inverse_columns([Column| Columns], CholeskyFactor, [InverseColumn| InverseColumns]) :-
		solve_cholesky(CholeskyFactor, Column, InverseColumn),
		solve_inverse_columns(Columns, CholeskyFactor, InverseColumns).

	columns_to_rows([], []).
	columns_to_rows([Column| Columns], Rows) :-
		length(Column, Size),
		columns_to_rows(1, Size, [Column| Columns], Rows).

	columns_to_rows(Index, Size, _Columns, []) :-
		Index > Size,
		!.
	columns_to_rows(Index, Size, Columns, [Row| Rows]) :-
		column_values_at(Columns, Index, Row),
		NextIndex is Index + 1,
		columns_to_rows(NextIndex, Size, Columns, Rows).

	column_values_at([], _Index, []).
	column_values_at([Column| Columns], Index, [Value| Values]) :-
		nth1(Index, Column, Value),
		column_values_at(Columns, Index, Values).

	diagonal_entry(Matrix, Index, Value) :-
		nth1(Index, Matrix, Row),
		nth1(Index, Row, Value).

	factorize_precision_matrix(Matrix, CholeskyFactor, stabilization(Attempts, Jitter)) :-
		precision_matrix_scale(Matrix, Scale),
		BaseJitter is max(1.0, Scale) * 1.0e-15,
		factorize_precision_matrix(Matrix, 0, 6, BaseJitter, CholeskyFactor, Attempts, Jitter).

	factorize_precision_matrix(Matrix, Attempt, MaxAttempts, _BaseJitter, _CholeskyFactor, _Attempts, _Jitter) :-
		Attempt > MaxAttempts,
		!,
		domain_error(positive_definite_precision_matrix, Matrix).
	factorize_precision_matrix(Matrix0, Attempt, MaxAttempts, BaseJitter, CholeskyFactor, Attempts, Jitter) :-
		add_precision_jitter(Matrix0, Attempt, BaseJitter, Matrix),
		(   catch(cholesky_decomposition(Matrix, CholeskyFactor), error(non_positive_definite_precision_matrix(_Value), _Context), fail) ->
			Attempts = Attempt,
			applied_precision_jitter(Attempt, BaseJitter, Jitter)
		;   NextAttempt is Attempt + 1,
			factorize_precision_matrix(Matrix0, NextAttempt, MaxAttempts, BaseJitter, CholeskyFactor, Attempts, Jitter)
		).

	applied_precision_jitter(0, _BaseJitter, 0.0) :-
		!.
	applied_precision_jitter(Attempt, BaseJitter, Jitter) :-
		Jitter is BaseJitter * 10.0 ** (Attempt - 1).

	precision_matrix_scale(Matrix, Scale) :-
		maximum_diagonal_magnitude(Matrix, 1, 0.0, Scale).

	maximum_diagonal_magnitude([], _Index, Scale, Scale).
	maximum_diagonal_magnitude([Row| Rows], Index, Scale0, Scale) :-
		nth1(Index, Row, Diagonal),
		Magnitude is abs(Diagonal),
		Scale1 is max(Scale0, Magnitude),
		NextIndex is Index + 1,
		maximum_diagonal_magnitude(Rows, NextIndex, Scale1, Scale).

	add_precision_jitter(Matrix, 0, _BaseJitter, Matrix) :-
		!.
	add_precision_jitter(Matrix0, Attempt, BaseJitter, Matrix) :-
		Jitter is BaseJitter * 10.0 ** (Attempt - 1),
		add_diagonal_jitter(Matrix0, 1, Jitter, Matrix).

	add_diagonal_jitter([], _Index, _Jitter, []).
	add_diagonal_jitter([Row0| Rows0], Index, Jitter, [Row| Rows]) :-
		add_row_diagonal_jitter(Row0, 1, Index, Jitter, Row),
		NextIndex is Index + 1,
		add_diagonal_jitter(Rows0, NextIndex, Jitter, Rows).

	add_row_diagonal_jitter([], _ColumnIndex, _DiagonalIndex, _Jitter, []).
	add_row_diagonal_jitter([Value0| Values0], ColumnIndex, DiagonalIndex, Jitter, [Value| Values]) :-
		(   ColumnIndex =:= DiagonalIndex ->
			Value is Value0 + Jitter
		;   Value = Value0
		),
		NextColumnIndex is ColumnIndex + 1,
		add_row_diagonal_jitter(Values0, NextColumnIndex, DiagonalIndex, Jitter, Values).

	cholesky_decomposition(Matrix, CholeskyFactor) :-
		length(Matrix, Size),
		cholesky_decomposition(1, Size, Matrix, [], CholeskyFactor).

	cholesky_decomposition(Index, Size, _Matrix, CholeskyFactor, CholeskyFactor) :-
		Index > Size,
		!.
	cholesky_decomposition(Index, Size, Matrix, PreviousRows, CholeskyFactor) :-
		nth1(Index, Matrix, MatrixRow),
		cholesky_row(1, Index, Size, MatrixRow, PreviousRows, [], CholeskyRow),
		append(PreviousRows, [CholeskyRow], NextRows),
		NextIndex is Index + 1,
		cholesky_decomposition(NextIndex, Size, Matrix, NextRows, CholeskyFactor).

	cholesky_row(ColumnIndex, RowIndex, Size, _MatrixRow, _PreviousRows, Prefix, CholeskyRow) :-
		ColumnIndex > RowIndex,
		!,
		Remaining is Size - RowIndex,
		zero_vector(Remaining, Zeroes),
		append(Prefix, Zeroes, CholeskyRow).
	cholesky_row(ColumnIndex, RowIndex, Size, MatrixRow, PreviousRows, Prefix, CholeskyRow) :-
		nth1(ColumnIndex, MatrixRow, MatrixValue),
		(   ColumnIndex =:= RowIndex ->
			sum_squares(Prefix, Correction),
			DiagonalValue0 is MatrixValue - Correction,
			(   DiagonalValue0 > 1.0e-12 ->
				DiagonalValue is sqrt(DiagonalValue0)
			;   throw(error(non_positive_definite_precision_matrix(DiagonalValue0), logtalk(cholesky_decomposition(MatrixRow), this)))
			),
			append(Prefix, [DiagonalValue], NextPrefix),
			NextColumnIndex is ColumnIndex + 1,
			cholesky_row(NextColumnIndex, RowIndex, Size, MatrixRow, PreviousRows, NextPrefix, CholeskyRow)
		;   nth1(ColumnIndex, PreviousRows, PreviousRow),
			prefix_dot(Prefix, PreviousRow, 0.0, Correction),
			nth1(ColumnIndex, PreviousRow, Diagonal),
			Entry is (MatrixValue - Correction) / Diagonal,
			append(Prefix, [Entry], NextPrefix),
			NextColumnIndex is ColumnIndex + 1,
			cholesky_row(NextColumnIndex, RowIndex, Size, MatrixRow, PreviousRows, NextPrefix, CholeskyRow)
		).

	sum_squares([], 0.0).
	sum_squares([Value| Values], SumSquares) :-
		SumSquares0 is Value * Value,
		sum_squares(Values, SumSquares0, SumSquares).

	sum_squares([], SumSquares, SumSquares).
	sum_squares([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		sum_squares(Values, SumSquares1, SumSquares).

	prefix_dot([], _OtherValues, DotProduct, DotProduct).
	prefix_dot([Value| Values], [OtherValue| OtherValues], DotProduct0, DotProduct) :-
		DotProduct1 is DotProduct0 + Value * OtherValue,
		prefix_dot(Values, OtherValues, DotProduct1, DotProduct).

	solve_cholesky(CholeskyFactor, Values, Solution) :-
		forward_substitution(CholeskyFactor, Values, ForwardSolution),
		backward_substitution(CholeskyFactor, ForwardSolution, Solution).

	forward_substitution(CholeskyFactor, Values, Solution) :-
		forward_substitution(CholeskyFactor, Values, 1, [], Solution).

	forward_substitution([], [], _Index, Solution, Solution).
	forward_substitution([Row| Rows], [Value| Values], Index, KnownSolutions0, Solution) :-
		PreviousCount is Index - 1,
		forward_correction(Row, KnownSolutions0, PreviousCount, 0.0, Correction),
		nth1(Index, Row, Diagonal),
		CurrentSolution is (Value - Correction) / Diagonal,
		append(KnownSolutions0, [CurrentSolution], KnownSolutions1),
		NextIndex is Index + 1,
		forward_substitution(Rows, Values, NextIndex, KnownSolutions1, Solution).

	forward_correction(_Row, _KnownSolutions, 0, Correction, Correction) :-
		!.
	forward_correction([Coefficient| Coefficients], [KnownSolution| KnownSolutions], Count, Correction0, Correction) :-
		Correction1 is Correction0 + Coefficient * KnownSolution,
		NextCount is Count - 1,
		forward_correction(Coefficients, KnownSolutions, NextCount, Correction1, Correction).

	backward_substitution(CholeskyFactor, Values, Solution) :-
		length(Values, Size),
		backward_substitution(Size, CholeskyFactor, Values, [], Solution).

	backward_substitution(0, _CholeskyFactor, _Values, Solution, Solution) :-
		!.
	backward_substitution(Index, CholeskyFactor, Values, KnownSolutions0, Solution) :-
		nth1(Index, CholeskyFactor, Row),
		nth1(Index, Row, Diagonal),
		nth1(Index, Values, Value),
		NextIndex is Index + 1,
		backward_correction(NextIndex, Index, CholeskyFactor, KnownSolutions0, 0.0, Correction),
		CurrentSolution is (Value - Correction) / Diagonal,
		NextRowIndex is Index - 1,
		backward_substitution(NextRowIndex, CholeskyFactor, Values, [CurrentSolution| KnownSolutions0], Solution).

	backward_correction(_RowIndex, _ColumnIndex, _CholeskyFactor, [], Correction, Correction) :-
		!.
	backward_correction(RowIndex, ColumnIndex, CholeskyFactor, [KnownSolution| KnownSolutions], Correction0, Correction) :-
		nth1(RowIndex, CholeskyFactor, Row),
		nth1(ColumnIndex, Row, Coefficient),
		Correction1 is Correction0 + Coefficient * KnownSolution,
		NextRowIndex is RowIndex + 1,
		backward_correction(NextRowIndex, ColumnIndex, CholeskyFactor, KnownSolutions, Correction1, Correction).

	sum_log_diagonal([], _Index, SumLog, SumLog).
	sum_log_diagonal([Row| Rows], Index, SumLog0, SumLog) :-
		nth1(Index, Row, Diagonal),
		SumLog1 is SumLog0 + log(Diagonal),
		NextIndex is Index + 1,
		sum_log_diagonal(Rows, NextIndex, SumLog1, SumLog).

	default_option(maximum_iterations(300)).
	default_option(tolerance(1.0e-6)).
	default_option(initial_weight_precision(1.0)).
	default_option(initial_noise_precision(auto)).
	default_option(alpha_1(1.0e-6)).
	default_option(alpha_2(1.0e-6)).
	default_option(lambda_1(1.0e-6)).
	default_option(lambda_2(1.0e-6)).
	default_option(feature_scaling(true)).
	default_option(precision_bounds(1.0e-12, 1.0e12)).

	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		valid(non_negative_float, Tolerance).
	valid_option(initial_weight_precision(Alpha)) :-
		valid(positive_float, Alpha).
	valid_option(initial_noise_precision(auto)).
	valid_option(initial_noise_precision(Beta)) :-
		valid(positive_float, Beta).
	valid_option(alpha_1(AlphaShape)) :-
		valid(non_negative_float, AlphaShape).
	valid_option(alpha_2(AlphaRate)) :-
		valid(non_negative_float, AlphaRate).
	valid_option(lambda_1(LambdaShape)) :-
		valid(non_negative_float, LambdaShape).
	valid_option(lambda_2(LambdaRate)) :-
		valid(non_negative_float, LambdaRate).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(precision_bounds(MinimumPrecision, MaximumPrecision)) :-
		valid(positive_float, MinimumPrecision),
		valid(positive_float, MaximumPrecision),
		MinimumPrecision =< MaximumPrecision.

:- end_object.

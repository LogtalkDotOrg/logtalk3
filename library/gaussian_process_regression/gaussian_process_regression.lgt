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


:- object(gaussian_process_regression,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Gaussian process regression regressor supporting continuous and mixed-feature datasets using an exact mixed Gaussian process with posterior uncertainty estimates. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction, predictive-distribution queries, and export as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses exact Gaussian process regression with a mixed covariance kernel: an automatic-relevance-determination squared-exponential component over continuous encoded features and a field-wise categorical overlap component over categorical attributes. Hyperparameters are selected by maximizing the log marginal likelihood using a deterministic coordinate search in log space.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are encoded using reference-level dummy coding from the declared dataset attribute values, and missing values are encoded using explicit missing-value indicator features.',
			'Uncertainty estimates' - 'In addition to mean predictions, the library can return the posterior predictive Gaussian distribution for a new instance, including observation noise variance. Small negative posterior variances caused by floating-point roundoff are clipped to zero while larger negative values raise an error.',
			'Numerical stability' - 'Covariance factorization uses adaptive jitter escalation and raises a domain error when the covariance matrix cannot be made positive definite within the configured retry budget.',
			'Memory-based representation' - 'The learned regressor stores the encoded training rows together with the cached Cholesky factor and dual coefficients required for exact posterior prediction.',
			'Unknown values' - 'Prediction requests containing categorical values that are not declared by the dataset raise a domain error.',
			'Regressor representation' - 'The learned regressor is represented by default as ``gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics)`` where ``Encoders`` stores feature encoding metadata, ``TrainingFeatures`` stores the encoded training rows, ``TargetMean`` stores the centered-mean offset, ``Alpha`` stores the cached dual coefficients, ``CholeskyFactor`` stores the lower-triangular covariance factor, ``Kernel`` stores the learned mixed-kernel hyperparameters including one continuous length scale per encoded continuous feature and one categorical penalty per categorical attribute, and ``Diagnostics`` stores training metadata including the effective options and learned hyperparameters.'
		],
		see_also is [
			linear_regression, ridge_regression, lasso_regression, elastic_net_regression, regression_tree,
			random_forest_regression
		]
	]).

	:- public(predict_distribution/3).
	:- mode(predict_distribution(+compound, +list, -compound), one).
	:- info(predict_distribution/3, [
		comment is 'Predicts the posterior predictive Gaussian distribution for a new instance using the learned regressor. The returned term has the shape ``gaussian(Mean, Variance)`` where ``Variance`` includes the learned observation noise variance.',
		argnames is ['Regressor', 'Instance', 'Distribution']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth1/3, reverse/2
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

	:- uses(linear_algebra, [
		cholesky_decomposition/2, forward_substitution/3, matrix_diagonal/2, solve_cholesky/3
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(Target),
		fit_gaussian_process_model(Dataset, Options, Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, TrainingExampleCount, TrainingDiagnostics),
		build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics),
		Regressor = gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics).

	predict(Regressor, Instance, Target) :-
		predict_distribution(Regressor, Instance, gaussian(Target, _Variance)).

	predict_distribution(Regressor, Instance, gaussian(Mean, Variance)) :-
		Regressor =.. [_, Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		kernel_vector(TrainingFeatures, Features, Kernel, KernelVector),
		dot_product(Alpha, KernelVector, CenteredMean),
		Mean is TargetMean + CenteredMean,
		forward_substitution(CholeskyFactor, KernelVector, Projection),
		dot_product(Projection, Projection, VarianceReduction),
		observation_variance(Kernel, PriorVariance),
		Variance0 is PriorVariance - VarianceReduction,
		ensure_predictive_variance(Variance0, Variance).

	ensure_predictive_variance(Variance0, Variance) :-
		(   Variance0 >= 0.0 ->
			Variance = Variance0
		;   Variance0 >= -1.0e-10 ->
			Variance = 0.0
		;   domain_error(non_negative_predictive_variance, Variance0)
		).

	build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics) :-
		^^encoded_feature_count(Encoders, FeatureCount),
		append(TrainingDiagnostics, [encoded_feature_count(FeatureCount)], ExtraDiagnostics),
		^^base_regressor_diagnostics(gaussian_process_regression, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics).

	fit_gaussian_process_model(Dataset, Options, Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, TrainingExampleCount, TrainingDiagnostics) :-
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_gaussian_process_encoders(Attributes, Examples, Options, Encoders),
		build_feature_layout(Encoders, FeatureLayout, ContinuousFeatureCount, CategoricalFeatureCount),
		^^examples_to_rows(Examples, Encoders, Rows),
		rows_training_data(Rows, TrainingFeatures, Targets),
		length(TrainingFeatures, TrainingExampleCount),
		arithmetic_mean(Targets, TargetMean),
		center_targets(Targets, TargetMean, CenteredTargets),
		factorization_settings(Options, FactorizationSettings),
		initial_kernel(FeatureLayout, ContinuousFeatureCount, CategoricalFeatureCount, TrainingFeatures, CenteredTargets, Options, InitialKernel),
		(   ^^option(optimize_hyperparameters(true), Options) ->
			optimize_kernel_hyperparameters(TrainingFeatures, CenteredTargets, Options, FactorizationSettings, InitialKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, Convergence, Iterations, FinalDelta, LogMarginalLikelihood)
		;   evaluate_kernel(TrainingFeatures, CenteredTargets, FactorizationSettings, InitialKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, LogMarginalLikelihood),
			Convergence = disabled,
			Iterations = 0,
			FinalDelta = 0.0
		),
		Kernel = squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		TrainingDiagnostics = [
			kernel(squared_exponential),
			length_scales(LengthScales),
			categorical_penalties(CategoricalPenalties),
			signal_variance(SignalVariance),
			noise_variance(NoiseVariance),
			jitter(Jitter),
			continuous_feature_count(ContinuousFeatureCount),
			categorical_feature_count(CategoricalFeatureCount),
			jitter_attempts(JitterAttempts),
			log_marginal_likelihood(LogMarginalLikelihood),
			convergence(Convergence),
			iterations(Iterations),
			final_delta(FinalDelta)
		].

	build_feature_layout([], [], 0, 0).
	build_feature_layout([continuous(Attribute, _Mean, _Scale)| Encoders], [continuous_segment(Attribute)| FeatureLayout], ContinuousFeatureCount, CategoricalFeatureCount) :-
		!,
		build_feature_layout(Encoders, FeatureLayout, ContinuousFeatureCount0, CategoricalFeatureCount),
		ContinuousFeatureCount is ContinuousFeatureCount0 + 2.
	build_feature_layout([categorical(Attribute, Values)| Encoders], [categorical_segment(Attribute, Width)| FeatureLayout], ContinuousFeatureCount, CategoricalFeatureCount) :-
		length(Values, Width),
		build_feature_layout(Encoders, FeatureLayout, ContinuousFeatureCount, CategoricalFeatureCount0),
		CategoricalFeatureCount is CategoricalFeatureCount0 + 1.

	feature_layout_encoded_feature_count([], 0).
	feature_layout_encoded_feature_count([continuous_segment(_Attribute)| FeatureLayout], FeatureCount) :-
		!,
		feature_layout_encoded_feature_count(FeatureLayout, FeatureCount0),
		FeatureCount is FeatureCount0 + 2.
	feature_layout_encoded_feature_count([categorical_segment(_Attribute, Width)| FeatureLayout], FeatureCount) :-
		feature_layout_encoded_feature_count(FeatureLayout, FeatureCount0),
		FeatureCount is FeatureCount0 + Width.

	build_gaussian_process_encoders([], _Examples, _Options, []).
	build_gaussian_process_encoders([Attribute-Values| Attributes], Examples, Options, [Encoder| Encoders]) :-
		(   Values == continuous ->
			^^continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;   Encoder = categorical(Attribute, Values)
		),
		build_gaussian_process_encoders(Attributes, Examples, Options, Encoders).

	rows_training_data(Rows, TrainingFeatures, Targets) :-
		rows_training_data(Rows, [], TrainingFeatures0, [], Targets0),
		reverse(TrainingFeatures0, TrainingFeatures),
		reverse(Targets0, Targets).

	rows_training_data([], TrainingFeatures, TrainingFeatures, Targets, Targets).
	rows_training_data([Features-Target| Rows], TrainingFeatures0, TrainingFeatures, Targets0, Targets) :-
		rows_training_data(Rows, [Features| TrainingFeatures0], TrainingFeatures, [Target| Targets0], Targets).

	center_targets([], _TargetMean, []).
	center_targets([Target| Targets], TargetMean, [Centered| CenteredTargets]) :-
		Centered is Target - TargetMean,
		center_targets(Targets, TargetMean, CenteredTargets).

	initial_kernel(FeatureLayout, ContinuousFeatureCount, CategoricalFeatureCount, TrainingFeatures, CenteredTargets, Options, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)) :-
		^^option(kernel(squared_exponential), Options),
		^^option(length_scale(LengthScaleOption), Options),
		resolve_length_scales(LengthScaleOption, FeatureLayout, TrainingFeatures, ContinuousFeatureCount, LengthScales),
		^^option(categorical_penalty(CategoricalPenaltyOption), Options),
		resolve_categorical_penalties(CategoricalPenaltyOption, CategoricalFeatureCount, CategoricalPenalties),
		^^option(signal_variance(SignalVarianceOption), Options),
		resolve_signal_variance(SignalVarianceOption, CenteredTargets, SignalVariance),
		^^option(noise_variance(NoiseVarianceOption), Options),
		resolve_noise_variance(NoiseVarianceOption, SignalVariance, NoiseVariance),
		^^option(jitter(Jitter), Options).

	resolve_length_scales(auto, FeatureLayout, TrainingFeatures, _ContinuousFeatureCount, LengthScales) :-
		feature_default_length_scales(FeatureLayout, TrainingFeatures, LengthScales),
		!.
	resolve_length_scales(LengthScale, _FeatureLayout, _TrainingFeatures, ContinuousFeatureCount, LengthScales) :-
		valid(positive_float, LengthScale),
		!,
		repeated_value(ContinuousFeatureCount, LengthScale, LengthScales).
	resolve_length_scales(LengthScales, _FeatureLayout, _TrainingFeatures, ContinuousFeatureCount, LengthScales) :-
		valid(list(positive_float), LengthScales),
		(   length(LengthScales, ContinuousFeatureCount) ->
			true
		;   domain_error(length_scale_dimensions(ContinuousFeatureCount), LengthScales)
		).

	resolve_categorical_penalties(auto, CategoricalFeatureCount, CategoricalPenalties) :-
		repeated_value(CategoricalFeatureCount, 1.0, CategoricalPenalties),
		!.
	resolve_categorical_penalties(CategoricalPenalty, CategoricalFeatureCount, CategoricalPenalties) :-
		valid(positive_float, CategoricalPenalty),
		!,
		repeated_value(CategoricalFeatureCount, CategoricalPenalty, CategoricalPenalties).
	resolve_categorical_penalties(CategoricalPenalties, CategoricalFeatureCount, CategoricalPenalties) :-
		valid(list(positive_float), CategoricalPenalties),
		(   length(CategoricalPenalties, CategoricalFeatureCount) ->
			true
		;   domain_error(categorical_penalty_dimensions(CategoricalFeatureCount), CategoricalPenalties)
		).

	feature_default_length_scales(FeatureLayout, TrainingFeatures, LengthScales) :-
		continuous_feature_indices(FeatureLayout, ContinuousIndices),
		feature_default_length_scales_from_indices(ContinuousIndices, TrainingFeatures, LengthScales).

	continuous_feature_indices(FeatureLayout, ContinuousIndices) :-
		continuous_feature_indices(FeatureLayout, 1, ContinuousIndices).

	continuous_feature_indices([], _Index, []).
	continuous_feature_indices([continuous_segment(_Attribute)| FeatureLayout], Index, [Index, NextIndex| ContinuousIndices]) :-
		!,
		NextIndex is Index + 1,
		NextStart is Index + 2,
		continuous_feature_indices(FeatureLayout, NextStart, ContinuousIndices).
	continuous_feature_indices([categorical_segment(_Attribute, Width)| FeatureLayout], Index, ContinuousIndices) :-
		NextIndex is Index + Width,
		continuous_feature_indices(FeatureLayout, NextIndex, ContinuousIndices).

	feature_default_length_scales_from_indices([], _TrainingFeatures, []).
	feature_default_length_scales_from_indices([Index| Indices], TrainingFeatures, [LengthScale| LengthScales]) :-
		collect_index_values(TrainingFeatures, Index, Values),
		default_feature_length_scale(Values, LengthScale),
		feature_default_length_scales_from_indices(Indices, TrainingFeatures, LengthScales).

	collect_index_values([], _Index, []).
	collect_index_values([TrainingFeature| TrainingFeatures], Index, [Value| Values]) :-
		nth1(Index, TrainingFeature, Value),
		collect_index_values(TrainingFeatures, Index, Values).

	default_feature_length_scale(Values, LengthScale) :-
		length(Values, Count),
		(   Count > 1 ->
			variance(Values, Variance0)
		;   Variance0 = 0.0
		),
		(   Variance0 > 1.0e-12 ->
			LengthScale is sqrt(Variance0)
		;   LengthScale = 1.0
		).

	repeated_value(0, _Value, []) :-
		!.
	repeated_value(Count, Value, [Value| Values]) :-
		Count > 0,
		NextCount is Count - 1,
		repeated_value(NextCount, Value, Values).

	resolve_signal_variance(auto, CenteredTargets, SignalVariance) :-
		length(CenteredTargets, Count),
		(   Count > 1 ->
			variance(CenteredTargets, Variance0)
		;   Variance0 = 0.0
		),
		(   Variance0 > 1.0e-8 ->
			SignalVariance = Variance0
		;   SignalVariance = 1.0
		),
		!.
	resolve_signal_variance(SignalVariance, _CenteredTargets, SignalVariance) :-
		SignalVariance \== auto.

	resolve_noise_variance(auto, SignalVariance, NoiseVariance) :-
		NoiseVariance0 is SignalVariance * 1.0e-4,
		(   NoiseVariance0 >= 1.0e-6 ->
			NoiseVariance = NoiseVariance0
		;   NoiseVariance = 1.0e-6
		),
		!.
	resolve_noise_variance(NoiseVariance, _SignalVariance, NoiseVariance) :-
		NoiseVariance \== auto.

	factorization_settings(Options, factorization_settings(MaxAttempts, JitterScaleFactor)) :-
		^^option(max_factorization_attempts(MaxAttempts), Options),
		^^option(jitter_scale_factor(JitterScaleFactor), Options).

	optimize_kernel_hyperparameters(TrainingFeatures, CenteredTargets, Options, FactorizationSettings, InitialKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, Convergence, Iterations, FinalDelta, LogMarginalLikelihood) :-
		evaluate_kernel(TrainingFeatures, CenteredTargets, FactorizationSettings, InitialKernel, InitialKernel1, CholeskyFactor0, Alpha0, JitterAttempts0, Score0),
		InitialState = state(InitialKernel1, Score0, CholeskyFactor0, Alpha0, JitterAttempts0),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		^^option(relative_improvement_factor(RelativeImprovementFactor), Options),
		^^option(hyperparameter_minimum(HyperparameterMinimum), Options),
		^^option(maximum_continuous_length_scale(MaximumContinuousLengthScale), Options),
		^^option(maximum_categorical_penalty(MaximumCategoricalPenalty), Options),
		InitialStep is log(2.0),
		MinimumStep is log(1.05),
		coordinate_search(TrainingFeatures, CenteredTargets, FactorizationSettings, MaximumIterations, Tolerance, RelativeImprovementFactor, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, MinimumStep, 0, InitialStep, InitialState, FinalState, Convergence, Iterations, FinalDelta),
		FinalState = state(Kernel, LogMarginalLikelihood, CholeskyFactor, Alpha, JitterAttempts).

	coordinate_search(_TrainingFeatures, _CenteredTargets, _FactorizationSettings, MaximumIterations, _Tolerance, _RelativeImprovementFactor, _HyperparameterMinimum, _MaximumContinuousLengthScale, _MaximumCategoricalPenalty, _MinimumStep, Iteration, _Step, State, State, maximum_iterations_exhausted, Iteration, 0.0) :-
		Iteration >= MaximumIterations,
		!.
	coordinate_search(TrainingFeatures, CenteredTargets, FactorizationSettings, MaximumIterations, Tolerance, RelativeImprovementFactor, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, MinimumStep, Iteration, Step, State0, State, Convergence, Iterations, FinalDelta) :-
		sweep_hyperparameters(TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State1),
		state_score(State0, Score0),
		state_score(State1, Score1),
		effective_tolerance(Score0, Tolerance, RelativeImprovementFactor, EffectiveTolerance),
		Improvement0 is Score1 - Score0,
		(   Improvement0 >= 0.0 ->
			Improvement = Improvement0,
			BestState = State1
		;   Improvement = 0.0,
			BestState = State0
		),
		(   Improvement > EffectiveTolerance ->
			NextIteration is Iteration + 1,
			coordinate_search(TrainingFeatures, CenteredTargets, FactorizationSettings, MaximumIterations, Tolerance, RelativeImprovementFactor, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, MinimumStep, NextIteration, Step, BestState, State, Convergence, Iterations, FinalDelta)
		;   Step > MinimumStep ->
			ReducedStep is Step / 2.0,
			NextIteration is Iteration + 1,
			coordinate_search(TrainingFeatures, CenteredTargets, FactorizationSettings, MaximumIterations, Tolerance, RelativeImprovementFactor, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, MinimumStep, NextIteration, ReducedStep, BestState, State, Convergence, Iterations, FinalDelta)
		;   State = BestState,
			Convergence = tolerance,
			Iterations is Iteration,
			FinalDelta = Improvement
		).

	state_score(state(_Kernel, Score, _CholeskyFactor, _Alpha, _JitterAttempts), Score).

	effective_tolerance(Score, Tolerance, RelativeImprovementFactor, EffectiveTolerance) :-
		RelativeTolerance is abs(Score) * RelativeImprovementFactor,
		(   RelativeTolerance > Tolerance ->
			EffectiveTolerance = RelativeTolerance
		;   EffectiveTolerance = Tolerance
		).

	sweep_hyperparameters(TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State) :-
		sweep_length_scales(TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State1),
		sweep_categorical_penalties(TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State1, State2),
		sweep_parameter(signal_variance, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State2, State3),
		sweep_parameter(noise_variance, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State3, State).

	sweep_length_scales(TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State) :-
		State0 = state(squared_exponential_kernel(_FeatureLayout, LengthScales, _CategoricalPenalties, _SignalVariance, _NoiseVariance, _Jitter), _Score, _CholeskyFactor, _Alpha, _JitterAttempts),
		length(LengthScales, FeatureCount),
		sweep_length_scales(1, FeatureCount, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State).

	sweep_length_scales(Index, FeatureCount, _TrainingFeatures, _CenteredTargets, _FactorizationSettings, _Step, _HyperparameterMinimum, _MaximumContinuousLengthScale, _MaximumCategoricalPenalty, State, State) :-
		Index > FeatureCount,
		!.
	sweep_length_scales(Index, FeatureCount, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State) :-
		sweep_parameter(length_scale(Index), TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State1),
		NextIndex is Index + 1,
		sweep_length_scales(NextIndex, FeatureCount, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State1, State).

	sweep_categorical_penalties(TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State) :-
		State0 = state(squared_exponential_kernel(_FeatureLayout, _LengthScales, CategoricalPenalties, _SignalVariance, _NoiseVariance, _Jitter), _Score, _CholeskyFactor, _Alpha, _JitterAttempts),
		length(CategoricalPenalties, FeatureCount),
		sweep_categorical_penalties(1, FeatureCount, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State).

	sweep_categorical_penalties(Index, FeatureCount, _TrainingFeatures, _CenteredTargets, _FactorizationSettings, _Step, _HyperparameterMinimum, _MaximumContinuousLengthScale, _MaximumCategoricalPenalty, State, State) :-
		Index > FeatureCount,
		!.
	sweep_categorical_penalties(Index, FeatureCount, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State) :-
		sweep_parameter(categorical_penalty(Index), TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State1),
		NextIndex is Index + 1,
		sweep_categorical_penalties(NextIndex, FeatureCount, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State1, State).

	sweep_parameter(Parameter, TrainingFeatures, CenteredTargets, FactorizationSettings, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, State0, State) :-
		State0 = state(Kernel0, _Score0, _CholeskyFactor0, _Alpha0, _JitterAttempts0),
		parameter_candidates(Parameter, Kernel0, Step, HyperparameterMinimum, MaximumContinuousLengthScale, MaximumCategoricalPenalty, CandidateKernels),
		best_candidate_state(CandidateKernels, TrainingFeatures, CenteredTargets, FactorizationSettings, State0, State).

	parameter_candidates(length_scale(Index), Kernel, Step, HyperparameterMinimum, MaximumContinuousLengthScale, _MaximumCategoricalPenalty, CandidateKernels) :-
		Kernel = squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		nth1(Index, LengthScales, LengthScale),
		scale_length_scale(LengthScale, Step, HyperparameterMinimum, MaximumContinuousLengthScale, LengthScale0, LengthScale1),
		replace_nth1(Index, LengthScales, LengthScale0, LengthScales0),
		replace_nth1(Index, LengthScales, LengthScale1, LengthScales1),
		unique_new_candidates(Kernel, [
			squared_exponential_kernel(FeatureLayout, LengthScales0, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
			squared_exponential_kernel(FeatureLayout, LengthScales1, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)
		], CandidateKernels).
	parameter_candidates(categorical_penalty(Index), Kernel, Step, HyperparameterMinimum, _MaximumContinuousLengthScale, MaximumCategoricalPenalty, CandidateKernels) :-
		Kernel = squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		nth1(Index, CategoricalPenalties, CategoricalPenalty),
		scale_categorical_penalty(CategoricalPenalty, Step, HyperparameterMinimum, MaximumCategoricalPenalty, CategoricalPenalty0, CategoricalPenalty1),
		replace_nth1(Index, CategoricalPenalties, CategoricalPenalty0, CategoricalPenalties0),
		replace_nth1(Index, CategoricalPenalties, CategoricalPenalty1, CategoricalPenalties1),
		unique_new_candidates(Kernel, [
			squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties0, SignalVariance, NoiseVariance, Jitter),
			squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties1, SignalVariance, NoiseVariance, Jitter)
		], CandidateKernels).
	parameter_candidates(signal_variance, Kernel, Step, HyperparameterMinimum, _MaximumContinuousLengthScale, _MaximumCategoricalPenalty, CandidateKernels) :-
		Kernel = squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		scale_parameter(SignalVariance, Step, HyperparameterMinimum, SignalVariance0, SignalVariance1),
		unique_new_candidates(Kernel, [
			squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance0, NoiseVariance, Jitter),
			squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance1, NoiseVariance, Jitter)
		], CandidateKernels).
	parameter_candidates(noise_variance, Kernel, Step, HyperparameterMinimum, _MaximumContinuousLengthScale, _MaximumCategoricalPenalty, CandidateKernels) :-
		Kernel = squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		scale_parameter(NoiseVariance, Step, HyperparameterMinimum, NoiseVariance0, NoiseVariance1),
		unique_new_candidates(Kernel, [
			squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance0, Jitter),
			squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance1, Jitter)
		], CandidateKernels).

	unique_new_candidates(CurrentKernel, CandidateKernels0, CandidateKernels) :-
		unique_new_candidates(CandidateKernels0, CurrentKernel, [], CandidateKernels1),
		reverse(CandidateKernels1, CandidateKernels).

	unique_new_candidates([], _CurrentKernel, CandidateKernels, CandidateKernels).
	unique_new_candidates([CandidateKernel| CandidateKernels0], CurrentKernel, CandidateKernels1, CandidateKernels) :-
		(   CandidateKernel == CurrentKernel ->
			CandidateKernels2 = CandidateKernels1
		;   member(ExistingKernel, CandidateKernels1), CandidateKernel == ExistingKernel ->
			CandidateKernels2 = CandidateKernels1
		;   CandidateKernels2 = [CandidateKernel| CandidateKernels1]
		),
		unique_new_candidates(CandidateKernels0, CurrentKernel, CandidateKernels2, CandidateKernels).

	replace_nth1(1, [_Value| Values], Replacement, [Replacement| Values]) :-
		!.
	replace_nth1(Index, [Value| Values], Replacement, [Value| Replacements]) :-
		Index > 1,
		NextIndex is Index - 1,
		replace_nth1(NextIndex, Values, Replacement, Replacements).

	scale_length_scale(LengthScale, Step, Minimum, Maximum, Smaller, Larger) :-
		bounded_scale_parameter(LengthScale, Step, Minimum, Maximum, Smaller, Larger).

	scale_categorical_penalty(CategoricalPenalty, Step, Minimum, Maximum, Smaller, Larger) :-
		bounded_scale_parameter(CategoricalPenalty, Step, Minimum, Maximum, Smaller, Larger).

	bounded_scale_parameter(Parameter, Step, Minimum, Maximum, Smaller, Larger) :-
		Scale is exp(Step),
		Smaller0 is Parameter / Scale,
		clamp_parameter(Smaller0, Minimum, Maximum, Smaller),
		Larger0 is Parameter * Scale,
		clamp_parameter(Larger0, Minimum, Maximum, Larger).

	scale_parameter(Parameter, Step, Minimum, Smaller, Larger) :-
		Scale is exp(Step),
		Smaller0 is Parameter / Scale,
		(   Smaller0 >= Minimum ->
			Smaller = Smaller0
		;   Smaller = Minimum
		),
		Larger is Parameter * Scale.

	clamp_parameter(Value, Minimum, Maximum, Clamped) :-
		(   Value < Minimum ->
			Clamped = Minimum
		;   Value > Maximum ->
			Clamped = Maximum
		;   Clamped = Value
		).

	best_candidate_state([], _TrainingFeatures, _CenteredTargets, _FactorizationSettings, State, State).
	best_candidate_state([CandidateKernel| CandidateKernels], TrainingFeatures, CenteredTargets, FactorizationSettings, State0, State) :-
		(   catch(candidate_state(CandidateKernel, TrainingFeatures, CenteredTargets, FactorizationSettings, CandidateState), error(domain_error(positive_definite_covariance, _), _), fail) ->
			better_state(CandidateState, State0, BetterState)
		;   BetterState = State0
		),
		best_candidate_state(CandidateKernels, TrainingFeatures, CenteredTargets, FactorizationSettings, BetterState, State).

	candidate_state(CandidateKernel, TrainingFeatures, CenteredTargets, FactorizationSettings, state(Kernel, Score, CholeskyFactor, Alpha, JitterAttempts)) :-
		evaluate_kernel(TrainingFeatures, CenteredTargets, FactorizationSettings, CandidateKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, Score).

	better_state(state(Kernel0, Score0, CholeskyFactor0, Alpha0, JitterAttempts0), state(_Kernel1, Score1, _CholeskyFactor1, _Alpha1, _JitterAttempts1), state(Kernel0, Score0, CholeskyFactor0, Alpha0, JitterAttempts0)) :-
		Score0 > Score1,
		!.
	better_state(_State0, State1, State1).

	evaluate_kernel(TrainingFeatures, CenteredTargets, FactorizationSettings, Kernel0, Kernel, CholeskyFactor, Alpha, JitterAttempts, LogMarginalLikelihood) :-
		factorize_covariance(TrainingFeatures, Kernel0, FactorizationSettings, Kernel, JitterAttempts, CholeskyFactor),
		solve_cholesky(CholeskyFactor, CenteredTargets, Alpha),
		log_marginal_likelihood(CenteredTargets, Alpha, CholeskyFactor, LogMarginalLikelihood).

	factorize_covariance(TrainingFeatures, Kernel0, factorization_settings(MaxAttempts, JitterScaleFactor), Kernel, JitterAttempts, CholeskyFactor) :-
		factorize_covariance(TrainingFeatures, Kernel0, MaxAttempts, JitterScaleFactor, 0, Kernel, JitterAttempts, CholeskyFactor).

	factorize_covariance(_TrainingFeatures, Kernel0, MaxAttempts, _JitterScaleFactor, Attempt, _Kernel, _JitterAttempts, _CholeskyFactor) :-
		Attempt > MaxAttempts,
		!,
		domain_error(positive_definite_covariance, Kernel0).
	factorize_covariance(TrainingFeatures, Kernel0, MaxAttempts, JitterScaleFactor, Attempt, Kernel, JitterAttempts, CholeskyFactor) :-
		update_kernel_jitter(Kernel0, JitterScaleFactor, Attempt, CandidateKernel),
		build_covariance_matrix(TrainingFeatures, CandidateKernel, CovarianceMatrix),
		(   catch(cholesky_decomposition(CovarianceMatrix, CholeskyFactor), error(non_positive_definite_matrix(_Value), _Context), fail) ->
			Kernel = CandidateKernel,
			JitterAttempts = Attempt
		;   NextAttempt is Attempt + 1,
			factorize_covariance(TrainingFeatures, Kernel0, MaxAttempts, JitterScaleFactor, NextAttempt, Kernel, JitterAttempts, CholeskyFactor)
		).

	update_kernel_jitter(squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), _JitterScaleFactor, 0, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)) :-
		!.
	update_kernel_jitter(squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter0), JitterScaleFactor, Attempt, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)) :-
		Scale is JitterScaleFactor ** Attempt,
		Jitter is Jitter0 * Scale.

	build_covariance_matrix(TrainingFeatures, Kernel, CovarianceMatrix) :-
		index_training_features(TrainingFeatures, IndexedTrainingFeatures),
		covariance_rows(IndexedTrainingFeatures, IndexedTrainingFeatures, Kernel, CovarianceMatrix).

	index_training_features(TrainingFeatures, IndexedTrainingFeatures) :-
		index_training_features(TrainingFeatures, 1, IndexedTrainingFeatures).

	index_training_features([], _Index, []).
	index_training_features([TrainingFeature| TrainingFeatures], Index, [Index-TrainingFeature| IndexedTrainingFeatures]) :-
		NextIndex is Index + 1,
		index_training_features(TrainingFeatures, NextIndex, IndexedTrainingFeatures).

	covariance_rows([], _AllTrainingFeatures, _Kernel, []).
	covariance_rows([Index-TrainingFeature| IndexedTrainingFeatures], AllTrainingFeatures, Kernel, [Row| CovarianceMatrix]) :-
		covariance_row(AllTrainingFeatures, Index, TrainingFeature, Kernel, Row),
		covariance_rows(IndexedTrainingFeatures, AllTrainingFeatures, Kernel, CovarianceMatrix).

	covariance_row(IndexedTrainingFeatures, Index, TrainingFeature, Kernel, Row) :-
		findall(
			Value,
			(
				member(OtherIndex-OtherTrainingFeature, IndexedTrainingFeatures),
				covariance_value(Index, TrainingFeature, OtherIndex, OtherTrainingFeature, Kernel, Value)
			),
			Row
		).

	covariance_value(Index, TrainingFeature, Index, TrainingFeature, Kernel, Value) :-
		!,
		latent_covariance(Kernel, TrainingFeature, TrainingFeature, LatentValue),
		Kernel = squared_exponential_kernel(_FeatureLayout, _LengthScales, _CategoricalPenalties, _SignalVariance, NoiseVariance, Jitter),
		Value is LatentValue + NoiseVariance + Jitter.
	covariance_value(_Index, TrainingFeature, _OtherIndex, OtherTrainingFeature, Kernel, Value) :-
		latent_covariance(Kernel, TrainingFeature, OtherTrainingFeature, Value).

	latent_covariance(squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, _NoiseVariance, _Jitter), TrainingFeature, OtherTrainingFeature, Value) :-
		mixed_distance(FeatureLayout, TrainingFeature, OtherTrainingFeature, LengthScales, CategoricalPenalties, 0.0, DistanceSquared),
		Value is SignalVariance * exp(-0.5 * DistanceSquared).

	mixed_distance([], [], [], [], [], DistanceSquared, DistanceSquared).
	mixed_distance([continuous_segment(_Attribute)| FeatureLayout], [Value, Missing| Values], [OtherValue, OtherMissing| OtherValues], [LengthScale, MissingScale| LengthScales], CategoricalPenalties, DistanceSquared0, DistanceSquared) :-
		!,
		Difference is Value - OtherValue,
		MissingDifference is Missing - OtherMissing,
		DistanceSquared1 is DistanceSquared0 + Difference * Difference / (LengthScale * LengthScale) + MissingDifference * MissingDifference / (MissingScale * MissingScale),
		mixed_distance(FeatureLayout, Values, OtherValues, LengthScales, CategoricalPenalties, DistanceSquared1, DistanceSquared).
	mixed_distance([categorical_segment(_Attribute, Width)| FeatureLayout], Values, OtherValues, LengthScales, [CategoricalPenalty| CategoricalPenalties], DistanceSquared0, DistanceSquared) :-
		prefix(Width, Values, Segment, RestValues),
		prefix(Width, OtherValues, OtherSegment, RestOtherValues),
		(   Segment == OtherSegment ->
			DistanceSquared1 = DistanceSquared0
		;   DistanceSquared1 is DistanceSquared0 + CategoricalPenalty
		),
		mixed_distance(FeatureLayout, RestValues, RestOtherValues, LengthScales, CategoricalPenalties, DistanceSquared1, DistanceSquared).

	prefix(0, Values, [], Values) :-
		!.
	prefix(Count, [Value| Values], [Value| Prefix], RestValues) :-
		Count > 0,
		NextCount is Count - 1,
		prefix(NextCount, Values, Prefix, RestValues).

	observation_variance(squared_exponential_kernel(_FeatureLayout, _LengthScales, _CategoricalPenalties, SignalVariance, NoiseVariance, _Jitter), Variance) :-
		Variance is SignalVariance + NoiseVariance.

	log_marginal_likelihood(CenteredTargets, Alpha, CholeskyFactor, LogMarginalLikelihood) :-
		length(CenteredTargets, Count),
		dot_product(CenteredTargets, Alpha, DataFit),
		matrix_diagonal(CholeskyFactor, Diagonal),
		sum_log_values(Diagonal, 0.0, LogDeterminantHalf),
		LogMarginalLikelihood is -0.5 * DataFit - LogDeterminantHalf - 0.5 * Count * log(2.0 * pi).

	sum_log_values([], SumLog, SumLog).
	sum_log_values([Value| Values], SumLog0, SumLog) :-
		SumLog1 is SumLog0 + log(Value),
		sum_log_values(Values, SumLog1, SumLog).

	kernel_vector([], _Features, _Kernel, []).
	kernel_vector([TrainingFeature| TrainingFeatures], Features, Kernel, [Value| Values]) :-
		latent_covariance(Kernel, TrainingFeature, Features, Value),
		kernel_vector(TrainingFeatures, Features, Kernel, Values).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'TrainingFeatures', 'TargetMean', 'Alpha', 'CholeskyFactor', 'Kernel', 'Diagnostics'].

	regressor_term_template(gaussian_process_regressor(_Encoders, _TrainingFeatures, _TargetMean, _Alpha, _CholeskyFactor, _Kernel, _Diagnostics), gaussian_process_regressor('Encoders', 'TrainingFeatures', 'TargetMean', 'Alpha', 'CholeskyFactor', 'Kernel', 'Diagnostics')).

	check_regressor(Regressor) :-
		(   Regressor = gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics),
			^^valid_regression_encoders(Encoders),
			valid(float, TargetMean),
			build_feature_layout(Encoders, FeatureLayout, ContinuousFeatureCount, CategoricalFeatureCount),
			^^encoded_feature_count(Encoders, FeatureCount),
			feature_layout_encoded_feature_count(FeatureLayout, FeatureCount),
			valid_kernel(Kernel, FeatureLayout, FeatureCount, ContinuousFeatureCount, CategoricalFeatureCount),
			valid_training_features(TrainingFeatures, FeatureCount),
			length(TrainingFeatures, TrainingExampleCount),
			TrainingExampleCount >= 1,
			valid(list(float, TrainingExampleCount), Alpha),
			valid_cholesky_factor(CholeskyFactor, TrainingExampleCount),
			^^valid_regressor_metadata(gaussian_process_regression, Diagnostics),
			valid_gaussian_process_diagnostics(Diagnostics, ContinuousFeatureCount, CategoricalFeatureCount),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount) ->
			true
		;   domain_error(regressor, Regressor)
		).

	valid_kernel(Kernel, FeatureLayout, FeatureCount, ContinuousFeatureCount, CategoricalFeatureCount) :-
		valid_kernel(Kernel),
		Kernel = squared_exponential_kernel(KernelFeatureLayout, LengthScales, CategoricalPenalties, _SignalVariance, _NoiseVariance, _Jitter),
		KernelFeatureLayout == FeatureLayout,
		feature_layout_encoded_feature_count(KernelFeatureLayout, FeatureCount),
		length(LengthScales, ContinuousFeatureCount),
		length(CategoricalPenalties, CategoricalFeatureCount).

	valid_kernel(squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)) :-
		valid_feature_layout(FeatureLayout),
		valid(list(positive_float), LengthScales),
		valid(list(positive_float), CategoricalPenalties),
		(   LengthScales \== []
		;   CategoricalPenalties \== []
		),
		valid(positive_float, SignalVariance),
		valid(non_negative_float, NoiseVariance),
		valid(positive_float, Jitter).

	valid_feature_layout([]).
	valid_feature_layout([continuous_segment(Attribute)| FeatureLayout]) :-
		!,
		atom(Attribute),
		valid_feature_layout(FeatureLayout).
	valid_feature_layout([categorical_segment(Attribute, Width)| FeatureLayout]) :-
		atom(Attribute),
		integer(Width),
		Width > 0,
		valid_feature_layout(FeatureLayout).

	valid_training_features([], _FeatureCount).
	valid_training_features([TrainingFeature| TrainingFeatures], FeatureCount) :-
		valid(list(float, FeatureCount), TrainingFeature),
		valid_training_features(TrainingFeatures, FeatureCount).

	valid_cholesky_factor(CholeskyFactor, Size) :-
		valid(list, CholeskyFactor),
		length(CholeskyFactor, Size),
		valid_cholesky_rows(CholeskyFactor, Size, 1).

	valid_cholesky_rows([], _Size, _RowIndex).
	valid_cholesky_rows([Row| Rows], Size, RowIndex) :-
		valid(list(float, Size), Row),
		nth1(RowIndex, Row, Diagonal),
		Diagonal > 0.0,
		valid_upper_zeroes(Row, RowIndex),
		NextRowIndex is RowIndex + 1,
		valid_cholesky_rows(Rows, Size, NextRowIndex).

	valid_upper_zeroes(Row, RowIndex) :-
		length(Row, Size),
		valid_upper_zeroes(Row, RowIndex, Size).

	valid_upper_zeroes(_Row, ColumnIndex, Size) :-
		ColumnIndex >= Size,
		!.
	valid_upper_zeroes(Row, ColumnIndex, Size) :-
		NextColumnIndex is ColumnIndex + 1,
		nth1(NextColumnIndex, Row, Value),
		abs(Value) =< 1.0e-12,
		valid_upper_zeroes(Row, NextColumnIndex, Size).

	valid_gaussian_process_diagnostics(Diagnostics, ContinuousFeatureCount, CategoricalFeatureCount) :-
		memberchk(kernel(squared_exponential), Diagnostics),
		memberchk(length_scales(LengthScales), Diagnostics),
		valid(list(positive_float), LengthScales),
		length(LengthScales, ContinuousFeatureCount),
		memberchk(categorical_penalties(CategoricalPenalties), Diagnostics),
		valid(list(positive_float), CategoricalPenalties),
		length(CategoricalPenalties, CategoricalFeatureCount),
		memberchk(signal_variance(SignalVariance), Diagnostics),
		valid(positive_float, SignalVariance),
		memberchk(noise_variance(NoiseVariance), Diagnostics),
		valid(non_negative_float, NoiseVariance),
		memberchk(jitter(Jitter), Diagnostics),
		valid(positive_float, Jitter),
		memberchk(continuous_feature_count(ContinuousFeatureCount), Diagnostics),
		memberchk(categorical_feature_count(CategoricalFeatureCount), Diagnostics),
		memberchk(jitter_attempts(JitterAttempts), Diagnostics),
		integer(JitterAttempts),
		JitterAttempts >= 0,
		memberchk(log_marginal_likelihood(LogMarginalLikelihood), Diagnostics),
		valid(float, LogMarginalLikelihood),
		memberchk(convergence(Convergence), Diagnostics),
		valid_gaussian_process_convergence(Convergence),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 0,
		memberchk(final_delta(FinalDelta), Diagnostics),
		valid(non_negative_float, FinalDelta).

	valid_gaussian_process_convergence(disabled).
	valid_gaussian_process_convergence(tolerance).
	valid_gaussian_process_convergence(maximum_iterations_exhausted).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics),
		Clause =.. [Functor, Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, _Alpha, _CholeskyFactor, Kernel, Diagnostics),
		Kernel = squared_exponential_kernel(_FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		length(TrainingFeatures, TrainingExampleCount),
		format('Gaussian Process Regression Regressor~n', []),
		format('=====================================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Training examples: ~w~n', [TrainingExampleCount]),
		format('Target mean: ~6f~n', [TargetMean]),
		format('Kernel: squared_exponential(length_scales=~w, categorical_penalties=~w, signal_variance=~6f, noise_variance=~6f, jitter=~12f)~n', [LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter]),
		format('Encoders: ~w~n', [Encoders]).

	default_option(kernel(squared_exponential)).
	default_option(feature_scaling(true)).
	default_option(optimize_hyperparameters(true)).
	default_option(length_scale(auto)).
	default_option(categorical_penalty(auto)).
	default_option(signal_variance(auto)).
	default_option(noise_variance(auto)).
	default_option(jitter(1.0e-8)).
	default_option(maximum_iterations(12)).
	default_option(tolerance(1.0e-6)).
	default_option(relative_improvement_factor(1.0e-4)).
	default_option(hyperparameter_minimum(1.0e-6)).
	default_option(maximum_continuous_length_scale(32.0)).
	default_option(maximum_categorical_penalty(32.0)).
	default_option(max_factorization_attempts(32)).
	default_option(jitter_scale_factor(2.0)).

	valid_option(kernel(Kernel)) :-
		Kernel == squared_exponential.
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(optimize_hyperparameters(OptimizeHyperparameters)) :-
		valid(boolean, OptimizeHyperparameters).
	valid_option(length_scale(LengthScale)) :-
		(   LengthScale == auto ->
			true
		;   valid(list(positive_float), LengthScale),
			true
		;   valid(positive_float, LengthScale)
		).
	valid_option(categorical_penalty(CategoricalPenalty)) :-
		(   CategoricalPenalty == auto ->
			true
		;   valid(list(positive_float), CategoricalPenalty),
			true
		;   valid(positive_float, CategoricalPenalty)
		).
	valid_option(signal_variance(SignalVariance)) :-
		(   SignalVariance == auto ->
			true
		;   valid(positive_float, SignalVariance)
		).
	valid_option(noise_variance(NoiseVariance)) :-
		(   NoiseVariance == auto ->
			true
		;   valid(non_negative_float, NoiseVariance)
		).
	valid_option(jitter(Jitter)) :-
		valid(positive_float, Jitter).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(non_negative_float, Tolerance).
	valid_option(relative_improvement_factor(RelativeImprovementFactor)) :-
		valid(non_negative_float, RelativeImprovementFactor).
	valid_option(hyperparameter_minimum(HyperparameterMinimum)) :-
		valid(positive_float, HyperparameterMinimum).
	valid_option(maximum_continuous_length_scale(MaximumContinuousLengthScale)) :-
		valid(positive_float, MaximumContinuousLengthScale).
	valid_option(maximum_categorical_penalty(MaximumCategoricalPenalty)) :-
		valid(positive_float, MaximumCategoricalPenalty).
	valid_option(max_factorization_attempts(MaxFactorizationAttempts)) :-
		valid(positive_integer, MaxFactorizationAttempts).
	valid_option(jitter_scale_factor(JitterScaleFactor)) :-
		valid(positive_float, JitterScaleFactor).

:- end_object.

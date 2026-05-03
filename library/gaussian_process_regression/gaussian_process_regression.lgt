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
		date is 2026-05-03,
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
		initial_kernel(FeatureLayout, ContinuousFeatureCount, CategoricalFeatureCount, TrainingFeatures, CenteredTargets, Options, InitialKernel),
		(   ^^option(optimize_hyperparameters(true), Options) ->
			optimize_kernel_hyperparameters(TrainingFeatures, CenteredTargets, Options, InitialKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, Convergence, Iterations, FinalDelta, LogMarginalLikelihood)
		;   evaluate_kernel(TrainingFeatures, CenteredTargets, InitialKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, LogMarginalLikelihood),
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

	training_feature_count([TrainingFeature| _TrainingFeatures], FeatureCount) :-
		length(TrainingFeature, FeatureCount).

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

	optimize_kernel_hyperparameters(TrainingFeatures, CenteredTargets, Options, InitialKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, Convergence, Iterations, FinalDelta, LogMarginalLikelihood) :-
		evaluate_kernel(TrainingFeatures, CenteredTargets, InitialKernel, InitialKernel1, CholeskyFactor0, Alpha0, JitterAttempts0, Score0),
		InitialState = state(InitialKernel1, Score0, CholeskyFactor0, Alpha0, JitterAttempts0),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		InitialStep is log(2.0),
		MinimumStep is log(1.05),
		coordinate_search(TrainingFeatures, CenteredTargets, MaximumIterations, Tolerance, MinimumStep, 0, InitialStep, InitialState, FinalState, Convergence, Iterations, FinalDelta),
		FinalState = state(Kernel, LogMarginalLikelihood, CholeskyFactor, Alpha, JitterAttempts).

	coordinate_search(_TrainingFeatures, _CenteredTargets, MaximumIterations, _Tolerance, _MinimumStep, Iteration, _Step, State, State, maximum_iterations_exhausted, Iteration, 0.0) :-
		Iteration >= MaximumIterations,
		!.
	coordinate_search(TrainingFeatures, CenteredTargets, MaximumIterations, Tolerance, MinimumStep, Iteration, Step, State0, State, Convergence, Iterations, FinalDelta) :-
		sweep_hyperparameters(TrainingFeatures, CenteredTargets, Step, State0, State1),
		state_score(State0, Score0),
		state_score(State1, Score1),
		Improvement0 is Score1 - Score0,
		(   Improvement0 >= 0.0 ->
			Improvement = Improvement0,
			BestState = State1
		;   Improvement = 0.0,
			BestState = State0
		),
		(   Improvement > Tolerance ->
			NextIteration is Iteration + 1,
			coordinate_search(TrainingFeatures, CenteredTargets, MaximumIterations, Tolerance, MinimumStep, NextIteration, Step, BestState, State, Convergence, Iterations, FinalDelta)
		;   Step > MinimumStep ->
			ReducedStep is Step / 2.0,
			NextIteration is Iteration + 1,
			coordinate_search(TrainingFeatures, CenteredTargets, MaximumIterations, Tolerance, MinimumStep, NextIteration, ReducedStep, BestState, State, Convergence, Iterations, FinalDelta)
		;   State = BestState,
			Convergence = tolerance,
			Iterations is Iteration,
			FinalDelta = Improvement
		).

	state_score(state(_Kernel, Score, _CholeskyFactor, _Alpha, _JitterAttempts), Score).

	sweep_hyperparameters(TrainingFeatures, CenteredTargets, Step, State0, State) :-
		sweep_length_scales(TrainingFeatures, CenteredTargets, Step, State0, State1),
		sweep_categorical_penalties(TrainingFeatures, CenteredTargets, Step, State1, State2),
		sweep_parameter(signal_variance, TrainingFeatures, CenteredTargets, Step, State2, State3),
		sweep_parameter(noise_variance, TrainingFeatures, CenteredTargets, Step, State3, State).

	sweep_length_scales(TrainingFeatures, CenteredTargets, Step, State0, State) :-
		State0 = state(squared_exponential_kernel(_FeatureLayout, LengthScales, _CategoricalPenalties, _SignalVariance, _NoiseVariance, _Jitter), _Score, _CholeskyFactor, _Alpha, _JitterAttempts),
		length(LengthScales, FeatureCount),
		sweep_length_scales(1, FeatureCount, TrainingFeatures, CenteredTargets, Step, State0, State).

	sweep_length_scales(Index, FeatureCount, _TrainingFeatures, _CenteredTargets, _Step, State, State) :-
		Index > FeatureCount,
		!.
	sweep_length_scales(Index, FeatureCount, TrainingFeatures, CenteredTargets, Step, State0, State) :-
		sweep_parameter(length_scale(Index), TrainingFeatures, CenteredTargets, Step, State0, State1),
		NextIndex is Index + 1,
		sweep_length_scales(NextIndex, FeatureCount, TrainingFeatures, CenteredTargets, Step, State1, State).

	sweep_categorical_penalties(TrainingFeatures, CenteredTargets, Step, State0, State) :-
		State0 = state(squared_exponential_kernel(_FeatureLayout, _LengthScales, CategoricalPenalties, _SignalVariance, _NoiseVariance, _Jitter), _Score, _CholeskyFactor, _Alpha, _JitterAttempts),
		length(CategoricalPenalties, FeatureCount),
		sweep_categorical_penalties(1, FeatureCount, TrainingFeatures, CenteredTargets, Step, State0, State).

	sweep_categorical_penalties(Index, FeatureCount, _TrainingFeatures, _CenteredTargets, _Step, State, State) :-
		Index > FeatureCount,
		!.
	sweep_categorical_penalties(Index, FeatureCount, TrainingFeatures, CenteredTargets, Step, State0, State) :-
		sweep_parameter(categorical_penalty(Index), TrainingFeatures, CenteredTargets, Step, State0, State1),
		NextIndex is Index + 1,
		sweep_categorical_penalties(NextIndex, FeatureCount, TrainingFeatures, CenteredTargets, Step, State1, State).

	sweep_parameter(Parameter, TrainingFeatures, CenteredTargets, Step, State0, State) :-
		State0 = state(Kernel0, _Score0, _CholeskyFactor0, _Alpha0, _JitterAttempts0),
		parameter_candidates(Parameter, Kernel0, Step, CandidateKernels),
		best_candidate_state(CandidateKernels, TrainingFeatures, CenteredTargets, State0, State).

	parameter_candidates(length_scale(Index), squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), Step, [
		squared_exponential_kernel(FeatureLayout, LengthScales0, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales1, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)
	]) :-
		nth1(Index, LengthScales, LengthScale),
		scale_parameter(LengthScale, Step, LengthScale0, LengthScale1),
		replace_nth1(Index, LengthScales, LengthScale0, LengthScales0),
		replace_nth1(Index, LengthScales, LengthScale1, LengthScales1).
	parameter_candidates(categorical_penalty(Index), squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), Step, [
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties0, SignalVariance, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties1, SignalVariance, NoiseVariance, Jitter)
	]) :-
		nth1(Index, CategoricalPenalties, CategoricalPenalty),
		scale_parameter(CategoricalPenalty, Step, CategoricalPenalty0, CategoricalPenalty1),
		replace_nth1(Index, CategoricalPenalties, CategoricalPenalty0, CategoricalPenalties0),
		replace_nth1(Index, CategoricalPenalties, CategoricalPenalty1, CategoricalPenalties1).
	parameter_candidates(signal_variance, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), Step, [
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance0, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance1, NoiseVariance, Jitter)
	]) :-
		scale_parameter(SignalVariance, Step, SignalVariance0, SignalVariance1).
	parameter_candidates(noise_variance, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), Step, [
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance0, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance1, Jitter)
	]) :-
		scale_parameter(NoiseVariance, Step, NoiseVariance0, NoiseVariance1).

	replace_nth1(1, [_Value| Values], Replacement, [Replacement| Values]) :-
		!.
	replace_nth1(Index, [Value| Values], Replacement, [Value| Replacements]) :-
		Index > 1,
		NextIndex is Index - 1,
		replace_nth1(NextIndex, Values, Replacement, Replacements).

	scale_parameter(Parameter, Step, Smaller, Larger) :-
		Scale is exp(Step),
		Smaller0 is Parameter / Scale,
		(   Smaller0 >= 1.0e-6 ->
			Smaller = Smaller0
		;   Smaller = 1.0e-6
		),
		Larger is Parameter * Scale.

	best_candidate_state([], _TrainingFeatures, _CenteredTargets, State, State).
	best_candidate_state([CandidateKernel| CandidateKernels], TrainingFeatures, CenteredTargets, State0, State) :-
		(   catch(candidate_state(CandidateKernel, TrainingFeatures, CenteredTargets, CandidateState), error(domain_error(positive_definite_covariance, _), _), fail) ->
			better_state(CandidateState, State0, BetterState)
		;   BetterState = State0
		),
		best_candidate_state(CandidateKernels, TrainingFeatures, CenteredTargets, BetterState, State).

	candidate_state(CandidateKernel, TrainingFeatures, CenteredTargets, state(Kernel, Score, CholeskyFactor, Alpha, JitterAttempts)) :-
		evaluate_kernel(TrainingFeatures, CenteredTargets, CandidateKernel, Kernel, CholeskyFactor, Alpha, JitterAttempts, Score).

	better_state(state(Kernel0, Score0, CholeskyFactor0, Alpha0, JitterAttempts0), state(_Kernel1, Score1, _CholeskyFactor1, _Alpha1, _JitterAttempts1), state(Kernel0, Score0, CholeskyFactor0, Alpha0, JitterAttempts0)) :-
		Score0 > Score1,
		!.
	better_state(_State0, State1, State1).

	evaluate_kernel(TrainingFeatures, CenteredTargets, Kernel0, Kernel, CholeskyFactor, Alpha, JitterAttempts, LogMarginalLikelihood) :-
		factorize_covariance(TrainingFeatures, Kernel0, Kernel, JitterAttempts, CholeskyFactor),
		solve_cholesky(CholeskyFactor, CenteredTargets, Alpha),
		log_marginal_likelihood(CenteredTargets, Alpha, CholeskyFactor, LogMarginalLikelihood).

	factorize_covariance(TrainingFeatures, Kernel0, Kernel, JitterAttempts, CholeskyFactor) :-
		max_factorization_attempts(MaxAttempts),
		factorize_covariance(TrainingFeatures, Kernel0, MaxAttempts, 0, Kernel, JitterAttempts, CholeskyFactor).

	factorize_covariance(_TrainingFeatures, Kernel0, MaxAttempts, Attempt, _Kernel, _JitterAttempts, _CholeskyFactor) :-
		Attempt > MaxAttempts,
		!,
		domain_error(positive_definite_covariance, Kernel0).
	factorize_covariance(TrainingFeatures, Kernel0, MaxAttempts, Attempt, Kernel, JitterAttempts, CholeskyFactor) :-
		update_kernel_jitter(Kernel0, Attempt, CandidateKernel),
		build_covariance_matrix(TrainingFeatures, CandidateKernel, CovarianceMatrix),
		(   catch(cholesky_decomposition(CovarianceMatrix, CholeskyFactor), error(non_positive_definite_covariance(_Value), _Context), fail) ->
			Kernel = CandidateKernel,
			JitterAttempts = Attempt
		;   NextAttempt is Attempt + 1,
			factorize_covariance(TrainingFeatures, Kernel0, MaxAttempts, NextAttempt, Kernel, JitterAttempts, CholeskyFactor)
		).

	max_factorization_attempts(32).

	jitter_scale_factor(2.0).

	update_kernel_jitter(squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), 0, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)) :-
		!.
	update_kernel_jitter(squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter0), Attempt, squared_exponential_kernel(FeatureLayout, LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter)) :-
		jitter_scale_factor(JitterScaleFactor),
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

	squared_distance([], [], DistanceSquared, DistanceSquared).
	squared_distance([Value| Values], [OtherValue| OtherValues], DistanceSquared0, DistanceSquared) :-
		Difference is Value - OtherValue,
		DistanceSquared1 is DistanceSquared0 + Difference * Difference,
		squared_distance(Values, OtherValues, DistanceSquared1, DistanceSquared).

	cholesky_decomposition(CovarianceMatrix, CholeskyFactor) :-
		length(CovarianceMatrix, Size),
		cholesky_decomposition(1, Size, CovarianceMatrix, [], CholeskyFactor).

	cholesky_decomposition(Index, Size, _CovarianceMatrix, CholeskyFactor, CholeskyFactor) :-
		Index > Size,
		!.
	cholesky_decomposition(Index, Size, CovarianceMatrix, PreviousRows, CholeskyFactor) :-
		nth1(Index, CovarianceMatrix, CovarianceRow),
		cholesky_row(1, Index, Size, CovarianceRow, PreviousRows, [], CholeskyRow),
		append(PreviousRows, [CholeskyRow], NextRows),
		NextIndex is Index + 1,
		cholesky_decomposition(NextIndex, Size, CovarianceMatrix, NextRows, CholeskyFactor).

	cholesky_row(ColumnIndex, RowIndex, Size, _CovarianceRow, _PreviousRows, Prefix, CholeskyRow) :-
		ColumnIndex > RowIndex,
		!,
		Remaining is Size - RowIndex,
		zero_vector(Remaining, Zeroes),
		append(Prefix, Zeroes, CholeskyRow).
	cholesky_row(ColumnIndex, RowIndex, Size, CovarianceRow, PreviousRows, Prefix, CholeskyRow) :-
		nth1(ColumnIndex, CovarianceRow, CovarianceValue),
		(   ColumnIndex =:= RowIndex ->
			sum_squares(Prefix, Correction),
			DiagonalValue0 is CovarianceValue - Correction,
			(   DiagonalValue0 > 1.0e-12 ->
				DiagonalValue is sqrt(DiagonalValue0)
			;   throw(error(non_positive_definite_covariance(DiagonalValue0), logtalk(cholesky_decomposition(CovarianceRow), this)))
			),
			append(Prefix, [DiagonalValue], NextPrefix),
			NextColumnIndex is ColumnIndex + 1,
			cholesky_row(NextColumnIndex, RowIndex, Size, CovarianceRow, PreviousRows, NextPrefix, CholeskyRow)
		;   nth1(ColumnIndex, PreviousRows, PreviousRow),
			prefix_dot(Prefix, PreviousRow, 0.0, Correction),
			nth1(ColumnIndex, PreviousRow, Diagonal),
			Entry is (CovarianceValue - Correction) / Diagonal,
			append(Prefix, [Entry], NextPrefix),
			NextColumnIndex is ColumnIndex + 1,
			cholesky_row(NextColumnIndex, RowIndex, Size, CovarianceRow, PreviousRows, NextPrefix, CholeskyRow)
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

	log_marginal_likelihood(CenteredTargets, Alpha, CholeskyFactor, LogMarginalLikelihood) :-
		length(CenteredTargets, Count),
		dot_product(CenteredTargets, Alpha, DataFit),
		sum_log_diagonal(CholeskyFactor, 1, 0.0, LogDeterminantHalf),
		LogMarginalLikelihood is -0.5 * DataFit - LogDeterminantHalf - 0.5 * Count * log(2.0 * pi).

	sum_log_diagonal([], _Index, SumLog, SumLog).
	sum_log_diagonal([Row| Rows], Index, SumLog0, SumLog) :-
		nth1(Index, Row, Diagonal),
		SumLog1 is SumLog0 + log(Diagonal),
		NextIndex is Index + 1,
		sum_log_diagonal(Rows, NextIndex, SumLog1, SumLog).

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
	default_option(maximum_iterations(25)).
	default_option(tolerance(1.0e-6)).

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

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

:- end_object.

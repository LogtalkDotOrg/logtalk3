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


:- object(exponential_smoothing,
	imports([forecaster_common, exponential_smoothing_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Simple, Holt linear-trend, and additive or multiplicative Holt-Winters exponential smoothing forecaster.',
		see_also is [forecaster_protocol, time_series_dataset_protocol]
	]).

	:- public(update/3).
	:- mode(update(+compound, @term, -compound), one_or_error).
	:- info(update/3, [
		comment is 'Returns a new forecaster after applying one numeric observation or, under skip_update, an unbound variable representing a missing observation. Does not bind missing observations.',
		argnames is ['Forecaster', 'Observation', 'UpdatedForecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is neither a variable nor a valid forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Observation`` is a variable under the error missing policy' - instantiation_error,
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation'),
			'``Observation`` is a number but is not finite' - domain_error(finite_number, 'Observation'),
			'``Observation`` is incompatible with the learned transformation' - domain_error(positive_transformation_series, 'Observation'),
			'``Observation`` is incompatible with a multiplicative model' - domain_error(positive_multiplicative_series, 'Observation'),
			'The multiplicative level update is not positive' - domain_error(positive_multiplicative_level, 'Level'),
			'Update or diagnostic arithmetic fails' - evaluation_error('Error')
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth1/3, sort/2, sort/4
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(integer, [
		sequence/3
	]).

	:- uses(numberlist, [
		linear_regression/4, sum/2
	]).

	learn(Dataset, Forecaster, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(model(Method), Options),
		^^option(transformation(Transformation), Options),
		^^option(bias_adjustment(BiasAdjustment), Options),
		check_relevant_transformation_options(Transformation, BiasAdjustment, UserOptions),
		^^option(retain_residuals(RetainResiduals), Options),
		^^option(missing_policy(MissingPolicy), Options),
		^^dataset_series(Dataset, RawSeries),
		check_series_policy(Dataset, RawSeries, MissingPolicy),
		check_observations(RawSeries, MissingPolicy, ObservedCount, MissingCount),
		fit_transformation(
			Transformation, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options,
			EffectiveTransformation, EffectiveOptions, SelectedMethod, SelectionDiagnostic, FitResult
		),
		FitResult = fit_result(
			InnerState, Parameters, SumSquaredError, ErrorCount, Convergence, Iterations, Evaluations,
			Residuals, _ParameterSpecification, Frequency, FreqDiag
		),
		(	ErrorCount > 0 ->
			true
		;	domain_error(insufficient_known_observations, SelectedMethod)
		),
		MeanSquaredError is SumSquaredError / ErrorCount,
		wrap_state(EffectiveTransformation, InnerState, MeanSquaredError, State),
		length(RawSeries, TrainingSeriesLength),
		retained_residuals_diagnostic(RetainResiduals, Residuals, ResidualsDiagnostic),
		^^option(optimizer(Optimizer), Options),
		build_diagnostics(
			SelectedMethod, Frequency, Optimizer, Parameters, SumSquaredError, MeanSquaredError,
			Convergence, Iterations, Evaluations, TrainingSeriesLength, ObservedCount, MissingCount, ErrorCount,
			EffectiveOptions, ResidualsDiagnostic,
			SelectionDiagnostic, FreqDiag, Diagnostics
		),
		Forecaster = exponential_smoothing_forecaster(SelectedMethod, State, Parameters, Diagnostics),
		!.

	fit_transformation(box_cox(auto), Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options,
			box_cox(Lambda), EffectiveOptions, SelectedMethod, SelectionDiagnostic, FitResult) :-
		!,
		check_positive_transformation_series(RawSeries, MissingPolicy),
		^^option(box_cox_bounds(Lower, Upper), Options),
		optimize_box_cox(
			Lower, Upper, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options,
			Lambda, EffectiveOptions, SelectedMethod, SelectionDiagnostic, FitResult
		).
	fit_transformation(Transformation, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options,
			Transformation, Options, SelectedMethod, SelectionDiagnostic, FitResult) :-
		prepare_series(Transformation, RawSeries, MissingPolicy, Series),
		fit_selected_model(Dataset, Method, RawSeries, Series, UserOptions, Options, false, SelectedMethod, SelectionDiagnostic, FitResult).

	fit_selected_model(Dataset, auto, RawSeries, Series, UserOptions, Options, LambdaAutomatic, SelectedMethod, SelectionDiagnostic, FitResult) :-
		!,
		check_relevant_parameter_options(auto, UserOptions),
		select_model_auto(Dataset, RawSeries, Series, Options, LambdaAutomatic, SelectedMethod, SelectionDiagnostic, FitResult).
	fit_selected_model(Dataset, Method, _RawSeries, Series, UserOptions, Options, LambdaAutomatic, Method, none, FitResult) :-
		check_relevant_parameter_options(Method, UserOptions),
		fit_forecaster_core(Dataset, Method, Series, Options, LambdaAutomatic, FitResult).

	optimize_box_cox(Lower, Upper, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options,
			Lambda, EffectiveOptions, SelectedMethod, SelectionDiagnostic, FitResult) :-
		GoldenRatio is (sqrt(5.0) - 1.0) / 2.0,
		LeftLambda is Upper - GoldenRatio * (Upper - Lower),
		RightLambda is Lower + GoldenRatio * (Upper - Lower),
		evaluate_box_cox_lambda(LeftLambda, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, LeftCandidate),
		evaluate_box_cox_lambda(RightLambda, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, RightCandidate),
		golden_section_box_cox(
			24, Lower, Upper, LeftCandidate, RightCandidate, GoldenRatio,
			Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, BestCandidate
		),
		BestCandidate = box_cox_candidate(Lambda, _Objective, EffectiveOptions, SelectedMethod, SelectionDiagnostic, FitResult).

	golden_section_box_cox(0, _Lower, _Upper, LeftCandidate, RightCandidate, _GoldenRatio,
			_Dataset, _Method, _RawSeries, _MissingPolicy, _UserOptions, _Options, BestCandidate) :-
		!,
		better_box_cox_candidate(LeftCandidate, RightCandidate, BestCandidate).
	golden_section_box_cox(Iterations, Lower, Upper, LeftCandidate, RightCandidate, GoldenRatio,
			Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, BestCandidate) :-
		LeftCandidate = box_cox_candidate(LeftLambda, LeftObjective, _LeftOptions, _LeftMethod, _LeftSelection, _LeftFit),
		RightCandidate = box_cox_candidate(RightLambda, RightObjective, _RightOptions, _RightMethod, _RightSelection, _RightFit),
		NextIterations is Iterations - 1,
		(	LeftObjective =< RightObjective ->
			NextUpper = RightLambda,
			NextLeftLambda is NextUpper - GoldenRatio * (NextUpper - Lower),
			evaluate_box_cox_lambda(NextLeftLambda, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, NextLeftCandidate),
			golden_section_box_cox(
				NextIterations, Lower, NextUpper, NextLeftCandidate, LeftCandidate, GoldenRatio,
				Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, BestCandidate
			)
		;	NextLower = LeftLambda,
			NextRightLambda is NextLower + GoldenRatio * (Upper - NextLower),
			evaluate_box_cox_lambda(NextRightLambda, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, NextRightCandidate),
			golden_section_box_cox(
				NextIterations, NextLower, Upper, RightCandidate, NextRightCandidate, GoldenRatio,
				Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options, BestCandidate
			)
		).

	evaluate_box_cox_lambda(Lambda, Dataset, Method, RawSeries, MissingPolicy, UserOptions, Options,
			box_cox_candidate(Lambda, Objective, EffectiveOptions, SelectedMethod, SelectionDiagnostic, FitResult)) :-
		Transformation = box_cox(Lambda),
		transform_series(Transformation, RawSeries, MissingPolicy, Series),
		replace_option(transformation, transformation(Transformation), Options, EffectiveOptions),
		fit_selected_model(Dataset, Method, RawSeries, Series, UserOptions, EffectiveOptions, true, SelectedMethod, SelectionDiagnostic, FitResult),
		FitResult = fit_result(
			_InnerState, _Parameters, SumSquaredError, ErrorCount, _Convergence, _Iterations, _Evaluations,
			_Residuals, _ParameterSpecification, _Frequency, _FreqDiag
		),
		box_cox_profile_objective(RawSeries, Lambda, SumSquaredError, ErrorCount, Objective).

	box_cox_profile_objective(RawSeries, Lambda, SumSquaredError, ErrorCount, Objective) :-
		observed_log_sum(RawSeries, 0.0, 0, LogSum, ObservedCount),
		AdjustedSumSquaredError is max(SumSquaredError, 1.0e-12),
		Objective is ObservedCount * log(AdjustedSumSquaredError / ErrorCount) - 2.0 * (Lambda - 1.0) * LogSum.

	observed_log_sum([], LogSum, ObservedCount, LogSum, ObservedCount).
	observed_log_sum([Value| Values], LogSum0, ObservedCount0, LogSum, ObservedCount) :-
		(	var(Value) ->
			LogSum1 = LogSum0,
			ObservedCount1 = ObservedCount0
		;	LogSum1 is LogSum0 + log(Value),
			ObservedCount1 is ObservedCount0 + 1
		),
		observed_log_sum(Values, LogSum1, ObservedCount1, LogSum, ObservedCount).

	better_box_cox_candidate(LeftCandidate, RightCandidate, BestCandidate) :-
		LeftCandidate = box_cox_candidate(LeftLambda, LeftObjective, _LeftOptions, _LeftMethod, _LeftSelection, _LeftFit),
		RightCandidate = box_cox_candidate(RightLambda, RightObjective, _RightOptions, _RightMethod, _RightSelection, _RightFit),
		(	LeftObjective < RightObjective ->
			BestCandidate = LeftCandidate
		;	LeftObjective > RightObjective ->
			BestCandidate = RightCandidate
		;	LeftLambda =< RightLambda ->
			BestCandidate = LeftCandidate
		;	BestCandidate = RightCandidate
		).

	retained_residuals_diagnostic(true, Residuals, Residuals).
	retained_residuals_diagnostic(false, _Residuals, none).

	update(Forecaster, Observation, UpdatedForecaster) :-
		check_forecaster(Forecaster),
		Forecaster = exponential_smoothing_forecaster(Method, State, Parameters, Diagnostics),
		memberchk(options(TrainingOptions), Diagnostics),
		memberchk(missing_policy(MissingPolicy), TrainingOptions),
		prepare_update_observation(Method, State, Observation, MissingPolicy, UpdateObservation, Missing),
		state_transformation_details(State, Transformation, InnerState, _Variance),
		^^update_smoothing(Method, InnerState, Parameters, UpdateObservation, UpdatedInnerState, SumSquaredErrorIncrement, StepResiduals),
		updated_forecaster_diagnostics(Diagnostics, Missing, SumSquaredErrorIncrement, StepResiduals, UpdatedDiagnostics, MeanSquaredError),
		wrap_state(Transformation, UpdatedInnerState, MeanSquaredError, UpdatedState),
		UpdatedForecaster = exponential_smoothing_forecaster(Method, UpdatedState, Parameters, UpdatedDiagnostics),
		!.

	prepare_update_observation(Method, State, Observation, MissingPolicy, UpdateObservation, Missing) :-
		check_update_observation(Observation, MissingPolicy, Missing),
		(	Missing == true ->
			true
		;	State = transformed(Transformation, _InnerState, _ResidualVariance) ->
			(	Observation > 0.0 ->
				apply_transform(Transformation, Observation, TransformedObservation),
				check_multiplicative_update_observation(Method, TransformedObservation),
				UpdateObservation = TransformedObservation
			;	domain_error(positive_transformation_series, Observation)
			)
		;	check_multiplicative_update_observation(Method, Observation),
			UpdateObservation = Observation
		).

	check_update_observation(Observation, error, _Missing) :-
		var(Observation),
		instantiation_error.
	check_update_observation(Observation, skip_update, true) :-
		var(Observation),
		!.
	check_update_observation(Observation, _MissingPolicy, false) :-
		^^finite_number(Observation),
		!.
	check_update_observation(Observation, _MissingPolicy, _Missing) :-
		(	number(Observation) ->
			domain_error(finite_number, Observation)
		;	type_error(number, Observation)
		).

	check_multiplicative_update_observation(Method, Observation) :-
		(	multiplicative_method(Method), Observation =< 0.0 ->
			domain_error(positive_multiplicative_series, Observation)
		;	true
		).

	updated_forecaster_diagnostics(Diagnostics, Missing, SumSquaredErrorIncrement, StepResiduals, UpdatedDiagnostics, MeanSquaredError) :-
		memberchk(training_series_length(TrainingSeriesLength0), Diagnostics),
		memberchk(observed_count(ObservedCount0), Diagnostics),
		memberchk(missing_count(MissingCount0), Diagnostics),
		memberchk(scored_count(ScoredCount0), Diagnostics),
		memberchk(sum_squared_error(SumSquaredError0), Diagnostics),
		memberchk(residuals(Residuals0), Diagnostics),
		memberchk(update_count(UpdateCount0), Diagnostics),
		TrainingSeriesLength is TrainingSeriesLength0 + 1,
		UpdateCount is UpdateCount0 + 1,
		SumSquaredError is SumSquaredError0 + SumSquaredErrorIncrement,
		(	Missing == true ->
			ObservedCount = ObservedCount0,
			MissingCount is MissingCount0 + 1,
			ScoredCount = ScoredCount0
		;	ObservedCount is ObservedCount0 + 1,
			MissingCount = MissingCount0,
			ScoredCount is ScoredCount0 + 1
		),
		MeanSquaredError is SumSquaredError / ScoredCount,
		updated_residuals(Residuals0, StepResiduals, Residuals),
		^^replace_diagnostic(training_series_length, TrainingSeriesLength, Diagnostics, Diagnostics1),
		^^replace_diagnostic(observed_count, ObservedCount, Diagnostics1, Diagnostics2),
		^^replace_diagnostic(missing_count, MissingCount, Diagnostics2, Diagnostics3),
		^^replace_diagnostic(scored_count, ScoredCount, Diagnostics3, Diagnostics4),
		^^replace_diagnostic(sum_squared_error, SumSquaredError, Diagnostics4, Diagnostics5),
		^^replace_diagnostic(mean_squared_error, MeanSquaredError, Diagnostics5, Diagnostics6),
		^^replace_diagnostic(residuals, Residuals, Diagnostics6, Diagnostics7),
		^^replace_diagnostic(update_count, UpdateCount, Diagnostics7, UpdatedDiagnostics).

	updated_residuals(none, _StepResiduals, none) :-
		!.
	updated_residuals(Residuals0, StepResiduals, Residuals) :-
		append(Residuals0, StepResiduals, Residuals).

	forecast(Forecaster, Horizon, Forecasts) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		Forecaster = exponential_smoothing_forecaster(Method, State, _Parameters, Diagnostics),
		(	State = transformed(Transformation, InnerState, residual_variance(Variance)) ->
			^^forecast_smoothing(Method, InnerState, Horizon, InnerForecasts),
			memberchk(options(Options), Diagnostics),
			memberchk(bias_adjustment(BiasAdjustment), Options),
			back_transform_forecasts(Transformation, BiasAdjustment, Variance, InnerForecasts, Forecasts)
		;	^^forecast_smoothing(Method, State, Horizon, Forecasts)
		).

	:- public(forecast_interval/5).
	:- mode(forecast_interval(+compound, +non_negative_integer, -list(number), -list(number), +list(compound)), one_or_error).
	:- info(forecast_interval/5, [
		comment is 'Computes residual-bootstrap prediction interval bounds for the next ``Horizon`` forecasts of a learned forecaster. Requires a forecaster learned with the ``retain_residuals(true)`` learning option. For each of ``samples/1`` simulated paths and each horizon step, independently resamples, with replacement, one retained one-step training residual, adds it to the deterministic point forecast for that step (thus preserving the seasonal phase of the point forecast), and inverse-transforms the simulated value using the same transformation and bias adjustment as ``forecast/3``. The reported bounds are the empirical ``(1-confidence)/2`` and ``(1+confidence)/2`` quantiles of the simulated values for each step, widened when necessary so that the point forecast always lies within its bounds. A zero horizon returns two empty lists and does not require retained residuals.',
		argnames is ['Forecaster', 'Horizon', 'Lower', 'Upper', 'Options'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is neither a variable nor a valid forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An option is a variable' - instantiation_error,
			'An option is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An option is a compound term but is not a valid interval option' - domain_error(option, 'Option'),
			'``Horizon`` is positive and the forecaster was learned without ``retain_residuals(true)``' - domain_error(retained_residuals, 'Forecaster')
		]
	]).

	forecast_interval(Forecaster, Horizon, Lower, Upper, UserOptions) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		check_interval_options(UserOptions),
		merge_interval_options(UserOptions, Options),
		memberchk(confidence(Confidence), Options),
		memberchk(method(residual_bootstrap), Options),
		memberchk(samples(Samples), Options),
		memberchk(seed(Seed), Options),
		(	Horizon =:= 0 ->
			Lower = [],
			Upper = []
		;	Forecaster = exponential_smoothing_forecaster(Method, State, _Parameters, Diagnostics),
			retained_residuals(Diagnostics, Forecaster, Residuals),
			state_transformation_details(State, Transformation, InnerState, Variance),
			memberchk(options(FitOptions), Diagnostics),
			memberchk(bias_adjustment(BiasAdjustment), FitOptions),
			^^forecast_smoothing(Method, InnerState, Horizon, InnerPointForecasts),
			forecast(Forecaster, Horizon, PointForecasts),
			bootstrap_quantile_bounds(
				InnerPointForecasts, Residuals, Samples, Seed, Transformation, BiasAdjustment, Variance,
				Confidence, PointForecasts, Lower, Upper
			)
		).

	retained_residuals(Diagnostics, Forecaster, Residuals) :-
		memberchk(residuals(ResidualsTerm), Diagnostics),
		(	ResidualsTerm == none ->
			domain_error(retained_residuals, Forecaster)
		;	Residuals = ResidualsTerm
		).

	state_transformation_details(transformed(Transformation, InnerState, residual_variance(Variance)), Transformation, InnerState, Variance) :-
		!.
	state_transformation_details(InnerState, none, InnerState, 0.0).

	% forecast_interval/5 option handling is intentionally independent from the
	% learn/3 options category machinery so that interval-only options never
	% leak into learned forecaster diagnostics.

	check_interval_options(Options) :-
		check_interval_options(Options, Options).

	check_interval_options(Options, _) :-
		var(Options),
		instantiation_error.
	check_interval_options([Option| Options], Options0) :-
		!,
		check_interval_option(Option),
		check_interval_options(Options, Options0).
	check_interval_options([], _) :-
		!.
	check_interval_options(_, Options0) :-
		type_error(list, Options0).

	check_interval_option(Option) :-
		(	var(Option) ->
			instantiation_error
		;	\+ compound(Option) ->
			type_error(compound, Option)
		;	\+ valid_interval_option(Option) ->
			domain_error(option, Option)
		;	true
		).

	merge_interval_options(UserOptions, Options) :-
		findall(
			DefaultOption,
			(	default_interval_option(DefaultOption),
				functor(DefaultOption, OptionName, Arity),
				functor(UserOption, OptionName, Arity),
				\+ member(UserOption, UserOptions)
			),
			DefaultOptions
		),
		append(UserOptions, DefaultOptions, Options).

	valid_interval_option(confidence(Level)) :-
		^^finite_number(Level),
		Level > 0.0,
		Level < 1.0.
	valid_interval_option(method(residual_bootstrap)).
	valid_interval_option(samples(Count)) :-
		valid(positive_integer, Count).
	valid_interval_option(seed(Seed)) :-
		valid(positive_integer, Seed).

	default_interval_option(confidence(0.95)).
	default_interval_option(method(residual_bootstrap)).
	default_interval_option(samples(1000)).
	default_interval_option(seed(42)).

	bootstrap_quantile_bounds(InnerPointForecasts, Residuals, Samples, Seed, Transformation, BiasAdjustment, Variance, Confidence, PointForecasts, Lower, Upper) :-
		length(InnerPointForecasts, Horizon),
		empty_columns(Horizon, Columns0),
		run_with_isolated_seed(
			Seed,
			collect_bootstrap_columns(Samples, InnerPointForecasts, Residuals, Transformation, BiasAdjustment, Variance, Columns0, Columns)
		),
		LowerQuantile is (1.0 - Confidence) / 2.0,
		UpperQuantile is 1.0 - LowerQuantile,
		quantile_bounds(Columns, LowerQuantile, UpperQuantile, PointForecasts, Lower, Upper).

	empty_columns(0, []) :-
		!.
	empty_columns(N, [[]| Columns]) :-
		N1 is N - 1,
		empty_columns(N1, Columns).

	collect_bootstrap_columns(0, _InnerPointForecasts, _Residuals, _Transformation, _BiasAdjustment, _Variance, Columns, Columns) :-
		!.
	collect_bootstrap_columns(N, InnerPointForecasts, Residuals, Transformation, BiasAdjustment, Variance, Columns0, Columns) :-
		sample_path_values(InnerPointForecasts, Residuals, Transformation, BiasAdjustment, Variance, PathValues),
		merge_columns(Columns0, PathValues, Columns1),
		N1 is N - 1,
		collect_bootstrap_columns(N1, InnerPointForecasts, Residuals, Transformation, BiasAdjustment, Variance, Columns1, Columns).

	merge_columns([], [], []).
	merge_columns([Column| Columns], [Value| Values], [[Value| Column]| MergedColumns]) :-
		merge_columns(Columns, Values, MergedColumns).

	sample_path_values([], _Residuals, _Transformation, _BiasAdjustment, _Variance, []).
	sample_path_values([InnerForecast| InnerForecasts], Residuals, Transformation, BiasAdjustment, Variance, [Value| Values]) :-
		fast_random(as183)::member(Residual, Residuals),
		SimulatedInner is InnerForecast + Residual,
		back_transform_value(Transformation, BiasAdjustment, Variance, SimulatedInner, Value),
		sample_path_values(InnerForecasts, Residuals, Transformation, BiasAdjustment, Variance, Values).

	back_transform_value(none, _BiasAdjustment, _Variance, Value, Value) :-
		!.
	back_transform_value(Transformation, BiasAdjustment, Variance, Value, Result) :-
		back_transform_forecast(Transformation, BiasAdjustment, Variance, Value, Result).

	quantile_bounds([], _LowerQuantile, _UpperQuantile, [], [], []).
	quantile_bounds([Column| Columns], LowerQuantile, UpperQuantile, [Point| Points], [Lower| Lowers], [Upper| Uppers]) :-
		sort(0, @=<, Column, SortedColumn),
		length(SortedColumn, Count),
		empirical_quantile(SortedColumn, Count, LowerQuantile, RawLower),
		empirical_quantile(SortedColumn, Count, UpperQuantile, RawUpper),
		Lower is min(RawLower, Point),
		Upper is max(RawUpper, Point),
		quantile_bounds(Columns, LowerQuantile, UpperQuantile, Points, Lowers, Uppers).

	empirical_quantile(SortedValues, Count, Quantile, Value) :-
		RawIndex is ceiling(Quantile * Count),
		clamp_index(RawIndex, Count, Index),
		nth1(Index, SortedValues, Value).

	clamp_index(Index, _Count, 1) :-
		Index < 1,
		!.
	clamp_index(Index, Count, Count) :-
		Index > Count,
		!.
	clamp_index(Index, _Count, Index).

	:- meta_predicate(run_with_isolated_seed(*, 0)).

	run_with_isolated_seed(Seed, Goal) :-
		fast_random(as183)::get_seed(SavedSeed),
		fast_random(as183)::randomize(Seed),
		catch(once(Goal), Error, true),
		fast_random(as183)::set_seed(SavedSeed),
		(	var(Error) ->
			true
		;	throw(Error)
		).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	(	Forecaster = exponential_smoothing_forecaster(Method, State, Parameters, Diagnostics),
				valid_method(Method),
				valid_model_state(Method, State),
				valid_parameters(Method, Parameters),
				valid_diagnostics(Method, State, Parameters, Diagnostics) ->
				true
			;	domain_error(forecaster, Forecaster)
			)
		).

	valid_diagnostics(Method, State, Parameters, Diagnostics) :-
		^^valid_forecaster_metadata(exponential_smoothing, Options, Diagnostics),
		valid_diagnostic_options(Method, Parameters, Options, Diagnostics, AutomaticParameters, Optimizer, OptimizerOptions),
		valid_transformation_diagnostics(State, Options),
		unwrap_state(State, InnerState),
		memberchk(training_series_length(TrainingSeriesLength), Diagnostics),
		valid(positive_integer, TrainingSeriesLength),
		memberchk(observed_count(ObservedCount), Diagnostics),
		valid(positive_integer, ObservedCount),
		memberchk(missing_count(MissingCount), Diagnostics),
		valid(non_negative_integer, MissingCount),
		TrainingSeriesLength =:= ObservedCount + MissingCount,
		memberchk(scored_count(ScoredCount), Diagnostics),
		valid(positive_integer, ScoredCount),
		ScoredCount =< ObservedCount,
		memberchk(update_count(UpdateCount), Diagnostics),
		valid(non_negative_integer, UpdateCount),
		memberchk(method(Method), Diagnostics),
		valid_frequency_diagnostics(Method, InnerState, Diagnostics),
		valid_frequency_selection_diagnostics(Method, Options, Diagnostics),
		memberchk(parameters(Parameters), Diagnostics),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		non_negative_number(SumSquaredError),
		memberchk(mean_squared_error(MeanSquaredError), Diagnostics),
		non_negative_number(MeanSquaredError),
		consistent_mean_squared_error(SumSquaredError, ScoredCount, MeanSquaredError),
		valid_retained_residuals(Options, ScoredCount, Diagnostics),
		memberchk(optimizer(Optimizer), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		valid(non_negative_integer, Iterations),
		memberchk(evaluations(Evaluations), Diagnostics),
		valid(non_negative_integer, Evaluations),
		valid_convergence(AutomaticParameters, Optimizer, Convergence, Iterations, Evaluations, OptimizerOptions).

	valid_diagnostic_options(Method, Parameters, Options, Diagnostics, AutomaticParameters, Optimizer, OptimizerOptions) :-
		^^valid_options(Options),
		valid_model_selection_diagnostics(Method, Options, Diagnostics),
		memberchk(optimizer(Optimizer), Options),
		memberchk(optimizer_options(OptimizerOptions), Options),
		memberchk(transformation(Transformation), Options),
		memberchk(bias_adjustment(BiasAdjustment), Options),
		bias_adjustment_consistent(Transformation, BiasAdjustment),
		valid_parameter_diagnostic_options(Method, Parameters, Options, SmoothingAutomaticParameters),
		initialization_automatic_parameters(Options, SmoothingAutomaticParameters, AutomaticParameters).

	initialization_automatic_parameters(Options, _SmoothingAutomaticParameters, automatic) :-
		memberchk(initialization(optimized), Options),
		!.
	initialization_automatic_parameters(_Options, AutomaticParameters, AutomaticParameters).

	valid_model_selection_diagnostics(Method, Options, Diagnostics) :-
		^^option(model(ModelOption), Options),
		(	ModelOption == auto ->
			memberchk(selection_criterion(Criterion), Options),
			memberchk(model_selection(criterion(Criterion), candidates(Candidates), selected(Method)), Diagnostics),
			valid_candidate_list(Candidates),
			memberchk(candidate(Method, ok, _Score, _Parameters), Candidates)
		;	ModelOption == Method,
			\+ member(model_selection(_), Diagnostics)
		).

	valid_candidate_list([]).
	valid_candidate_list([Candidate| Candidates]) :-
		valid_candidate(Candidate),
		valid_candidate_list(Candidates).

	valid_candidate(candidate(Method, ok, Score, Parameters)) :-
		!,
		valid_method(Method),
		^^finite_number(Score),
		valid(list(number), Parameters).
	valid_candidate(candidate(Method, rejected(_Reason), none, none)) :-
		valid_method(Method).

	valid_frequency_selection_diagnostics(Method, Options, Diagnostics) :-
		^^option(frequency(FrequencyOption), Options),
		(	FrequencyOption == auto,
			seasonal_method(Method) ->
			memberchk(frequency_selection(candidates(FrequencyCandidates), selected(Frequency)), Diagnostics),
			valid_frequency_candidate_list(FrequencyCandidates),
			memberchk(candidate(Frequency, ok, _Score), FrequencyCandidates),
			memberchk(frequency(Frequency), Diagnostics)
		;	\+ member(frequency_selection(_), Diagnostics)
		).

	valid_frequency_candidate_list([]).
	valid_frequency_candidate_list([Candidate| Candidates]) :-
		valid_frequency_candidate(Candidate),
		valid_frequency_candidate_list(Candidates).

	valid_frequency_candidate(candidate(Frequency, ok, Score)) :-
		!,
		valid(positive_integer, Frequency),
		Frequency >= 2,
		^^finite_number(Score).
	valid_frequency_candidate(candidate(Frequency, rejected(insufficient_observations), none)) :-
		!,
		valid(positive_integer, Frequency),
		Frequency >= 2.
	valid_frequency_candidate(candidate(Frequency, rejected(non_positive_autocorrelation), Score)) :-
		valid(positive_integer, Frequency),
		Frequency >= 2,
		^^finite_number(Score).

	valid_transformation_diagnostics(State, Options) :-
		memberchk(transformation(OptionTransformation), Options),
		(	State = transformed(StateTransformation, _InnerState, residual_variance(Variance)) ->
			StateTransformation == OptionTransformation,
			OptionTransformation \== none,
			^^finite_number(Variance),
			Variance >= 0.0
		;	OptionTransformation == none
		).

	unwrap_state(transformed(_Transformation, InnerState, residual_variance(_Variance)), InnerState) :-
		!.
	unwrap_state(State, State).

	valid_parameter_diagnostic_options(simple, [Alpha], Options, AutomaticParameters) :-
		memberchk(alpha(AlphaOption), Options),
		parameter_option_matches(AlphaOption, Alpha, AutomaticParameters),
		memberchk(beta(auto), Options),
		memberchk(gamma(auto), Options).
	valid_parameter_diagnostic_options(holt, [Alpha, Beta], Options, AutomaticParameters) :-
		memberchk(alpha(AlphaOption), Options),
		memberchk(beta(BetaOption), Options),
		parameter_options_match([AlphaOption, BetaOption], [Alpha, Beta], AutomaticParameters),
		memberchk(gamma(auto), Options),
		memberchk(phi(auto), Options).
	valid_parameter_diagnostic_options(holt_damped, [Alpha, Beta, Phi], Options, AutomaticParameters) :-
		memberchk(alpha(AlphaOption), Options),
		memberchk(beta(BetaOption), Options),
		memberchk(phi(PhiOption), Options),
		parameter_options_match([AlphaOption, BetaOption, PhiOption], [Alpha, Beta, Phi], AutomaticParameters),
		memberchk(gamma(auto), Options).
	valid_parameter_diagnostic_options(Method, [Alpha, Beta, Gamma, Phi], Options, AutomaticParameters) :-
		damped_seasonal_method(Method),
		memberchk(alpha(AlphaOption), Options),
		memberchk(beta(BetaOption), Options),
		memberchk(gamma(GammaOption), Options),
		memberchk(phi(PhiOption), Options),
		parameter_options_match([AlphaOption, BetaOption, GammaOption, PhiOption], [Alpha, Beta, Gamma, Phi], AutomaticParameters).
	valid_parameter_diagnostic_options(Method, [Alpha, Beta, Gamma], Options, AutomaticParameters) :-
		seasonal_method(Method),
		memberchk(alpha(AlphaOption), Options),
		memberchk(beta(BetaOption), Options),
		memberchk(gamma(GammaOption), Options),
		parameter_options_match([AlphaOption, BetaOption, GammaOption], [Alpha, Beta, Gamma], AutomaticParameters),
		diagnostic_undamped_phi(Options).

	diagnostic_undamped_phi(Options) :-
		memberchk(phi(auto), Options).

	parameter_options_match([], [], fixed).
	parameter_options_match([Option| Options], [Parameter| Parameters], AutomaticParameters) :-
		parameter_option_matches(Option, Parameter, AutomaticParameter),
		parameter_options_match(Options, Parameters, RestAutomaticParameters),
		combine_automatic_parameters(AutomaticParameter, RestAutomaticParameters, AutomaticParameters).

	parameter_option_matches(auto, _Parameter, automatic) :-
		!.
	parameter_option_matches(Parameter, Parameter, fixed).

	combine_automatic_parameters(automatic, _Rest, automatic) :-
		!.
	combine_automatic_parameters(fixed, AutomaticParameters, AutomaticParameters).

	valid_frequency_diagnostics(simple, _State, Diagnostics) :-
		\+ member(frequency(_), Diagnostics).
	valid_frequency_diagnostics(holt, _State, Diagnostics) :-
		\+ member(frequency(_), Diagnostics).
	valid_frequency_diagnostics(holt_damped, _State, Diagnostics) :-
		\+ member(frequency(_), Diagnostics).
	valid_frequency_diagnostics(holt_winters_additive, holt_winters(_Level, _Trend, Frequency, _SeasonalQueue), Diagnostics) :-
		memberchk(frequency(Frequency), Diagnostics).
	valid_frequency_diagnostics(holt_winters_multiplicative, holt_winters(_Level, _Trend, Frequency, _SeasonalQueue), Diagnostics) :-
		memberchk(frequency(Frequency), Diagnostics).
	valid_frequency_diagnostics(Method, holt_winters_damped(_Level, _Trend, _Phi, Frequency, _SeasonalQueue), Diagnostics) :-
		damped_seasonal_method(Method),
		memberchk(frequency(Frequency), Diagnostics).

	valid_convergence(fixed, _Optimizer, fixed_parameters, 0, 0, _OptimizerOptions).
	valid_convergence(automatic, nelder_mead, converged, Iterations, Evaluations, OptimizerOptions) :-
		Evaluations > 0,
		optimizer_maximum_iterations(OptimizerOptions, MaximumIterations),
		Iterations < MaximumIterations.
	valid_convergence(automatic, nelder_mead, maximum_iterations, Iterations, Evaluations, OptimizerOptions) :-
		Evaluations > 0,
		optimizer_maximum_iterations(OptimizerOptions, MaximumIterations),
		Iterations >= MaximumIterations.
	valid_convergence(automatic, multi_start(Starts), Convergence, Iterations, Evaluations, _OptimizerOptions) :-
		valid(positive_integer, Starts),
		Evaluations > 0,
		Iterations >= 0,
		memberchk(Convergence, [converged, maximum_iterations]).
	valid_convergence(automatic, differential_evolution, Convergence, Iterations, Evaluations, _OptimizerOptions) :-
		Evaluations > 0,
		Iterations >= 0,
		memberchk(Convergence, [converged, maximum_iterations]).

	non_negative_number(Number) :-
		^^finite_number(Number),
		Number >= 0.0.

	initialization_cycle_count(two_cycles, _InitialCycles, 2).
	initialization_cycle_count(regression, InitialCycles, InitialCycles).
	initialization_cycle_count(optimized, InitialCycles, InitialCycles).

	valid_retained_residuals(Options, ErrorCount, Diagnostics) :-
		memberchk(retain_residuals(RetainResiduals), Options),
		memberchk(residuals(ResidualsTerm), Diagnostics),
		(	RetainResiduals == true ->
			valid(list(number), ResidualsTerm),
			length(ResidualsTerm, ErrorCount),
			finite_values(ResidualsTerm)
		;	ResidualsTerm == none
		).

	consistent_mean_squared_error(SumSquaredError, ErrorCount, MeanSquaredError) :-
		ErrorCount > 0,
		Expected is SumSquaredError / ErrorCount,
		Difference is abs(MeanSquaredError - Expected),
		AbsoluteExpected is abs(Expected),
		(	AbsoluteExpected > 1.0 ->
			Scale = AbsoluteExpected
		;	Scale = 1.0
		),
		Difference =< 1.0e-12 * Scale.

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	forecaster_term_template(
		exponential_smoothing_forecaster(_Method, _State, _Parameters, _Diagnostics),
		exponential_smoothing_forecaster('Method', 'State', 'Parameters', 'Diagnostics')
	).

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		Clause =.. [Functor, Forecaster].

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		Forecaster = exponential_smoothing_forecaster(Method, State, Parameters, Diagnostics),
		^^print_forecaster_template(Forecaster),
		format('Method: ~w~n', [Method]),
		format('State: ~w~n', [State]),
		format('Parameters: ~w~n', [Parameters]),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		memberchk(mean_squared_error(MeanSquaredError), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		format('Sum squared error: ~w~n', [SumSquaredError]),
		format('Mean squared error: ~w~n', [MeanSquaredError]),
		format('Convergence: ~w~n', [Convergence]).

	model_configuration(Dataset, simple, Series, Options, none, [alpha(Alpha)]) :-
		!,
		^^check_series_length(Dataset, Series, 2),
		^^option(alpha(Alpha), Options).
	model_configuration(Dataset, holt, Series, Options, none, [alpha(Alpha), beta(Beta)]) :-
		!,
		^^check_series_length(Dataset, Series, 3),
		^^option(alpha(Alpha), Options),
		^^option(beta(Beta), Options).
	model_configuration(Dataset, holt_damped, Series, Options, none, [alpha(Alpha), beta(Beta), phi(Phi)]) :-
		!,
		^^check_series_length(Dataset, Series, 3),
		^^option(alpha(Alpha), Options),
		^^option(beta(Beta), Options),
		^^option(phi(Phi), Options).
	model_configuration(Dataset, Method, Series, Options, Frequency, ParameterSpecification) :-
		seasonal_method(Method),
		^^check_frequency(Frequency),
		(	Frequency >= 2 ->
			true
		;	domain_error(seasonal_frequency, Frequency)
		),
		^^option(initialization(Initialization), Options),
		^^option(initial_cycles(InitialCycles), Options),
		initialization_cycle_count(Initialization, InitialCycles, CycleCount),
		MinimumLength is CycleCount * Frequency + 1,
		^^check_series_length(Dataset, Series, MinimumLength),
		(	multiplicative_method(Method) ->
			^^option(missing_policy(MissingPolicy), Options),
			check_positive_series(Series, MissingPolicy)
		;	true
		),
		^^option(alpha(Alpha), Options),
		^^option(beta(Beta), Options),
		^^option(gamma(Gamma), Options),
		seasonal_parameter_specification(Method, Options, Alpha, Beta, Gamma, ParameterSpecification).

	seasonal_parameter_specification(Method, Options, Alpha, Beta, Gamma, [alpha(Alpha), beta(Beta), gamma(Gamma), phi(Phi)]) :-
		damped_seasonal_method(Method),
		!,
		^^option(phi(Phi), Options).
	seasonal_parameter_specification(_Method, _Options, Alpha, Beta, Gamma, [alpha(Alpha), beta(Beta), gamma(Gamma)]).

	% shared fitting core, reused by explicit-model learning and by every
	% model- and frequency-selection candidate evaluation

	fit_forecaster_core(Dataset, Method, Series, Options, LambdaAutomatic, FitResult) :-
		(	seasonal_method(Method) ->
			resolve_seasonal_frequency(Dataset, Method, Series, Options, LambdaAutomatic, Frequency, FreqDiag)
		;	Frequency = none,
			FreqDiag = none
		),
		model_configuration(Dataset, Method, Series, Options, Frequency, ParameterSpecification),
		^^option(initialization(Initialization), Options),
		^^option(initial_cycles(InitialCycles), Options),
		InitializationSpecification = initialization(Initialization, InitialCycles),
		^^option(optimizer(Optimizer), Options),
		^^option(optimizer_options(OptimizerOptions), Options),
		^^option(de_options(DeOptions), Options),
		fit_parameters(
			Method, Series, Frequency, ParameterSpecification, InitializationSpecification, Optimizer, OptimizerOptions, DeOptions,
			Parameters, State, SumSquaredError, ErrorCount, Convergence, Iterations, Evaluations, Residuals
		),
		FitResult = fit_result(
			State, Parameters, SumSquaredError, ErrorCount, Convergence, Iterations, Evaluations,
			Residuals, ParameterSpecification, Frequency, FreqDiag
		).

	% seasonal frequency resolution: dataset (default), explicit integer, or automatic selection

	resolve_seasonal_frequency(Dataset, Method, Series, Options, LambdaAutomatic, Frequency, FreqDiag) :-
		^^option(frequency(FrequencyOption), Options),
		resolve_seasonal_frequency_(FrequencyOption, Dataset, Method, Series, Options, LambdaAutomatic, Frequency, FreqDiag).

	resolve_seasonal_frequency_(dataset, Dataset, _Method, _Series, _Options, _LambdaAutomatic, Frequency, none) :-
		!,
		(	Dataset::frequency(Frequency0) ->
			Frequency = Frequency0
		;	domain_error(seasonal_frequency, Dataset)
		).
	resolve_seasonal_frequency_(auto, Dataset, Method, Series, Options, LambdaAutomatic, Frequency, FreqDiag) :-
		!,
		select_frequency_auto(Dataset, Method, Series, Options, LambdaAutomatic, Frequency, FreqDiag).
	resolve_seasonal_frequency_(Frequency, _Dataset, _Method, _Series, _Options, _LambdaAutomatic, Frequency, none) :-
		integer(Frequency).

	% direct O(n*k) detrended autocorrelation frequency search: the linear trend is
	% estimated and removed once, then every candidate lag is scored in O(n); the
	% autocorrelation-ranked shortlist is then evaluated through an AICc fit (not
	% autocorrelation alone) so the final choice reflects actual model fit quality

	select_frequency_auto(Dataset, Method, Series, Options, LambdaAutomatic, SelectedFrequency, FreqDiag) :-
		^^option(frequency_candidates(RawCandidates), Options),
		(	RawCandidates == none ->
			domain_error(missing_frequency_candidates, Method)
		;	true
		),
		normalize_frequency_candidates(RawCandidates, CandidateFrequencies),
		length(Series, Length),
		score_frequency_candidates(CandidateFrequencies, Series, Length, CandidateTerms),
		shortlist_frequency_candidates(CandidateTerms, ShortList),
		(	ShortList == [] ->
			domain_error(no_viable_frequency_candidate, Method)
		;	true
		),
		evaluate_frequency_shortlist(ShortList, Dataset, Method, Series, Options, LambdaAutomatic, EvaluatedShortlist),
		select_best_frequency(EvaluatedShortlist, SelectedFrequency, Found),
		(	Found == yes ->
			FreqDiag = frequency_selection(candidates(CandidateTerms), selected(SelectedFrequency))
		;	domain_error(no_viable_frequency_candidate, Method)
		).

	normalize_frequency_candidates(Min-Max, CandidateFrequencies) :-
		integer(Min),
		integer(Max),
		!,
		sequence(Min, Max, CandidateFrequencies).
	normalize_frequency_candidates(List, CandidateFrequencies) :-
		sort(List, CandidateFrequencies).

	score_frequency_candidates(CandidateFrequencies, Series, Length, CandidateTerms) :-
		indexed_regression(Series, Trend, Intercept),
		detrend_values_missing(Series, 1, Trend, Intercept, Detrended),
		score_frequency_candidates_(CandidateFrequencies, Detrended, Length, CandidateTerms).

	score_frequency_candidates_([], _Detrended, _Length, []).
	score_frequency_candidates_([Frequency| Frequencies], Detrended, Length, [candidate(Frequency, Status, Score)| Terms]) :-
		MinimumLength is 2 * Frequency + 1,
		(	Length >= MinimumLength ->
			autocorrelation_at_lag_missing(Detrended, Frequency, Score0),
			(	Score0 > 0.0 ->
				Status = ok,
				Score = Score0
			;	Status = rejected(non_positive_autocorrelation),
				Score = Score0
			)
		;	Status = rejected(insufficient_observations),
			Score = none
		),
		score_frequency_candidates_(Frequencies, Detrended, Length, Terms).

	indexed_regression(Series, Trend, Intercept) :-
		indexed_observations(Series, 1, Indices, Observations),
		(	Indices = [_First, _Second| _] ->
			linear_regression(Indices, Observations, Trend, Intercept)
		;	domain_error(insufficient_known_observations, frequency_selection)
		).

	indexed_observations([], _Index, [], []).
	indexed_observations([Value| Values], Index, Indices, Observations) :-
		NextIndex is Index + 1,
		(	var(Value) ->
			Indices = RestIndices,
			Observations = RestObservations
		;	Indices = [Index| RestIndices],
			Observations = [Value| RestObservations]
		),
		indexed_observations(Values, NextIndex, RestIndices, RestObservations).

	detrend_values_missing([], _Index, _Trend, _Intercept, []).
	detrend_values_missing([Value| Values], Index, Trend, Intercept, [Detrended| Detrendeds]) :-
		(	var(Value) ->
			true
		;	Prediction is Intercept + Trend * Index,
			Detrended is Value - Prediction
		),
		NextIndex is Index + 1,
		detrend_values_missing(Values, NextIndex, Trend, Intercept, Detrendeds).

	autocorrelation_at_lag_missing(Detrended, Lag, Score) :-
		^^observed_series(Detrended, Observations),
		length(Observations, Count),
		sum(Observations, Sum),
		Mean is Sum / Count,
		length(Detrended, Length),
		PairCount is Length - Lag,
		length(Prefix, PairCount),
		append(Prefix, _Rest, Detrended),
		length(Skip, Lag),
		append(Skip, Suffix, Detrended),
		missing_covariance(Prefix, Suffix, Mean, 0.0, 0.0, Covariance, Variance),
		( Variance =< 0.0 -> Score = 0.0; Score is Covariance / Variance ).

	missing_covariance([], [], _Mean, Covariance, Variance, Covariance, Variance).
	missing_covariance([First| Firsts], [Second| Seconds], Mean, Covariance0, Variance0, Covariance, Variance) :-
		( var(First) ->
			Covariance1 = Covariance0, Variance1 = Variance0
		; var(Second) ->
			Covariance1 = Covariance0, Variance1 = Variance0
		; CenteredFirst is First - Mean,
			CenteredSecond is Second - Mean,
			Covariance1 is Covariance0 + CenteredFirst * CenteredSecond,
			Variance1 is Variance0 + CenteredFirst * CenteredFirst
		),
		missing_covariance(Firsts, Seconds, Mean, Covariance1, Variance1, Covariance, Variance).

	shortlist_frequency_candidates(CandidateTerms, ShortList) :-
		ok_frequency_pairs(CandidateTerms, Pairs),
		sort_frequency_pairs(Pairs, SortedFrequencies),
		top_n(SortedFrequencies, 3, ShortList).

	ok_frequency_pairs([], []).
	ok_frequency_pairs([candidate(Frequency, ok, Score)| Terms], [Frequency-Score| Pairs]) :-
		!,
		ok_frequency_pairs(Terms, Pairs).
	ok_frequency_pairs([_Term| Terms], Pairs) :-
		ok_frequency_pairs(Terms, Pairs).

	sort_frequency_pairs(Pairs, SortedFrequencies) :-
		negate_pairs(Pairs, NegatedPairs),
		sort(0, @=<, NegatedPairs, SortedNegatedPairs),
		strip_negated_pairs(SortedNegatedPairs, SortedFrequencies).

	negate_pairs([], []).
	negate_pairs([Frequency-Score| Pairs], [NegatedScore-Frequency| NegatedPairs]) :-
		NegatedScore is -Score,
		negate_pairs(Pairs, NegatedPairs).

	strip_negated_pairs([], []).
	strip_negated_pairs([_NegatedScore-Frequency| NegatedPairs], [Frequency| Frequencies]) :-
		strip_negated_pairs(NegatedPairs, Frequencies).

	top_n(_List, 0, []) :-
		!.
	top_n([], _N, []) :-
		!.
	top_n([Value| Values], N, [Value| Result]) :-
		N > 0,
		N1 is N - 1,
		top_n(Values, N1, Result).

	evaluate_frequency_shortlist([], _Dataset, _Method, _Series, _Options, _LambdaAutomatic, []).
	evaluate_frequency_shortlist([Frequency| Frequencies], Dataset, Method, Series, Options, LambdaAutomatic, [Term| Terms]) :-
		evaluate_frequency_candidate(Dataset, Method, Series, Options, LambdaAutomatic, Frequency, Term),
		evaluate_frequency_shortlist(Frequencies, Dataset, Method, Series, Options, LambdaAutomatic, Terms).

	evaluate_frequency_candidate(Dataset, Method, Series, Options, LambdaAutomatic, Frequency, candidate(Frequency, Status, Score)) :-
		replace_option(frequency, frequency(Frequency), Options, FrequencyOptions),
		catch(
			(	fit_forecaster_core(Dataset, Method, Series, FrequencyOptions, LambdaAutomatic, FitResult) ->
				FitResult = fit_result(
					_State, _Parameters, SumSquaredError, ErrorCount, _Convergence, _Iterations, _Evaluations,
					_Residuals, ParameterSpecification, _ResultFrequency, _FitFreqDiag
				),
				^^option(initialization(Initialization), Options),
				aicc_score(Method, Frequency, Initialization, SumSquaredError, ErrorCount, ParameterSpecification, LambdaAutomatic, Score0),
				Status = ok,
				Score = Score0
			;	Status = rejected(evaluation_failed),
				Score = none
			),
			Error,
			(	Status = rejected(Error),
				Score = none
			)
		).

	select_best_frequency([], none, no).
	select_best_frequency([First| Rest], SelectedFrequency, Found) :-
		select_best_frequency_(Rest, First, Best),
		(	Best = candidate(BestFrequency, ok, _Score) ->
			SelectedFrequency = BestFrequency,
			Found = yes
		;	Found = no,
			SelectedFrequency = none
		).

	select_best_frequency_([], Best, Best).
	select_best_frequency_([Candidate| Candidates], Best0, Best) :-
		(	better_frequency_candidate(Candidate, Best0) ->
			select_best_frequency_(Candidates, Candidate, Best)
		;	select_best_frequency_(Candidates, Best0, Best)
		).

	better_frequency_candidate(candidate(_Frequency, ok, Score), candidate(_BestFrequency, ok, BestScore)) :-
		!,
		Score < BestScore.
	better_frequency_candidate(candidate(_Frequency, ok, _Score), candidate(_BestFrequency, rejected(_Reason), _BestScore)) :-
		!.

	% opt-in model selection: deterministic default candidate order over the seven
	% supported models, seasonal candidates only considered when a usable frequency
	% exists or an explicit candidate list opts in; AICc or holdout-validation
	% scoring, with deterministic first-encountered tie-breaking by candidate order

	select_model_auto(Dataset, RawSeries, Series, Options, LambdaAutomatic, SelectedMethod, SelectionDiagnostic, FitResult) :-
		^^option(selection_criterion(Criterion), Options),
		candidate_methods(Options, Dataset, CandidateMethods),
		(	CandidateMethods == [] ->
			domain_error(no_viable_candidate, Dataset)
		;	true
		),
		evaluate_candidates(CandidateMethods, Dataset, RawSeries, Series, Options, LambdaAutomatic, Criterion, CandidateTerms),
		select_best_candidate(CandidateTerms, BestCandidate),
		(	BestCandidate = candidate(SelectedMethod, ok, _Score, _Parameters) ->
			SelectionDiagnostic = model_selection(criterion(Criterion), candidates(CandidateTerms), selected(SelectedMethod)),
			fit_forecaster_core(Dataset, SelectedMethod, Series, Options, LambdaAutomatic, FitResult)
		;	domain_error(no_viable_candidate, Dataset)
		).

	default_candidate_order([
		simple, holt, holt_damped,
		holt_winters_additive, holt_winters_multiplicative,
		holt_winters_additive_damped, holt_winters_multiplicative_damped
	]).

	candidate_methods(Options, Dataset, CandidateMethods) :-
		^^option(candidate_models(CandidateOption), Options),
		(	CandidateOption == default ->
			default_candidate_order(DefaultOrder),
			(	usable_frequency_exists(Dataset, Options) ->
				CandidateMethods = DefaultOrder
			;	exclude_seasonal_methods(DefaultOrder, CandidateMethods)
			)
		;	CandidateMethods = CandidateOption
		).

	exclude_seasonal_methods([], []).
	exclude_seasonal_methods([Method| Methods], Filtered) :-
		(	seasonal_method(Method) ->
			exclude_seasonal_methods(Methods, Filtered)
		;	Filtered = [Method| Filtered0],
			exclude_seasonal_methods(Methods, Filtered0)
		).

	usable_frequency_exists(Dataset, Options) :-
		^^option(frequency(FrequencyOption), Options),
		(	FrequencyOption == dataset ->
			Dataset::frequency(_)
		;	FrequencyOption == auto ->
			^^option(frequency_candidates(Candidates), Options),
			Candidates \== none
		;	integer(FrequencyOption)
		).

	evaluate_candidates([], _Dataset, _RawSeries, _Series, _Options, _LambdaAutomatic, _Criterion, []).
	evaluate_candidates([Method| Methods], Dataset, RawSeries, Series, Options, LambdaAutomatic, Criterion, [Term| Terms]) :-
		replace_option(model, model(Method), Options, CandidateOptions),
		evaluate_candidate(Dataset, Method, RawSeries, Series, CandidateOptions, LambdaAutomatic, Criterion, Term),
		evaluate_candidates(Methods, Dataset, RawSeries, Series, Options, LambdaAutomatic, Criterion, Terms).

	evaluate_candidate(Dataset, Method, RawSeries, Series, Options, LambdaAutomatic, Criterion, candidate(Method, Status, Score, Parameters)) :-
		catch(
			(	evaluate_candidate_ok(Dataset, Method, RawSeries, Series, Options, LambdaAutomatic, Criterion, Score0, Parameters0) ->
				Status = ok,
				Score = Score0,
				Parameters = Parameters0
			;	Status = rejected(evaluation_failed),
				Score = none,
				Parameters = none
			),
			Error,
			(	candidate_rejection_reason(Error, Reason),
				Status = rejected(Reason),
				Score = none,
				Parameters = none
			)
		).

	candidate_rejection_reason(error(Formal, _Context), Formal) :-
		!.
	candidate_rejection_reason(Error, Error).

	evaluate_candidate_ok(Dataset, Method, _RawSeries, Series, Options, LambdaAutomatic, aicc, Score, Parameters) :-
		!,
		fit_forecaster_core(Dataset, Method, Series, Options, LambdaAutomatic, FitResult),
		FitResult = fit_result(
			_State, Parameters, SumSquaredError, ErrorCount, _Convergence, _Iterations, _Evaluations,
			_Residuals, ParameterSpecification, Frequency, _FreqDiag
		),
		^^option(initialization(Initialization), Options),
		aicc_score(Method, Frequency, Initialization, SumSquaredError, ErrorCount, ParameterSpecification, LambdaAutomatic, Score).
	evaluate_candidate_ok(Dataset, Method, RawSeries, Series, Options, LambdaAutomatic, validation(ValidationSize), Score, Parameters) :-
		validation_score(Dataset, Method, RawSeries, Series, Options, LambdaAutomatic, ValidationSize, Score, Parameters).

	validation_score(Dataset, Method, RawSeries, Series, Options, LambdaAutomatic, ValidationSize, Score, Parameters) :-
		length(Series, Length),
		PrefixLength is Length - ValidationSize,
		(	PrefixLength >= 1 ->
			true
		;	domain_error(validation_size, ValidationSize)
		),
		length(Prefix, PrefixLength),
		append(Prefix, _SuffixTransformed, Series),
		length(RawPrefix, PrefixLength),
		append(RawPrefix, RawSuffix, RawSeries),
		fit_forecaster_core(Dataset, Method, Prefix, Options, LambdaAutomatic, FitResult),
		FitResult = fit_result(
			InnerState, Parameters, SumSquaredError, ErrorCount, _Convergence, _Iterations, _Evaluations,
			_Residuals, _ParameterSpecification, _Frequency, _FreqDiag
		),
		MeanSquaredError is SumSquaredError / ErrorCount,
		^^forecast_smoothing(Method, InnerState, ValidationSize, InnerForecasts),
		^^option(transformation(Transformation), Options),
		^^option(bias_adjustment(BiasAdjustment), Options),
		back_transform_all(Transformation, BiasAdjustment, MeanSquaredError, InnerForecasts, Forecasts),
		sum_squared_errors(Forecasts, RawSuffix, 0.0, 0, Score, Count),
		(	Count > 0 ->
			true
		;	domain_error(insufficient_known_observations, validation)
		).

	back_transform_all(none, _BiasAdjustment, _Variance, Forecasts, Forecasts) :-
		!.
	back_transform_all(Transformation, BiasAdjustment, Variance, InnerForecasts, Forecasts) :-
		back_transform_forecasts(Transformation, BiasAdjustment, Variance, InnerForecasts, Forecasts).

	sum_squared_errors([], [], Sum, Count, Sum, Count).
	sum_squared_errors([_Forecast| Forecasts], [Actual| Actuals], Sum0, Count0, Sum, Count) :-
		var(Actual),
		!,
		sum_squared_errors(Forecasts, Actuals, Sum0, Count0, Sum, Count).
	sum_squared_errors([Forecast| Forecasts], [Actual| Actuals], Sum0, Count0, Sum, Count) :-
		Difference is Forecast - Actual,
		Sum1 is Sum0 + Difference * Difference,
		Count1 is Count0 + 1,
		sum_squared_errors(Forecasts, Actuals, Sum1, Count1, Sum, Count).

	% AICc under a Gaussian one-step-error approximation: k counts every automatic
	% smoothing parameter, every optimized initial-state coordinate, and one for
	% the estimated residual variance; candidates whose effective sample size
	% (n - k - 1) is not positive are rejected outright

	aicc_score(Method, Frequency, Initialization, SumSquaredError, ErrorCount, ParameterSpecification, LambdaAutomatic, Score) :-
		count_automatic_parameters(ParameterSpecification, 0, AutomaticCount),
		count_optimized_initial_parameters(Initialization, Method, Frequency, InitialCount),
		count_automatic_lambda(LambdaAutomatic, LambdaCount),
		K is AutomaticCount + InitialCount + LambdaCount + 1,
		EffectiveDegrees is ErrorCount - K - 1,
		(	EffectiveDegrees =< 0 ->
			domain_error(insufficient_sample_size, ErrorCount)
		;	AdjustedSumSquaredError is max(SumSquaredError, 1.0e-12),
			MeanSquaredError is AdjustedSumSquaredError / ErrorCount,
			Aic is ErrorCount * log(MeanSquaredError) + 2 * K,
			Score is Aic + (2 * K * (K + 1)) / EffectiveDegrees
		).

	count_automatic_lambda(true, 1).
	count_automatic_lambda(false, 0).

	count_automatic_parameters([], Count, Count).
	count_automatic_parameters([Option| Options], Count0, Count) :-
		Option =.. [_Name, Value],
		(	Value == auto ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		count_automatic_parameters(Options, Count1, Count).

	count_optimized_initial_parameters(optimized, simple, none, 1) :-
		!.
	count_optimized_initial_parameters(optimized, Method, none, 2) :-
		(Method == holt; Method == holt_damped),
		!.
	count_optimized_initial_parameters(optimized, _Method, Frequency, Count) :-
		integer(Frequency),
		!,
		Count is Frequency + 1.
	count_optimized_initial_parameters(_Initialization, _Method, _Frequency, 0).

	select_best_candidate([First| Rest], Best) :-
		select_best_candidate_(Rest, First, Best).

	select_best_candidate_([], Best, Best).
	select_best_candidate_([Candidate| Candidates], Best0, Best) :-
		(	better_candidate(Candidate, Best0) ->
			select_best_candidate_(Candidates, Candidate, Best)
		;	select_best_candidate_(Candidates, Best0, Best)
		).

	better_candidate(candidate(_Method, ok, Score, _Parameters), candidate(_BestMethod, ok, BestScore, _BestParameters)) :-
		!,
		Score < BestScore.
	better_candidate(candidate(_Method, ok, _Score, _Parameters), candidate(_BestMethod, rejected(_Reason), _BestScore, _BestParameters)) :-
		!.

	% generic single-option substitution shared by candidate construction

	replace_option(Name, Replacement, [Option| Options], [Replacement| Options]) :-
		functor(Option, Name, 1),
		!.
	replace_option(Name, Replacement, [Option| Options], [Option| NewOptions]) :-
		!,
		replace_option(Name, Replacement, Options, NewOptions).
	replace_option(_Name, Replacement, [], [Replacement]).

	fit_parameters(Method, Series, Frequency, ParameterSpecification, InitializationSpecification, Optimizer, OptimizerOptions, DeOptions, Parameters, State, SumSquaredError, ErrorCount, Convergence, Iterations, Evaluations, Residuals) :-
		^^observed_series(Series, OptimizationSeries),
		^^optimization_initial_point(Method, OptimizationSeries, Frequency, ParameterSpecification, InitializationSpecification, InitialPoint),
		(	InitialPoint == [] ->
			^^parameters_from_point(ParameterSpecification, [], Parameters),
			EffectiveInitializationSpecification = InitializationSpecification,
			Convergence = fixed_parameters,
			Iterations = 0,
			Evaluations = 0
		;	Problem = exponential_smoothing_problem(Method, Series, OptimizationSeries, Frequency, ParameterSpecification, InitializationSpecification),
			run_optimizer(Optimizer, Problem, OptimizerOptions, DeOptions, Point, Iterations, Evaluations, Convergence),
			^^optimization_components(Method, OptimizationSeries, Frequency, ParameterSpecification, InitializationSpecification, Point, Parameters, EffectiveInitializationSpecification)
		),
		^^fit_smoothing(Method, Series, Frequency, Parameters, EffectiveInitializationSpecification, State, SumSquaredError, ErrorCount, Residuals).

	% optimizer strategy dispatch

	run_optimizer(nelder_mead, Problem, OptimizerOptions, _DeOptions, Point, Iterations, Evaluations, Convergence) :-
		!,
		SolverOptions = [objective(minimize), updates(0)| OptimizerOptions],
		nelder_mead(Problem)::run(Point, _BestValue, Statistics, SolverOptions),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics),
		optimizer_convergence(OptimizerOptions, Iterations, Convergence).
	run_optimizer(multi_start(Starts), Problem, OptimizerOptions, _DeOptions, Point, Iterations, Evaluations, Convergence) :-
		!,
		multi_start_points(Starts, Problem, StartPoints),
		run_multi_start(StartPoints, Problem, OptimizerOptions, Point, WinnerIterations, Iterations, Evaluations),
		optimizer_convergence(OptimizerOptions, WinnerIterations, Convergence).
	run_optimizer(differential_evolution, Problem, OptimizerOptions, DeOptions, Point, Iterations, Evaluations, Convergence) :-
		run_differential_evolution(Problem, OptimizerOptions, DeOptions, Point, Iterations, Evaluations, Convergence).

	optimizer_convergence(OptimizerOptions, Iterations, Convergence) :-
		optimizer_maximum_iterations(OptimizerOptions, MaximumIterations),
		(	Iterations >= MaximumIterations ->
			Convergence = maximum_iterations
		;	Convergence = converged
		).

	optimizer_maximum_iterations(OptimizerOptions, MaximumIterations) :-
		(	member(max_iterations(MaximumIterations), OptimizerOptions) ->
			true
		;	MaximumIterations = 1000
		).

	% deterministic multi-start bounded Nelder-Mead

	multi_start_points(Starts, Problem, Points) :-
		Problem::initial_point(BasePoint),
		Problem::position_bounds(Bounds),
		AdditionalStarts is Starts - 1,
		halton_points(AdditionalStarts, Bounds, HaltonPoints),
		Points = [BasePoint| HaltonPoints].

	run_multi_start([FirstPoint| RestPoints], Problem, OptimizerOptions, BestPoint, BestIterations, TotalIterations, TotalEvaluations) :-
		run_start(FirstPoint, Problem, OptimizerOptions, Point0, Value0, Iterations0, Evaluations0),
		fold_multi_start(RestPoints, Problem, OptimizerOptions, Point0, Value0, Iterations0, Iterations0, Evaluations0, BestPoint, BestIterations, TotalIterations, TotalEvaluations).

	fold_multi_start([], _Problem, _OptimizerOptions, BestPoint, _BestValue, BestIterations, TotalIterations, TotalEvaluations, BestPoint, BestIterations, TotalIterations, TotalEvaluations) :-
		!.
	fold_multi_start([StartPoint| StartPoints], Problem, OptimizerOptions, BestPoint0, BestValue0, BestIterations0, TotalIterations0, TotalEvaluations0, BestPoint, BestIterations, TotalIterations, TotalEvaluations) :-
		run_start(StartPoint, Problem, OptimizerOptions, Point, Value, Iterations, Evaluations),
		(	better_start(Value, BestValue0, Point, BestPoint0) ->
			BestPoint1 = Point,
			BestValue1 = Value,
			BestIterations1 = Iterations
		;	BestPoint1 = BestPoint0,
			BestValue1 = BestValue0,
			BestIterations1 = BestIterations0
		),
		TotalIterations1 is TotalIterations0 + Iterations,
		TotalEvaluations1 is TotalEvaluations0 + Evaluations,
		fold_multi_start(StartPoints, Problem, OptimizerOptions, BestPoint1, BestValue1, BestIterations1, TotalIterations1, TotalEvaluations1, BestPoint, BestIterations, TotalIterations, TotalEvaluations).

	better_start(CandidateValue, BestValue, CandidatePoint, BestPoint) :-
		(	CandidateValue < BestValue ->
			true
		;	CandidateValue =:= BestValue,
			CandidatePoint @< BestPoint
		).

	run_start(StartPoint, Problem, OptimizerOptions, Point, Value, Iterations, Evaluations) :-
		SolverOptions = [objective(minimize), updates(0), initial_point(StartPoint)| OptimizerOptions],
		nelder_mead(Problem)::run(Point, Value, Statistics, SolverOptions),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics).

	% deterministic low-discrepancy (Halton sequence) start points inside the search box

	halton_points(N, Bounds, Points) :-
		primes_for_dimension(Bounds, Primes),
		halton_points(1, N, Primes, Bounds, Points).

	halton_points(Index, N, _Primes, _Bounds, []) :-
		Index > N,
		!.
	halton_points(Index, N, Primes, Bounds, [Point| Points]) :-
		halton_point(Primes, Index, Bounds, Point),
		Index1 is Index + 1,
		halton_points(Index1, N, Primes, Bounds, Points).

	halton_point([], _Index, [], []).
	halton_point([Base| Bases], Index, [Lower-Upper| Bounds], [Value| Values]) :-
		halton_fraction(Index, Base, Fraction),
		Value is Lower + Fraction * (Upper - Lower),
		halton_point(Bases, Index, Bounds, Values).

	halton_fraction(Index, Base, Fraction) :-
		halton_fraction(Index, Base, 1.0, 0.0, Fraction).

	halton_fraction(0, _Base, _Weight, Fraction, Fraction) :-
		!.
	halton_fraction(Index, Base, Weight0, Fraction0, Fraction) :-
		Weight is Weight0 / Base,
		Digit is Index mod Base,
		Fraction1 is Fraction0 + Digit * Weight,
		Index1 is Index // Base,
		halton_fraction(Index1, Base, Weight, Fraction1, Fraction).

	available_primes([2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131]).

	primes_for_dimension(Bounds, Primes) :-
		length(Bounds, Dimension),
		available_primes(AllPrimes),
		(	length(Primes, Dimension),
			append(Primes, _, AllPrimes) ->
			true
		;	domain_error(multi_start_dimension, Dimension)
		).

	% differential evolution with optional Nelder-Mead polishing

	run_differential_evolution(Problem, OptimizerOptions, DeOptions, Point, Iterations, Evaluations, Convergence) :-
		de_polish(DeOptions, Polish),
		de_run_options(DeOptions, DeRunOptions0),
		DeRunOptions = [objective(minimize), updates(0)| DeRunOptions0],
		run_de_with_isolated_seed(differential_evolution(Problem)::run(DePoint, _DeValue, DeStatistics, DeRunOptions)),
		memberchk(generations(DeGenerations), DeStatistics),
		memberchk(evaluations(DeEvaluations), DeStatistics),
		de_convergence(DeRunOptions0, DeGenerations, Convergence),
		(	Polish == true ->
			polish_with_nelder_mead(Problem, OptimizerOptions, DePoint, Point, PolishIterations, PolishEvaluations)
		;	Point = DePoint,
			PolishIterations = 0,
			PolishEvaluations = 0
		),
		Iterations is DeGenerations + PolishIterations,
		Evaluations is DeEvaluations + PolishEvaluations.

	polish_with_nelder_mead(Problem, OptimizerOptions, StartPoint, Point, Iterations, Evaluations) :-
		SolverOptions = [objective(minimize), updates(0), initial_point(StartPoint)| OptimizerOptions],
		nelder_mead(Problem)::run(Point, _Value, Statistics, SolverOptions),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics).

	de_convergence(DeRunOptions, Generations, Convergence) :-
		(	member(max_generations(MaximumGenerations), DeRunOptions) ->
			true
		;	MaximumGenerations = 100
		),
		(	Generations >= MaximumGenerations ->
			Convergence = maximum_iterations
		;	Convergence = converged
		).

	de_polish(DeOptions, Polish) :-
		(	member(polish(Polish), DeOptions) ->
			true
		;	Polish = true
		).

	de_run_options(DeOptions, RunOptions) :-
		exclude_de_option(polish, DeOptions, WithoutPolish),
		(	member(seed(_), WithoutPolish) ->
			RunOptions = WithoutPolish
		;	RunOptions = [seed(42)| WithoutPolish]
		).

	exclude_de_option(_Name, [], []) :-
		!.
	exclude_de_option(Name, [Option| Options], Result) :-
		functor(Option, Name, _),
		!,
		exclude_de_option(Name, Options, Result).
	exclude_de_option(Name, [Option| Options], [Option| Result]) :-
		exclude_de_option(Name, Options, Result).

	:- meta_predicate(run_de_with_isolated_seed(0)).

	run_de_with_isolated_seed(Goal) :-
		fast_random(xoshiro128pp)::get_seed(SavedSeed),
		catch(once(Goal), Error, true),
		fast_random(xoshiro128pp)::set_seed(SavedSeed),
		(	var(Error) ->
			true
		;	throw(Error)
		).

	build_diagnostics(
		Method, Frequency, Optimizer, Parameters, SumSquaredError, MeanSquaredError, Convergence, Iterations,
		Evaluations, TrainingSeriesLength, ObservedCount, MissingCount, ScoredCount, Options, ResidualsDiagnostic,
		SelectionDiagnostic, FreqDiag, Diagnostics
	) :-
		method_diagnostics(Frequency, MethodDiagnostics),
		append(MethodDiagnostics, [
			parameters(Parameters),
			sum_squared_error(SumSquaredError),
			mean_squared_error(MeanSquaredError),
			optimizer(Optimizer),
			convergence(Convergence),
			iterations(Iterations),
			evaluations(Evaluations),
			observed_count(ObservedCount),
			missing_count(MissingCount),
			scored_count(ScoredCount),
			update_count(0),
			residuals(ResidualsDiagnostic)
		], BaseExtraDiagnostics),
		selection_diagnostics_terms(SelectionDiagnostic, SelectionTerms),
		frequency_diagnostics_terms(FreqDiag, FreqDiagTerms),
		append(SelectionTerms, FreqDiagTerms, TrailingTerms),
		append(BaseExtraDiagnostics, TrailingTerms, ExtraDiagnostics),
		^^base_forecaster_diagnostics(exponential_smoothing, TrainingSeriesLength, Options, [method(Method)| ExtraDiagnostics], Diagnostics).

	selection_diagnostics_terms(none, []) :-
		!.
	selection_diagnostics_terms(SelectionDiagnostic, [SelectionDiagnostic]).

	frequency_diagnostics_terms(none, []) :-
		!.
	frequency_diagnostics_terms(FreqDiag, [FreqDiag]).

	method_diagnostics(none, []) :-
		!.
	method_diagnostics(Frequency, [frequency(Frequency)]) :-
		integer(Frequency).

	check_relevant_parameter_options(simple, UserOptions) :-
		!,
		check_irrelevant_parameter(beta, UserOptions),
		check_irrelevant_parameter(gamma, UserOptions),
		check_irrelevant_parameter(phi, UserOptions).
	check_relevant_parameter_options(holt, UserOptions) :-
		!,
		check_irrelevant_parameter(gamma, UserOptions),
		check_irrelevant_parameter(phi, UserOptions).
	check_relevant_parameter_options(holt_damped, UserOptions) :-
		!,
		check_irrelevant_parameter(gamma, UserOptions).
	check_relevant_parameter_options(auto, UserOptions) :-
		!,
		check_irrelevant_parameter(alpha, UserOptions),
		check_irrelevant_parameter(beta, UserOptions),
		check_irrelevant_parameter(gamma, UserOptions),
		check_irrelevant_parameter(phi, UserOptions).
	check_relevant_parameter_options(Method, UserOptions) :-
		seasonal_method(Method),
		(	damped_seasonal_method(Method) ->
			true
		;	check_irrelevant_parameter(phi, UserOptions)
		).

	check_irrelevant_parameter(Name, UserOptions) :-
		Option =.. [Name, Value],
		(	member(Option, UserOptions), number(Value) ->
			domain_error(exponential_smoothing_parameter, Option)
		;	true
		).

	check_positive_series([], _MissingPolicy).
	check_positive_series([Value| Values], skip_update) :-
		var(Value),
		!,
		check_positive_series(Values, skip_update).
	check_positive_series([Value| Values], MissingPolicy) :-
		(	Value > 0 ->
			check_positive_series(Values, MissingPolicy)
		;	domain_error(positive_multiplicative_series, Value)
		).

	check_finite_series([]).
	check_finite_series([Value| Values]) :-
		(	^^finite_number(Value) ->
			check_finite_series(Values)
		;	domain_error(finite_number, Value)
		).

	check_observations(Series, error, ObservedCount, 0) :-
		check_finite_series(Series),
		length(Series, ObservedCount).
	check_observations(Series, skip_update, ObservedCount, MissingCount) :-
		check_observations_(Series, 0, 0, ObservedCount, MissingCount).

	check_observations_([], ObservedCount, MissingCount, ObservedCount, MissingCount).
	check_observations_([Value| Values], ObservedCount0, MissingCount0, ObservedCount, MissingCount) :-
		^^check_observation(Value),
		(	var(Value) ->
			ObservedCount1 = ObservedCount0,
			MissingCount1 is MissingCount0 + 1
		;	^^finite_number(Value) ->
			ObservedCount1 is ObservedCount0 + 1,
			MissingCount1 = MissingCount0
		;	domain_error(finite_number, Value)
		),
		check_observations_(Values, ObservedCount1, MissingCount1, ObservedCount, MissingCount).

	check_series_policy(Dataset, Series, error) :-
		^^check_series(Dataset, Series).
	check_series_policy(_Dataset, _Series, skip_update).

	% transformations

	check_relevant_transformation_options(Transformation, BiasAdjustment, UserOptions) :-
		(	bias_adjustment_consistent(Transformation, BiasAdjustment) ->
			check_relevant_box_cox_bounds(Transformation, UserOptions)
		;	domain_error(exponential_smoothing_parameter, bias_adjustment(BiasAdjustment))
		).

	check_relevant_box_cox_bounds(box_cox(auto), _UserOptions) :-
		!.
	check_relevant_box_cox_bounds(_Transformation, UserOptions) :-
		(	member(box_cox_bounds(Lower, Upper), UserOptions) ->
			domain_error(exponential_smoothing_parameter, box_cox_bounds(Lower, Upper))
		;	true
		).

	bias_adjustment_consistent(none, none).
	bias_adjustment_consistent(log, _BiasAdjustment).
	bias_adjustment_consistent(box_cox(_Lambda), _BiasAdjustment).

	prepare_series(none, Series, _MissingPolicy, Series) :-
		!.
	prepare_series(Transformation, Series, MissingPolicy, TransformedSeries) :-
		check_positive_transformation_series(Series, MissingPolicy),
		transform_series(Transformation, Series, MissingPolicy, TransformedSeries).

	check_positive_transformation_series([], _MissingPolicy).
	check_positive_transformation_series([Value| Values], skip_update) :-
		var(Value),
		!,
		check_positive_transformation_series(Values, skip_update).
	check_positive_transformation_series([Value| Values], MissingPolicy) :-
		(	Value > 0 ->
			check_positive_transformation_series(Values, MissingPolicy)
		;	domain_error(positive_transformation_series, Value)
		).

	transform_series(_Transformation, [], _MissingPolicy, []).
	transform_series(Transformation, [Value| Values], skip_update, [Value| Transformeds]) :-
		var(Value),
		!,
		transform_series(Transformation, Values, skip_update, Transformeds).
	transform_series(Transformation, [Value| Values], MissingPolicy, [Transformed| Transformeds]) :-
		apply_transform(Transformation, Value, Transformed),
		transform_series(Transformation, Values, MissingPolicy, Transformeds).

	apply_transform(log, Value, Transformed) :-
		!,
		Transformed is log(Value).
	apply_transform(box_cox(Lambda), Value, Transformed) :-
		(	near_zero_lambda(Lambda) ->
			Transformed is log(Value)
		;	Transformed is (Value ** Lambda - 1.0) / Lambda
		).

	near_zero_lambda(Lambda) :-
		abs(Lambda) =< 1.0e-6.

	wrap_state(none, InnerState, _Variance, InnerState) :-
		!.
	wrap_state(Transformation, InnerState, Variance, transformed(Transformation, InnerState, residual_variance(Variance))).

	back_transform_forecasts(_Transformation, _BiasAdjustment, _Variance, [], []) :-
		!.
	back_transform_forecasts(Transformation, BiasAdjustment, Variance, [InnerForecast| InnerForecasts], [Forecast| Forecasts]) :-
		back_transform_forecast(Transformation, BiasAdjustment, Variance, InnerForecast, Forecast),
		back_transform_forecasts(Transformation, BiasAdjustment, Variance, InnerForecasts, Forecasts).

	back_transform_forecast(log, none, _Variance, Value, Forecast) :-
		!,
		Forecast is exp(Value).
	back_transform_forecast(log, delta, Variance, Value, Forecast) :-
		!,
		Forecast is exp(Value) * exp(Variance / 2.0).
	back_transform_forecast(box_cox(Lambda), none, _Variance, Value, Forecast) :-
		!,
		box_cox_inverse(Lambda, Value, Forecast).
	back_transform_forecast(box_cox(Lambda), delta, Variance, Value, Forecast) :-
		(	near_zero_lambda(Lambda) ->
			Forecast is exp(Value) * exp(Variance / 2.0)
		;	Base is Lambda * Value + 1.0,
			(	Base > 0.0 ->
				BaseForecast is Base ** (1.0 / Lambda),
				Correction is 1.0 + Variance * (1.0 - Lambda) / (2.0 * Base * Base),
				Forecast is BaseForecast * Correction
			;	domain_error(box_cox_inverse_domain, Base)
			)
		).

	box_cox_inverse(Lambda, Value, Forecast) :-
		(	near_zero_lambda(Lambda) ->
			Forecast is exp(Value)
		;	Base is Lambda * Value + 1.0,
			(	Base > 0.0 ->
				Forecast is Base ** (1.0 / Lambda)
			;	domain_error(box_cox_inverse_domain, Base)
			)
		).

	valid_method(simple).
	valid_method(holt).
	valid_method(holt_damped).
	valid_method(holt_winters_additive).
	valid_method(holt_winters_multiplicative).
	valid_method(holt_winters_additive_damped).
	valid_method(holt_winters_multiplicative_damped).

	seasonal_method(holt_winters_additive).
	seasonal_method(holt_winters_multiplicative).
	seasonal_method(holt_winters_additive_damped).
	seasonal_method(holt_winters_multiplicative_damped).

	damped_seasonal_method(holt_winters_additive_damped).
	damped_seasonal_method(holt_winters_multiplicative_damped).

	multiplicative_method(holt_winters_multiplicative).
	multiplicative_method(holt_winters_multiplicative_damped).

	valid_model_state(simple, level(Level)) :-
		^^finite_number(Level).
	valid_model_state(Method, transformed(Transformation, InnerState, residual_variance(Variance))) :-
		!,
		valid_transformation_term(Transformation),
		^^finite_number(Variance),
		Variance >= 0.0,
		valid_model_state(Method, InnerState).
	valid_model_state(holt, holt(Level, Trend)) :-
		^^finite_number(Level),
		^^finite_number(Trend).
	valid_model_state(holt_damped, holt_damped(Level, Trend, Phi)) :-
		^^finite_number(Level),
		^^finite_number(Trend),
		valid_damping_parameter(Phi).
	valid_model_state(Method, holt_winters(Level, Trend, Frequency, SeasonalQueue)) :-
		seasonal_method(Method),
		\+ damped_seasonal_method(Method),
		^^finite_number(Level),
		^^finite_number(Trend),
		valid(positive_integer, Frequency),
		Frequency >= 2,
		valid(list(number), SeasonalQueue),
		length(SeasonalQueue, Frequency),
		finite_values(SeasonalQueue),
		(	multiplicative_method(Method) ->
			positive_values(SeasonalQueue)
		;	true
		).
	valid_model_state(Method, holt_winters_damped(Level, Trend, Phi, Frequency, SeasonalQueue)) :-
		damped_seasonal_method(Method),
		^^finite_number(Level),
		^^finite_number(Trend),
		valid_damping_parameter(Phi),
		valid(positive_integer, Frequency),
		Frequency >= 2,
		valid(list(number), SeasonalQueue),
		length(SeasonalQueue, Frequency),
		finite_values(SeasonalQueue),
		(	multiplicative_method(Method) ->
			positive_values(SeasonalQueue)
		;	true
		).

	valid_parameters(simple, [Alpha]) :-
		valid_smoothing_parameter(Alpha).
	valid_parameters(holt, [Alpha, Beta]) :-
		valid_smoothing_parameter(Alpha),
		valid_smoothing_parameter(Beta).
	valid_parameters(holt_damped, [Alpha, Beta, Phi]) :-
		valid_smoothing_parameter(Alpha),
		valid_smoothing_parameter(Beta),
		valid_damping_parameter(Phi).
	valid_parameters(Method, [Alpha, Beta, Gamma]) :-
		seasonal_method(Method),
		\+ damped_seasonal_method(Method),
		valid_smoothing_parameter(Alpha),
		valid_smoothing_parameter(Beta),
		valid_smoothing_parameter(Gamma).
	valid_parameters(Method, [Alpha, Beta, Gamma, Phi]) :-
		damped_seasonal_method(Method),
		valid_smoothing_parameter(Alpha),
		valid_smoothing_parameter(Beta),
		valid_smoothing_parameter(Gamma),
		valid_damping_parameter(Phi).

	valid_smoothing_parameter(Value) :-
		^^finite_number(Value),
		Value >= 0.0,
		Value =< 1.0.

	valid_damping_parameter(Value) :-
		^^finite_number(Value),
		Value > 0.0,
		Value =< 1.0.

	valid_transformation_term(log).
	valid_transformation_term(box_cox(Lambda)) :-
		^^finite_number(Lambda).

	finite_values([]).
	finite_values([Value| Values]) :-
		^^finite_number(Value),
		finite_values(Values).

	positive_values([]).
	positive_values([Value| Values]) :-
		Value > 0.0,
		positive_values(Values).

	default_option(model(simple)).
	default_option(alpha(auto)).
	default_option(beta(auto)).
	default_option(gamma(auto)).
	default_option(phi(auto)).
	default_option(initialization(two_cycles)).
	default_option(initial_cycles(2)).
	default_option(optimizer(nelder_mead)).
	default_option(optimizer_options([])).
	default_option(de_options([])).
	default_option(transformation(none)).
	default_option(box_cox_bounds(-1.0, 2.0)).
	default_option(bias_adjustment(none)).
	default_option(retain_residuals(false)).
	default_option(selection_criterion(aicc)).
	default_option(candidate_models(default)).
	default_option(frequency(dataset)).
	default_option(frequency_candidates(none)).
	default_option(missing_policy(error)).

	valid_option(model(Method)) :-
		(	Method == auto ->
			true
		;	atom(Method),
			valid_method(Method)
		).
	valid_option(alpha(Value)) :-
		valid_parameter_option(Value).
	valid_option(beta(Value)) :-
		valid_parameter_option(Value).
	valid_option(gamma(Value)) :-
		valid_parameter_option(Value).
	valid_option(phi(Phi)) :-
		(	Phi == auto ->
			true
		;	valid_damping_parameter(Phi)
		).
	valid_option(initialization(Initialization)) :-
		once((Initialization == two_cycles; Initialization == regression; Initialization == optimized)).
	valid_option(initial_cycles(InitialCycles)) :-
		valid(positive_integer, InitialCycles),
		InitialCycles >= 2.
	valid_option(optimizer(Optimizer)) :-
		(	Optimizer == nelder_mead ->
			true
		;	Optimizer == differential_evolution ->
			true
		;	Optimizer = multi_start(Starts),
			valid(positive_integer, Starts)
		).
	valid_option(optimizer_options(Options)) :-
		valid(list(compound), Options),
		valid_optimizer_options(Options).
	valid_option(de_options(Options)) :-
		valid(list(compound), Options),
		valid_de_options(Options).
	valid_option(transformation(Transformation)) :-
		(	Transformation == none ->
			true
		;	Transformation == log ->
			true
		;	Transformation == box_cox(auto) ->
			true
		;	Transformation = box_cox(Lambda),
			^^finite_number(Lambda)
		).
	valid_option(box_cox_bounds(Lower, Upper)) :-
		^^finite_number(Lower),
		^^finite_number(Upper),
		Lower < Upper.
	valid_option(bias_adjustment(BiasAdjustment)) :-
		once((BiasAdjustment == none; BiasAdjustment == delta)).
	valid_option(retain_residuals(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(selection_criterion(SelectionCriterion)) :-
		(	SelectionCriterion == aicc ->
			true
		;	SelectionCriterion = validation(ValidationSize),
			valid(positive_integer, ValidationSize)
		).
	valid_option(candidate_models(Methods)) :-
		(	Methods == default ->
			true
		;	valid(non_empty_list(atom), Methods),
			valid_candidate_model_list(Methods)
		).
	valid_option(frequency(Frequency)) :-
		(	Frequency == dataset ->
			true
		;	Frequency == auto ->
			true
		;	integer(Frequency),
			Frequency >= 2
		).
	valid_option(frequency_candidates(FrequencyCandidates)) :-
		(	FrequencyCandidates == none ->
			true
		;	FrequencyCandidates = Min-Max,
			integer(Min),
			integer(Max),
			Min >= 2,
			Max >= Min
		).
	valid_option(frequency_candidates(Frequencies)) :-
		valid(non_empty_list(integer), Frequencies),
		valid_frequency_candidate_option_list(Frequencies).
	valid_option(missing_policy(Policy)) :-
		once((Policy == error; Policy == skip_update)).

	valid_candidate_model_list([]).
	valid_candidate_model_list([Method| Methods]) :-
		valid_method(Method),
		valid_candidate_model_list(Methods).

	valid_frequency_candidate_option_list([]).
	valid_frequency_candidate_option_list([Frequency| Frequencies]) :-
		Frequency >= 2,
		valid_frequency_candidate_option_list(Frequencies).

	valid_parameter_option(auto).
	valid_parameter_option(Value) :-
		valid_smoothing_parameter(Value).

	valid_optimizer_options([]).
	valid_optimizer_options([Option| Options]) :-
		valid_optimizer_option(Option),
		valid_optimizer_options(Options).

	valid_optimizer_option(max_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_optimizer_option(tol_x(Tolerance)) :-
		^^finite_number(Tolerance),
		Tolerance >= 0.0.
	valid_optimizer_option(tol_f(Tolerance)) :-
		^^finite_number(Tolerance),
		Tolerance >= 0.0.
	valid_optimizer_option(initial_step(Step)) :-
		^^finite_number(Step),
		Step > 0.0.
	valid_optimizer_option(adaptive(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	valid_de_options([]).
	valid_de_options([Option| Options]) :-
		valid_de_option(Option),
		valid_de_options(Options).

	valid_de_option(seed(Seed)) :-
		valid(positive_integer, Seed).
	valid_de_option(population_size(Size)) :-
		integer(Size),
		Size >= 4.
	valid_de_option(max_generations(Generations)) :-
		valid(positive_integer, Generations).
	valid_de_option(crossover_probability(Probability)) :-
		^^finite_number(Probability),
		Probability >= 0.0,
		Probability =< 1.0.
	valid_de_option(differential_weight(Weight)) :-
		^^finite_number(Weight),
		Weight > 0.0.
	valid_de_option(strategy(Strategy)) :-
		valid_de_strategy(Strategy).
	valid_de_option(polish(Boolean)) :-
		once((Boolean == true; Boolean == false)).

	valid_de_strategy(rand/1/bin).
	valid_de_strategy(rand/1/exp).
	valid_de_strategy(best/1/bin).
	valid_de_strategy(current-to-best/1/bin).

:- end_object.

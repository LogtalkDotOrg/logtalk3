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


:- object(time_series_regression,
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-28,
		comment is 'Autoregressive (AR) time series forecaster fitted by least squares, with optional intercept, optional differencing (ARI models), and automatic order selection using information criteria.',
		see_also is [forecaster_protocol, time_series_dataset_protocol]
	]).

	:- public(update/4).
	:- mode(update(+compound, +number, -compound, +list(compound)), one_or_error).
	:- info(update/4, [
		comment is 'Returns a new forecaster after appending one observation to the series, keeping the fitted coefficients unchanged. The original forecaster is unchanged. The one-step prediction error of the new observation is added to the training error diagnostics (and to the retained residuals when residual retention was enabled during learning); the information criteria diagnostics keep describing the original fit. No update options are currently defined.',
		argnames is ['Forecaster', 'Observation', 'UpdatedForecaster', 'Options'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is neither a variable nor a valid forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Observation`` is a variable' - instantiation_error,
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An option is a variable' - instantiation_error,
			'An option is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An option is a compound term but is not a valid update option' - domain_error(option, 'Option')
		]
	]).

	:- public(update/3).
	:- mode(update(+compound, +number, -compound), one_or_error).
	:- info(update/3, [
		comment is 'Returns a new forecaster after appending one observation using default update options.',
		argnames is ['Forecaster', 'Observation', 'UpdatedForecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is neither a variable nor a valid forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Observation`` is a variable' - instantiation_error,
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation')
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, last/2, length/2, memberchk/2, reverse/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(integer, [
		sequence/3
	]).

	:- uses(linear_algebra, [
		least_squares/3, matrix_rank/2
	]).

	% learning

	learn(Dataset, Forecaster, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(order(OrderOption), Options),
		^^option(max_order(MaxOrder), Options),
		^^option(selection_criterion(Criterion), Options),
		^^option(intercept(Intercept), Options),
		^^option(differencing(Differencing), Options),
		^^option(retain_residuals(RetainResiduals), Options),
		check_relevant_options(OrderOption, UserOptions),
		^^dataset_series(Dataset, Series),
		^^check_series(Dataset, Series),
		intercept_count(Intercept, InterceptCount),
		minimum_series_length(OrderOption, Differencing, InterceptCount, MinimumLength),
		^^check_series_length(Dataset, Series, MinimumLength),
		length(Series, TrainingSeriesLength),
		difference_levels(Differencing, Series, Levels, DifferencedSeries),
		select_order(OrderOption, MaxOrder, Criterion, Intercept, InterceptCount, DifferencedSeries, Order, SelectionDiagnostic),
		^^lagged_rows(DifferencedSeries, Order, Rows),
		fit_rows(Rows, Intercept, Matrix, InterceptValue, Coefficients, Residuals, SumSquaredError),
		matrix_rank(Matrix, Rank),
		length(Rows, ScoredCount),
		ParameterCount is Order + InterceptCount,
		MeanSquaredError is SumSquaredError / ScoredCount,
		initial_window(DifferencedSeries, Order, Window),
		retained_residuals_diagnostic(RetainResiduals, Residuals, ResidualsDiagnostic),
		criteria_diagnostics(ScoredCount, ParameterCount, SumSquaredError, CriteriaDiagnostics),
		selection_diagnostics_terms(SelectionDiagnostic, SelectionTerms),
		append(CriteriaDiagnostics, [update_count(0), residuals(ResidualsDiagnostic)| SelectionTerms], TrailingDiagnostics),
		^^base_forecaster_diagnostics(
			time_series_regression, TrainingSeriesLength, Options,
			[
				order(Order),
				differencing(Differencing),
				intercept(Intercept),
				parameter_count(ParameterCount),
				scored_count(ScoredCount),
				design_rank(Rank),
				sum_squared_error(SumSquaredError),
				mean_squared_error(MeanSquaredError)
			| TrailingDiagnostics],
			Diagnostics
		),
		Forecaster = time_series_regression_forecaster(
			ar(Order, Differencing),
			ar_state(Window, Levels),
			ar_parameters(InterceptValue, Coefficients),
			Diagnostics
		).

	check_relevant_options(OrderOption, UserOptions) :-
		(	OrderOption == auto ->
			true
		;	check_irrelevant_option(max_order, UserOptions),
			check_irrelevant_option(selection_criterion, UserOptions)
		).

	check_irrelevant_option(Name, UserOptions) :-
		Option =.. [Name, _],
		(	memberchk(Option, UserOptions) ->
			domain_error(time_series_regression_option, Option)
		;	true
		).

	intercept_count(true, 1).
	intercept_count(false, 0).

	minimum_series_length(auto, Differencing, InterceptCount, MinimumLength) :-
		!,
		MinimumLength is Differencing + InterceptCount + 4.
	minimum_series_length(Order, Differencing, InterceptCount, MinimumLength) :-
		MinimumLength is Differencing + 2 * Order + InterceptCount.

	% differencing; the levels list holds the last value of the series at
	% each differencing level, starting with the original series

	difference_levels(0, Series, [], Series) :-
		!.
	difference_levels(Differencing, Series, [Last| Levels], DifferencedSeries) :-
		Differencing > 0,
		last(Series, Last),
		^^difference_series(Series, Series1),
		Differencing1 is Differencing - 1,
		difference_levels(Differencing1, Series1, Levels, DifferencedSeries).

	initial_window(DifferencedSeries, Order, Window) :-
		reverse(DifferencedSeries, Reversed),
		first_values(Order, Reversed, Window).

	% order selection

	select_order(OrderOption, _MaxOrder, _Criterion, _Intercept, _InterceptCount, _DifferencedSeries, OrderOption, none) :-
		integer(OrderOption),
		!.
	select_order(auto, MaxOrder, Criterion, Intercept, InterceptCount, DifferencedSeries, Order, order_selection(Criterion, Candidates)) :-
		length(DifferencedSeries, Length),
		Cap is (Length - InterceptCount - 2) // 2,
		EffectiveMaxOrder is min(MaxOrder, Cap),
		^^lagged_rows(DifferencedSeries, EffectiveMaxOrder, CommonRows),
		length(CommonRows, SampleSize),
		sequence(1, EffectiveMaxOrder, Orders),
		candidate_scores(Orders, CommonRows, Intercept, InterceptCount, Criterion, SampleSize, Candidates),
		best_candidate(Candidates, Order).

	candidate_scores([], _, _, _, _, _, []).
	candidate_scores([Order| Orders], CommonRows, Intercept, InterceptCount, Criterion, SampleSize, [Order-Score| Candidates]) :-
		truncate_rows(CommonRows, Order, Rows),
		fit_rows(Rows, Intercept, _Matrix, _InterceptValue, _Coefficients, _Residuals, SumSquaredError),
		ParameterCount is Order + InterceptCount,
		information_criterion(Criterion, SampleSize, ParameterCount, SumSquaredError, Score),
		candidate_scores(Orders, CommonRows, Intercept, InterceptCount, Criterion, SampleSize, Candidates).

	best_candidate([Order-Score| Candidates], Best) :-
		best_candidate(Candidates, Order, Score, Best).

	best_candidate([], Best, _, Best).
	best_candidate([Order-Score| Candidates], Order0, Score0, Best) :-
		(	Score < Score0 ->
			best_candidate(Candidates, Order, Score, Best)
		;	best_candidate(Candidates, Order0, Score0, Best)
		).

	truncate_rows([], _, []).
	truncate_rows([Lags-Target| Rows], Order, [TruncatedLags-Target| TruncatedRows]) :-
		first_values(Order, Lags, TruncatedLags),
		truncate_rows(Rows, Order, TruncatedRows).

	% least-squares fitting

	fit_rows(Rows, Intercept, Matrix, InterceptValue, Coefficients, Residuals, SumSquaredError) :-
		design_matrix(Rows, Intercept, Matrix, Targets),
		least_squares(Matrix, Targets, Solution),
		split_solution(Intercept, Solution, InterceptValue, Coefficients),
		row_residuals(Rows, InterceptValue, Coefficients, Residuals, 0.0, SumSquaredError).

	design_matrix([], _, [], []).
	design_matrix([Lags-Target| Rows], Intercept, [Row| Matrix], [Target| Targets]) :-
		design_row(Intercept, Lags, Row),
		design_matrix(Rows, Intercept, Matrix, Targets).

	design_row(true, Lags, [1| Lags]).
	design_row(false, Lags, Lags).

	split_solution(true, [InterceptValue| Coefficients], InterceptValue, Coefficients).
	split_solution(false, Coefficients, 0.0, Coefficients).

	row_residuals([], _, _, [], SumSquaredError, SumSquaredError).
	row_residuals([Lags-Target| Rows], InterceptValue, Coefficients, [Residual| Residuals], SumSquaredError0, SumSquaredError) :-
		predict(InterceptValue, Coefficients, Lags, Prediction),
		Residual is Target - Prediction,
		SumSquaredError1 is SumSquaredError0 + Residual * Residual,
		row_residuals(Rows, InterceptValue, Coefficients, Residuals, SumSquaredError1, SumSquaredError).

	predict(InterceptValue, Coefficients, Window, Prediction) :-
		dot_product(Coefficients, Window, 0.0, Sum),
		Prediction is InterceptValue + Sum.

	dot_product([], [], Sum, Sum).
	dot_product([Coefficient| Coefficients], [Value| Values], Sum0, Sum) :-
		Sum1 is Sum0 + Coefficient * Value,
		dot_product(Coefficients, Values, Sum1, Sum).

	% information criteria

	information_criterion(aic, SampleSize, ParameterCount, SumSquaredError, Value) :-
		Variance is max(SumSquaredError / SampleSize, 1.0e-300),
		Value is SampleSize * log(Variance) + 2 * ParameterCount.
	information_criterion(aicc, SampleSize, ParameterCount, SumSquaredError, Value) :-
		SampleSize - ParameterCount - 1 > 0,
		information_criterion(aic, SampleSize, ParameterCount, SumSquaredError, AIC),
		Value is AIC + 2.0 * ParameterCount * (ParameterCount + 1) / (SampleSize - ParameterCount - 1).
	information_criterion(bic, SampleSize, ParameterCount, SumSquaredError, Value) :-
		Variance is max(SumSquaredError / SampleSize, 1.0e-300),
		Value is SampleSize * log(Variance) + ParameterCount * log(SampleSize).

	criteria_diagnostics(SampleSize, ParameterCount, SumSquaredError, [aic(AIC), aicc(AICc), bic(BIC)]) :-
		information_criterion(aic, SampleSize, ParameterCount, SumSquaredError, AIC),
		(	information_criterion(aicc, SampleSize, ParameterCount, SumSquaredError, Value) ->
			AICc = Value
		;	AICc = none
		),
		information_criterion(bic, SampleSize, ParameterCount, SumSquaredError, BIC).

	selection_diagnostics_terms(none, []) :-
		!.
	selection_diagnostics_terms(SelectionDiagnostic, [SelectionDiagnostic]).

	retained_residuals_diagnostic(true, Residuals, Residuals).
	retained_residuals_diagnostic(false, _Residuals, none).

	% forecasting

	forecast(Forecaster, Horizon, Forecasts) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		Forecaster = time_series_regression_forecaster(_Model, ar_state(Window, Levels), ar_parameters(InterceptValue, Coefficients), _Diagnostics),
		project(Horizon, InterceptValue, Coefficients, Window, DifferencedForecasts),
		reverse(Levels, ReversedLevels),
		integrate_levels(ReversedLevels, DifferencedForecasts, Forecasts).

	project(0, _, _, _, []) :-
		!.
	project(Horizon, InterceptValue, Coefficients, Window, [Prediction| Predictions]) :-
		predict(InterceptValue, Coefficients, Window, Prediction),
		push_window(Window, Prediction, NextWindow),
		Horizon1 is Horizon - 1,
		project(Horizon1, InterceptValue, Coefficients, NextWindow, Predictions).

	integrate_levels([], Forecasts, Forecasts).
	integrate_levels([Last| Lasts], Forecasts0, Forecasts) :-
		^^integrate_series(Forecasts0, Last, Forecasts1),
		integrate_levels(Lasts, Forecasts1, Forecasts).

	push_window(Window, Value, [Value| Kept]) :-
		drop_last(Window, Kept).

	drop_last([_], []) :-
		!.
	drop_last([Value| Values], [Value| Kept]) :-
		drop_last(Values, Kept).

	first_values(0, _, []) :-
		!.
	first_values(Count, [Value| Values], [Value| Kept]) :-
		Count1 is Count - 1,
		first_values(Count1, Values, Kept).

	% online updates

	update(Forecaster, Observation, UpdatedForecaster, Options) :-
		check_forecaster(Forecaster),
		check_observation(Observation),
		check_update_options(Options),
		Forecaster = time_series_regression_forecaster(Model, ar_state(Window, Levels), Parameters, Diagnostics),
		Parameters = ar_parameters(InterceptValue, Coefficients),
		update_levels(Levels, Observation, UpdatedLevels, DifferencedObservation),
		predict(InterceptValue, Coefficients, Window, Prediction),
		Residual is DifferencedObservation - Prediction,
		push_window(Window, DifferencedObservation, UpdatedWindow),
		updated_diagnostics(Diagnostics, Residual, UpdatedDiagnostics),
		UpdatedForecaster = time_series_regression_forecaster(Model, ar_state(UpdatedWindow, UpdatedLevels), Parameters, UpdatedDiagnostics).

	update(Forecaster, Observation, UpdatedForecaster) :-
		update(Forecaster, Observation, UpdatedForecaster, []).

	check_observation(Observation) :-
		(	var(Observation) ->
			instantiation_error
		;	number(Observation) ->
			true
		;	type_error(number, Observation)
		).

	check_update_options(Options) :-
		context(Context),
		check(list, Options, Context),
		check_update_options_(Options).

	check_update_options_([]).
	check_update_options_([Option| Options]) :-
		(	var(Option) ->
			instantiation_error
		;	\+ compound(Option) ->
			type_error(compound, Option)
		;	domain_error(option, Option)
		),
		check_update_options_(Options).

	update_levels([], DifferencedObservation, [], DifferencedObservation).
	update_levels([Last| Lasts], Value, [Value| UpdatedLasts], DifferencedObservation) :-
		Difference is Value - Last,
		update_levels(Lasts, Difference, UpdatedLasts, DifferencedObservation).

	updated_diagnostics(Diagnostics, Residual, UpdatedDiagnostics) :-
		memberchk(training_series_length(TrainingSeriesLength0), Diagnostics),
		memberchk(scored_count(ScoredCount0), Diagnostics),
		memberchk(sum_squared_error(SumSquaredError0), Diagnostics),
		memberchk(update_count(UpdateCount0), Diagnostics),
		memberchk(residuals(Residuals0), Diagnostics),
		TrainingSeriesLength is TrainingSeriesLength0 + 1,
		ScoredCount is ScoredCount0 + 1,
		SumSquaredError is SumSquaredError0 + Residual * Residual,
		MeanSquaredError is SumSquaredError / ScoredCount,
		UpdateCount is UpdateCount0 + 1,
		updated_residuals(Residuals0, Residual, Residuals),
		replace_diagnostic(training_series_length, TrainingSeriesLength, Diagnostics, Diagnostics1),
		replace_diagnostic(scored_count, ScoredCount, Diagnostics1, Diagnostics2),
		replace_diagnostic(sum_squared_error, SumSquaredError, Diagnostics2, Diagnostics3),
		replace_diagnostic(mean_squared_error, MeanSquaredError, Diagnostics3, Diagnostics4),
		replace_diagnostic(update_count, UpdateCount, Diagnostics4, Diagnostics5),
		replace_diagnostic(residuals, Residuals, Diagnostics5, UpdatedDiagnostics).

	updated_residuals(none, _Residual, none) :-
		!.
	updated_residuals(Residuals0, Residual, Residuals) :-
		append(Residuals0, [Residual], Residuals).

	replace_diagnostic(Name, Value, [Diagnostic| Diagnostics], [UpdatedDiagnostic| Diagnostics]) :-
		functor(Diagnostic, Name, 1),
		!,
		UpdatedDiagnostic =.. [Name, Value].
	replace_diagnostic(Name, Value, [Diagnostic| Diagnostics], [Diagnostic| UpdatedDiagnostics]) :-
		replace_diagnostic(Name, Value, Diagnostics, UpdatedDiagnostics).

	% forecaster validation, export, and printing

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	(	Forecaster = time_series_regression_forecaster(Model, State, Parameters, Diagnostics),
				valid_model(Model, Order, Differencing),
				valid_parameters(Order, Parameters),
				valid_state(Order, Differencing, State),
				valid_diagnostics(Order, Differencing, Parameters, Diagnostics) ->
				true
			;	domain_error(forecaster, Forecaster)
			)
		).

	valid_model(ar(Order, Differencing), Order, Differencing) :-
		valid(positive_integer, Order),
		valid(non_negative_integer, Differencing).

	valid_parameters(Order, ar_parameters(InterceptValue, Coefficients)) :-
		number(InterceptValue),
		valid(list(number), Coefficients),
		length(Coefficients, Order).

	valid_state(Order, Differencing, ar_state(Window, Levels)) :-
		valid(list(number), Window),
		length(Window, Order),
		valid(list(number), Levels),
		length(Levels, Differencing).

	valid_diagnostics(Order, Differencing, ar_parameters(InterceptValue, _), Diagnostics) :-
		^^valid_forecaster_metadata(time_series_regression, _Options, Diagnostics),
		memberchk(order(Order), Diagnostics),
		memberchk(differencing(Differencing), Diagnostics),
		memberchk(intercept(Intercept), Diagnostics),
		(	Intercept == true ->
			true
		;	Intercept == false,
			InterceptValue =:= 0
		),
		memberchk(training_series_length(TrainingSeriesLength), Diagnostics),
		valid(positive_integer, TrainingSeriesLength),
		memberchk(parameter_count(ParameterCount), Diagnostics),
		valid(positive_integer, ParameterCount),
		memberchk(scored_count(ScoredCount), Diagnostics),
		valid(positive_integer, ScoredCount),
		memberchk(design_rank(Rank), Diagnostics),
		valid(non_negative_integer, Rank),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		number(SumSquaredError),
		SumSquaredError >= 0,
		memberchk(mean_squared_error(MeanSquaredError), Diagnostics),
		number(MeanSquaredError),
		MeanSquaredError >= 0,
		memberchk(aic(AIC), Diagnostics),
		number(AIC),
		memberchk(aicc(AICc), Diagnostics),
		(	AICc == none ->
			true
		;	number(AICc)
		),
		memberchk(bic(BIC), Diagnostics),
		number(BIC),
		memberchk(update_count(UpdateCount), Diagnostics),
		valid(non_negative_integer, UpdateCount),
		memberchk(residuals(Residuals), Diagnostics),
		(	Residuals == none ->
			true
		;	valid(list(number), Residuals),
			length(Residuals, ScoredCount)
		).

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	forecaster_term_template(
		time_series_regression_forecaster(_Model, _State, _Parameters, _Diagnostics),
		time_series_regression_forecaster('Model', 'State', 'Parameters', 'Diagnostics')
	).

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		Clause =.. [Functor, Forecaster].

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		Forecaster = time_series_regression_forecaster(ar(Order, Differencing), State, ar_parameters(InterceptValue, Coefficients), Diagnostics),
		^^print_forecaster_template(Forecaster),
		format('Order: ~w~n', [Order]),
		format('Differencing: ~w~n', [Differencing]),
		format('Intercept: ~w~n', [InterceptValue]),
		format('Coefficients: ~w~n', [Coefficients]),
		format('State: ~w~n', [State]),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		memberchk(mean_squared_error(MeanSquaredError), Diagnostics),
		format('Sum squared error: ~w~n', [SumSquaredError]),
		format('Mean squared error: ~w~n', [MeanSquaredError]).

	% options

	default_option(order(1)).
	default_option(max_order(10)).
	default_option(selection_criterion(aicc)).
	default_option(intercept(true)).
	default_option(differencing(0)).
	default_option(retain_residuals(false)).

	valid_option(order(Order)) :-
		once((Order == auto; integer(Order), Order > 0)).
	valid_option(max_order(MaxOrder)) :-
		integer(MaxOrder),
		MaxOrder > 0.
	valid_option(selection_criterion(Criterion)) :-
		once((Criterion == aic; Criterion == aicc; Criterion == bic)).
	valid_option(intercept(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(differencing(Differencing)) :-
		integer(Differencing),
		Differencing >= 0.
	valid_option(retain_residuals(Boolean)) :-
		once((Boolean == true; Boolean == false)).

:- end_object.

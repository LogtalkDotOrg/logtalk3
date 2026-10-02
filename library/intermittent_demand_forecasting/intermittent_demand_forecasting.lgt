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


:- object(intermittent_demand_forecasting,
	implements(forecaster_protocol),
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Croston, SBA, and TSB intermittent-demand forecasters with causal initialization, configurable missing observations, and immutable online updates.'
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one_or_error).
	:- info(learn/2, [
		comment is 'Learns a forecaster from ``Dataset`` using default options.',
		argnames is ['Dataset', 'Forecaster'],
		exceptions is [
			'The declared series length is a variable' - instantiation_error,
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Learning or error-total arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one_or_error).
	:- info(learn/3, [
		comment is 'Learns a forecaster from ``Dataset`` using the specified ``Options``.',
		argnames is ['Dataset', 'Forecaster', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Learning or error-total arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(fitted_values/2).
	:- mode(fitted_values(+object_identifier, -list), one_or_error).
	:- info(fitted_values/2, [
		comment is 'Returns aligned one-step fitted values for ``Dataset`` using default options.',
		argnames is ['Dataset', 'Values'],
		exceptions is [
			'The declared series length is a variable' - instantiation_error,
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Fitting, error-total, or replay arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(fitted_values/3).
	:- mode(fitted_values(+object_identifier, -list, +list(compound)), one_or_error).
	:- info(fitted_values/3, [
		comment is 'Returns aligned one-step fitted values for ``Dataset`` using the specified ``Options``.',
		argnames is ['Dataset', 'Values', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Fitting, error-total, or replay arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(forecast/3).
	:- mode(forecast(+compound, +non_negative_integer, -list(number)), one_or_error).
	:- info(forecast/3, [
		comment is 'Returns ``Horizon`` constant per-period expected-demand forecasts from ``Forecaster``.',
		argnames is ['Forecaster', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Forecaster`` or ``Horizon`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Horizon`` is not an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is negative' - domain_error(non_negative_integer, 'Horizon'),
			'Forecast or diagnostic validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(update/3).
	:- mode(update(+compound, @term, -compound), one_or_error).
	:- info(update/3, [
		comment is 'Returns a new forecaster incorporating ``Observation`` without modifying ``Forecaster``.',
		argnames is ['Forecaster', 'Observation', 'UpdatedForecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation'),
			'``Observation`` is negative' - domain_error(non_negative_number, 'Observation'),
			'Update, error-total, or diagnostic validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(check_forecaster/1).
	:- mode(check_forecaster(@term), one_or_error).
	:- info(check_forecaster/1, [
		comment is 'Checks whether ``Forecaster`` is a valid model.',
		argnames is ['Forecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'Counter or metric validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +atom, -list(clause)), one_or_error).
	:- info(export_to_clauses/4, [
		comment is 'Exports the validated forecaster as a single fact with the specified functor.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'Clauses'],
		exceptions is [
			'``Forecaster`` or ``Functor`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Functor`` is not an atom' - type_error(atom, 'Functor'),
			'Counter validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +atom, +atom), one_or_error).
	:- info(export_to_file/4, [
		comment is 'Exports a validated forecaster as a single fact with dataset and diagnostics comments. The shared implementation closes the stream on writing exceptions.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'File'],
		exceptions is [
			'``Forecaster``, ``Functor``, ``File``, or the declared dataset length is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Functor`` is not an atom' - type_error(atom, 'Functor'),
			'``File`` is not a valid file specification' - type_error(source_sink, 'File'),
			'The output location does not exist' - existence_error(source_sink, 'File'),
			'The output file cannot be opened' - permission_error(open, source_sink, 'File'),
			'The output stream cannot be written to' - permission_error(output, stream, 'Stream'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The observation indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'Counter validation arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(print_forecaster/1).
	:- mode(print_forecaster(+compound), one_or_error).
	:- info(print_forecaster/1, [
		comment is 'Prints the method, state, and diagnostics of ``Forecaster`` to the current output stream.',
		argnames is ['Forecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid intermittent-demand forecaster' - domain_error(forecaster, 'Forecaster'),
			'Counter validation arithmetic raises an evaluation error' - evaluation_error('Error'),
			'The current stream cannot be written to' - permission_error(output, stream, 'Stream')
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	learn(Dataset, Forecaster) :-
		^^learn(Dataset, Forecaster).

	learn(Dataset, Forecaster, UserOptions) :-
		prepare_learning(Dataset, _Series, Forecaster, UserOptions).

	fitted_values(Dataset, Values) :-
		fitted_values(Dataset, Values, []).

	fitted_values(Dataset, Values, UserOptions) :-
		prepare_learning(Dataset, Series, Forecaster, UserOptions),
		Forecaster = intermittent_demand_forecaster(Method, _, Diagnostics),
		memberchk(options([model(Method),alpha(Alpha),beta(Beta),missing(Policy)]), Diagnostics),
		replay_series(Series, Method, parameters(Alpha, Beta, Policy), pending_state(0), Values).

	:- private(replay_series/5).
	:- mode(replay_series(@list, +atom, +compound, +compound, -list), one_or_error).
	:- info(replay_series/5, [
		comment is 'Replays an already validated series using a concrete method, validated parameters, and valid starting state. Emits pre-observation predictions for numeric targets and fresh placeholders for missing targets, preserving order and all input variables.',
		argnames is ['Series', 'Method', 'Parameters', 'State', 'Values'],
		exceptions is [
			'Prediction or transition arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	replay_series([], _Method, _Parameters, _State, []).
	replay_series([Observation| Observations], Method, Parameters, State0, [Prediction| Predictions]) :-
		Parameters = parameters(_, Beta, _),
		(	var(Observation) ->
			true
		;	forecast_value(Method, State0, Beta, Prediction)
		),
		transition(Method, Parameters, State0, Observation, State1),
		replay_series(Observations, Method, Parameters, State1, Predictions).

	:- private(prepare_learning/4).
	:- mode(prepare_learning(+object_identifier, -list, -compound, +list(compound)), one_or_error).
	:- info(prepare_learning/4, [
		comment is 'Resolves numeric or auto method/coefficient requests, collects the dataset once, and fits the lowest-RMSE concrete configuration, returning the collected series for optional replay. Normalizes the validated coefficient_grid once when a coefficient is auto, default [0.1,0.2,0.5,0.8,1.0], to ascending distinct floats; ties follow method priority and ascending alpha/beta. Numeric requests remain unchanged. Never stores the grid in effective options. Preserves caller grid and missing observation variables.',
		argnames is ['Dataset', 'Series', 'Forecaster', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'The declared series length is not an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Learning or error-total arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	prepare_learning(Dataset, Series, Forecaster, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, MergedOptions),
		^^option(model(Method), MergedOptions),
		^^option(alpha(Alpha), MergedOptions),
		^^option(beta(Beta), MergedOptions),
		^^option(missing(Policy), MergedOptions),
		^^dataset_series(Dataset, Series),
		(	Method \== auto, Alpha \== auto, Beta \== auto ->
			learn_series(Dataset, Series, Method, parameters(Alpha, Beta, Policy), Forecaster)
		;	(	Method == auto ->
				Methods = [sba,croston,tsb]
			;	Methods = [Method]
			),
			(	(Alpha == auto; Beta == auto) ->
				^^option(coefficient_grid(RequestedGrid), MergedOptions),
				normalize_coefficient_grid(RequestedGrid, Grid)
			;	Grid = []
			),
			coefficient_candidates(Alpha, Grid, Alphas),
			coefficient_candidates(Beta, Grid, Betas),
			findall(candidate(CandidateMethod,CandidateAlpha,CandidateBeta), (
				member(CandidateMethod, Methods),
				member(CandidateAlpha, Alphas),
				member(CandidateBeta, Betas)
			), Specifications),
			learn_candidates(Specifications, Dataset, Series, Policy, Candidates),
			keysort(Candidates, [_-Forecaster| _])
		).

	coefficient_candidates(Value, Grid, Candidates) :-
		(	Value == auto ->
			Candidates = Grid
		;	Candidates = [Value]
		).

	:- private(normalize_coefficient_grid/2).
	:- mode(normalize_coefficient_grid(+list(number), -list(float)), one_or_error).
	:- info(normalize_coefficient_grid/2, [
		comment is 'Normalizes a validated nonempty coefficient grid to ascending distinct floats without modifying the supplied list. Integer 1 and float 1.0 become one candidate. No comparison tolerance is used.',
		argnames is ['Grid', 'NormalizedGrid'],
		exceptions is [
			'Coefficient conversion raises an evaluation error' - evaluation_error('Error')
		]
	]).

	normalize_coefficient_grid(Grid, NormalizedGrid) :-
		findall(Float, (member(Value, Grid), Float is float(Value)), Floats),
		sort(Floats, NormalizedGrid).

	:- private(learn_candidates/5).
	:- mode(learn_candidates(+list(compound), +object_identifier, @list, +atom, -list), one_or_error).
	:- info(learn_candidates/5, [
		comment is 'Fits concrete candidate(Method,Alpha,Beta) specifications in priority order from an already collected series using the validated missing policy, returning RMSE-Forecaster pairs. Preserves missing variables and propagates candidate errors without skipping configurations.',
		argnames is ['Specifications', 'Dataset', 'Series', 'Policy', 'Candidates'],
		exceptions is [
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Candidate fitting or error arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	learn_candidates([], _Dataset, _Series, _Policy, []).
	learn_candidates([candidate(Method,Alpha,Beta)| Specifications], Dataset, Series, Policy, [RMSE-Forecaster| Candidates]) :-
		learn_series(Dataset, Series, Method, parameters(Alpha, Beta, Policy), Forecaster),
		Forecaster = intermittent_demand_forecaster(Method, _, Diagnostics),
		memberchk(root_mean_squared_error(RMSE), Diagnostics),
		learn_candidates(Specifications, Dataset, Series, Policy, Candidates).

	:- private(learn_series/5).
	:- mode(learn_series(+object_identifier, @list, +atom, +compound, -compound), one_or_error).
	:- info(learn_series/5, [
		comment is 'Fits a concrete method to an already collected series using validated parameters, preserving missing variables and recording canonical concrete options and causal error diagnostics.',
		argnames is ['Dataset', 'Series', 'Method', 'Parameters', 'Forecaster'],
		exceptions is [
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'An observation is negative' - domain_error(non_negative_number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Fitting or error arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	learn_series(Dataset, Series, Method, parameters(Alpha, Beta, Policy), intermittent_demand_forecaster(Method, State, Diagnostics)) :-
		Options = [model(Method), alpha(Alpha), beta(Beta), missing(Policy)],
		learn_state(Series, Method, parameters(Alpha, Beta, Policy),
			learning_state(pending_state(0), 0, forecast_error_totals(0,0,0)),
			learning_state(State, Positive, Totals)),
		Totals = forecast_error_totals(Count, Absolute, Squared),
		(	Count > 0 ->
			true
		;	domain_error(insufficient_observations, Dataset)
		),
		length(Series, Length),
		Missing is Length - Count,
		effective_periods(Policy, Length, Count, Effective),
		^^forecast_error_metrics(Totals, MAE, RMSE),
		^^base_forecaster_diagnostics(intermittent_demand_forecasting, Length, Options, [
			method(Method), observed_count(Count), missing_count(Missing), positive_count(Positive),
			effective_period_count(Effective), update_count(0), scored_count(Count),
			sum_absolute_error(Absolute), sum_squared_error(Squared),
			mean_absolute_error(MAE), root_mean_squared_error(RMSE)
		], Diagnostics).

	learn_state([], _Method, _Parameters, Accumulator, Accumulator).
	learn_state([Observation| Observations], Method, Parameters, learning_state(State0, Positive0, Totals0), Accumulator) :-
		check_demand(Observation),
		Parameters = parameters(_, Beta, _),
		score_observation(Method, State0, Beta, Observation, Totals0, Totals1),
		transition(Method, Parameters, State0, Observation, State1),
		positive_count(Observation, Positive0, Positive1),
		learn_state(Observations, Method, Parameters, learning_state(State1, Positive1, Totals1), Accumulator).

	score_observation(Method, State, Beta, Observation, Totals0, Totals) :-
		(	var(Observation) ->
			Totals = Totals0
		;	forecast_value(Method, State, Beta, Prediction),
			^^accumulate_forecast_error(Observation, Prediction, Totals0, Totals)
		).

	check_demand(Observation) :-
		^^check_observation(Observation),
		(	var(Observation) ->
			true
		;	Observation >= 0 ->
			true
		;	domain_error(non_negative_number, Observation)
		).

	positive_count(Observation, Positive0, Positive) :-
		(	var(Observation) ->
			Positive = Positive0
		;	Observation > 0 ->
			Positive is Positive0 + 1
		;	Positive = Positive0
		).

	effective_periods(skip, _Length, Count, Count).
	effective_periods(elapsed, Length, _Count, Length).

	transition(Method, parameters(Alpha, Beta, Policy), State, Observation, Updated) :-
		(	var(Observation) ->
			missing_state(Policy, State, Updated)
		;	numeric_state(Method, Alpha, Beta, State, Observation, Updated)
		).

	missing_state(skip, State, State).
	missing_state(elapsed, pending_state(Clock), pending_state(Next)) :-
		!,
		Next is Clock + 1.
	missing_state(elapsed, croston_state(Size, Interval, Age), croston_state(Size, Interval, Next)) :-
		!,
		Next is Age + 1.
	missing_state(elapsed, tsb_state(Size, Probability), tsb_state(Size, Probability)).

	numeric_state(Method, _Alpha, _Beta, pending_state(Clock), Observation, Updated) :-
		!,
		Period is Clock + 1,
		(	Observation > 0 ->
			initialized_state(Method, Observation, Period, Updated)
		;	Updated = pending_state(Period)
		).
	numeric_state(_Method, Alpha, Beta, croston_state(Size, Interval, Age), Observation, Updated) :-
		!,
		Gap is Age + 1,
		(	Observation > 0 ->
			NewSize is Alpha * Observation + (1 - Alpha) * Size,
			NewInterval is Beta * Gap + (1 - Beta) * Interval,
			Updated = croston_state(NewSize, NewInterval, 0)
		;	Updated = croston_state(Size, Interval, Gap)
		).
	numeric_state(tsb, Alpha, Beta, tsb_state(Size, Probability), Observation, tsb_state(NewSize, NewProbability)) :-
		(	Observation > 0 ->
			NewSize is Alpha * Observation + (1 - Alpha) * Size,
			NewProbability is Beta + (1 - Beta) * Probability
		;	NewSize = Size,
			NewProbability is (1 - Beta) * Probability
		).

	initialized_state(tsb, Size, Period, tsb_state(Size, Probability)) :-
		!,
		Probability is 1 / Period.
	initialized_state(_Method, Size, Period, croston_state(Size, Period, 0)).

	forecast(Forecaster, Horizon, Forecasts) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		(	Horizon =:= 0 ->
			Forecasts = []
		;	Forecaster = intermittent_demand_forecaster(Method, State, Diagnostics),
			memberchk(options([model(Method), alpha(_), beta(Beta), missing(_)]), Diagnostics),
			forecast_value(Method, State, Beta, Value),
			^^constant_forecast(Value, Horizon, Forecasts)
		).

	forecast_value(_Method, pending_state(_), _Beta, 0) :-
		!.
	forecast_value(croston, croston_state(Size, Interval, _), _Beta, Value) :-
		Value is Size / Interval.
	forecast_value(sba, croston_state(Size, Interval, _), Beta, Value) :-
		Value is (1 - Beta / 2) * (Size / Interval).
	forecast_value(tsb, tsb_state(Size, Probability), _Beta, Value) :-
		Value is Size * Probability.

	update(Forecaster, Observation, intermittent_demand_forecaster(Method, State, UpdatedDiagnostics)) :-
		check_forecaster(Forecaster),
		check_demand(Observation),
		Forecaster = intermittent_demand_forecaster(Method, OriginalState, Diagnostics),
		memberchk(options([model(Method), alpha(Alpha), beta(Beta), missing(Policy)]), Diagnostics),
		updated_error_diagnostics(Method, OriginalState, Beta, Observation, Diagnostics, ErrorDiagnostics),
		transition(Method, parameters(Alpha, Beta, Policy), OriginalState, Observation, State),
		^^updated_observation_diagnostics(ErrorDiagnostics, Observation, CommonDiagnostics),
		memberchk(positive_count(Positive0), Diagnostics),
		positive_count(Observation, Positive0, Positive),
		memberchk(training_series_length(Length), CommonDiagnostics),
		memberchk(observed_count(Count), CommonDiagnostics),
		effective_periods(Policy, Length, Count, Effective),
		^^replace_diagnostic(positive_count, Positive, CommonDiagnostics, Diagnostics1),
		^^replace_diagnostic(effective_period_count, Effective, Diagnostics1, UpdatedDiagnostics).

	updated_error_diagnostics(Method, State, Beta, Observation, Diagnostics, UpdatedDiagnostics) :-
		(	var(Observation) ->
			UpdatedDiagnostics = Diagnostics
		;	memberchk(scored_count(Count0), Diagnostics),
			memberchk(sum_absolute_error(Absolute0), Diagnostics),
			memberchk(sum_squared_error(Squared0), Diagnostics),
			score_observation(Method, State, Beta, Observation, forecast_error_totals(Count0,Absolute0,Squared0), Totals),
			Totals = forecast_error_totals(Count,Absolute,Squared),
			^^forecast_error_metrics(Totals, MAE, RMSE),
			^^replace_diagnostic(scored_count, Count, Diagnostics, Diagnostics1),
			^^replace_diagnostic(sum_absolute_error, Absolute, Diagnostics1, Diagnostics2),
			^^replace_diagnostic(sum_squared_error, Squared, Diagnostics2, Diagnostics3),
			^^replace_diagnostic(mean_absolute_error, MAE, Diagnostics3, Diagnostics4),
			^^replace_diagnostic(root_mean_squared_error, RMSE, Diagnostics4, UpdatedDiagnostics)
		).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	(	ground(Forecaster),
				Forecaster = intermittent_demand_forecaster(Method, State, Diagnostics),
				valid_diagnostics(Method, State, Diagnostics) ->
				true
			;	domain_error(forecaster, Forecaster)
			)
		).

	valid_diagnostics(Method, State, Diagnostics) :-
		^^valid_forecaster_metadata(intermittent_demand_forecasting, Options, Diagnostics),
		Options = [model(Method), alpha(Alpha), beta(Beta), missing(Policy)],
		concrete_method(Method),
		valid_coefficient(Alpha), valid_coefficient(Beta), valid_option(missing(Policy)),
		memberchk(method(Method), Diagnostics),
		memberchk(training_series_length(Length), Diagnostics),
		valid(positive_integer, Length),
		memberchk(observed_count(Count), Diagnostics),
		valid(positive_integer, Count),
		memberchk(missing_count(Missing), Diagnostics),
		valid(non_negative_integer, Missing),
		Count + Missing =:= Length,
		memberchk(positive_count(Positive), Diagnostics),
		valid(non_negative_integer, Positive), Positive =< Count,
		memberchk(effective_period_count(Effective), Diagnostics),
		effective_periods(Policy, Length, Count, Expected), Effective == Expected,
		memberchk(update_count(Updates), Diagnostics),
		valid(non_negative_integer, Updates), Updates < Length,
		valid_error_diagnostics(Count, Diagnostics),
		valid_state(Method, State, Positive, Effective).

	valid_error_diagnostics(Count, Diagnostics) :-
		findall(
			Diagnostic,
			(	member(Diagnostic, Diagnostics),
				functor(Diagnostic, Name, _),
				error_diagnostic_name(Name)
			),
			Errors
		),
		length(Errors, 5),
		memberchk(scored_count(Count), Errors),
		memberchk(sum_absolute_error(Absolute), Errors),
		memberchk(sum_squared_error(Squared), Errors),
		memberchk(mean_absolute_error(MAE), Errors),
		memberchk(root_mean_squared_error(RMSE), Errors),
		number(Absolute), Absolute >= 0,
		number(Squared), Squared >= 0,
		number(MAE), MAE >= 0,
		number(RMSE), RMSE >= 0,
		^^forecast_error_metrics(forecast_error_totals(Count,Absolute,Squared), ExpectedMAE, ExpectedRMSE),
		MAE =:= ExpectedMAE,
		RMSE =:= ExpectedRMSE.

	error_diagnostic_name(scored_count).
	error_diagnostic_name(sum_absolute_error).
	error_diagnostic_name(sum_squared_error).
	error_diagnostic_name(mean_absolute_error).
	error_diagnostic_name(root_mean_squared_error).

	valid_state(_Method, pending_state(Clock), Positive, Effective) :-
		!,
		Positive =:= 0, Clock == Effective.
	valid_state(Method, croston_state(Size, Interval, Age), Positive, Effective) :-
		!,
		Method \== tsb,
		Positive > 0,
		number(Size), Size > 0,
		number(Interval), Interval >= 1, Interval =< Effective,
		valid(non_negative_integer, Age), Age < Effective.
	valid_state(tsb, tsb_state(Size, Probability), Positive, _Effective) :-
		Positive > 0,
		number(Size), Size > 0,
		number(Probability), Probability >= 0, Probability =< 1.

	forecaster_term_template(intermittent_demand_forecaster(_, _, _), intermittent_demand_forecaster('Method', 'State', 'Diagnostics')).

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		context(Context),
		check(atom, Functor, Context),
		Clause =.. [Functor, Forecaster].

	export_to_file(Dataset, Forecaster, Functor, File) :-
		^^export_to_file(Dataset, Forecaster, Functor, File).

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		^^print_forecaster_template(Forecaster),
		Forecaster = intermittent_demand_forecaster(Method, State, Diagnostics),
		format('Method: ~w~nState: ~q~nDiagnostics: ~q~n', [Method, State, Diagnostics]).

	default_option(model(sba)).
	default_option(alpha(0.1)).
	default_option(beta(0.1)).
	default_option(missing(skip)).
	default_option(coefficient_grid([0.1,0.2,0.5,0.8,1.0])).

	valid_option(model(Method)) :-
		(	Method == auto ->
			true
		;	concrete_method(Method)
		).
	valid_option(alpha(Value)) :-
		(	Value == auto ->
			true
		;	valid_coefficient(Value)
		).
	valid_option(beta(Value)) :-
		(	Value == auto ->
			true
		;	valid_coefficient(Value)
		).
	valid_option(missing(Policy)) :-
		once((Policy == skip; Policy == elapsed)).
	valid_option(coefficient_grid(Grid)) :-
		valid(non_empty_list, Grid),
		valid_grid_values(Grid).

	valid_grid_values([]).
	valid_grid_values([Value| Values]) :-
		valid_coefficient(Value),
		valid_grid_values(Values).

	concrete_method(Method) :-
		once((Method == croston; Method == sba; Method == tsb)).

	valid_coefficient(Value) :-
		number(Value), Value > 0, Value =< 1.

:- end_object.

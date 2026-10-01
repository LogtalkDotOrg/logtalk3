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


:- category(forecaster_common,
	implements(forecaster_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-29,
		comment is 'Shared predicates for forecaster diagnostics, time series dataset validation, differencing, lag construction, forecast error metrics, and naive baselines.'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(integer, [
		sequence/3
	]).

	:- uses(list, [
		append/3, last/2, length/2, member/2, memberchk/2, reverse/2, same_length/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(pairs, [
		keys/2, values/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	% hook predicates that concrete forecaster implementations must define

	:- protected(forecaster_diagnostics_data/2).
	:- mode(forecaster_diagnostics_data(+compound, -list(compound)), one).
	:- info(forecaster_diagnostics_data/2, [
		comment is 'Hook predicate that importing forecaster implementations must define in order to expose diagnostics metadata. A default implementation is provided that assumes the diagnostics list is the last argument of the forecaster term; concrete implementations following that convention do not need to override it.',
		argnames is ['Forecaster', 'Diagnostics']
	]).

	:- protected(forecaster_export_template/4).
	:- mode(forecaster_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(forecaster_export_template/4, [
		comment is 'Hook predicate that importing forecaster implementations must define in order to expose the exported forecaster template for a given functor.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'Template']
	]).

	:- protected(forecaster_term_template/2).
	:- mode(forecaster_term_template(+compound, -callable), one).
	:- info(forecaster_term_template/2, [
		comment is 'Hook predicate that importing forecaster implementations must define in order to expose the learned forecaster term template used by pretty-printing helpers.',
		argnames is ['Forecaster', 'Template']
	]).

	% pretty-printing helper

	:- protected(print_forecaster_template/1).
	:- mode(print_forecaster_template(+compound), one).
	:- info(print_forecaster_template/1, [
		comment is 'Pretty-printing helper predicate used by importing forecaster implementations to show the learned forecaster term template.',
		argnames is ['Forecaster']
	]).

	% default protocol predicate implementations

	learn(Dataset, Forecaster) :-
		::learn(Dataset, Forecaster, []).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	::forecaster_term_template(Forecaster, _Template),
			::forecaster_diagnostics_data(Forecaster, _Diagnostics) ->
			true
		;	domain_error(forecaster, Forecaster)
		).

	valid_forecaster(Forecaster) :-
		catch(::check_forecaster(Forecaster), _Error, fail).

	diagnostics(Forecaster, Diagnostics) :-
		::forecaster_diagnostics_data(Forecaster, Diagnostics).

	diagnostic(Forecaster, Diagnostic) :-
		::forecaster_diagnostics_data(Forecaster, Diagnostics),
		member(Diagnostic, Diagnostics).

	forecaster_options(Forecaster, Options) :-
		::forecaster_diagnostics_data(Forecaster, Diagnostics),
		memberchk(options(Options), Diagnostics).

	forecaster_diagnostics_data(Forecaster, Diagnostics) :-
		Forecaster =.. [_| Arguments],
		last(Arguments, Diagnostics).

	print_forecaster_template(Forecaster) :-
		::forecaster_term_template(Forecaster, Template),
		format('Template: ~w~n', [Template]).

	% dataset collection and validation

	:- protected(dataset_series/2).
	:- mode(dataset_series(+object_identifier, -list), one_or_error).
	:- info(dataset_series/2, [
		comment is 'Collects the dataset time-ordered observation values. Checks that the observation indices form a complete, gap-free, 1-based sequence and that the declared series length matches the observed length.',
		argnames is ['Dataset', 'Series'],
		exceptions is [
			'The dataset contains no observations' - domain_error(non_empty_series, 'Dataset'),
			'The observation indices do not form a complete, gap-free, 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared series length is a variable' - instantiation_error,
			'The declared series length is neither a variable nor an integer' - type_error(integer, 'DeclaredLength'),
			'The declared series length is an integer but is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The declared and observed series lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength')
		]
	]).

	dataset_series(Dataset, Series) :-
		findall(
			Index-Value,
			Dataset::observation(Index, Value),
			Pairs0
		),
		(	Pairs0 == [] ->
			domain_error(non_empty_series, Dataset)
		;	true
		),
		keysort(Pairs0, Pairs),
		keys(Pairs, Indices),
		length(Indices, ObservedLength),
		(	sequence(1, ObservedLength, Indices) ->
			true
		;	domain_error(series_index_sequence, Dataset)
		),
		Dataset::series_length(DeclaredLength),
		context(Context),
		check(positive_integer, DeclaredLength, Context),
		(	DeclaredLength =:= ObservedLength ->
			values(Pairs, Series)
		;	consistency_error(series_length, DeclaredLength, ObservedLength)
		).

	:- protected(check_series/2).
	:- mode(check_series(+object_identifier, +list), one_or_error).
	:- info(check_series/2, [
		comment is 'Checks that a time series is non-empty and that every observation value is a number.',
		argnames is ['Dataset', 'Series'],
		exceptions is [
			'``Series`` is a partial list or a list with an element which is a variable' - instantiation_error,
			'An element ``Value`` of the ``Series`` list is neither a variable nor a number' - type_error(number, 'Value'),
			'``Series`` is empty' - domain_error(non_empty_series, 'Dataset')
		]
	]).

	check_series(Dataset, Series) :-
		context(Context),
		(	Series == [] ->
			domain_error(non_empty_series, Dataset)
		;	true
		),
		check_series_values(Series, Context).

	check_series_values(Values, Context) :-
		check(list(number), Values, Context).

	:- protected(check_series/3).
	:- mode(check_series(+object_identifier, +list, +list), one_or_error).
	:- info(check_series/3, [
		comment is 'Checks that a time series is non-empty and that every observation value is of one of the given types.',
		argnames is ['Dataset', 'Series', 'Types'],
		exceptions is [
			'``Series`` is a partial list' - instantiation_error,
			'An element ``Value`` of the ``Series`` list is neither a variable nor a number' - domain_error(types('Types'), 'Value'),
			'``Series`` is empty' - domain_error(non_empty_series, 'Dataset')
		]
	]).

	check_series(Dataset, Series, Types) :-
		context(Context),
		(	Series == [] ->
			domain_error(non_empty_series, Dataset)
		;	true
		),
		check_series_values(Series, Types, Context).

	check_series_values(Values, Types, Context) :-
		check(list(types(Types)), Values, Context).

	:- protected(check_series_length/3).
	:- mode(check_series_length(+object_identifier, +list, +non_negative_integer), one_or_error).
	:- info(check_series_length/3, [
		comment is 'Checks that a time series has at least the given minimum number of observations.',
		argnames is ['Dataset', 'Series', 'MinimumLength'],
		exceptions is [
			'``MinimumLength`` is a variable' - instantiation_error,
			'``MinimumLength`` is neither a variable nor an integer' - type_error(integer, 'MinimumLength'),
			'``MinimumLength`` is an integer but is negative' - domain_error(non_negative_integer, 'MinimumLength'),
			'``Series`` is shorter than ``MinimumLength``' - domain_error(series_length, 'Dataset')
		]
	]).

	check_series_length(Dataset, Series, MinimumLength) :-
		context(Context),
		check(non_negative_integer, MinimumLength, Context),
		length(Series, Length),
		(	Length >= MinimumLength ->
			true
		;	domain_error(series_length, Dataset)
		).

	:- protected(check_observation/1).
	:- mode(check_observation(@term), one_or_error).
	:- info(check_observation/1, [
		comment is 'Checks that an observation is a number or an unbound variable representing a missing observation.',
		argnames is ['Observation'],
		exceptions is [
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation')
		]
	]).

	check_observation(Observation) :-
		(	var(Observation) ->
			true
		;	number(Observation) ->
			true
		;	type_error(number, Observation)
		).

	:- protected(series_observation_summary/4).
	:- mode(series_observation_summary(+list, -non_negative_integer, -non_negative_integer, -number), one_or_error).
	:- info(series_observation_summary/4, [
		comment is 'Returns the elapsed length, numeric observation count, and numeric sum of a series. Unbound observations are counted in the length only and are not instantiated. An empty list has zero length, count, and sum.',
		argnames is ['Series', 'Length', 'ObservedCount', 'Sum'],
		exceptions is [
			'``Series`` is a variable or a partial list' - instantiation_error,
			'``Series`` is neither a variable nor a list' - type_error(list, 'Series'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'Numeric summation raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	series_observation_summary(Series, Length, ObservedCount, Sum) :-
		context(Context),
		check(list, Series, Context),
		observation_summary(Series, Length, ObservedCount, Sum).

	observation_summary(Series, Length, ObservedCount, Sum) :-
		observation_summary(Series, 0, 0, 0, Length, ObservedCount, Sum).

	observation_summary([], Length, ObservedCount, Sum, Length, ObservedCount, Sum).
	observation_summary([Observation| Observations], Length0, ObservedCount0, Sum0, Length, ObservedCount, Sum) :-
		check_observation(Observation),
		Length1 is Length0 + 1,
		(	var(Observation) ->
			ObservedCount1 = ObservedCount0,
			Sum1 = Sum0
		;	ObservedCount1 is ObservedCount0 + 1,
			Sum1 is Sum0 + Observation
		),
		observation_summary(Observations, Length1, ObservedCount1, Sum1, Length, ObservedCount, Sum).

	% diagnostics helpers

	:- protected(base_forecaster_diagnostics/5).
	:- mode(base_forecaster_diagnostics(+atom, +integer, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_forecaster_diagnostics/5, [
		comment is 'Builds the common part of a forecaster diagnostics metadata list, combined with implementation-specific extra diagnostics terms.',
		argnames is ['Model', 'TrainingSeriesLength', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	base_forecaster_diagnostics(Model, TrainingSeriesLength, Options, ExtraDiagnostics, Diagnostics) :-
		Diagnostics = [
			model(Model),
			training_series_length(TrainingSeriesLength),
			options(Options)
		| ExtraDiagnostics
		].

	:- protected(valid_forecaster_metadata/2).
	:- mode(valid_forecaster_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_forecaster_metadata/2, [
		comment is 'True when diagnostics metadata contains the expected model term.',
		argnames is ['Model', 'Diagnostics']
	]).

	valid_forecaster_metadata(Model, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics).

	:- protected(valid_forecaster_metadata/3).
	:- mode(valid_forecaster_metadata(+atom, +list(compound), +list(compound)), zero_or_one).
	:- info(valid_forecaster_metadata/3, [
		comment is 'True when diagnostics metadata contains the expected model term and records the given effective options.',
		argnames is ['Model', 'Options', 'Diagnostics']
	]).

	valid_forecaster_metadata(Model, Options, Diagnostics) :-
		valid_forecaster_metadata(Model, Diagnostics),
		memberchk(options(Options), Diagnostics).

	:- protected(replace_diagnostic/4).
	:- mode(replace_diagnostic(+atom, +term, +list(compound), -list(compound)), zero_or_one).
	:- info(replace_diagnostic/4, [
		comment is 'Replaces the value of the first unary diagnostic with the given name, preserving order. Fails when no such diagnostic exists.',
		argnames is ['Name', 'Value', 'Diagnostics', 'UpdatedDiagnostics']
	]).

	replace_diagnostic(Name, Value, [Diagnostic| Diagnostics], [UpdatedDiagnostic| Diagnostics]) :-
		functor(Diagnostic, Name, 1),
		!,
		UpdatedDiagnostic =.. [Name, Value].
	replace_diagnostic(Name, Value, [Diagnostic| Diagnostics], [Diagnostic| UpdatedDiagnostics]) :-
		replace_diagnostic(Name, Value, Diagnostics, UpdatedDiagnostics).

	export_to_file(Dataset, Forecaster, Functor, File) :-
		::export_to_clauses(Dataset, Forecaster, Functor, Clauses),
		open(File, write, Stream),
		(	catch(
			(	write_comment_header(Dataset, Functor, Forecaster, Stream),
				write_clauses(Clauses, Stream)
			),
			Error,
			(safe_close_stream(Stream), throw(Error))
		) ->
			close(Stream)
		;	safe_close_stream(Stream),
			fail
		).

	safe_close_stream(Stream) :-
		catch(close(Stream), _, true).

	write_comment_header(Dataset, Functor, Forecaster, Stream) :-
		::forecaster_export_template(Dataset, Forecaster, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported forecaster predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		::dataset_series(Dataset, Series),
		length(Series, Length),
		format(Stream, '% training series length: ~d~n', [Length]),
		(	::diagnostics(Forecaster, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	% differencing and reconstruction

	:- protected(difference_series/2).
	:- mode(difference_series(+list(number), -list(number)), one_or_error).
	:- info(difference_series/2, [
		comment is 'Computes the first-order differences of a series: ``Differences[i] = Series[i+1] - Series[i]``. The result has one fewer element than ``Series``.',
		argnames is ['Series', 'Differences'],
		exceptions is [
			'``Series`` is empty' - domain_error(non_empty_series, 'Series')
		]
	]).

	difference_series([], _) :-
		domain_error(non_empty_series, []).
	difference_series([First| Rest], Differences) :-
		difference_series(Rest, First, Differences).

	difference_series([], _, []).
	difference_series([Value| Values], Previous, [Difference| Differences]) :-
		Difference is Value - Previous,
		difference_series(Values, Value, Differences).

	:- protected(integrate_series/3).
	:- mode(integrate_series(+list(number), +number, -list(number)), one).
	:- info(integrate_series/3, [
		comment is 'Reconstructs a series of levels from a series of differences and a starting base value (typically the last observed original value), by cumulative summing. Used to undo ``difference_series/2`` on forecasted differences.',
		argnames is ['Differences', 'Base', 'Series']
	]).

	integrate_series([], _, []).
	integrate_series([Difference| Differences], Previous, [Value| Values]) :-
		Value is Previous + Difference,
		integrate_series(Differences, Value, Values).

	% lag construction for autoregressive-style fitting

	:- protected(lagged_rows/3).
	:- mode(lagged_rows(+list(number), +positive_integer, -list(pair)), one_or_error).
	:- info(lagged_rows/3, [
		comment is 'Builds a list of ``Lags-Target`` rows from a series, where ``Lags`` is a list of the ``Order`` most recent values preceding ``Target``, most recent first (``[X(t-1), X(t-2), ..., X(t-Order)]-X(t)``). Requires the series to have more than ``Order`` observations.',
		argnames is ['Series', 'Order', 'Rows'],
		exceptions is [
			'``Order`` is a variable' - instantiation_error,
			'``Order`` is neither a variable nor an integer' - type_error(integer, 'Order'),
			'``Order`` is an integer but is not positive' - domain_error(positive_integer, 'Order'),
			'``Series`` has ``Order`` or fewer observations' - domain_error(series_length, 'Series')
		]
	]).

	lagged_rows(Series, Order, Rows) :-
		context(Context),
		check(positive_integer, Order, Context),
		length(InitialLags0, Order),
		(	append(InitialLags0, Rest, Series),
			Rest \== [] ->
			true
		;	domain_error(series_length, Series)
		),
		reverse(InitialLags0, InitialLags),
		lagged_rows_(Rest, InitialLags, Order, Rows).

	lagged_rows_([], _, _, []).
	lagged_rows_([Target| Values], Lags, Order, [Lags-Target| Rows]) :-
		update_lags(Lags, Target, Order, NewLags),
		lagged_rows_(Values, NewLags, Order, Rows).

	update_lags(Lags, Target, Order, [Target| Trimmed]) :-
		Order1 is Order - 1,
		length(Trimmed, Order1),
		append(Trimmed, [_], Lags).

	% forecast error metrics

	:- protected(mean_absolute_error/3).
	:- mode(mean_absolute_error(+list(number), +list(number), -float), one_or_error).
	:- info(mean_absolute_error/3, [
		comment is 'Computes the mean absolute error (MAE) between actual and predicted value lists of matching length.',
		argnames is ['Actual', 'Predicted', 'MAE'],
		exceptions is [
			'An input series is empty' - domain_error(non_empty_series, 'Series'),
			'The actual and predicted series have different lengths' - consistency_error(same_length, 'Actual', 'Predicted')
		]
	]).

	mean_absolute_error(Actual, Predicted, MAE) :-
		check_metric_series(Actual, Predicted),
		absolute_differences(Actual, Predicted, AbsoluteDifferences),
		length(AbsoluteDifferences, N),
		N > 0,
		sum(AbsoluteDifferences, Sum),
		MAE is float(Sum / N).

	absolute_differences([], [], []).
	absolute_differences([A| As], [P| Ps], [D| Ds]) :-
		D is abs(A - P),
		absolute_differences(As, Ps, Ds).

	:- protected(root_mean_squared_error/3).
	:- mode(root_mean_squared_error(+list(number), +list(number), -float), one_or_error).
	:- info(root_mean_squared_error/3, [
		comment is 'Computes the root mean squared error (RMSE) between actual and predicted value lists of matching length.',
		argnames is ['Actual', 'Predicted', 'RMSE'],
		exceptions is [
			'An input series is empty' - domain_error(non_empty_series, 'Series'),
			'The actual and predicted series have different lengths' - consistency_error(same_length, 'Actual', 'Predicted')
		]
	]).

	root_mean_squared_error(Actual, Predicted, RMSE) :-
		check_metric_series(Actual, Predicted),
		squared_differences(Actual, Predicted, SquaredDifferences),
		length(SquaredDifferences, N),
		N > 0,
		sum(SquaredDifferences, Sum),
		MeanSquaredError is Sum / N,
		RMSE is float(sqrt(MeanSquaredError)).

	squared_differences([], [], []).
	squared_differences([A| As], [P| Ps], [D| Ds]) :-
		D is (A - P) ** 2,
		squared_differences(As, Ps, Ds).

	:- protected(mean_absolute_percentage_error/3).
	:- mode(mean_absolute_percentage_error(+list(number), +list(number), -float), one_or_error).
	:- info(mean_absolute_percentage_error/3, [
		comment is 'Computes the mean absolute percentage error (MAPE), as a percentage, between actual and predicted value lists of matching length.',
		argnames is ['Actual', 'Predicted', 'MAPE'],
		exceptions is [
			'An input series is empty' - domain_error(non_empty_series, 'Series'),
			'The actual and predicted series have different lengths' - consistency_error(same_length, 'Actual', 'Predicted'),
			'An actual value is zero' - evaluation_error(zero_divisor)
		]
	]).

	mean_absolute_percentage_error(Actual, Predicted, MAPE) :-
		check_metric_series(Actual, Predicted),
		percentage_errors(Actual, Predicted, PercentageErrors),
		length(PercentageErrors, N),
		N > 0,
		sum(PercentageErrors, Sum),
		MAPE is float((Sum / N) * 100).

	percentage_errors([], [], []).
	percentage_errors([A| As], [P| Ps], [E| Es]) :-
		(	A =:= 0 ->
			evaluation_error(zero_divisor)
		;	E is abs((A - P) / A)
		),
		percentage_errors(As, Ps, Es).

	check_metric_series(Actual, Predicted) :-
		context(Context),
		check(list(number), Actual, Context),
		check(list(number), Predicted, Context),
		(	Actual == [] ->
			domain_error(non_empty_series, Actual)
		;	Predicted == [] ->
			domain_error(non_empty_series, Predicted)
		;	same_length(Actual, Predicted) ->
			true
		;	consistency_error(same_length, Actual, Predicted)
		).

	% naive baselines, also useful as fallbacks and building blocks

	:- protected(naive_forecast/3).
	:- mode(naive_forecast(+list(number), +non_negative_integer, -list(number)), one_or_error).
	:- info(naive_forecast/3, [
		comment is 'Repeats the last observed series value ``Horizon`` times (the naive/persistence baseline forecast). A zero horizon returns an empty list.',
		argnames is ['Series', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	naive_forecast(Series, Horizon, Forecasts) :-
		check_forecast_horizon(Horizon),
		last(Series, LastValue),
		constant_forecast(LastValue, Horizon, Forecasts).

	:- protected(constant_forecast/3).
	:- mode(constant_forecast(+number, +non_negative_integer, -list(number)), one_or_error).
	:- info(constant_forecast/3, [
		comment is 'Repeats a value for the given forecast horizon. A zero horizon returns an empty list.',
		argnames is ['Value', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	constant_forecast(Value, Horizon, Forecasts) :-
		check_forecast_horizon(Horizon),
		length(Forecasts, Horizon),
		repeat_value(Forecasts, Value).

	:- protected(linear_trend_forecast/4).
	:- mode(linear_trend_forecast(+number, +number, +non_negative_integer, -list(number)), one_or_error).
	:- info(linear_trend_forecast/4, [
		comment is 'Forecasts ``Last + Step * Slope`` for steps one through the horizon. A zero horizon returns an empty list.',
		argnames is ['Last', 'Slope', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Horizon``, ``Last``, or ``Slope`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon'),
			'``Last`` is neither a variable nor a number' - type_error(number, 'Last'),
			'``Slope`` is neither a variable nor a number' - type_error(number, 'Slope'),
			'Trend extrapolation raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	linear_trend_forecast(Last, Slope, Horizon, Forecasts) :-
		check_forecast_horizon(Horizon),
		context(Context),
		check(number, Last, Context),
		check(number, Slope, Context),
		trend_forecasts(Horizon, 1, Last, Slope, Forecasts).

	trend_forecasts(0, _, _, _, []) :-
		!.
	trend_forecasts(Remaining, Step, Last, Slope, [Value| Values]) :-
		Value is Last + Step * Slope,
		NextStep is Step + 1,
		NextRemaining is Remaining - 1,
		trend_forecasts(NextRemaining, NextStep, Last, Slope, Values).

	:- protected(check_forecast_horizon/1).
	:- mode(check_forecast_horizon(+non_negative_integer), one_or_error).
	:- info(check_forecast_horizon/1, [
		comment is 'Checks that a forecast horizon is a non-negative integer.',
		argnames is ['Horizon'],
		exceptions is [
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon')
		]
	]).

	check_forecast_horizon(Horizon) :-
		context(Context),
		check(non_negative_integer, Horizon, Context).

	repeat_value([], _).
	repeat_value([Value| Values], Value) :-
		repeat_value(Values, Value).

	:- protected(seasonal_naive_forecast/4).
	:- mode(seasonal_naive_forecast(+list(number), +positive_integer, +non_negative_integer, -list(number)), one_or_error).
	:- info(seasonal_naive_forecast/4, [
		comment is 'Repeats the last full seasonal cycle of ``Frequency`` observations, cycling as needed to cover ``Horizon`` forecasts (the seasonal naive baseline forecast).',
		argnames is ['Series', 'Frequency', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Frequency`` is a variable' - instantiation_error,
			'``Frequency`` is neither a variable nor an integer' - type_error(integer, 'Frequency'),
			'``Frequency`` is an integer but is not positive' - domain_error(positive_integer, 'Frequency'),
			'``Horizon`` is a variable' - instantiation_error,
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is an integer but is negative' - domain_error(non_negative_integer, 'Horizon'),
			'``Series`` has fewer than ``Frequency`` observations' - domain_error(series_length, 'Series')
		]
	]).

	seasonal_naive_forecast(Series, Frequency, Horizon, Forecasts) :-
		check_frequency(Frequency),
		check_forecast_horizon(Horizon),
		length(Series, Length),
		PrefixLength is Length - Frequency,
		(	PrefixLength >= 0 ->
			true
		;	domain_error(series_length, Series)
		),
		length(Prefix, PrefixLength),
		append(Prefix, LastSeason, Series),
		cycle_take(LastSeason, LastSeason, Horizon, Forecasts).

	:- protected(check_frequency/1).
	:- mode(check_frequency(+positive_integer), one_or_error).
	:- info(check_frequency/1, [
		comment is 'Checks that a seasonal frequency is a positive integer.',
		argnames is ['Frequency'],
		exceptions is [
			'``Frequency`` is a variable' - instantiation_error,
			'``Frequency`` is neither a variable nor an integer' - type_error(integer, 'Frequency'),
			'``Frequency`` is an integer but is not positive' - domain_error(positive_integer, 'Frequency')
		]
	]).

	check_frequency(Frequency) :-
		context(Context),
		check(positive_integer, Frequency, Context).

	cycle_take(_, _, 0, []) :-
		!.
	cycle_take([], Cycle, Horizon, Forecasts) :-
		!,
		cycle_take(Cycle, Cycle, Horizon, Forecasts).
	cycle_take([Value| Rest], Cycle, Horizon, [Value| Forecasts]) :-
		Horizon > 0,
		Horizon1 is Horizon - 1,
		cycle_take(Rest, Cycle, Horizon1, Forecasts).

:- end_category.

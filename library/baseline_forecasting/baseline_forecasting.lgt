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


:- object(baseline_forecasting,
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-14,
		comment is 'Naive, seasonal naive, mean, and drift time series forecasters with missing observations and immutable online updates.'
	]).

	:- public(update/3).
	:- mode(update(+compound, @term, -compound), one_or_error).
	:- info(update/3, [
		comment is 'Returns a new forecaster after appending a numeric or missing (unbound) observation. The original forecaster is not modified.',
		argnames is ['Forecaster', 'Observation', 'UpdatedForecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid baseline forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Observation`` is neither a variable nor a number' - type_error(number, 'Observation'),
			'Updating the numeric sum raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, last/2, length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	learn(Dataset, Forecaster) :-
		^^learn(Dataset, Forecaster).

	learn(Dataset, baseline_forecaster(Method, State, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, MergedOptions),
		^^option(model(Method), MergedOptions),
		resolve_options(Method, Dataset, MergedOptions, Options),
		^^dataset_series(Dataset, Series),
		^^series_observation_summary(Series, Length, ObservedCount, Sum),
		initial_state(Method, Dataset, Series, Options, Sum, ObservedCount, State),
		(	ObservedCount > 0 ->
			true
		;	domain_error(insufficient_observations, Dataset)
		),
		MissingCount is Length - ObservedCount,
		^^base_forecaster_diagnostics(baseline_forecasting, Length, Options, [
			method(Method), observed_count(ObservedCount), missing_count(MissingCount), update_count(0)
		], Diagnostics).

	resolve_options(seasonal_naive, Dataset, UserOptions, [model(seasonal_naive), frequency(Frequency)]) :-
		!,
		(	member(frequency(Frequency0), UserOptions) ->
			Frequency = Frequency0
		;	Dataset::frequency(Frequency) ->
			true
		;	domain_error(seasonal_frequency, Dataset)
		),
		^^check_frequency(Frequency).
	resolve_options(Method, _Dataset, UserOptions, [model(Method)]) :-
		(	member(frequency(Frequency), UserOptions) ->
			domain_error(baseline_forecasting_option, frequency(Frequency))
		;	true
		).

	initial_state(naive, _Dataset, Series, _Options, _Sum, _Count, naive_state(Last)) :-
		last(Series, Last).
	initial_state(seasonal_naive, Dataset, Series, Options, _Sum, _Count, seasonal_naive_state(Frequency, Cycle)) :-
		memberchk(frequency(Frequency), Options),
		^^check_series_length(Dataset, Series, Frequency),
		length(Series, Length),
		PrefixLength is Length - Frequency,
		length(Prefix, PrefixLength),
		append(Prefix, Cycle, Series).
	initial_state(mean, _Dataset, _Series, _Options, Sum, Count, mean_state(Sum, Count)).
	initial_state(drift, Dataset, Series, _Options, _Sum, _Count, drift_state(First, Last)) :-
		^^check_series_length(Dataset, Series, 2),
		Series = [First| _],
		last(Series, Last).

	forecast(Forecaster, Horizon, Forecasts) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		(	Horizon =:= 0 ->
			Forecasts = []
		;	Forecaster = baseline_forecaster(Method, State, Diagnostics),
			forecast_state(Method, State, Diagnostics, Forecaster, Horizon, Forecasts)
		).

	forecast_state(naive, naive_state(Last), _Diagnostics, Forecaster, Horizon, Forecasts) :-
		check_known_value(Last, Forecaster),
		^^naive_forecast([Last], Horizon, Forecasts).
	forecast_state(seasonal_naive, seasonal_naive_state(Frequency, Cycle), _Diagnostics, Forecaster, Horizon, Forecasts) :-
		^^seasonal_naive_forecast(Cycle, Frequency, Horizon, Values),
		check_known_values(Values, Forecaster),
		Forecasts = Values.
	forecast_state(mean, mean_state(Sum, Count), _Diagnostics, _Forecaster, Horizon, Forecasts) :-
		Mean is float(Sum / Count),
		^^constant_forecast(Mean, Horizon, Forecasts).
	forecast_state(drift, drift_state(First, Last), Diagnostics, Forecaster, Horizon, Forecasts) :-
		check_known_value(First, Forecaster),
		check_known_value(Last, Forecaster),
		memberchk(training_series_length(Length), Diagnostics),
		Slope is (Last - First) / (Length - 1),
		^^linear_trend_forecast(Last, Slope, Horizon, Forecasts).

	check_known_value(Value, Forecaster) :-
		(	var(Value) ->
			domain_error(missing_observation, Forecaster)
		;	true
		).

	check_known_values([], _).
	check_known_values([Value| Values], Forecaster) :-
		check_known_value(Value, Forecaster),
		check_known_values(Values, Forecaster).

	update(Forecaster, Observation, baseline_forecaster(Method, UpdatedState, UpdatedDiagnostics)) :-
		check_forecaster(Forecaster),
		^^check_observation(Observation),
		copy_term(Forecaster-Observation, baseline_forecaster(Method, State, Diagnostics)-NewObservation),
		updated_state(Method, State, NewObservation, UpdatedState),
		^^updated_observation_diagnostics(Diagnostics, NewObservation, UpdatedDiagnostics).

	updated_state(naive, _State, Observation, naive_state(Observation)).
	updated_state(seasonal_naive, seasonal_naive_state(Frequency, [_| Cycle]), Observation, seasonal_naive_state(Frequency, UpdatedCycle)) :-
		append(Cycle, [Observation], UpdatedCycle).
	updated_state(mean, mean_state(Sum, Count), Observation, mean_state(UpdatedSum, UpdatedCount)) :-
		(	var(Observation) ->
			UpdatedSum = Sum, UpdatedCount = Count
		;	UpdatedSum is Sum + Observation,
			UpdatedCount is Count + 1
		).
	updated_state(drift, drift_state(First, _), Observation, drift_state(First, Observation)).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	forecaster_data(Forecaster, Method, State, Diagnostics),
			valid_option(model(Method)),
			nonvar(State),
			ground(Diagnostics),
			valid_diagnostics(Method, State, Diagnostics) ->
			true
		;	domain_error(forecaster, Forecaster)
		).

	valid_diagnostics(Method, State, Diagnostics) :-
		^^valid_forecaster_metadata(baseline_forecasting, Options, Diagnostics),
		memberchk(method(Method), Diagnostics),
		memberchk(training_series_length(Length), Diagnostics),
		valid(positive_integer, Length),
		memberchk(observed_count(Count), Diagnostics),
		valid(positive_integer, Count),
		memberchk(missing_count(Missing), Diagnostics),
		valid(non_negative_integer, Missing),
		Count + Missing =:= Length,
		memberchk(update_count(Updates), Diagnostics),
		valid(non_negative_integer, Updates),
		Updates < Length,
		valid_state(Method, State, Length, Count, Missing, Options),
		minimum_length(Method, State, Minimum),
		Length - Updates >= Minimum.

	valid_state(naive, naive_state(Last), _Length, Count, Missing, Options) :-
		Options == [model(naive)],
		valid_retained_observations([Last], Count, Missing).
	valid_state(seasonal_naive, seasonal_naive_state(Frequency, Cycle), Length, Count, Missing, Options) :-
		valid(positive_integer, Frequency),
		Options == [model(seasonal_naive), frequency(Frequency)],
		valid(list(types([number, var])), Cycle),
		length(Cycle, Frequency),
		Length >= Frequency,
		valid_retained_observations(Cycle, Count, Missing).
	valid_state(mean, mean_state(Sum, StateCount), _Length, Count, _Missing, Options) :-
		Options == [model(mean)],
		number(Sum),
		StateCount == Count.
	valid_state(drift, drift_state(First, Last), Length, Count, Missing, Options) :-
		Options == [model(drift)],
		Length >= 2,
		valid_retained_observations([First, Last], Count, Missing).

	valid_retained_observations(Values, Count, Missing) :-
		valid(list(types([number, var])), Values),
		^^series_observation_summary(Values, Length, RetainedCount, _Sum),
		RetainedCount =< Count,
		Length - RetainedCount =< Missing.

	minimum_length(seasonal_naive, seasonal_naive_state(Frequency, _), Frequency) :-
		!.
	minimum_length(drift, _, 2) :-
		!.
	minimum_length(_, _, 1).

	forecaster_data(Forecaster, Method, State, Diagnostics) :-
		Forecaster =.. [_, Method, State, Diagnostics].

	forecaster_term_template(Forecaster, Template) :-
		functor(Forecaster, Functor, 3),
		Template =.. [Functor, 'Method', 'State', 'Diagnostics'].

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		context(Context),
		check(atom, Functor, Context),
		Clause =.. [Functor, Forecaster].

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		^^print_forecaster_template(Forecaster),
		forecaster_data(Forecaster, Method, State, Diagnostics),
		format('Method: ~w~nState: ~q~nDiagnostics: ~q~n', [Method, State, Diagnostics]).

	default_option(model(naive)).

	valid_option(model(Method)) :-
		once((Method == naive; Method == seasonal_naive; Method == mean; Method == drift)).
	valid_option(frequency(Frequency)) :-
		integer(Frequency), Frequency > 0.

:- end_object.

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
	implements(forecaster_protocol),
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
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

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one_or_error).
	:- info(learn/3, [
		comment is 'Learns a baseline forecaster. The default model is naive; seasonal naive uses an explicit frequency option ahead of dataset metadata. Unbound observations remain missing and retain their time positions.',
		argnames is ['Dataset', 'Forecaster', 'Options'],
		exceptions is [
			'``Options`` is a variable or partial list, or an option is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An option is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An option is invalid' - domain_error(option, 'Option'),
			'A frequency option is supplied for a non-seasonal model' - domain_error(baseline_forecasting_option, 'Option'),
			'The seasonal model has no explicit or dataset frequency' - domain_error(seasonal_frequency, 'Dataset'),
			'The dataset frequency or declared length is a variable' - instantiation_error,
			'The dataset frequency is neither a variable nor an integer' - type_error(integer, 'Frequency'),
			'The dataset frequency is not positive' - domain_error(positive_integer, 'Frequency'),
			'The declared length is neither a variable nor an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'The observation indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'The series is shorter than the model minimum' - domain_error(series_length, 'Dataset'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Numeric summation raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(learn/2).
	:- mode(learn(+object_identifier, -compound), one_or_error).
	:- info(learn/2, [
		comment is 'Learns a naive baseline forecaster using default options. Equivalent to learn/3 with an empty options list, propagating its dataset validation and numeric summation exceptions.',
		argnames is ['Dataset', 'Forecaster'],
		exceptions is [
			'The declared series length is a variable' - instantiation_error,
			'The declared length is neither a variable nor an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'The observation indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'An observation is neither a variable nor a number' - type_error(number, 'Observation'),
			'All observations are missing' - domain_error(insufficient_observations, 'Dataset'),
			'Numeric summation raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(forecast/3).
	:- mode(forecast(+compound, +non_negative_integer, -list(number)), one_or_error).
	:- info(forecast/3, [
		comment is 'Forecasts the next horizon values. A zero horizon returns an empty list even with missing state. A positive horizon requires only the values actually used by the selected model.',
		argnames is ['Forecaster', 'Horizon', 'Forecasts'],
		exceptions is [
			'``Forecaster`` or ``Horizon`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid baseline forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Horizon`` is neither a variable nor an integer' - type_error(integer, 'Horizon'),
			'``Horizon`` is negative' - domain_error(non_negative_integer, 'Horizon'),
			'A requested forecast needs a missing observation' - domain_error(missing_observation, 'Forecaster'),
			'Forecast arithmetic raises an evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(check_forecaster/1).
	:- mode(check_forecaster(@compound), one_or_error).
	:- info(check_forecaster/1, [
		comment is 'Checks model-specific state, effective options, and diagnostic count consistency without instantiating missing observations.',
		argnames is ['Forecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid baseline forecaster' - domain_error(forecaster, 'Forecaster'),
			'Checking retained numeric observations raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(export_to_clauses/4).
	:- mode(export_to_clauses(+object_identifier, +compound, +atom, -list(clause)), one_or_error).
	:- info(export_to_clauses/4, [
		comment is 'Exports the validated forecaster as a single fact with the specified functor.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'Clauses'],
		exceptions is [
			'``Forecaster`` or ``Functor`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid baseline forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Functor`` is neither a variable nor an atom' - type_error(atom, 'Functor'),
			'Checking retained numeric observations raises an arithmetic evaluation error' - evaluation_error('Error')
		]
	]).

	:- public(print_forecaster/1).
	:- mode(print_forecaster(+compound), one_or_error).
	:- info(print_forecaster/1, [
		comment is 'Prints the validated method, state, and diagnostics to the current output stream.',
		argnames is ['Forecaster'],
		exceptions is [
			'``Forecaster`` is a variable' - instantiation_error,
			'``Forecaster`` is not a valid baseline forecaster' - domain_error(forecaster, 'Forecaster'),
			'Checking retained numeric observations raises an arithmetic evaluation error' - evaluation_error('Error'),
			'The current stream cannot be written to' - permission_error(output, stream, 'Stream')
		]
	]).

	:- public(export_to_file/4).
	:- mode(export_to_file(+object_identifier, +compound, +atom, +atom), one_or_error).
	:- info(export_to_file/4, [
		comment is 'Exports a validated forecaster as a single fact, with dataset and diagnostics comments. The shared export implementation closes the output stream if writing raises an exception.',
		argnames is ['Dataset', 'Forecaster', 'Functor', 'File'],
		exceptions is [
			'``Forecaster``, ``Functor``, ``File``, or the declared dataset length is a variable' - instantiation_error,
			'``Forecaster`` is not a valid baseline forecaster' - domain_error(forecaster, 'Forecaster'),
			'``Functor`` is neither a variable nor an atom' - type_error(atom, 'Functor'),
			'``File`` is not a valid file specification' - type_error(source_sink, 'File'),
			'The output location does not exist' - existence_error(source_sink, 'File'),
			'The output file cannot be opened' - permission_error(open, source_sink, 'File'),
			'The output stream cannot be written to' - permission_error(output, stream, 'Stream'),
			'The dataset has no observations' - domain_error(non_empty_series, 'Dataset'),
			'The observation indices are not a gap-free 1-based sequence' - domain_error(series_index_sequence, 'Dataset'),
			'The declared length is neither a variable nor an integer' - type_error(integer, 'DeclaredLength'),
			'The declared length is not positive' - domain_error(positive_integer, 'DeclaredLength'),
			'The declared and observed lengths differ' - consistency_error(series_length, 'DeclaredLength', 'ObservedLength'),
			'Checking retained numeric observations raises an arithmetic evaluation error' - evaluation_error('Error')
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
		updated_diagnostics(Diagnostics, NewObservation, UpdatedDiagnostics).

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

	updated_diagnostics(Diagnostics, Observation, UpdatedDiagnostics) :-
		memberchk(training_series_length(Length0), Diagnostics),
		memberchk(update_count(Updates0), Diagnostics),
		memberchk(observed_count(Count0), Diagnostics),
		memberchk(missing_count(Missing0), Diagnostics),
		Length is Length0 + 1,
		Updates is Updates0 + 1,
		(	var(Observation) ->
			Count = Count0, Missing is Missing0 + 1
		;	Count is Count0 + 1, Missing = Missing0
		),
		^^replace_diagnostic(training_series_length, Length, Diagnostics, Diagnostics1),
		^^replace_diagnostic(update_count, Updates, Diagnostics1, Diagnostics2),
		^^replace_diagnostic(observed_count, Count, Diagnostics2, Diagnostics3),
		^^replace_diagnostic(missing_count, Missing, Diagnostics3, UpdatedDiagnostics).

	check_forecaster(Forecaster) :-
		(	var(Forecaster) ->
			instantiation_error
		;	(	Forecaster = baseline_forecaster(Method, State, Diagnostics),
				valid_option(model(Method)),
				nonvar(State),
				ground(Diagnostics),
				valid_diagnostics(Method, State, Diagnostics) ->
				true
			;	domain_error(forecaster, Forecaster)
			)
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

	forecaster_term_template(baseline_forecaster(_, _, _), baseline_forecaster('Method', 'State', 'Diagnostics')).

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
		Forecaster = baseline_forecaster(Method, State, Diagnostics),
		format('Method: ~w~nState: ~q~nDiagnostics: ~q~n', [Method, State, Diagnostics]).

	default_option(model(naive)).

	valid_option(model(Method)) :-
		once((Method == naive; Method == seasonal_naive; Method == mean; Method == drift)).
	valid_option(frequency(Frequency)) :-
		integer(Frequency), Frequency > 0.

:- end_object.

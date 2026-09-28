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


:- category(exponential_smoothing_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-27,
		comment is 'Shared fitting and forecasting predicates for exponential smoothing models.'
	]).

	:- uses(list, [
		append/3, last/2, length/2, nth1/3, take/4
	]).

	:- uses(numberlist, [
		linear_regression/4, sum/2
	]).

	:- protected(finite_number/1).
	:- mode(finite_number(+term), zero_or_one).
	:- info(finite_number/1, [
		comment is 'True when the argument is a finite integer or floating point number.',
		argnames is ['Number']
	]).

	finite_number(Number) :-
		number(Number),
		catch((Difference is Number - Number, Difference =:= 0), _Error, fail).

	:- protected(observed_series/3).
	:- mode(observed_series(+list, +term, -list(number)), one).
	:- info(observed_series/3, [
		comment is 'Returns the observations that are not identical to the configured missing marker.',
		argnames is ['Series', 'MissingMarker', 'ObservedSeries']
	]).

	observed_series([], _MissingMarker, []).
	observed_series([Value| Values], MissingMarker, ObservedSeries) :-
		(	Value == MissingMarker ->
			ObservedSeries = Rest
		;	ObservedSeries = [Value| Rest]
		),
		observed_series(Values, MissingMarker, Rest).

	:- protected(fit_smoothing/10).
	:- mode(fit_smoothing(+atom, +list, +term, +list(float), +compound, +term, -compound, -float, -non_negative_integer, -list(float)), one_or_error).
	:- info(fit_smoothing/10, [
		comment is 'Fits the selected exponential smoothing method using fixed parameters and the requested initialization strategy. Values identical to the missing marker advance predictions and seasonal phase without being scored or updating smoothing components.',
		argnames is ['Method', 'Series', 'Frequency', 'Parameters', 'InitializationSpecification', 'MissingMarker', 'State', 'SumSquaredError', 'ErrorCount', 'Residuals'],
		exceptions is [
			'The multiplicative Holt-Winters level update is not positive' - domain_error(positive_multiplicative_level, 'Level')
		]
	]).

	fit_smoothing(simple, Series, none, [Alpha], InitializationSpecification, MissingMarker, level(Level), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_simple_missing(InitializationSpecification, Series, MissingMarker, Values, Level0),
		fit_simple(Values, MissingMarker, Alpha, Level0, 0.0, 0, Level, SumSquaredError, ErrorCount, Residuals).
	fit_smoothing(holt, Series, none, [Alpha, Beta], InitializationSpecification, MissingMarker, holt(Level, Trend), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_holt_missing(InitializationSpecification, Series, MissingMarker, Values, Level0, Trend0),
		fit_holt(Values, MissingMarker, Alpha, Beta, Level0, Trend0, 0.0, 0, Level, Trend, SumSquaredError, ErrorCount, Residuals).
	fit_smoothing(holt_damped, Series, none, [Alpha, Beta, Phi], InitializationSpecification, MissingMarker, holt_damped(Level, Trend, Phi), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_holt_missing(InitializationSpecification, Series, MissingMarker, Values, Level0, Trend0),
		fit_holt_damped(Values, MissingMarker, Alpha, Beta, Phi, Level0, Trend0, 0.0, 0, Level, Trend, SumSquaredError, ErrorCount, Residuals).
	fit_smoothing(holt_winters_additive, Series, Frequency, [Alpha, Beta, Gamma], InitializationSpecification, MissingMarker, holt_winters(Level, Trend, Frequency, SeasonalQueue), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_seasonal_missing(InitializationSpecification, additive, Series, Frequency, MissingMarker, Remaining, Level0, Trend0, SeasonalFactors),
		deque::as_deque(SeasonalFactors, SeasonalDeque0),
		fit_seasonal(additive, Remaining, MissingMarker, Alpha, Beta, Gamma, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).
	fit_smoothing(holt_winters_multiplicative, Series, Frequency, [Alpha, Beta, Gamma], InitializationSpecification, MissingMarker, holt_winters(Level, Trend, Frequency, SeasonalQueue), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_seasonal_missing(InitializationSpecification, multiplicative, Series, Frequency, MissingMarker, Remaining, Level0, Trend0, SeasonalFactors),
		deque::as_deque(SeasonalFactors, SeasonalDeque0),
		fit_seasonal(multiplicative, Remaining, MissingMarker, Alpha, Beta, Gamma, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).
	fit_smoothing(holt_winters_additive_damped, Series, Frequency, [Alpha, Beta, Gamma, Phi], InitializationSpecification, MissingMarker, holt_winters_damped(Level, Trend, Phi, Frequency, SeasonalQueue), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_seasonal_missing(InitializationSpecification, additive, Series, Frequency, MissingMarker, Remaining, Level0, Trend0, SeasonalFactors),
		deque::as_deque(SeasonalFactors, SeasonalDeque0),
		fit_seasonal_damped(additive, Remaining, MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).
	fit_smoothing(holt_winters_multiplicative_damped, Series, Frequency, [Alpha, Beta, Gamma, Phi], InitializationSpecification, MissingMarker, holt_winters_damped(Level, Trend, Phi, Frequency, SeasonalQueue), SumSquaredError, ErrorCount, Residuals) :-
		!,
		initialize_seasonal_missing(InitializationSpecification, multiplicative, Series, Frequency, MissingMarker, Remaining, Level0, Trend0, SeasonalFactors),
		deque::as_deque(SeasonalFactors, SeasonalDeque0),
		fit_seasonal_damped(multiplicative, Remaining, MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).

	:- protected(update_smoothing/8).
	:- mode(update_smoothing(+atom, +compound, +list(float), +term, +term, -compound, -float, -list(float)), one_or_error).
	:- info(update_smoothing/8, [
		comment is 'Updates a learned smoothing state with one observation by applying the same fixed-parameter transition used during batch fitting. The returned residual list is empty for a skipped missing observation and a singleton for an observed value.',
		argnames is ['Method', 'State', 'Parameters', 'Observation', 'MissingMarker', 'UpdatedState', 'SumSquaredErrorIncrement', 'Residuals'],
		exceptions is [
			'The multiplicative Holt-Winters level update is not positive' - domain_error(positive_multiplicative_level, 'Level')
		]
	]).

	update_smoothing(simple, level(Level0), [Alpha], Observation, MissingMarker, level(Level), SumSquaredError, Residuals) :-
		fit_simple([Observation], MissingMarker, Alpha, Level0, 0.0, 0, Level, SumSquaredError, _ErrorCount, Residuals).
	update_smoothing(holt, holt(Level0, Trend0), [Alpha, Beta], Observation, MissingMarker, holt(Level, Trend), SumSquaredError, Residuals) :-
		fit_holt([Observation], MissingMarker, Alpha, Beta, Level0, Trend0, 0.0, 0, Level, Trend, SumSquaredError, _ErrorCount, Residuals).
	update_smoothing(holt_damped, holt_damped(Level0, Trend0, Phi), [Alpha, Beta, Phi], Observation, MissingMarker, holt_damped(Level, Trend, Phi), SumSquaredError, Residuals) :-
		fit_holt_damped([Observation], MissingMarker, Alpha, Beta, Phi, Level0, Trend0, 0.0, 0, Level, Trend, SumSquaredError, _ErrorCount, Residuals).
	update_smoothing(holt_winters_additive, holt_winters(Level0, Trend0, Frequency, SeasonalQueue0), [Alpha, Beta, Gamma], Observation, MissingMarker, holt_winters(Level, Trend, Frequency, SeasonalQueue), SumSquaredError, Residuals) :-
		deque::as_deque(SeasonalQueue0, SeasonalDeque0),
		fit_seasonal(additive, [Observation], MissingMarker, Alpha, Beta, Gamma, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, _ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).
	update_smoothing(holt_winters_multiplicative, holt_winters(Level0, Trend0, Frequency, SeasonalQueue0), [Alpha, Beta, Gamma], Observation, MissingMarker, holt_winters(Level, Trend, Frequency, SeasonalQueue), SumSquaredError, Residuals) :-
		deque::as_deque(SeasonalQueue0, SeasonalDeque0),
		fit_seasonal(multiplicative, [Observation], MissingMarker, Alpha, Beta, Gamma, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, _ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).
	update_smoothing(holt_winters_additive_damped, holt_winters_damped(Level0, Trend0, Phi, Frequency, SeasonalQueue0), [Alpha, Beta, Gamma, Phi], Observation, MissingMarker, holt_winters_damped(Level, Trend, Phi, Frequency, SeasonalQueue), SumSquaredError, Residuals) :-
		deque::as_deque(SeasonalQueue0, SeasonalDeque0),
		fit_seasonal_damped(additive, [Observation], MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, _ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).
	update_smoothing(holt_winters_multiplicative_damped, holt_winters_damped(Level0, Trend0, Phi, Frequency, SeasonalQueue0), [Alpha, Beta, Gamma, Phi], Observation, MissingMarker, holt_winters_damped(Level, Trend, Phi, Frequency, SeasonalQueue), SumSquaredError, Residuals) :-
		deque::as_deque(SeasonalQueue0, SeasonalDeque0),
		fit_seasonal_damped(multiplicative, [Observation], MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, Frequency, Level0, Trend0, SeasonalDeque0, 0.0, 0, Level, Trend, SeasonalDeque, SumSquaredError, _ErrorCount, Residuals),
		deque::as_list(SeasonalDeque, SeasonalQueue).

	:- protected(forecast_smoothing/4).
	:- mode(forecast_smoothing(+atom, +compound, +non_negative_integer, -list(number)), one_or_error).
	:- info(forecast_smoothing/4, [
		comment is 'Generates forecasts for the selected exponential smoothing method from a learned final state.',
		argnames is ['Method', 'State', 'Horizon', 'Forecasts'],
		exceptions is [
			'A multiplicative Holt-Winters forecast is not positive' - domain_error(positive_multiplicative_forecast, 'Forecast')
		]
	]).

	forecast_smoothing(simple, level(Level), Horizon, Forecasts) :-
		length(Forecasts, Horizon),
		repeat_value(Forecasts, Level).
	forecast_smoothing(holt, holt(Level, Trend), Horizon, Forecasts) :-
		forecast_holt(1, Horizon, Level, Trend, Forecasts).
	forecast_smoothing(holt_damped, holt_damped(Level, Trend, Phi), Horizon, Forecasts) :-
		forecast_holt_damped(1, Horizon, Level, Trend, Phi, Phi, Forecasts).
	forecast_smoothing(holt_winters_additive, holt_winters(Level, Trend, _Frequency, SeasonalQueue), Horizon, Forecasts) :-
		forecast_seasonal(additive, 1, Horizon, Level, Trend, SeasonalQueue, SeasonalQueue, Forecasts).
	forecast_smoothing(holt_winters_multiplicative, holt_winters(Level, Trend, _Frequency, SeasonalQueue), Horizon, Forecasts) :-
		forecast_seasonal(multiplicative, 1, Horizon, Level, Trend, SeasonalQueue, SeasonalQueue, Forecasts).
	forecast_smoothing(holt_winters_additive_damped, holt_winters_damped(Level, Trend, Phi, _Frequency, SeasonalQueue), Horizon, Forecasts) :-
		forecast_seasonal_damped(additive, 1, Horizon, Level, Trend, Phi, Phi, SeasonalQueue, SeasonalQueue, Forecasts).
	forecast_smoothing(holt_winters_multiplicative_damped, holt_winters_damped(Level, Trend, Phi, _Frequency, SeasonalQueue), Horizon, Forecasts) :-
		forecast_seasonal_damped(multiplicative, 1, Horizon, Level, Trend, Phi, Phi, SeasonalQueue, SeasonalQueue, Forecasts).

	:- protected(initial_parameter_point/2).
	:- mode(initial_parameter_point(+list(compound), -list(float)), one).
	:- info(initial_parameter_point/2, [
		comment is 'Builds the optimizer initial point from the parameters whose option value is ``auto``.',
		argnames is ['ParameterSpecification', 'Point']
	]).

	initial_parameter_point([], []).
	initial_parameter_point([Parameter| Parameters], Point) :-
		Parameter =.. [Name, Value],
		(	Value == auto ->
			initial_parameter_value(Name, InitialValue),
			Point = [InitialValue| Values]
		;	Point = Values
		),
		initial_parameter_point(Parameters, Values).

	:- protected(parameter_bounds/2).
	:- mode(parameter_bounds(+list(compound), -list(pair(float))), one).
	:- info(parameter_bounds/2, [
		comment is 'Builds closed unit-interval bounds for the parameters whose option value is ``auto``.',
		argnames is ['ParameterSpecification', 'Bounds']
	]).

	parameter_bounds([], []).
	parameter_bounds([Parameter| Parameters], Bounds) :-
		arg(1, Parameter, Value),
		(	Value == auto ->
			Bounds = [0.0-1.0| Rest]
		;	Bounds = Rest
		),
		parameter_bounds(Parameters, Rest).

	:- protected(parameters_from_point/3).
	:- mode(parameters_from_point(+list(compound), +list(float), -list(float)), one).
	:- info(parameters_from_point/3, [
		comment is 'Combines fixed parameter option values and an optimizer point into the canonical numeric parameter list.',
		argnames is ['ParameterSpecification', 'Point', 'Parameters']
	]).

	parameters_from_point([], [], []).
	parameters_from_point([Parameter| Specifications], Point, [Value| Parameters]) :-
		arg(1, Parameter, OptionValue),
		(	OptionValue == auto ->
			Point = [Value| Values]
		;	Value = OptionValue,
			Point = Values
		),
		parameters_from_point(Specifications, Values, Parameters).

	:- protected(optimization_initial_point/6).
	:- mode(optimization_initial_point(+atom, +list(number), +term, +list(compound), +compound, -list(float)), one).
	:- info(optimization_initial_point/6, [
		comment is 'Builds the joint optimizer initial point for automatic smoothing parameters and, when requested, the initial model state.',
		argnames is ['Method', 'Series', 'Frequency', 'ParameterSpecification', 'InitializationSpecification', 'Point']
	]).

	optimization_initial_point(Method, Series, Frequency, ParameterSpecification, InitializationSpecification, Point) :-
		initial_parameter_point(ParameterSpecification, ParameterPoint),
		initial_state_point(Method, Series, Frequency, InitializationSpecification, StatePoint),
		append(ParameterPoint, StatePoint, Point).

	:- protected(optimization_bounds/6).
	:- mode(optimization_bounds(+atom, +list(number), +term, +list(compound), +compound, -list(pair(float))), one).
	:- info(optimization_bounds/6, [
		comment is 'Builds finite data-derived bounds for the joint smoothing-parameter and initial-state optimization point.',
		argnames is ['Method', 'Series', 'Frequency', 'ParameterSpecification', 'InitializationSpecification', 'Bounds']
	]).

	optimization_bounds(Method, Series, Frequency, ParameterSpecification, InitializationSpecification, Bounds) :-
		parameter_bounds(ParameterSpecification, ParameterBounds),
		initial_state_bounds(Method, Series, Frequency, InitializationSpecification, StateBounds),
		append(ParameterBounds, StateBounds, Bounds).

	:- protected(optimization_components/8).
	:- mode(optimization_components(+atom, +list(number), +term, +list(compound), +compound, +list(float), -list(float), -compound), one).
	:- info(optimization_components/8, [
		comment is 'Decodes a joint optimization point into the canonical smoothing parameter list and effective initialization specification.',
		argnames is ['Method', 'Series', 'Frequency', 'ParameterSpecification', 'InitializationSpecification', 'Point', 'Parameters', 'EffectiveInitializationSpecification']
	]).

	optimization_components(Method, Series, Frequency, ParameterSpecification, InitializationSpecification, Point, Parameters, EffectiveInitializationSpecification) :-
		parameters_from_point(ParameterSpecification, Point, StatePoint, Parameters),
		initial_state_from_point(Method, Series, Frequency, InitializationSpecification, StatePoint, EffectiveInitializationSpecification).

	parameters_from_point([], Point, Point, []).
	parameters_from_point([Parameter| Specifications], Point0, Point, [Value| Parameters]) :-
		arg(1, Parameter, OptionValue),
		(	OptionValue == auto ->
			Point0 = [Value| Point1]
		;	Value = OptionValue,
			Point1 = Point0
		),
		parameters_from_point(Specifications, Point1, Point, Parameters).

	initial_parameter_value(alpha, 0.2).
	initial_parameter_value(beta, 0.1).
	initial_parameter_value(gamma, 0.1).
	initial_parameter_value(phi, 0.98).

	initial_state_point(_Method, _Series, _Frequency, initialization(Initialization, _InitialCycles), []) :-
		Initialization \== optimized,
		!.
	initial_state_point(simple, [First| _], none, initialization(optimized, _InitialCycles), [First]) :-
		!.
	initial_state_point(Method, [First, Second| _], none, initialization(optimized, _InitialCycles), [Second, Trend]) :-
		(Method == holt; Method == holt_damped),
		!,
		Trend is Second - First.
	initial_state_point(Method, Series, Frequency, initialization(optimized, InitialCycles), [Level, Trend| Coordinates]) :-
		seasonal_initialization_method(Method, SeasonalMethod),
		seasonal_initial_point(SeasonalMethod, Series, Frequency, InitialCycles, Level, Trend, Seasonal),
		seasonal_coordinates(SeasonalMethod, Seasonal, Coordinates).

	seasonal_initial_point(Method, Series, Frequency, InitialCycles, Level, Trend, Seasonal) :-
		initialize_seasonal_two_cycles(Method, Series, Frequency, _Remaining, _SecondCycleLevel, Trend, Seasonal),
		InitializationLength is InitialCycles * Frequency,
		take(InitializationLength, Series, InitialSeries, _),
		cycle_means(InitialSeries, Frequency, Means),
		last(Means, LastMean),
		initial_seasonal_level(Method, Frequency, LastMean, Trend, Level).

	seasonal_coordinates(additive, Seasonal, Coordinates) :-
		last(Seasonal, Anchor),
		seasonal_additive_coordinates(Seasonal, Anchor, Coordinates).
	seasonal_coordinates(multiplicative, Seasonal, Coordinates) :-
		last(Seasonal, Anchor),
		seasonal_multiplicative_coordinates(Seasonal, Anchor, Coordinates).

	seasonal_additive_coordinates([_Last], _Anchor, []) :-
		!.
	seasonal_additive_coordinates([Seasonal| Seasonals], Anchor, [Coordinate| Coordinates]) :-
		Coordinate is Seasonal - Anchor,
		seasonal_additive_coordinates(Seasonals, Anchor, Coordinates).

	seasonal_multiplicative_coordinates([_Last], _Anchor, []) :-
		!.
	seasonal_multiplicative_coordinates([Seasonal| Seasonals], Anchor, [Coordinate| Coordinates]) :-
		Coordinate is log(Seasonal / Anchor),
		seasonal_multiplicative_coordinates(Seasonals, Anchor, Coordinates).

	initial_state_bounds(_Method, _Series, _Frequency, initialization(Initialization, _InitialCycles), []) :-
		Initialization \== optimized,
		!.
	initial_state_bounds(Method, Series, Frequency, initialization(optimized, _InitialCycles), Bounds) :-
		series_range(Series, Minimum, Maximum, Scale),
		initial_level_bounds(Method, Minimum, Maximum, Scale, LevelBounds),
		initial_state_bounds_(Method, Frequency, Minimum, Maximum, Scale, LevelBounds, Bounds).

	initial_state_bounds_(simple, none, _Minimum, _Maximum, _Scale, LevelBounds, [LevelBounds]) :-
		!.
	initial_state_bounds_(Method, none, _Minimum, _Maximum, Scale, LevelBounds, [LevelBounds, TrendLower-TrendUpper]) :-
		(Method == holt; Method == holt_damped),
		!,
		TrendLower is -2.0 * Scale,
		TrendUpper is 2.0 * Scale.
	initial_state_bounds_(Method, Frequency, Minimum, Maximum, Scale, LevelBounds, [LevelBounds, TrendLower-TrendUpper| SeasonalBounds]) :-
		TrendLower is -2.0 * Scale,
		TrendUpper is 2.0 * Scale,
		seasonal_coordinate_bound(Method, Minimum, Maximum, Scale, SeasonalBound),
		CoordinateCount is Frequency - 1,
		repeat_bound(CoordinateCount, SeasonalBound, SeasonalBounds).

	initial_level_bounds(Method, Minimum, Maximum, _Scale, Lower-Upper) :-
		optimized_multiplicative_method(Method),
		!,
		Lower is Minimum / 10.0,
		Upper is Maximum * 10.0.
	initial_level_bounds(_Method, Minimum, Maximum, Scale, Lower-Upper) :-
		Lower is Minimum - Scale,
		Upper is Maximum + Scale.

	seasonal_coordinate_bound(Method, Minimum, Maximum, _Scale, Lower-Upper) :-
		optimized_multiplicative_method(Method),
		!,
		Extent is log(Maximum / Minimum) + log(10.0),
		Lower is -Extent,
		Upper is Extent.
	seasonal_coordinate_bound(_Method, _Minimum, _Maximum, Scale, Lower-Upper) :-
		Lower is -4.0 * Scale,
		Upper is 4.0 * Scale.

	series_range([First| Values], Minimum, Maximum, Scale) :-
		series_min_max(Values, First, First, Minimum, Maximum),
		Magnitude is max(abs(Minimum), abs(Maximum)),
		Scale is max(Maximum - Minimum, max(1.0, Magnitude) * 0.1).

	series_min_max([], Minimum, Maximum, Minimum, Maximum).
	series_min_max([Value| Values], Minimum0, Maximum0, Minimum, Maximum) :-
		Minimum1 is min(Minimum0, Value),
		Maximum1 is max(Maximum0, Value),
		series_min_max(Values, Minimum1, Maximum1, Minimum, Maximum).

	repeat_bound(0, _Bound, []) :-
		!.
	repeat_bound(Count, Bound, [Bound| Bounds]) :-
		Count1 is Count - 1,
		repeat_bound(Count1, Bound, Bounds).

	initial_state_from_point(_Method, _Series, _Frequency, initialization(Initialization, InitialCycles), [], initialization(Initialization, InitialCycles)) :-
		Initialization \== optimized,
		!.
	initial_state_from_point(simple, _Series, none, initialization(optimized, InitialCycles), [Level], initialization(optimized, InitialCycles, level(Level))) :-
		!.
	initial_state_from_point(Method, _Series, none, initialization(optimized, InitialCycles), [Level, Trend], initialization(optimized, InitialCycles, holt(Level, Trend))) :-
		(Method == holt; Method == holt_damped),
		!.
	initial_state_from_point(Method, _Series, Frequency, initialization(optimized, InitialCycles), [Level, Trend| Coordinates], initialization(optimized, InitialCycles, seasonal(Level, Trend, Seasonal))) :-
		seasonal_initialization_method(Method, SeasonalMethod),
		seasonal_factors_from_coordinates(SeasonalMethod, Frequency, Coordinates, Seasonal).

	seasonal_initialization_method(holt_winters_additive, additive).
	seasonal_initialization_method(holt_winters_additive_damped, additive).
	seasonal_initialization_method(holt_winters_multiplicative, multiplicative).
	seasonal_initialization_method(holt_winters_multiplicative_damped, multiplicative).

	optimized_multiplicative_method(holt_winters_multiplicative).
	optimized_multiplicative_method(holt_winters_multiplicative_damped).

	seasonal_factors_from_coordinates(additive, Frequency, Coordinates, Seasonal) :-
		CoordinateCount is Frequency - 1,
		length(Coordinates, CoordinateCount),
		append(Coordinates, [0.0], RawSeasonal),
		normalize_seasonal_factors(additive, RawSeasonal, Seasonal).
	seasonal_factors_from_coordinates(multiplicative, Frequency, Coordinates, Seasonal) :-
		CoordinateCount is Frequency - 1,
		length(Coordinates, CoordinateCount),
		exponentiate_coordinates(Coordinates, RawCoordinates),
		append(RawCoordinates, [1.0], RawSeasonal),
		normalize_seasonal_factors(multiplicative, RawSeasonal, Seasonal).

	exponentiate_coordinates([], []).
	exponentiate_coordinates([Coordinate| Coordinates], [Value| Values]) :-
		Value is exp(Coordinate),
		exponentiate_coordinates(Coordinates, Values).

	initialize_simple_missing(InitializationSpecification, Series, MissingMarker, Values, Level) :-
		\+ contains_missing(Series, MissingMarker),
		!,
		initialize_simple(InitializationSpecification, Series, Values, Level).
	initialize_simple_missing(initialization(two_cycles, _InitialCycles), Series, MissingMarker, Values, Level) :-
		!,
		first_known_with_tail(Series, MissingMarker, 1, _Index, Level, Values).
	initialize_simple_missing(initialization(regression, _InitialCycles), Series, MissingMarker, Values, Level) :-
		!,
		indexed_regression_parameters(Series, MissingMarker, Trend, Intercept),
		Level is Intercept + Trend,
		take(1, Series, _Initial, Values).
	initialize_simple_missing(initialization(optimized, _InitialCycles, level(Level)), Series, _MissingMarker, Values, Level) :-
		take(1, Series, _Initial, Values).

	initialize_holt_missing(InitializationSpecification, Series, MissingMarker, Values, Level, Trend) :-
		\+ contains_missing(Series, MissingMarker),
		!,
		initialize_holt(InitializationSpecification, Series, Values, Level, Trend).
	initialize_holt_missing(initialization(two_cycles, _InitialCycles), Series, MissingMarker, Values, Level, Trend) :-
		!,
		first_known_with_tail(Series, MissingMarker, 1, FirstIndex, First, AfterFirst),
		NextIndex is FirstIndex + 1,
		first_known_with_tail(AfterFirst, MissingMarker, NextIndex, SecondIndex, Level, Values),
		Trend is (Level - First) / (SecondIndex - FirstIndex).
	initialize_holt_missing(initialization(regression, _InitialCycles), Series, MissingMarker, Values, Level, Trend) :-
		!,
		indexed_regression_parameters(Series, MissingMarker, Trend, Intercept),
		Level is Intercept + 2.0 * Trend,
		take(2, Series, _Initial, Values).
	initialize_holt_missing(initialization(optimized, _InitialCycles, holt(Level, Trend)), Series, _MissingMarker, Values, Level, Trend) :-
		take(2, Series, _Initial, Values).

	initialize_seasonal_missing(InitializationSpecification, Method, Series, Frequency, MissingMarker, Remaining, Level, Trend, Seasonal) :-
		\+ contains_missing(Series, MissingMarker),
		!,
		initialize_seasonal(InitializationSpecification, Method, Series, Frequency, Remaining, Level, Trend, Seasonal).
	initialize_seasonal_missing(initialization(two_cycles, _InitialCycles), Method, Series, Frequency, MissingMarker, Remaining, Level, Trend, Seasonal) :-
		!,
		InitializationLength is 2 * Frequency,
		take(InitializationLength, Series, InitialSeries, Remaining),
		seasonal_missing_parameters(Method, InitialSeries, Frequency, MissingMarker, Level, Trend, Seasonal).
	initialize_seasonal_missing(initialization(regression, InitialCycles), Method, Series, Frequency, MissingMarker, Remaining, Level, Trend, Seasonal) :-
		!,
		InitializationLength is InitialCycles * Frequency,
		take(InitializationLength, Series, InitialSeries, Remaining),
		indexed_regression_parameters(InitialSeries, MissingMarker, Trend, Intercept),
		Level is Intercept + InitializationLength * Trend,
		initial_regression_factors_missing(Method, InitialSeries, Frequency, MissingMarker, Trend, Intercept, Seasonal0),
		normalize_seasonal_factors(Method, Seasonal0, Seasonal).
	initialize_seasonal_missing(initialization(optimized, InitialCycles, seasonal(Level, Trend, Seasonal)), _Method, Series, Frequency, _MissingMarker, Remaining, Level, Trend, Seasonal) :-
		InitializationLength is InitialCycles * Frequency,
		take(InitializationLength, Series, _InitialSeries, Remaining).

	contains_missing([Value| _Values], MissingMarker) :-
		Value == MissingMarker,
		!.
	contains_missing([_Value| Values], MissingMarker) :-
		contains_missing(Values, MissingMarker).

	first_known_with_tail([], _MissingMarker, _Index, _KnownIndex, _Value, _Tail) :-
		domain_error(insufficient_known_observations, nonseasonal).
	first_known_with_tail([Value| Values], MissingMarker, Index, KnownIndex, KnownValue, Tail) :-
		(	Value == MissingMarker ->
			NextIndex is Index + 1,
			first_known_with_tail(Values, MissingMarker, NextIndex, KnownIndex, KnownValue, Tail)
		;	KnownIndex = Index,
			KnownValue = Value,
			Tail = Values
		).

	indexed_regression_parameters(Series, MissingMarker, Trend, Intercept) :-
		known_indexed_values(Series, MissingMarker, 1, Indices, Values),
		(	Indices = [_First, _Second| _] ->
			linear_regression(Indices, Values, Trend, Intercept)
		;	domain_error(insufficient_known_observations, regression)
		).

	known_indexed_values([], _MissingMarker, _Index, [], []).
	known_indexed_values([Value| Values], MissingMarker, Index, Indices, KnownValues) :-
		NextIndex is Index + 1,
		(	Value == MissingMarker ->
			Indices = RestIndices,
			KnownValues = RestValues
		;	Indices = [Index| RestIndices],
			KnownValues = [Value| RestValues]
		),
		known_indexed_values(Values, MissingMarker, NextIndex, RestIndices, RestValues).

	seasonal_missing_parameters(Method, InitialSeries, Frequency, MissingMarker, Level, Trend, Seasonal) :-
		take(Frequency, InitialSeries, FirstCycle, SecondCycle),
		cycle_mean_missing(FirstCycle, MissingMarker, FirstMean),
		cycle_mean_missing(SecondCycle, MissingMarker, SecondMean),
		Trend is (SecondMean - FirstMean) / Frequency,
		initial_seasonal_level(Method, Frequency, SecondMean, Trend, Level),
		initial_missing_factors(1, Frequency, Method, FirstCycle, SecondCycle, MissingMarker, FirstMean, SecondMean, Trend, Seasonal0),
		normalize_seasonal_factors(Method, Seasonal0, Seasonal).

	cycle_mean_missing(Cycle, MissingMarker, Mean) :-
		known_sum_count(Cycle, MissingMarker, 0.0, 0, Sum, Count),
		(	Count > 0 ->
			Mean is Sum / Count
		;	domain_error(insufficient_known_observations, seasonal_cycle)
		).

	known_sum_count([], _MissingMarker, Sum, Count, Sum, Count).
	known_sum_count([Value| Values], MissingMarker, Sum0, Count0, Sum, Count) :-
		(	Value == MissingMarker ->
			Sum1 = Sum0,
			Count1 = Count0
		;	Sum1 is Sum0 + Value,
			Count1 is Count0 + 1
		),
		known_sum_count(Values, MissingMarker, Sum1, Count1, Sum, Count).

	initial_missing_factors(Phase, Frequency, _Method, _FirstCycle, _SecondCycle, _MissingMarker, _FirstMean, _SecondMean, _Trend, []) :-
		Phase > Frequency,
		!.
	initial_missing_factors(Phase, Frequency, Method, FirstCycle, SecondCycle, MissingMarker, FirstMean, SecondMean, Trend, [Factor| Factors]) :-
		nth1(Phase, FirstCycle, First),
		nth1(Phase, SecondCycle, Second),
		missing_phase_factor(Method, First, Second, MissingMarker, Phase, Frequency, FirstMean, SecondMean, Trend, Factor),
		NextPhase is Phase + 1,
		initial_missing_factors(NextPhase, Frequency, Method, FirstCycle, SecondCycle, MissingMarker, FirstMean, SecondMean, Trend, Factors).

	missing_phase_factor(Method, First, Second, MissingMarker, Phase, Frequency, FirstMean, SecondMean, Trend, Factor) :-
		phase_candidate(Method, First, MissingMarker, Phase, Frequency, FirstMean, Trend, FirstCandidate),
		phase_candidate(Method, Second, MissingMarker, Phase, Frequency, SecondMean, Trend, SecondCandidate),
		average_phase_candidates(FirstCandidate, SecondCandidate, Phase, Factor).

	phase_candidate(_Method, Value, MissingMarker, _Phase, _Frequency, _Mean, _Trend, none) :-
		Value == MissingMarker,
		!.
	phase_candidate(additive, Value, _MissingMarker, Phase, Frequency, Mean, Trend, value(Factor)) :-
		Midpoint is (Frequency + 1) / 2.0,
		Factor is Value - Mean - (Phase - Midpoint) * Trend.
	phase_candidate(multiplicative, Value, _MissingMarker, _Phase, _Frequency, Mean, _Trend, value(Factor)) :-
		Factor is Value / Mean.

	average_phase_candidates(none, none, Phase, _Factor) :-
		domain_error(insufficient_seasonal_phase_observations, Phase).
	average_phase_candidates(value(First), none, _Phase, First) :-
		!.
	average_phase_candidates(none, value(Second), _Phase, Second) :-
		!.
	average_phase_candidates(value(First), value(Second), _Phase, Factor) :-
		Factor is (First + Second) / 2.0.

	initial_regression_factors_missing(Method, Series, Frequency, MissingMarker, Trend, Intercept, Factors) :-
		initial_regression_factors_missing(1, Frequency, Method, Series, MissingMarker, Trend, Intercept, Factors).

	initial_regression_factors_missing(Phase, Frequency, _Method, _Series, _MissingMarker, _Trend, _Intercept, []) :-
		Phase > Frequency,
		!.
	initial_regression_factors_missing(Phase, Frequency, Method, Series, MissingMarker, Trend, Intercept, [Factor| Factors]) :-
		phase_factor_missing(Series, MissingMarker, 1, Phase, Frequency, Method, Trend, Intercept, 0.0, 0, Sum, Count),
		(	Count > 0 ->
			Factor is Sum / Count
		;	domain_error(insufficient_seasonal_phase_observations, Phase)
		),
		NextPhase is Phase + 1,
		initial_regression_factors_missing(NextPhase, Frequency, Method, Series, MissingMarker, Trend, Intercept, Factors).

	phase_factor_missing([], _MissingMarker, _Index, _Phase, _Frequency, _Method, _Trend, _Intercept, Sum, Count, Sum, Count).
	phase_factor_missing([Value| Values], MissingMarker, Index, Phase, Frequency, Method, Trend, Intercept, Sum0, Count0, Sum, Count) :-
		Position is (Index - 1) mod Frequency + 1,
		(	Position =:= Phase,
			Value \== MissingMarker ->
			Baseline is Intercept + Trend * Index,
			regression_seasonal_value(Method, Value, Baseline, SeasonalValue),
			Sum1 is Sum0 + SeasonalValue,
			Count1 is Count0 + 1
		;	Sum1 = Sum0,
			Count1 = Count0
		),
		NextIndex is Index + 1,
		phase_factor_missing(Values, MissingMarker, NextIndex, Phase, Frequency, Method, Trend, Intercept, Sum1, Count1, Sum, Count).

	initialize_simple(initialization(two_cycles, _InitialCycles), [First| Values], Values, First) :-
		!.
	initialize_simple(initialization(regression, _InitialCycles), [First| Values], Values, Level) :-
		regression_parameters([First| Values], Trend, Intercept),
		Level is Intercept + Trend.
	initialize_simple(initialization(optimized, _InitialCycles, level(Level)), [_First| Values], Values, Level).

	initialize_holt(initialization(two_cycles, _InitialCycles), [First, Second| Values], Values, Second, Trend) :-
		!,
		Trend is Second - First.
	initialize_holt(initialization(regression, _InitialCycles), [First, Second| Values], Values, Level, Trend) :-
		regression_parameters([First, Second| Values], Trend, Intercept),
		Level is Intercept + 2.0 * Trend.
	initialize_holt(initialization(optimized, _InitialCycles, holt(Level, Trend)), [_First, _Second| Values], Values, Level, Trend).

	regression_parameters(Series, Trend, Intercept) :-
		series_indices(Series, 1, Indices),
		linear_regression(Indices, Series, Trend, Intercept).

	series_indices([], _Index, []).
	series_indices([_Value| Values], Index, [Index| Indices]) :-
		NextIndex is Index + 1,
		series_indices(Values, NextIndex, Indices).

	fit_simple([], _MissingMarker, _Alpha, Level, SumSquaredError, ErrorCount, Level, SumSquaredError, ErrorCount, []).
	fit_simple([Value| Values], MissingMarker, Alpha, Level0, SumSquaredError0, ErrorCount0, Level, SumSquaredError, ErrorCount, Residuals) :-
		Value == MissingMarker,
		!,
		fit_simple(Values, MissingMarker, Alpha, Level0, SumSquaredError0, ErrorCount0, Level, SumSquaredError, ErrorCount, Residuals).
	fit_simple([Value| Values], MissingMarker, Alpha, Level0, SumSquaredError0, ErrorCount0, Level, SumSquaredError, ErrorCount, [Error| Residuals]) :-
		Error is Value - Level0,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		Level1 is Alpha * Value + (1.0 - Alpha) * Level0,
		ErrorCount1 is ErrorCount0 + 1,
		fit_simple(Values, MissingMarker, Alpha, Level1, SumSquaredError1, ErrorCount1, Level, SumSquaredError, ErrorCount, Residuals).

	fit_holt([], _MissingMarker, _Alpha, _Beta, Level, Trend, SumSquaredError, ErrorCount, Level, Trend, SumSquaredError, ErrorCount, []).
	fit_holt([Value| Values], MissingMarker, Alpha, Beta, Level0, Trend0, SumSquaredError0, ErrorCount0, Level, Trend, SumSquaredError, ErrorCount, Residuals) :-
		Value == MissingMarker,
		!,
		Level1 is Level0 + Trend0,
		fit_holt(Values, MissingMarker, Alpha, Beta, Level1, Trend0, SumSquaredError0, ErrorCount0, Level, Trend, SumSquaredError, ErrorCount, Residuals).
	fit_holt([Value| Values], MissingMarker, Alpha, Beta, Level0, Trend0, SumSquaredError0, ErrorCount0, Level, Trend, SumSquaredError, ErrorCount, [Error| Residuals]) :-
		Prediction is Level0 + Trend0,
		Error is Value - Prediction,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		Level1 is Alpha * Value + (1.0 - Alpha) * Prediction,
		Trend1 is Beta * (Level1 - Level0) + (1.0 - Beta) * Trend0,
		ErrorCount1 is ErrorCount0 + 1,
		fit_holt(Values, MissingMarker, Alpha, Beta, Level1, Trend1, SumSquaredError1, ErrorCount1, Level, Trend, SumSquaredError, ErrorCount, Residuals).

	fit_holt_damped([], _MissingMarker, _Alpha, _Beta, _Phi, Level, Trend, SumSquaredError, ErrorCount, Level, Trend, SumSquaredError, ErrorCount, []).
	fit_holt_damped([Value| Values], MissingMarker, Alpha, Beta, Phi, Level0, Trend0, SumSquaredError0, ErrorCount0, Level, Trend, SumSquaredError, ErrorCount, Residuals) :-
		Value == MissingMarker,
		!,
		Trend1 is Phi * Trend0,
		Level1 is Level0 + Trend1,
		fit_holt_damped(Values, MissingMarker, Alpha, Beta, Phi, Level1, Trend1, SumSquaredError0, ErrorCount0, Level, Trend, SumSquaredError, ErrorCount, Residuals).
	fit_holt_damped([Value| Values], MissingMarker, Alpha, Beta, Phi, Level0, Trend0, SumSquaredError0, ErrorCount0, Level, Trend, SumSquaredError, ErrorCount, [Error| Residuals]) :-
		Prediction is Level0 + Phi * Trend0,
		Error is Value - Prediction,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		Level1 is Alpha * Value + (1.0 - Alpha) * Prediction,
		Trend1 is Beta * (Level1 - Level0) + (1.0 - Beta) * Phi * Trend0,
		ErrorCount1 is ErrorCount0 + 1,
		fit_holt_damped(Values, MissingMarker, Alpha, Beta, Phi, Level1, Trend1, SumSquaredError1, ErrorCount1, Level, Trend, SumSquaredError, ErrorCount, Residuals).

	initialize_seasonal(initialization(two_cycles, _InitialCycles), Method, Series, Frequency, Remaining, Level, Trend, Seasonal) :-
		!,
		initialize_seasonal_two_cycles(Method, Series, Frequency, Remaining, Level, Trend, Seasonal).
	initialize_seasonal(initialization(regression, InitialCycles), Method, Series, Frequency, Remaining, Level, Trend, Seasonal) :-
		InitializationLength is InitialCycles * Frequency,
		take(InitializationLength, Series, InitialSeries, Remaining),
		seasonal_regression_parameters(InitialSeries, Frequency, Trend, Intercept),
		Level is Intercept + InitializationLength * Trend,
		initial_regression_factors(Method, InitialSeries, Frequency, Trend, Intercept, Seasonal0),
		normalize_seasonal_factors(Method, Seasonal0, Seasonal).
	initialize_seasonal(initialization(optimized, InitialCycles, seasonal(Level, Trend, Seasonal)), _Method, Series, Frequency, Remaining, Level, Trend, Seasonal) :-
		InitializationLength is InitialCycles * Frequency,
		take(InitializationLength, Series, _InitialSeries, Remaining).

	seasonal_regression_parameters(InitialSeries, Frequency, Trend, Intercept) :-
		cycle_means(InitialSeries, Frequency, Means),
		regression_parameters(Means, CycleTrend, CycleIntercept),
		Trend is CycleTrend / Frequency,
		Midpoint is (Frequency + 1) / 2.0,
		Intercept is CycleIntercept + Trend * (Frequency - Midpoint).

	cycle_means([], _Frequency, []) :-
		!.
	cycle_means(Series, Frequency, [Mean| Means]) :-
		take(Frequency, Series, Cycle, Rest),
		cycle_mean(Cycle, Frequency, Mean),
		cycle_means(Rest, Frequency, Means).

	initialize_seasonal_two_cycles(Method, Series, Frequency, Remaining, Level, Trend, Seasonal) :-
		take(Frequency, Series, FirstCycle, Rest0),
		take(Frequency, Rest0, SecondCycle, Remaining),
		cycle_mean(FirstCycle, Frequency, FirstMean),
		cycle_mean(SecondCycle, Frequency, SecondMean),
		Trend is (SecondMean - FirstMean) / Frequency,
		initial_seasonal_level(Method, Frequency, SecondMean, Trend, Level),
		initial_seasonal_factors(Method, FirstCycle, SecondCycle, Frequency, FirstMean, SecondMean, Trend, Seasonal).

	initial_regression_factors(Method, Series, Frequency, Trend, Intercept, Factors) :-
		initial_regression_factors(1, Frequency, Method, Series, Frequency, Trend, Intercept, Factors).

	initial_regression_factors(Phase, Frequency, _Method, _Series, _CycleLength, _Trend, _Intercept, []) :-
		Phase > Frequency,
		!.
	initial_regression_factors(Phase, Frequency, Method, Series, CycleLength, Trend, Intercept, [Factor| Factors]) :-
		phase_factor(Series, 1, Phase, CycleLength, Method, Trend, Intercept, 0.0, 0, Sum, Count),
		Factor is Sum / Count,
		NextPhase is Phase + 1,
		initial_regression_factors(NextPhase, Frequency, Method, Series, CycleLength, Trend, Intercept, Factors).

	phase_factor([], _Index, _Phase, _Frequency, _Method, _Trend, _Intercept, Sum, Count, Sum, Count).
	phase_factor([Value| Values], Index, Phase, Frequency, Method, Trend, Intercept, Sum0, Count0, Sum, Count) :-
		Position is (Index - 1) mod Frequency + 1,
		(	Position =:= Phase ->
			Baseline is Intercept + Trend * Index,
			regression_seasonal_value(Method, Value, Baseline, SeasonalValue),
			Sum1 is Sum0 + SeasonalValue,
			Count1 is Count0 + 1
		;	Sum1 = Sum0,
			Count1 = Count0
		),
		NextIndex is Index + 1,
		phase_factor(Values, NextIndex, Phase, Frequency, Method, Trend, Intercept, Sum1, Count1, Sum, Count).

	regression_seasonal_value(additive, Value, Baseline, SeasonalValue) :-
		SeasonalValue is Value - Baseline.
	regression_seasonal_value(multiplicative, Value, Baseline, SeasonalValue) :-
		(	Baseline > 0.0 ->
			SeasonalValue is Value / Baseline
		;	domain_error(positive_multiplicative_level, Baseline)
		).

	normalize_seasonal_factors(additive, Factors, Seasonal) :-
		length(Factors, Count),
		sum(Factors, Sum),
		Mean is Sum / Count,
		normalize_additive_factors(Factors, Mean, Seasonal).
	normalize_seasonal_factors(multiplicative, Factors, Seasonal) :-
		length(Factors, Count),
		sum(Factors, Sum),
		Mean is Sum / Count,
		normalize_multiplicative_factors(Factors, Mean, Seasonal).

	normalize_additive_factors([], _Mean, []).
	normalize_additive_factors([Factor| Factors], Mean, [Seasonal| Seasonals]) :-
		Seasonal is Factor - Mean,
		normalize_additive_factors(Factors, Mean, Seasonals).

	normalize_multiplicative_factors([], _Mean, []).
	normalize_multiplicative_factors([Factor| Factors], Mean, [Seasonal| Seasonals]) :-
		Seasonal is Factor / Mean,
		normalize_multiplicative_factors(Factors, Mean, Seasonals).

	cycle_mean(Cycle, Frequency, Mean) :-
		sum(Cycle, Sum),
		Mean is Sum / Frequency.

	initial_seasonal_level(additive, Frequency, SecondMean, Trend, Level) :-
		Level is SecondMean + (Frequency - 1) * Trend / 2.0.
	initial_seasonal_level(multiplicative, _Frequency, SecondMean, _Trend, SecondMean).

	initial_seasonal_factors(additive, FirstCycle, SecondCycle, Frequency, FirstMean, SecondMean, Trend, Seasonals) :-
		Midpoint is (Frequency + 1) / 2.0,
		initial_additive_factors(FirstCycle, SecondCycle, 1, Midpoint, FirstMean, SecondMean, Trend, Seasonals).
	initial_seasonal_factors(multiplicative, FirstCycle, SecondCycle, _Frequency, FirstMean, SecondMean, _Trend, Seasonals) :-
		initial_multiplicative_factors(FirstCycle, SecondCycle, FirstMean, SecondMean, Seasonals).

	initial_additive_factors([], [], _Position, _Midpoint, _FirstMean, _SecondMean, _Trend, []).
	initial_additive_factors([First| FirstCycle], [Second| SecondCycle], Position, Midpoint, FirstMean, SecondMean, Trend, [Seasonal| Seasonals]) :-
		PositionOffset is (Position - Midpoint) * Trend,
		Seasonal is ((First - FirstMean - PositionOffset) + (Second - SecondMean - PositionOffset)) / 2.0,
		Position1 is Position + 1,
		initial_additive_factors(FirstCycle, SecondCycle, Position1, Midpoint, FirstMean, SecondMean, Trend, Seasonals).

	initial_multiplicative_factors([], [], _FirstMean, _SecondMean, []).
	initial_multiplicative_factors([First| FirstCycle], [Second| SecondCycle], FirstMean, SecondMean, [Seasonal| Seasonals]) :-
		Seasonal is (First / FirstMean + Second / SecondMean) / 2.0,
		initial_multiplicative_factors(FirstCycle, SecondCycle, FirstMean, SecondMean, Seasonals).

	fit_seasonal(_Method, [], _MissingMarker, _Alpha, _Beta, _Gamma, _Frequency, _RemainingInCycle, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, []) :-
		!.
	fit_seasonal(Method, [Value| Values], MissingMarker, Alpha, Beta, Gamma, Frequency, RemainingInCycle0, Level0, Trend0, SeasonalDeque0, SumSquaredError0, ErrorCount0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals) :-
		deque::pop_front(SeasonalDeque0, Seasonal0, SeasonalDeque1),
		Value == MissingMarker,
		!,
		Level1 is Level0 + Trend0,
		deque::push_back(Seasonal0, SeasonalDeque1, SeasonalDeque2),
		advance_seasonal_queue(RemainingInCycle0, Frequency, SeasonalDeque2, RemainingInCycle, SeasonalDeque3),
		fit_seasonal(Method, Values, MissingMarker, Alpha, Beta, Gamma, Frequency, RemainingInCycle, Level1, Trend0, SeasonalDeque3, SumSquaredError0, ErrorCount0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals).
	fit_seasonal(Method, [Value| Values], MissingMarker, Alpha, Beta, Gamma, Frequency, RemainingInCycle0, Level0, Trend0, SeasonalDeque0, SumSquaredError0, ErrorCount0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, [Error| Residuals]) :-
		deque::pop_front(SeasonalDeque0, Seasonal0, SeasonalDeque1),
		seasonal_prediction(Method, Level0, Trend0, Seasonal0, Prediction),
		Error is Value - Prediction,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		seasonal_update(Method, Value, Alpha, Gamma, Level0, Trend0, Seasonal0, Level1, Seasonal1),
		Trend1 is Beta * (Level1 - Level0) + (1.0 - Beta) * Trend0,
		deque::push_back(Seasonal1, SeasonalDeque1, SeasonalDeque2),
		advance_seasonal_queue(RemainingInCycle0, Frequency, SeasonalDeque2, RemainingInCycle, SeasonalDeque3),
		ErrorCount1 is ErrorCount0 + 1,
		fit_seasonal(Method, Values, MissingMarker, Alpha, Beta, Gamma, Frequency, RemainingInCycle, Level1, Trend1, SeasonalDeque3, SumSquaredError1, ErrorCount1, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals).

	fit_seasonal_damped(_Method, [], _MissingMarker, _Alpha, _Beta, _Gamma, _Phi, _Frequency, _RemainingInCycle, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, []) :-
		!.
	fit_seasonal_damped(Method, [Value| Values], MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, RemainingInCycle0, Level0, Trend0, SeasonalDeque0, SumSquaredError0, ErrorCount0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals) :-
		deque::pop_front(SeasonalDeque0, Seasonal0, SeasonalDeque1),
		DampedTrend is Phi * Trend0,
		Value == MissingMarker,
		!,
		Level1 is Level0 + DampedTrend,
		deque::push_back(Seasonal0, SeasonalDeque1, SeasonalDeque2),
		advance_seasonal_queue(RemainingInCycle0, Frequency, SeasonalDeque2, RemainingInCycle, SeasonalDeque3),
		fit_seasonal_damped(Method, Values, MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, RemainingInCycle, Level1, DampedTrend, SeasonalDeque3, SumSquaredError0, ErrorCount0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals).
	fit_seasonal_damped(Method, [Value| Values], MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, RemainingInCycle0, Level0, Trend0, SeasonalDeque0, SumSquaredError0, ErrorCount0, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, [Error| Residuals]) :-
		deque::pop_front(SeasonalDeque0, Seasonal0, SeasonalDeque1),
		DampedTrend is Phi * Trend0,
		seasonal_prediction(Method, Level0, DampedTrend, Seasonal0, Prediction),
		Error is Value - Prediction,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		seasonal_update(Method, Value, Alpha, Gamma, Level0, DampedTrend, Seasonal0, Level1, Seasonal1),
		Trend1 is Beta * (Level1 - Level0) + (1.0 - Beta) * DampedTrend,
		deque::push_back(Seasonal1, SeasonalDeque1, SeasonalDeque2),
		advance_seasonal_queue(RemainingInCycle0, Frequency, SeasonalDeque2, RemainingInCycle, SeasonalDeque3),
		ErrorCount1 is ErrorCount0 + 1,
		fit_seasonal_damped(Method, Values, MissingMarker, Alpha, Beta, Gamma, Phi, Frequency, RemainingInCycle, Level1, Trend1, SeasonalDeque3, SumSquaredError1, ErrorCount1, Level, Trend, SeasonalDeque, SumSquaredError, ErrorCount, Residuals).

	advance_seasonal_queue(1, Frequency, SeasonalDeque0, Frequency, SeasonalDeque) :-
		!,
		deque::as_list(SeasonalDeque0, SeasonalFactors),
		deque::as_deque(SeasonalFactors, SeasonalDeque).
	advance_seasonal_queue(RemainingInCycle0, _Frequency, SeasonalDeque, RemainingInCycle, SeasonalDeque) :-
		RemainingInCycle is RemainingInCycle0 - 1.

	seasonal_prediction(additive, Level, Trend, Seasonal, Prediction) :-
		Prediction is Level + Trend + Seasonal.
	seasonal_prediction(multiplicative, Level, Trend, Seasonal, Prediction) :-
		Prediction is (Level + Trend) * Seasonal.

	seasonal_update(additive, Value, Alpha, Gamma, Level0, Trend0, Seasonal0, Level, Seasonal) :-
		Level is Alpha * (Value - Seasonal0) + (1.0 - Alpha) * (Level0 + Trend0),
		Seasonal is Gamma * (Value - Level) + (1.0 - Gamma) * Seasonal0.
	seasonal_update(multiplicative, Value, Alpha, Gamma, Level0, Trend0, Seasonal0, Level, Seasonal) :-
		Level is Alpha * (Value / Seasonal0) + (1.0 - Alpha) * (Level0 + Trend0),
		(	Level > 0.0 ->
			Seasonal is Gamma * (Value / Level) + (1.0 - Gamma) * Seasonal0
		;	domain_error(positive_multiplicative_level, Level)
		).

	repeat_value([], _Value).
	repeat_value([Value| Values], Value) :-
		repeat_value(Values, Value).

	forecast_holt(Step, Horizon, _Level, _Trend, []) :-
		Step > Horizon,
		!.
	forecast_holt(Step, Horizon, Level, Trend, [Forecast| Forecasts]) :-
		Forecast is Level + Step * Trend,
		Step1 is Step + 1,
		forecast_holt(Step1, Horizon, Level, Trend, Forecasts).

	forecast_holt_damped(Step, Horizon, _Level, _Trend, _Phi, _DampingSum, []) :-
		Step > Horizon,
		!.
	forecast_holt_damped(Step, Horizon, Level, Trend, Phi, DampingSum, [Forecast| Forecasts]) :-
		Forecast is Level + DampingSum * Trend,
		NextPower is Phi ** (Step + 1),
		NextDampingSum is DampingSum + NextPower,
		NextStep is Step + 1,
		forecast_holt_damped(NextStep, Horizon, Level, Trend, Phi, NextDampingSum, Forecasts).

	forecast_seasonal(_Method, Step, Horizon, _Level, _Trend, _Seasonals, _Cycle, []) :-
		Step > Horizon,
		!.
	forecast_seasonal(Method, Step, Horizon, Level, Trend, [], Cycle, Forecasts) :-
		!,
		forecast_seasonal(Method, Step, Horizon, Level, Trend, Cycle, Cycle, Forecasts).
	forecast_seasonal(Method, Step, Horizon, Level, Trend, [Seasonal| Seasonals], Cycle, [Forecast| Forecasts]) :-
		seasonal_horizon_forecast(Method, Level, Trend, Step, Seasonal, Forecast),
		Step1 is Step + 1,
		forecast_seasonal(Method, Step1, Horizon, Level, Trend, Seasonals, Cycle, Forecasts).

	forecast_seasonal_damped(_Method, Step, Horizon, _Level, _Trend, _Phi, _DampingSum, _Seasonals, _Cycle, []) :-
		Step > Horizon,
		!.
	forecast_seasonal_damped(Method, Step, Horizon, Level, Trend, Phi, DampingSum, [], Cycle, Forecasts) :-
		!,
		forecast_seasonal_damped(Method, Step, Horizon, Level, Trend, Phi, DampingSum, Cycle, Cycle, Forecasts).
	forecast_seasonal_damped(Method, Step, Horizon, Level, Trend, Phi, DampingSum, [Seasonal| Seasonals], Cycle, [Forecast| Forecasts]) :-
		DampedLevel is Level + DampingSum * Trend,
		seasonal_horizon_forecast(Method, DampedLevel, 0.0, 0, Seasonal, Forecast),
		NextPower is Phi ** (Step + 1),
		NextDampingSum is DampingSum + NextPower,
		NextStep is Step + 1,
		forecast_seasonal_damped(Method, NextStep, Horizon, Level, Trend, Phi, NextDampingSum, Seasonals, Cycle, Forecasts).

	seasonal_horizon_forecast(additive, Level, Trend, Step, Seasonal, Forecast) :-
		Forecast is Level + Step * Trend + Seasonal.
	seasonal_horizon_forecast(multiplicative, Level, Trend, Step, Seasonal, Forecast) :-
		Forecast0 is (Level + Step * Trend) * Seasonal,
		(	Forecast0 > 0.0 ->
			Forecast = Forecast0
		;	domain_error(positive_multiplicative_forecast, Forecast0)
		).

:- end_category.

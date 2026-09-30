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


:- object(knn_forecasting,
	imports(forecaster_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-29,
		comment is 'k-Nearest Neighbors (analog method) time series forecaster: matches the most recent window of observations against historical windows of the same length and predicts by aggregating what followed the most similar ones. Supports multiple distance metrics, neighbor weighting schemes, and optional differencing.',
		see_also is [exponential_smoothing, time_series_regression]
	]).

	:- public(update/4).
	:- mode(update(+compound, +number, -compound, +list(compound)), one_or_error).
	:- info(update/4, [
		comment is 'Returns a new forecaster after appending one observation to the series, keeping the memorized historical windows (and so the set of possible analogs) unchanged. The original forecaster is unchanged. The one-step prediction error of the new observation (predicted from the prior window using the same K nearest neighbors search used for forecasting) is added to the training error diagnostics. No update options are currently defined.',
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
		last/2, length/2, member/2, memberchk/2, reverse/2, take/3
	]).

	:- uses(numberlist, [
		chebyshev_distance/3, euclidean_distance/3, manhattan_distance/3, minkowski_distance/4
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	% learning

	learn(Dataset, Forecaster, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(order(Order), Options),
		^^option(k(K), Options),
		^^option(distance_metric(DistanceMetric), Options),
		^^option(minkowski_power(MinkowskiPower), Options),
		^^option(weight_scheme(WeightScheme), Options),
		^^option(differencing(Differencing), Options),
		^^dataset_series(Dataset, Series),
		^^check_series(Dataset, Series),
		% every candidate order requires more than Order observations to
		% form even a single row (see lagged_rows/3); requiring K + 1 rows
		% guarantees enough neighbors for both forecasting (K rows) and the
		% leave-one-out cross-validation computed below (K rows excluding
		% the one left out)
		MinimumLength is Differencing + Order + K + 1,
		^^check_series_length(Dataset, Series, MinimumLength),
		length(Series, TrainingSeriesLength),
		difference_levels(Differencing, Series, Levels, DifferencedSeries),
		^^lagged_rows(DifferencedSeries, Order, Rows),
		length(Rows, RowCount),
		initial_window(DifferencedSeries, Order, Window),
		leave_one_out(Rows, K, DistanceMetric, MinkowskiPower, WeightScheme, Actual, Predicted),
		^^mean_absolute_error(Actual, Predicted, MeanAbsoluteError),
		^^root_mean_squared_error(Actual, Predicted, RootMeanSquaredError),
		SumAbsoluteError is MeanAbsoluteError * RowCount,
		MeanSquaredError is RootMeanSquaredError * RootMeanSquaredError,
		SumSquaredError is MeanSquaredError * RowCount,
		^^base_forecaster_diagnostics(
			knn_forecasting, TrainingSeriesLength, Options,
			[
				order(Order),
				differencing(Differencing),
				k(K),
				distance_metric(DistanceMetric),
				minkowski_power(MinkowskiPower),
				weight_scheme(WeightScheme),
				scored_count(RowCount),
				sum_squared_error(SumSquaredError),
				mean_squared_error(MeanSquaredError),
				sum_absolute_error(SumAbsoluteError),
				mean_absolute_error(MeanAbsoluteError),
				update_count(0)
			],
			Diagnostics
		),
		Forecaster = knn_forecaster(
			knn(Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme),
			knn_state(Window, Levels),
			Rows,
			Diagnostics
		).

	% differencing; the levels list holds the last value of the series at
	% each differencing level, starting with the original series (same
	% construction as in the time_series_regression library)

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
		take(Order, Reversed, Window).

	% leave-one-out cross-validation over the memorized rows, used to seed
	% the training error diagnostics; each row's target is predicted from
	% the K nearest of the OTHER rows only, excluded by position so that
	% rows with identical lag values are not accidentally also excluded

	leave_one_out(Rows, K, DistanceMetric, MinkowskiPower, WeightScheme, Actual, Predicted) :-
		leave_one_out(Rows, 1, Rows, K, DistanceMetric, MinkowskiPower, WeightScheme, Actual, Predicted).

	leave_one_out([], _, _, _, _, _, _, [], []).
	leave_one_out([Lags-Target| Rest], Index, AllRows, K, DistanceMetric, MinkowskiPower, WeightScheme, [Target| Actual], [Prediction| Predicted]) :-
		exclude_index(AllRows, Index, CandidateRows),
		find_k_nearest(Lags, CandidateRows, K, DistanceMetric, MinkowskiPower, Neighbors),
		predict_from_neighbors(Neighbors, WeightScheme, Prediction),
		Index1 is Index + 1,
		leave_one_out(Rest, Index1, AllRows, K, DistanceMetric, MinkowskiPower, WeightScheme, Actual, Predicted).

	exclude_index(Rows, Index, Excluded) :-
		exclude_index(Rows, 1, Index, Excluded).

	exclude_index([], _, _, []).
	exclude_index([Row| Rows], Position, Index, Excluded) :-
		(	Position =:= Index ->
			Excluded = Rest
		;	Excluded = [Row| Rest]
		),
		Position1 is Position + 1,
		exclude_index(Rows, Position1, Index, Rest).

	% neighbor search and aggregation

	find_k_nearest(Lags, Rows, K, DistanceMetric, MinkowskiPower, Neighbors) :-
		findall(
			Distance-Target,
			(	member(NeighborLags-Target, Rows),
				compute_distance(Lags, NeighborLags, DistanceMetric, MinkowskiPower, Distance)
			),
			Distances
		),
		keysort(Distances, SortedDistances),
		take(K, SortedDistances, Neighbors).

	compute_distance(Lags1, Lags2, DistanceMetric, MinkowskiPower, Distance) :-
		(	DistanceMetric == euclidean ->
			euclidean_distance(Lags1, Lags2, Distance)
		;	DistanceMetric == manhattan ->
			manhattan_distance(Lags1, Lags2, Distance)
		;	DistanceMetric == chebyshev ->
			chebyshev_distance(Lags1, Lags2, Distance)
		;	minkowski_distance(Lags1, Lags2, MinkowskiPower, Distance)
		).

	predict_from_neighbors(Neighbors, WeightScheme, Prediction) :-
		apply_weighting(WeightScheme, Neighbors, WeightedNeighbors),
		weighted_average_target(WeightedNeighbors, 0.0, 0.0, Prediction).

	apply_weighting(uniform, Neighbors, WeightedNeighbors) :-
		uniform_weights(Neighbors, WeightedNeighbors).
	apply_weighting(distance, Neighbors, WeightedNeighbors) :-
		distance_weights(Neighbors, WeightedNeighbors).
	apply_weighting(gaussian, Neighbors, WeightedNeighbors) :-
		gaussian_weights(Neighbors, WeightedNeighbors).

	uniform_weights([], []).
	uniform_weights([_Distance-Target| Neighbors], [1.0-Target| WeightedNeighbors]) :-
		uniform_weights(Neighbors, WeightedNeighbors).

	distance_weights([], []).
	distance_weights([Distance-Target| Neighbors], [Weight-Target| WeightedNeighbors]) :-
		(	Distance =< 1.0e-12 ->
			Weight = 1.0e10
		;	Weight is 1.0 / Distance
		),
		distance_weights(Neighbors, WeightedNeighbors).

	gaussian_weights([], []).
	gaussian_weights([Distance-Target| Neighbors], [Weight-Target| WeightedNeighbors]) :-
		Sigma = 1.0,
		Weight is exp(-(Distance * Distance) / (2.0 * Sigma * Sigma)),
		gaussian_weights(Neighbors, WeightedNeighbors).

	weighted_average_target(WeightedNeighbors, WeightedSum0, TotalWeight0, Target) :-
		accumulate_weighted_targets(WeightedNeighbors, WeightedSum0, WeightedSum, TotalWeight0, TotalWeight),
		(	TotalWeight =< 1.0e-12 ->
			Target = 0.0
		;	Target is WeightedSum / TotalWeight
		).

	accumulate_weighted_targets([], WeightedSum, WeightedSum, TotalWeight, TotalWeight).
	accumulate_weighted_targets([Weight-Value| WeightedNeighbors], WeightedSum0, WeightedSum, TotalWeight0, TotalWeight) :-
		WeightedSum1 is WeightedSum0 + Weight * Value,
		TotalWeight1 is TotalWeight0 + Weight,
		accumulate_weighted_targets(WeightedNeighbors, WeightedSum1, WeightedSum, TotalWeight1, TotalWeight).

	% forecasting

	forecast(Forecaster, Horizon, Forecasts) :-
		check_forecaster(Forecaster),
		^^check_forecast_horizon(Horizon),
		Forecaster = knn_forecaster(knn(_Order, _Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme), knn_state(Window, Levels), Rows, _Diagnostics),
		project(Horizon, Rows, K, DistanceMetric, MinkowskiPower, WeightScheme, Window, DifferencedForecasts),
		reverse(Levels, ReversedLevels),
		integrate_levels(ReversedLevels, DifferencedForecasts, Forecasts).

	project(0, _, _, _, _, _, _, []) :-
		!.
	project(Horizon, Rows, K, DistanceMetric, MinkowskiPower, WeightScheme, Window, [Prediction| Predictions]) :-
		find_k_nearest(Window, Rows, K, DistanceMetric, MinkowskiPower, Neighbors),
		predict_from_neighbors(Neighbors, WeightScheme, Prediction),
		push_window(Window, Prediction, NextWindow),
		Horizon1 is Horizon - 1,
		project(Horizon1, Rows, K, DistanceMetric, MinkowskiPower, WeightScheme, NextWindow, Predictions).

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

	% online updates

	update(Forecaster, Observation, UpdatedForecaster, Options) :-
		check_forecaster(Forecaster),
		check_observation(Observation),
		check_update_options(Options),
		Forecaster = knn_forecaster(Model, knn_state(Window, Levels), Rows, Diagnostics),
		Model = knn(_Order, _Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme),
		update_levels(Levels, Observation, UpdatedLevels, DifferencedObservation),
		find_k_nearest(Window, Rows, K, DistanceMetric, MinkowskiPower, Neighbors),
		predict_from_neighbors(Neighbors, WeightScheme, Prediction),
		Residual is DifferencedObservation - Prediction,
		push_window(Window, DifferencedObservation, UpdatedWindow),
		updated_diagnostics(Diagnostics, Residual, UpdatedDiagnostics),
		UpdatedForecaster = knn_forecaster(Model, knn_state(UpdatedWindow, UpdatedLevels), Rows, UpdatedDiagnostics).

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
		(	\+ ground(Option) ->
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
		memberchk(sum_absolute_error(SumAbsoluteError0), Diagnostics),
		memberchk(update_count(UpdateCount0), Diagnostics),
		TrainingSeriesLength is TrainingSeriesLength0 + 1,
		ScoredCount is ScoredCount0 + 1,
		SumSquaredError is SumSquaredError0 + Residual * Residual,
		MeanSquaredError is SumSquaredError / ScoredCount,
		AbsoluteResidual is abs(Residual),
		SumAbsoluteError is SumAbsoluteError0 + AbsoluteResidual,
		MeanAbsoluteError is SumAbsoluteError / ScoredCount,
		UpdateCount is UpdateCount0 + 1,
		replace_diagnostic(training_series_length, TrainingSeriesLength, Diagnostics, Diagnostics1),
		replace_diagnostic(scored_count, ScoredCount, Diagnostics1, Diagnostics2),
		replace_diagnostic(sum_squared_error, SumSquaredError, Diagnostics2, Diagnostics3),
		replace_diagnostic(mean_squared_error, MeanSquaredError, Diagnostics3, Diagnostics4),
		replace_diagnostic(sum_absolute_error, SumAbsoluteError, Diagnostics4, Diagnostics5),
		replace_diagnostic(mean_absolute_error, MeanAbsoluteError, Diagnostics5, Diagnostics6),
		replace_diagnostic(update_count, UpdateCount, Diagnostics6, UpdatedDiagnostics).

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
		;	(	Forecaster = knn_forecaster(Model, State, Rows, Diagnostics),
				valid_model(Model, Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme),
				valid_state(Order, Differencing, State),
				valid_rows(Order, K, Rows),
				valid_diagnostics(Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme, Diagnostics) ->
				true
			;	domain_error(forecaster, Forecaster)
			)
		).

	valid_model(knn(Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme), Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme) :-
		valid_option(order(Order)),
		valid_option(differencing(Differencing)),
		valid_option(k(K)),
		valid_option(distance_metric(DistanceMetric)),
		valid_option(minkowski_power(MinkowskiPower)),
		valid_option(weight_scheme(WeightScheme)).

	valid_state(Order, Differencing, knn_state(Window, Levels)) :-
		valid(list(number), Window),
		length(Window, Order),
		valid(list(number), Levels),
		length(Levels, Differencing).

	valid_rows(_Order, _K, []) :-
		!,
		fail.
	valid_rows(Order, K, Rows) :-
		length(Rows, RowCount),
		RowCount > K,
		valid_rows_(Rows, Order).

	valid_rows_([], _).
	valid_rows_([Lags-Target| Rows], Order) :-
		valid(list(number), Lags),
		length(Lags, Order),
		number(Target),
		valid_rows_(Rows, Order).

	valid_diagnostics(Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme, Diagnostics) :-
		^^valid_forecaster_metadata(knn_forecasting, _Options, Diagnostics),
		memberchk(order(Order), Diagnostics),
		memberchk(differencing(Differencing), Diagnostics),
		memberchk(k(K), Diagnostics),
		memberchk(distance_metric(DistanceMetric), Diagnostics),
		memberchk(minkowski_power(MinkowskiPower), Diagnostics),
		memberchk(weight_scheme(WeightScheme), Diagnostics),
		memberchk(training_series_length(TrainingSeriesLength), Diagnostics),
		integer(TrainingSeriesLength),
		TrainingSeriesLength > 0,
		memberchk(scored_count(ScoredCount), Diagnostics),
		integer(ScoredCount),
		ScoredCount > 0,
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		number(SumSquaredError),
		SumSquaredError >= 0,
		memberchk(mean_squared_error(MeanSquaredError), Diagnostics),
		number(MeanSquaredError),
		MeanSquaredError >= 0,
		memberchk(sum_absolute_error(SumAbsoluteError), Diagnostics),
		number(SumAbsoluteError),
		SumAbsoluteError >= 0,
		memberchk(mean_absolute_error(MeanAbsoluteError), Diagnostics),
		number(MeanAbsoluteError),
		MeanAbsoluteError >= 0,
		memberchk(update_count(UpdateCount), Diagnostics),
		integer(UpdateCount),
		UpdateCount >= 0.

	forecaster_export_template(_Dataset, _Forecaster, Functor, Template) :-
		Template =.. [Functor, 'Forecaster'].

	forecaster_term_template(
		knn_forecaster(_Model, _State, _Rows, _Diagnostics),
		knn_forecaster('Model', 'State', 'Rows', 'Diagnostics')
	).

	export_to_clauses(_Dataset, Forecaster, Functor, [Clause]) :-
		check_forecaster(Forecaster),
		Clause =.. [Functor, Forecaster].

	print_forecaster(Forecaster) :-
		check_forecaster(Forecaster),
		Forecaster = knn_forecaster(knn(Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme), State, Rows, Diagnostics),
		^^print_forecaster_template(Forecaster),
		format('Order: ~w~n', [Order]),
		format('Differencing: ~w~n', [Differencing]),
		format('K: ~w~n', [K]),
		format('Distance metric: ~w~n', [DistanceMetric]),
		(	DistanceMetric == minkowski ->
			format('Minkowski power: ~w~n', [MinkowskiPower])
		;	true
		),
		format('Weight scheme: ~w~n', [WeightScheme]),
		format('State: ~w~n', [State]),
		length(Rows, RowCount),
		format('Memorized rows: ~w~n', [RowCount]),
		memberchk(mean_absolute_error(MeanAbsoluteError), Diagnostics),
		memberchk(mean_squared_error(MeanSquaredError), Diagnostics),
		format('Mean absolute error: ~w~n', [MeanAbsoluteError]),
		format('Mean squared error: ~w~n', [MeanSquaredError]).

	% options

	default_option(order(3)).
	default_option(k(3)).
	default_option(distance_metric(euclidean)).
	default_option(minkowski_power(3.0)).
	default_option(weight_scheme(uniform)).
	default_option(differencing(0)).

	valid_option(order(Order)) :-
		integer(Order),
		Order > 0.
	valid_option(k(K)) :-
		integer(K),
		K > 0.
	valid_option(distance_metric(DistanceMetric)) :-
		once((DistanceMetric == euclidean; DistanceMetric == manhattan; DistanceMetric == chebyshev; DistanceMetric == minkowski)).
	valid_option(minkowski_power(MinkowskiPower)) :-
		number(MinkowskiPower),
		MinkowskiPower >= 1.0.
	valid_option(weight_scheme(WeightScheme)) :-
		once((WeightScheme == uniform; WeightScheme == distance; WeightScheme == gaussian)).
	valid_option(differencing(Differencing)) :-
		integer(Differencing),
		Differencing >= 0.

:- end_object.

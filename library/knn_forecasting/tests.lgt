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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Tests for the "knn_forecasting" library. Reference values for the noisy_pattern_series dataset were computed independently using a Python re-implementation of the nearest-neighbor search and weighting schemes.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(knn_forecasting).

	cleanup :-
		^^clean_file('test_output.pl').

	% exact recovery on periodic and seasonal patterns

	test(knn_periodic_1nn_forecast, deterministic(Forecasts =~= [1.0, 2.0, 3.0, 10.0, 1.0])) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, 5, Forecasts).

	test(knn_periodic_1nn_perfect_fit, true) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::diagnostic(Forecaster, mean_absolute_error(MeanAbsoluteError)),
		knn_forecasting::diagnostic(Forecaster, mean_squared_error(MeanSquaredError)),
		assertion(MeanAbsoluteError =~= 0.0),
		assertion(MeanSquaredError =~= 0.0).

	test(knn_periodic_2nn_uniform, deterministic(Forecasts =~= [1.0])) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(2), weight_scheme(uniform)]),
		knn_forecasting::forecast(Forecaster, 1, Forecasts).

	test(knn_seasonal_forecast, deterministic(Forecasts =~= [10.0, 20.0, 15.0, 5.0])) :-
		knn_forecasting::learn(seasonal_series, Forecaster, [order(4), k(1)]),
		knn_forecasting::forecast(Forecaster, 4, Forecasts).

	% differencing

	test(knn_differencing_1_linear_trend, deterministic(Forecasts =~= [22.0, 24.0, 26.0])) :-
		knn_forecasting::learn(linear_trend, Forecaster, [order(2), k(1), differencing(1)]),
		knn_forecasting::forecast(Forecaster, 3, Forecasts).

	test(knn_no_differencing_by_default, true(Differencing == 0)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::diagnostic(Forecaster, differencing(Differencing)).

	% distance metrics and weighting schemes (cross-checked independently)

	test(knn_euclidean_distance_weighted, deterministic(Forecasts =~= [5.76295471315825, 8.919527903694872, 6.772501394662145, 3.5308409760469703])) :-
		knn_forecasting::learn(noisy_pattern_series, Forecaster, [order(8), k(3), distance_metric(euclidean), weight_scheme(distance)]),
		knn_forecasting::forecast(Forecaster, 4, Forecasts).

	test(knn_manhattan_gaussian_weighted, deterministic(Forecasts =~= [5.729899999999946, 9.124399999999898, 7.299899999999926])) :-
		knn_forecasting::learn(noisy_pattern_series, Forecaster, [order(8), k(2), distance_metric(manhattan), weight_scheme(gaussian)]),
		knn_forecasting::forecast(Forecaster, 3, Forecasts).

	test(knn_chebyshev_uniform_weighted, deterministic(Forecasts =~= [5.958333333333333, 8.608033333333333, 5.802])) :-
		knn_forecasting::learn(noisy_pattern_series, Forecaster, [order(5), k(3), distance_metric(chebyshev), weight_scheme(uniform)]),
		knn_forecasting::forecast(Forecaster, 3, Forecasts).

	test(knn_minkowski_uniform_weighted, deterministic(Forecasts =~= [5.4748, 8.465399999999999, 6.6598500000000005])) :-
		knn_forecasting::learn(noisy_pattern_series, Forecaster, [order(6), k(2), distance_metric(minkowski), minkowski_power(4.0), weight_scheme(uniform)]),
		knn_forecasting::forecast(Forecaster, 3, Forecasts).

	test(knn_leave_one_out_diagnostics, true) :-
		knn_forecasting::learn(noisy_pattern_series, Forecaster, [order(8), k(3), distance_metric(euclidean), weight_scheme(distance)]),
		knn_forecasting::diagnostic(Forecaster, mean_absolute_error(MeanAbsoluteError)),
		knn_forecasting::diagnostic(Forecaster, mean_squared_error(MeanSquaredError)),
		RootMeanSquaredError is sqrt(MeanSquaredError),
		assertion(MeanAbsoluteError =~= 0.9758538017255322),
		assertion(RootMeanSquaredError =~= 1.18835369439477),
		assertion(knn_forecasting::diagnostic(Forecaster, scored_count(16))).

	% options

	test(knn_default_options, deterministic) :-
		knn_forecasting::learn(noisy_pattern_series, Forecaster),
		knn_forecasting::forecaster_options(Forecaster, Options),
		memberchk(order(3), Options),
		memberchk(k(3), Options),
		memberchk(distance_metric(euclidean), Options),
		memberchk(minkowski_power(3.0), Options),
		memberchk(weight_scheme(uniform), Options),
		memberchk(differencing(0), Options).

	test(knn_invalid_order_zero, error(domain_error(option, order(0)))) :-
		knn_forecasting::learn(periodic_series, _, [order(0)]).

	test(knn_invalid_order_atom, error(domain_error(option, order(foo)))) :-
		knn_forecasting::learn(periodic_series, _, [order(foo)]).

	test(knn_invalid_k_zero, error(domain_error(option, k(0)))) :-
		knn_forecasting::learn(periodic_series, _, [k(0)]).

	test(knn_invalid_distance_metric, error(domain_error(option, distance_metric(cosine)))) :-
		knn_forecasting::learn(periodic_series, _, [distance_metric(cosine)]).

	test(knn_invalid_minkowski_power, error(domain_error(option, minkowski_power(0.5)))) :-
		knn_forecasting::learn(periodic_series, _, [minkowski_power(0.5)]).

	test(knn_invalid_weight_scheme, error(domain_error(option, weight_scheme(linear)))) :-
		knn_forecasting::learn(periodic_series, _, [weight_scheme(linear)]).

	test(knn_invalid_differencing, error(domain_error(option, differencing(-1)))) :-
		knn_forecasting::learn(periodic_series, _, [differencing(-1)]).

	test(knn_unknown_option, error(domain_error(option, bogus(1)))) :-
		knn_forecasting::learn(periodic_series, _, [bogus(1)]).

	% invalid or insufficient datasets

	test(knn_short_series, error(domain_error(series_length, short_series))) :-
		knn_forecasting::learn(short_series, _).

	test(knn_k_too_large_for_series, error(domain_error(series_length, periodic_series))) :-
		knn_forecasting::learn(periodic_series, _, [order(3), k(20)]).

	test(knn_gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		knn_forecasting::learn(gap_index, _).

	test(knn_non_numeric_value, error(domain_error(types([number,var]), bad))) :-
		knn_forecasting::learn(non_numeric_value, _).

	% missing observations

	test(knn_missing_interior_diagnostics, true) :-
		knn_forecasting::learn(periodic_series_missing_interior, Forecaster, [order(3), k(1)]),
		knn_forecasting::diagnostic(Forecaster, missing_count(1)),
		knn_forecasting::diagnostic(Forecaster, scored_count(9)).

	test(knn_missing_interior_perfect_fit, true) :-
		knn_forecasting::learn(periodic_series_missing_interior, Forecaster, [order(3), k(1)]),
		knn_forecasting::diagnostic(Forecaster, mean_absolute_error(MeanAbsoluteError)),
		knn_forecasting::diagnostic(Forecaster, mean_squared_error(MeanSquaredError)),
		MeanAbsoluteError =~= 0.0,
		MeanSquaredError =~= 0.0.

	test(knn_missing_interior_forecast, deterministic(Forecasts =~= [1.0, 2.0, 3.0, 10.0, 1.0])) :-
		knn_forecasting::learn(periodic_series_missing_interior, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, 5, Forecasts).

	test(knn_no_missing_observations, true(MissingCount == 0)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::diagnostic(Forecaster, missing_count(MissingCount)).

	test(knn_missing_trailing_learn_succeeds, deterministic) :-
		knn_forecasting::learn(periodic_series_missing_trailing, Forecaster, [order(1), k(1)]),
		knn_forecasting::valid_forecaster(Forecaster).

	test(knn_missing_trailing_window_unknown, false) :-
		knn_forecasting::learn(periodic_series_missing_trailing, Forecaster, [order(1), k(1)]),
		Forecaster = knn_forecaster(_, knn_state(Window, _), _, _),
		ground(Window).

	test(knn_missing_trailing_forecast_fails, error(domain_error(missing_observation, _))) :-
		knn_forecasting::learn(periodic_series_missing_trailing, Forecaster, [order(1), k(1)]),
		knn_forecasting::forecast(Forecaster, 1, _).

	test(knn_missing_trailing_zero_horizon_succeeds, deterministic(Forecasts == [])) :-
		knn_forecasting::learn(periodic_series_missing_trailing, Forecaster, [order(1), k(1)]),
		knn_forecasting::forecast(Forecaster, 0, Forecasts).

	test(knn_missing_trailing_resolved_by_update, deterministic(Forecasts =~= [1.0, 2.0])) :-
		knn_forecasting::learn(periodic_series_missing_trailing, Forecaster, [order(1), k(1)]),
		knn_forecasting::update(Forecaster, 10.0, Updated),
		knn_forecasting::forecast(Updated, 2, Forecasts).

	test(knn_missing_trailing_resolved_diagnostics, true) :-
		knn_forecasting::learn(periodic_series_missing_trailing, Forecaster, [order(1), k(1)]),
		knn_forecasting::update(Forecaster, 10.0, Updated),
		knn_forecasting::diagnostic(Updated, training_series_length(17)),
		knn_forecasting::diagnostic(Updated, scored_count(14)),
		knn_forecasting::diagnostic(Updated, update_count(1)).

	test(knn_insufficient_observations, error(consistency_error(k, 3, 0))) :-
		knn_forecasting::learn(mostly_missing_series, _).

	test(knn_missing_observation_appended, true) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, _, Updated),
		knn_forecasting::diagnostic(Updated, missing_count(1)),
		knn_forecasting::diagnostic(Updated, training_series_length(17)),
		knn_forecasting::diagnostic(Updated, scored_count(13)).

	test(knn_missing_observation_appended_blocks_forecast, error(domain_error(missing_observation, _))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, _, Updated),
		knn_forecasting::forecast(Updated, 1, _).

	test(knn_missing_observation_appended_resolved, deterministic(Forecasts =~= [10.0])) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, _, Updated),
		knn_forecasting::update(Updated, 1.0, Updated1),
		knn_forecasting::update(Updated1, 2.0, Updated2),
		knn_forecasting::update(Updated2, 3.0, Updated3),
		knn_forecasting::forecast(Updated3, 1, Forecasts).

	% forecasting

	test(knn_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, 0, Forecasts).

	test(knn_forecast_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, -1, _).

	test(knn_forecast_non_integer_horizon, error(type_error(integer, foo))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, foo, _).

	test(knn_forecast_unbound_horizon, error(instantiation_error)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, _, _).

	test(knn_forecast_unbound_forecaster, error(instantiation_error)) :-
		knn_forecasting::forecast(_, 1, _).

	test(knn_forecast_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		knn_forecasting::forecast(foo, 1, _).

	test(knn_learn_2_same_as_learn_3, deterministic(Forecaster0 == Forecaster1)) :-
		knn_forecasting::learn(periodic_series, Forecaster0, [order(3), k(1)]),
		knn_forecasting::learn(periodic_series, Forecaster1, [order(3), k(1)]).

	% online updates

	test(knn_update_forecast, deterministic(Forecasts =~= [2.0, 3.0, 10.0])) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 1.0, Updated),
		knn_forecasting::forecast(Updated, 3, Forecasts).

	test(knn_update_keeps_original_forecaster, deterministic(Forecasts1 =~= Forecasts0)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::forecast(Forecaster, 2, Forecasts0),
		knn_forecasting::update(Forecaster, 1.0, _Updated),
		knn_forecasting::forecast(Forecaster, 2, Forecasts1).

	test(knn_update_keeps_rows_fixed, deterministic(RowCount1 == RowCount0)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		Forecaster = knn_forecaster(_Model0, _State0, Rows0, _Diagnostics0),
		length(Rows0, RowCount0),
		knn_forecasting::update(Forecaster, 1.0, Updated),
		Updated = knn_forecaster(_Model1, _State1, Rows1, _Diagnostics1),
		length(Rows1, RowCount1).

	test(knn_update_diagnostics, true) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 1.0, Updated),
		knn_forecasting::diagnostic(Updated, training_series_length(17)),
		knn_forecasting::diagnostic(Updated, scored_count(14)),
		knn_forecasting::diagnostic(Updated, update_count(1)),
		knn_forecasting::diagnostic(Updated, sum_squared_error(SumSquaredError)),
		SumSquaredError =~= 0.0,
		knn_forecasting::valid_forecaster(Updated).

	test(knn_update_off_pattern_observation, true(SumSquaredError =~= ExpectedSumSquaredError)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 99.0, Updated),
		knn_forecasting::diagnostic(Updated, sum_squared_error(SumSquaredError)),
		Residual is 99.0 - 1.0,
		ExpectedSumSquaredError is Residual * Residual.

	test(knn_update_differencing_1, deterministic(Forecasts =~= [24.0, 26.0])) :-
		knn_forecasting::learn(linear_trend, Forecaster, [order(2), k(1), differencing(1)]),
		knn_forecasting::update(Forecaster, 22, Updated),
		knn_forecasting::forecast(Updated, 2, Forecasts).

	test(knn_update_unbound_forecaster, error(instantiation_error)) :-
		knn_forecasting::update(_, 1.0, _).

	test(knn_update_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		knn_forecasting::update(foo, 1.0, _).

	test(knn_update_unbound_observation_is_missing, deterministic) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, _, Updated),
		knn_forecasting::valid_forecaster(Updated).

	test(knn_update_non_numeric_observation, error(type_error(number, foo))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, foo, _).

	test(knn_update_options_not_a_list, error(type_error(list, foo))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 1.0, _, foo).

	test(knn_update_options_partial_list, error(instantiation_error)) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 1.0, _, [_| _]).

	test(knn_update_option_not_compound, error(type_error(compound, foo))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 1.0, _, [foo]).

	test(knn_update_invalid_option, error(domain_error(option, foo(1)))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::update(Forecaster, 1.0, _, [foo(1)]).

	% forecaster protocol predicates

	test(knn_valid_forecaster, deterministic) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::valid_forecaster(Forecaster).

	test(knn_check_forecaster_unbound, error(instantiation_error)) :-
		knn_forecasting::check_forecaster(_).

	test(knn_check_forecaster_invalid, error(domain_error(forecaster, foo))) :-
		knn_forecasting::check_forecaster(foo).

	test(knn_invalid_forecaster_wrong_window_length, false) :-
		knn_forecasting::learn(periodic_series, knn_forecaster(Model, knn_state([Value| _], Levels), Rows, Diagnostics), [order(3), k(1)]),
		knn_forecasting::valid_forecaster(knn_forecaster(Model, knn_state([Value], Levels), Rows, Diagnostics)).

	test(knn_invalid_forecaster_wrong_row_lags_length, false) :-
		knn_forecasting::learn(periodic_series, knn_forecaster(Model, State, [[A, B, _C]-Target| Rows], Diagnostics), [order(3), k(1)]),
		knn_forecasting::valid_forecaster(knn_forecaster(Model, State, [[A, B]-Target| Rows], Diagnostics)).

	test(knn_invalid_forecaster_too_few_rows, false) :-
		knn_forecasting::learn(periodic_series, knn_forecaster(Model, State, Rows, Diagnostics), [order(3), k(3)]),
		knn_take_one(Rows, [Row]),
		knn_forecasting::valid_forecaster(knn_forecaster(Model, State, [Row], Diagnostics)).

	test(knn_invalid_forecaster_empty_rows, false) :-
		knn_forecasting::learn(periodic_series, knn_forecaster(Model, State, _Rows, Diagnostics), [order(3), k(1)]),
		knn_forecasting::valid_forecaster(knn_forecaster(Model, State, [], Diagnostics)).

	test(knn_invalid_forecaster_missing_diagnostic, false) :-
		knn_forecasting::learn(periodic_series, knn_forecaster(Model, State, Rows, _Diagnostics), [order(3), k(1)]),
		knn_forecasting::valid_forecaster(knn_forecaster(Model, State, Rows, [])).

	test(knn_diagnostics_2, deterministic) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::diagnostics(Forecaster, Diagnostics),
		memberchk(model(knn_forecasting), Diagnostics),
		memberchk(training_series_length(16), Diagnostics),
		memberchk(order(3), Diagnostics),
		memberchk(k(1), Diagnostics),
		memberchk(update_count(0), Diagnostics).

	test(knn_diagnostic_2_enumeration, deterministic(Names == [model, training_series_length, options])) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		findall(Name, (knn_forecasting::diagnostic(Forecaster, Diagnostic), functor(Diagnostic, Name, 1)), [Name1, Name2, Name3| _]),
		Names = [Name1, Name2, Name3].

	% export and printing

	test(knn_export_to_clauses_4, deterministic(Clause == forecaster_model(Forecaster))) :-
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::export_to_clauses(periodic_series, Forecaster, forecaster_model, [Clause]).

	test(knn_export_to_file_4_header, deterministic) :-
		^^file_path('test_output.pl', File),
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::export_to_file(periodic_series, Forecaster, forecaster_model, File),
		header_lines(File, [Line1, Line2, Line3, _, Line5]),
		assertion(Line1 == '% exported forecaster predicate: forecaster_model/1'),
		assertion(Line2 == '% training dataset: periodic_series'),
		assertion(Line3 == '% training series length: 16'),
		assertion(Line5 == '% forecaster_model(Forecaster)').

	test(knn_export_to_file_4_loadable, deterministic(Forecasts1 =~= Forecasts0)) :-
		^^file_path('test_output.pl', File),
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::export_to_file(periodic_series, Forecaster, forecaster_model, File),
		logtalk_load(File),
		{forecaster_model(Loaded)},
		knn_forecasting::valid_forecaster(Loaded),
		knn_forecasting::forecast(Forecaster, 2, Forecasts0),
		knn_forecasting::forecast(Loaded, 2, Forecasts1).

	test(knn_print_forecaster_1, deterministic) :-
		^^suppress_text_output,
		knn_forecasting::learn(periodic_series, Forecaster, [order(3), k(1)]),
		knn_forecasting::print_forecaster(Forecaster).

	% auxiliary predicates

	knn_take_one([Row| _], [Row]).

	header_lines(File, Lines) :-
		open(File, read, Stream),
		read_header_lines(Stream, Lines),
		close(Stream).

	read_header_lines(Stream, Lines) :-
		read_line_atom(Stream, Line),
		(	Line == end_of_file ->
			Lines = []
		;	sub_atom(Line, 0, 1, _, '%') ->
			Lines = [Line| RestLines],
			read_header_lines(Stream, RestLines)
		;	Lines = []
		).

	read_line_atom(Stream, Line) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Line = end_of_file
		;	read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.

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
		date is 2026-09-28,
		comment is 'Tests for the "time_series_regression" library. Reference values for the noisy AR(2) dataset were computed independently using NumPy least squares.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	cover(time_series_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	% exact recovery of noiseless processes

	test(ts_regression_ar1_parameters, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		parameters(Forecaster, Intercept, [Coefficient]),
		close(Intercept, 2.0),
		close(Coefficient, 0.5).

	test(ts_regression_ar1_forecast, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [3.984375, 3.9921875, 3.99609375]).

	test(ts_regression_ar2_parameters, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		parameters(Forecaster, Intercept, Coefficients),
		close(Intercept, 1.0),
		close_list(Coefficients, [0.5, -0.25]).

	test(ts_regression_ar2_forecast, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [1.33203125, 1.33251953125, 1.333251953125]).

	test(ts_regression_no_intercept, deterministic) :-
		time_series_regression::learn(decay_series, Forecaster, [intercept(false)]),
		parameters(Forecaster, Intercept, [Coefficient]),
		close(Intercept, 0.0),
		close(Coefficient, 0.5),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [0.0625, 0.03125, 0.015625]).

	% differencing

	test(ts_regression_differencing_1, deterministic) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [22.0, 24.0, 26.0]).

	test(ts_regression_differencing_2, deterministic) :-
		time_series_regression::learn(quadratic_trend, Forecaster, [differencing(2)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [81.0, 100.0, 121.0]).

	% rank-deficient design matrices

	test(ts_regression_constant_series_forecast, deterministic) :-
		time_series_regression::learn(constant_series, Forecaster),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [5.0, 5.0, 5.0]).

	test(ts_regression_constant_series_rank, true(Rank == 1)) :-
		time_series_regression::learn(constant_series, Forecaster),
		time_series_regression::diagnostic(Forecaster, design_rank(Rank)).

	% comparison with independently computed least-squares fits

	test(ts_regression_noisy_ar2_parameters, deterministic) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		parameters(Forecaster, Intercept, Coefficients),
		close(Intercept, 0.8840570534290365),
		close_list(Coefficients, [0.6382169512066711, -0.21981628666999298]).

	test(ts_regression_noisy_ar2_forecast, deterministic) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		close_list(Forecasts, [2.366094149595506, 1.698573771941963, 1.4480095976817724]).

	test(ts_regression_noisy_ar2_diagnostics, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		close(SumSquaredError, 18.486552852643516),
		time_series_regression::diagnostic(Forecaster, scored_count(78)),
		time_series_regression::diagnostic(Forecaster, parameter_count(3)),
		time_series_regression::diagnostic(Forecaster, aic(AIC)),
		close(AIC, -106.29388807537143),
		time_series_regression::diagnostic(Forecaster, aicc(AICc)),
		close(AICc, -105.9695637510471),
		time_series_regression::diagnostic(Forecaster, bic(BIC)),
		close(BIC, -99.22376159530265).

	test(ts_regression_noisy_ar2_no_intercept, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(1), intercept(false)]),
		parameters(Forecaster, Intercept, [Coefficient]),
		close(Intercept, 0.0),
		close(Coefficient, 0.95588704),
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		close(SumSquaredError, 25.080785846809995).

	% automatic order selection

	test(ts_regression_auto_order_aicc, true(Order == 2)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4)]),
		time_series_regression::diagnostic(Forecaster, order(Order)).

	test(ts_regression_auto_order_aicc_candidates, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4)]),
		time_series_regression::diagnostic(Forecaster, order_selection(aicc, Candidates)),
		Candidates = [1-Score1, 2-Score2, 3-Score3, 4-Score4],
		close(Score1, -109.4554269388882),
		close(Score2, -110.27190041015955),
		close(Score3, -108.07961515949322),
		close(Score4, -106.04102157651033).

	test(ts_regression_auto_order_bic, true(Order == 1)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4), selection_criterion(bic)]),
		time_series_regression::diagnostic(Forecaster, order(Order)).

	test(ts_regression_auto_order_bic_candidates, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4), selection_criterion(bic)]),
		time_series_regression::diagnostic(Forecaster, order_selection(bic, Candidates)),
		Candidates = [1-Score1, 2-Score2, 3-Score3, 4-Score4],
		close(Score1, -104.95834381995937),
		close(Score2, -103.61303372263387),
		close(Score3, -99.32006208003804),
		close(Score4, -95.24449773222153).

	test(ts_regression_auto_order_aic_selects, true(Order == 2)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4), selection_criterion(aic)]),
		time_series_regression::diagnostic(Forecaster, order(Order)).

	test(ts_regression_auto_order_capped_by_series_length, true(Candidates == [1])) :-
		time_series_regression::learn(linear_trend, Forecaster, [order(auto)]),
		time_series_regression::diagnostic(Forecaster, order_selection(aicc, Scores)),
		keys(Scores, Candidates).

	test(ts_regression_explicit_order_has_no_selection_diagnostic, false) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::diagnostic(Forecaster, order_selection(_, _)).

	% retained residuals

	test(ts_regression_residuals_not_retained_by_default, true(Residuals == none)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::diagnostic(Forecaster, residuals(Residuals)).

	test(ts_regression_residuals_retained, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2), retain_residuals(true)]),
		time_series_regression::diagnostic(Forecaster, residuals(Residuals)),
		length(Residuals, 78),
		sum_squares(Residuals, 0.0, SumSquares),
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		close(SumSquares, SumSquaredError).

	% options

	test(ts_regression_invalid_order_0, error(domain_error(option, order(0)))) :-
		time_series_regression::learn(ar1_series, _, [order(0)]).

	test(ts_regression_invalid_order_atom, error(domain_error(option, order(foo)))) :-
		time_series_regression::learn(ar1_series, _, [order(foo)]).

	test(ts_regression_invalid_max_order, error(domain_error(option, max_order(0)))) :-
		time_series_regression::learn(ar1_series, _, [order(auto), max_order(0)]).

	test(ts_regression_invalid_selection_criterion, error(domain_error(option, selection_criterion(hqic)))) :-
		time_series_regression::learn(ar1_series, _, [order(auto), selection_criterion(hqic)]).

	test(ts_regression_invalid_intercept, error(domain_error(option, intercept(maybe)))) :-
		time_series_regression::learn(ar1_series, _, [intercept(maybe)]).

	test(ts_regression_invalid_differencing, error(domain_error(option, differencing(-1)))) :-
		time_series_regression::learn(ar1_series, _, [differencing(-1)]).

	test(ts_regression_invalid_retain_residuals, error(domain_error(option, retain_residuals(yes)))) :-
		time_series_regression::learn(ar1_series, _, [retain_residuals(yes)]).

	test(ts_regression_unknown_option, error(domain_error(option, bogus(1)))) :-
		time_series_regression::learn(ar1_series, _, [bogus(1)]).

	test(ts_regression_irrelevant_max_order, error(domain_error(time_series_regression_option, max_order(4)))) :-
		time_series_regression::learn(ar1_series, _, [order(2), max_order(4)]).

	test(ts_regression_irrelevant_selection_criterion, error(domain_error(time_series_regression_option, selection_criterion(bic)))) :-
		time_series_regression::learn(ar1_series, _, [order(2), selection_criterion(bic)]).

	test(ts_regression_forecaster_options, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2), intercept(true)]),
		time_series_regression::forecaster_options(Forecaster, Options),
		memberchk(order(2), Options),
		memberchk(intercept(true), Options),
		memberchk(differencing(0), Options),
		memberchk(retain_residuals(false), Options).

	% invalid datasets

	test(ts_regression_short_series, error(domain_error(series_length, short_series))) :-
		time_series_regression::learn(short_series, _).

	test(ts_regression_short_series_auto, error(domain_error(series_length, short_series))) :-
		time_series_regression::learn(short_series, _, [order(auto)]).

	test(ts_regression_series_too_short_for_order, error(domain_error(series_length, linear_trend))) :-
		time_series_regression::learn(linear_trend, _, [order(3)]).

	test(ts_regression_series_too_short_for_differencing, error(domain_error(series_length, linear_trend))) :-
		time_series_regression::learn(linear_trend, _, [differencing(4)]).

	test(ts_regression_gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		time_series_regression::learn(gap_index, _).

	test(ts_regression_non_numeric_value, error(type_error(number, bad))) :-
		time_series_regression::learn(non_numeric_value, _).

	% forecasting

	test(ts_regression_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, 0, Forecasts).

	test(ts_regression_forecast_zero_horizon_differenced, deterministic(Forecasts == [])) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::forecast(Forecaster, 0, Forecasts).

	test(ts_regression_forecast_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, -1, _).

	test(ts_regression_forecast_non_integer_horizon, error(type_error(integer, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, foo, _).

	test(ts_regression_forecast_unbound_horizon, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, _, _).

	test(ts_regression_forecast_unbound_forecaster, error(instantiation_error)) :-
		time_series_regression::forecast(_, 1, _).

	test(ts_regression_forecast_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		time_series_regression::forecast(foo, 1, _).

	% online updates

	test(ts_regression_update_forecast, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::update(Forecaster, 1.33203125, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		close_list(Forecasts, [1.33251953125, 1.333251953125]).

	test(ts_regression_update_keeps_original_forecaster, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 2, Forecasts0),
		time_series_regression::update(Forecaster, 1.33203125, _Updated),
		time_series_regression::forecast(Forecaster, 2, Forecasts1),
		close_list(Forecasts1, Forecasts0).

	test(ts_regression_update_diagnostics, true) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::update(Forecaster, 1.33203125, Updated, []),
		time_series_regression::diagnostic(Updated, training_series_length(11)),
		time_series_regression::diagnostic(Updated, scored_count(9)),
		time_series_regression::diagnostic(Updated, update_count(1)),
		time_series_regression::diagnostic(Updated, sum_squared_error(SumSquaredError)),
		SumSquaredError < 1.0e-12,
		time_series_regression::valid_forecaster(Updated).

	test(ts_regression_update_differencing_1, deterministic) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::update(Forecaster, 22, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		close_list(Forecasts, [24.0, 26.0]).

	test(ts_regression_update_differencing_2, deterministic) :-
		time_series_regression::learn(quadratic_trend, Forecaster, [differencing(2)]),
		time_series_regression::update(Forecaster, 81, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		close_list(Forecasts, [100.0, 121.0]).

	test(ts_regression_update_off_model_observation, true) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::update(Forecaster, 2.0, Updated),
		time_series_regression::forecast(Updated, 1, Forecasts),
		close_list(Forecasts, [1.66650390625]),
		time_series_regression::diagnostic(Updated, sum_squared_error(SumSquaredError)),
		Residual is 2.0 - 1.33203125,
		close(SumSquaredError, Residual * Residual).

	test(ts_regression_update_retained_residuals, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2), retain_residuals(true)]),
		time_series_regression::update(Forecaster, 1.5, Updated),
		time_series_regression::diagnostic(Updated, residuals(Residuals)),
		length(Residuals, 79),
		time_series_regression::diagnostic(Updated, scored_count(79)).

	test(ts_regression_update_unbound_forecaster, error(instantiation_error)) :-
		time_series_regression::update(_, 1.0, _).

	test(ts_regression_update_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		time_series_regression::update(foo, 1.0, _).

	test(ts_regression_update_unbound_observation, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, _, _).

	test(ts_regression_update_non_numeric_observation, error(type_error(number, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, foo, _).

	test(ts_regression_update_options_not_a_list, error(type_error(list, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, 1.0, _, foo).

	test(ts_regression_update_options_partial_list, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, 1.0, _, [_| _]).

	test(ts_regression_update_option_not_compound, error(type_error(compound, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, 1.0, _, [foo]).

	test(ts_regression_update_invalid_option, error(domain_error(option, foo(1)))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, 1.0, _, [foo(1)]).

	% forecaster protocol predicates

	test(ts_regression_valid_forecaster, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::valid_forecaster(Forecaster).

	test(ts_regression_check_forecaster_unbound, error(instantiation_error)) :-
		time_series_regression::check_forecaster(_).

	test(ts_regression_check_forecaster_invalid, error(domain_error(forecaster, foo))) :-
		time_series_regression::check_forecaster(foo).

	test(ts_regression_invalid_forecaster_coefficient_count, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, State, ar_parameters(Intercept, [Coefficient| _]), Diagnostics), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, State, ar_parameters(Intercept, [Coefficient]), Diagnostics)).

	test(ts_regression_invalid_forecaster_window_length, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, ar_state([Value| _], Levels), Parameters, Diagnostics), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, ar_state([Value], Levels), Parameters, Diagnostics)).

	test(ts_regression_invalid_forecaster_levels_length, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, ar_state(Window, _), Parameters, Diagnostics), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, ar_state(Window, [1.0]), Parameters, Diagnostics)).

	test(ts_regression_invalid_forecaster_diagnostics, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, State, Parameters, _), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, State, Parameters, [])).

	test(ts_regression_invalid_forecaster_nonzero_intercept_without_intercept, false) :-
		time_series_regression::learn(decay_series, time_series_regression_forecaster(Model, State, ar_parameters(_, Coefficients), Diagnostics), [intercept(false)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, State, ar_parameters(1.0, Coefficients), Diagnostics)).

	test(ts_regression_diagnostics_2, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::diagnostics(Forecaster, Diagnostics),
		memberchk(model(time_series_regression), Diagnostics),
		memberchk(training_series_length(8), Diagnostics),
		memberchk(order(1), Diagnostics),
		memberchk(differencing(0), Diagnostics),
		memberchk(intercept(true), Diagnostics),
		memberchk(scored_count(7), Diagnostics),
		memberchk(design_rank(2), Diagnostics),
		memberchk(update_count(0), Diagnostics),
		memberchk(residuals(none), Diagnostics).

	test(ts_regression_diagnostic_2_enumeration, deterministic(Names == [model, training_series_length, options])) :-
		time_series_regression::learn(ar1_series, Forecaster),
		findall(Name, (time_series_regression::diagnostic(Forecaster, Diagnostic), functor(Diagnostic, Name, 1)), [Name1, Name2, Name3| _]),
		Names = [Name1, Name2, Name3].

	test(ts_regression_learn_2_same_as_learn_3, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster0),
		time_series_regression::learn(ar2_series, Forecaster1, []),
		Forecaster0 == Forecaster1.

	% export and printing

	test(ts_regression_export_to_clauses_4, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::export_to_clauses(ar2_series, Forecaster, forecaster_model, [Clause]),
		Clause == forecaster_model(Forecaster).

	test(ts_regression_export_to_file_4_header, deterministic) :-
		^^file_path('test_output.pl', File),
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::export_to_file(ar2_series, Forecaster, forecaster_model, File),
		header_lines(File, [Line1, Line2, Line3, _, Line5]),
		Line1 == '% exported forecaster predicate: forecaster_model/1',
		Line2 == '% training dataset: ar2_series',
		Line3 == '% training series length: 10',
		Line5 == '% forecaster_model(Forecaster)'.

	test(ts_regression_export_to_file_4_loadable, deterministic) :-
		^^file_path('test_output.pl', File),
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::export_to_file(ar2_series, Forecaster, forecaster_model, File),
		logtalk_load(File),
		{forecaster_model(Loaded)},
		time_series_regression::valid_forecaster(Loaded),
		time_series_regression::forecast(Forecaster, 2, Forecasts0),
		time_series_regression::forecast(Loaded, 2, Forecasts1),
		close_list(Forecasts1, Forecasts0).

	test(ts_regression_print_forecaster_1, deterministic) :-
		^^suppress_text_output,
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::print_forecaster(Forecaster).

	% auxiliary predicates

	parameters(time_series_regression_forecaster(_, _, ar_parameters(Intercept, Coefficients), _), Intercept, Coefficients).

	close(Value, Expected) :-
		abs(Value - Expected) < 1.0e-6.

	close_list([], []).
	close_list([Value| Values], [Expected| Expecteds]) :-
		close(Value, Expected),
		close_list(Values, Expecteds).

	sum_squares([], SumSquares, SumSquares).
	sum_squares([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		sum_squares(Values, SumSquares1, SumSquares).

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

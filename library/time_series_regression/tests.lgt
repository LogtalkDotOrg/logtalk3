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
		comment is 'Tests for the "time_series_regression" library. Reference values for the noisy AR(2) dataset were computed independently using NumPy least squares.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
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

	test(time_series_regression_ar1_parameters, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		parameters(Forecaster, Intercept, [Coefficient]),
		Intercept =~= 2.0,
		Coefficient =~= 0.5.

	test(time_series_regression_ar1_forecast, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [3.984375, 3.9921875, 3.99609375].

	test(time_series_regression_ar2_parameters, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		parameters(Forecaster, Intercept, Coefficients),
		Intercept =~= 1.0,
		Coefficients =~= [0.5, -0.25].

	test(time_series_regression_ar2_forecast, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [1.33203125, 1.33251953125, 1.333251953125].

	test(time_series_regression_no_intercept, deterministic) :-
		time_series_regression::learn(decay_series, Forecaster, [intercept(false)]),
		parameters(Forecaster, Intercept, [Coefficient]),
		Intercept =~= 0.0,
		Coefficient =~= 0.5,
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [0.0625, 0.03125, 0.015625].

	% differencing

	test(time_series_regression_differencing_1, deterministic) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [22.0, 24.0, 26.0].

	test(time_series_regression_differencing_2, deterministic) :-
		time_series_regression::learn(quadratic_trend, Forecaster, [differencing(2)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [81.0, 100.0, 121.0].

	% rank-deficient design matrices

	test(time_series_regression_constant_series_forecast, deterministic) :-
		time_series_regression::learn(constant_series, Forecaster),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [5.0, 5.0, 5.0].

	test(time_series_regression_constant_series_rank, true(Rank == 1)) :-
		time_series_regression::learn(constant_series, Forecaster),
		time_series_regression::diagnostic(Forecaster, design_rank(Rank)).

	% comparison with independently computed least-squares fits

	test(time_series_regression_noisy_ar2_parameters, deterministic) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		parameters(Forecaster, Intercept, Coefficients),
		Intercept =~= 0.8840570534290365,
		Coefficients =~= [0.6382169512066711, -0.21981628666999298].

	test(time_series_regression_noisy_ar2_forecast, deterministic) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [2.366094149595506, 1.698573771941963, 1.4480095976817724].

	test(time_series_regression_noisy_ar2_diagnostics, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		SumSquaredError =~= 18.486552852643516,
		time_series_regression::diagnostic(Forecaster, scored_count(78)),
		time_series_regression::diagnostic(Forecaster, parameter_count(3)),
		time_series_regression::diagnostic(Forecaster, aic(AIC)),
		AIC =~= -106.29388807537143,
		time_series_regression::diagnostic(Forecaster, aicc(AICc)),
		AICc =~= -105.9695637510471,
		time_series_regression::diagnostic(Forecaster, bic(BIC)),
		BIC =~= -99.22376159530265.

	test(time_series_regression_noisy_ar2_no_intercept, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(1), intercept(false)]),
		parameters(Forecaster, Intercept, [Coefficient]),
		Intercept =~= 0.0,
		Coefficient =~= 0.95588704,
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		SumSquaredError =~= 25.080785846809995.

	% automatic order selection

	test(time_series_regression_auto_order_aicc, true(Order == 2)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4)]),
		time_series_regression::diagnostic(Forecaster, order(Order)).

	test(time_series_regression_auto_order_aicc_candidates, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4)]),
		time_series_regression::diagnostic(Forecaster, order_selection(aicc, Candidates)),
		Candidates = [1-Score1, 2-Score2, 3-Score3, 4-Score4],
		Score1 =~= -109.4554269388882,
		Score2 =~= -110.27190041015955,
		Score3 =~= -108.07961515949322,
		Score4 =~= -106.04102157651033.

	test(time_series_regression_auto_order_bic, true(Order == 1)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4), selection_criterion(bic)]),
		time_series_regression::diagnostic(Forecaster, order(Order)).

	test(time_series_regression_auto_order_bic_candidates, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4), selection_criterion(bic)]),
		time_series_regression::diagnostic(Forecaster, order_selection(bic, Candidates)),
		Candidates = [1-Score1, 2-Score2, 3-Score3, 4-Score4],
		Score1 =~= -104.95834381995937,
		Score2 =~= -103.61303372263387,
		Score3 =~= -99.32006208003804,
		Score4 =~= -95.24449773222153.

	test(time_series_regression_auto_order_aic_selects, true(Order == 2)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(auto), max_order(4), selection_criterion(aic)]),
		time_series_regression::diagnostic(Forecaster, order(Order)).

	test(time_series_regression_auto_order_capped_by_series_length, true(Candidates == [1])) :-
		time_series_regression::learn(linear_trend, Forecaster, [order(auto)]),
		time_series_regression::diagnostic(Forecaster, order_selection(aicc, Scores)),
		keys(Scores, Candidates).

	test(time_series_regression_explicit_order_has_no_selection_diagnostic, false) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::diagnostic(Forecaster, order_selection(_, _)).

	% retained residuals

	test(time_series_regression_residuals_not_retained_by_default, true(Residuals == none)) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::diagnostic(Forecaster, residuals(Residuals)).

	test(time_series_regression_residuals_retained, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2), retain_residuals(true)]),
		time_series_regression::diagnostic(Forecaster, residuals(Residuals)),
		length(Residuals, 78),
		sum_squares(Residuals, 0.0, SumSquares),
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		SumSquares =~= SumSquaredError.

	% options

	test(time_series_regression_invalid_order_0, error(domain_error(option, order(0)))) :-
		time_series_regression::learn(ar1_series, _, [order(0)]).

	test(time_series_regression_invalid_order_atom, error(domain_error(option, order(foo)))) :-
		time_series_regression::learn(ar1_series, _, [order(foo)]).

	test(time_series_regression_invalid_max_order, error(domain_error(option, max_order(0)))) :-
		time_series_regression::learn(ar1_series, _, [order(auto), max_order(0)]).

	test(time_series_regression_invalid_selection_criterion, error(domain_error(option, selection_criterion(hqic)))) :-
		time_series_regression::learn(ar1_series, _, [order(auto), selection_criterion(hqic)]).

	test(time_series_regression_invalid_intercept, error(domain_error(option, intercept(maybe)))) :-
		time_series_regression::learn(ar1_series, _, [intercept(maybe)]).

	test(time_series_regression_invalid_differencing, error(domain_error(option, differencing(-1)))) :-
		time_series_regression::learn(ar1_series, _, [differencing(-1)]).

	test(time_series_regression_invalid_retain_residuals, error(domain_error(option, retain_residuals(yes)))) :-
		time_series_regression::learn(ar1_series, _, [retain_residuals(yes)]).

	test(time_series_regression_unknown_option, error(domain_error(option, bogus(1)))) :-
		time_series_regression::learn(ar1_series, _, [bogus(1)]).

	test(time_series_regression_irrelevant_max_order, error(domain_error(time_series_regression_option, max_order(4)))) :-
		time_series_regression::learn(ar1_series, _, [order(2), max_order(4)]).

	test(time_series_regression_irrelevant_selection_criterion, error(domain_error(time_series_regression_option, selection_criterion(bic)))) :-
		time_series_regression::learn(ar1_series, _, [order(2), selection_criterion(bic)]).

	test(time_series_regression_forecaster_options, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2), intercept(true)]),
		time_series_regression::forecaster_options(Forecaster, Options),
		memberchk(order(2), Options),
		memberchk(intercept(true), Options),
		memberchk(differencing(0), Options),
		memberchk(retain_residuals(false), Options).

	% invalid datasets

	test(time_series_regression_short_series, error(domain_error(series_length, short_series))) :-
		time_series_regression::learn(short_series, _).

	test(time_series_regression_short_series_auto, error(domain_error(series_length, short_series))) :-
		time_series_regression::learn(short_series, _, [order(auto)]).

	test(time_series_regression_series_too_short_for_order, error(domain_error(series_length, linear_trend))) :-
		time_series_regression::learn(linear_trend, _, [order(3)]).

	test(time_series_regression_series_too_short_for_differencing, error(domain_error(series_length, linear_trend))) :-
		time_series_regression::learn(linear_trend, _, [differencing(4)]).

	test(time_series_regression_gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		time_series_regression::learn(gap_index, _).

	test(time_series_regression_non_numeric_value, error(domain_error(types([number,var]), bad))) :-
		time_series_regression::learn(non_numeric_value, _).

	% missing observations

	test(time_series_regression_missing_interior_diagnostics, true) :-
		time_series_regression::learn(ar1_series_missing_interior, Forecaster),
		time_series_regression::diagnostic(Forecaster, missing_count(1)),
		time_series_regression::diagnostic(Forecaster, scored_count(5)).

	test(time_series_regression_missing_interior_parameters, deterministic) :-
		time_series_regression::learn(ar1_series_missing_interior, Forecaster),
		parameters(Forecaster, Intercept, [Coefficient]),
		Intercept =~= 2.0,
		Coefficient =~= 0.5.

	test(time_series_regression_missing_interior_forecast, deterministic) :-
		time_series_regression::learn(ar1_series_missing_interior, Forecaster),
		time_series_regression::forecast(Forecaster, 3, Forecasts),
		Forecasts =~= [3.984375, 3.9921875, 3.99609375].

	test(time_series_regression_no_missing_observations, true(MissingCount == 0)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::diagnostic(Forecaster, missing_count(MissingCount)).

	test(time_series_regression_missing_trailing_learn_succeeds, deterministic) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		time_series_regression::valid_forecaster(Forecaster).

	test(time_series_regression_missing_trailing_window_unknown, false) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		Forecaster = time_series_regression_forecaster(_, ar_state(Window, _), _, _),
		ground(Window).

	test(time_series_regression_missing_trailing_forecast_fails, error(domain_error(missing_observation, _))) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		time_series_regression::forecast(Forecaster, 1, _).

	test(time_series_regression_missing_trailing_forecast_interval_fails, error(domain_error(missing_observation, _))) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, []).

	test(time_series_regression_missing_trailing_zero_horizon_succeeds, true(Forecasts == [])) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		time_series_regression::forecast(Forecaster, 0, Forecasts).

	test(time_series_regression_missing_trailing_resolved_by_update, deterministic) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		time_series_regression::update(Forecaster, 3.984375, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		Forecasts =~= [3.9921875, 3.99609375].

	test(time_series_regression_missing_trailing_resolved_diagnostics, true) :-
		time_series_regression::learn(ar1_series_missing_trailing, Forecaster),
		time_series_regression::update(Forecaster, 3.984375, Updated),
		time_series_regression::diagnostic(Updated, training_series_length(9)),
		time_series_regression::diagnostic(Updated, scored_count(6)),
		time_series_regression::diagnostic(Updated, update_count(1)).

	test(time_series_regression_insufficient_observations, error(domain_error(insufficient_observations, mostly_missing_series))) :-
		time_series_regression::learn(mostly_missing_series, _).

	test(time_series_regression_insufficient_observations_auto, error(domain_error(insufficient_observations, mostly_missing_series))) :-
		time_series_regression::learn(mostly_missing_series, _, [order(auto)]).

	test(time_series_regression_missing_noisy_ar2_parameters, deterministic) :-
		time_series_regression::learn(noisy_ar2_series_missing, Forecaster, [order(2)]),
		parameters(Forecaster, Intercept, Coefficients),
		Intercept =~= 0.897384797302548,
		Coefficients =~= [0.6358515329152057, -0.2466415211322606].

	test(time_series_regression_missing_noisy_ar2_diagnostics, true) :-
		time_series_regression::learn(noisy_ar2_series_missing, Forecaster, [order(2)]),
		time_series_regression::diagnostic(Forecaster, sum_squared_error(SumSquaredError)),
		SumSquaredError =~= 15.730060233008743,
		time_series_regression::diagnostic(Forecaster, scored_count(69)),
		time_series_regression::diagnostic(Forecaster, design_rank(3)),
		time_series_regression::diagnostic(Forecaster, missing_count(3)).

	test(time_series_regression_missing_noisy_ar2_auto_order, true) :-
		time_series_regression::learn(noisy_ar2_series_missing, Forecaster, [order(auto), max_order(4)]),
		time_series_regression::diagnostic(Forecaster, order_selection(aicc, Candidates)),
		Candidates = [1-Score1, 2-Score2, 3-Score3, 4-Score4],
		Score1 =~= -90.07575784188968,
		Score2 =~= -89.9510741643165,
		Score3 =~= -88.77615020174682,
		Score4 =~= -87.4621955240711.

	test(time_series_regression_missing_observation_appended, true) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, _, Updated),
		time_series_regression::diagnostic(Updated, missing_count(1)),
		time_series_regression::diagnostic(Updated, training_series_length(9)),
		time_series_regression::diagnostic(Updated, scored_count(7)).

	test(time_series_regression_missing_observation_appended_blocks_forecast, error(domain_error(missing_observation, _))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, _, Updated),
		time_series_regression::forecast(Updated, 1, _).

	test(time_series_regression_missing_observation_appended_resolved, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, _, Updated),
		time_series_regression::update(Updated, 3.98, Resolved),
		time_series_regression::forecast(Resolved, 1, Forecasts),
		Forecasts =~= [3.99].

	% forecasting

	test(time_series_regression_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, 0, Forecasts).

	test(time_series_regression_forecast_zero_horizon_differenced, deterministic(Forecasts == [])) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::forecast(Forecaster, 0, Forecasts).

	test(time_series_regression_forecast_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, -1, _).

	test(time_series_regression_forecast_non_integer_horizon, error(type_error(integer, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, foo, _).

	test(time_series_regression_forecast_unbound_horizon, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, _, _).

	test(time_series_regression_forecast_unbound_forecaster, error(instantiation_error)) :-
		time_series_regression::forecast(_, 1, _).

	test(time_series_regression_forecast_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		time_series_regression::forecast(foo, 1, _).

	% online updates

	test(time_series_regression_update_forecast, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::update(Forecaster, 1.33203125, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		Forecasts =~= [1.33251953125, 1.333251953125].

	test(time_series_regression_update_keeps_original_forecaster, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 2, Forecasts0),
		time_series_regression::update(Forecaster, 1.33203125, _Updated),
		time_series_regression::forecast(Forecaster, 2, Forecasts1),
		Forecasts1 =~= Forecasts0.

	test(time_series_regression_update_diagnostics, true) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::update(Forecaster, 1.33203125, Updated),
		time_series_regression::diagnostic(Updated, training_series_length(11)),
		time_series_regression::diagnostic(Updated, scored_count(9)),
		time_series_regression::diagnostic(Updated, update_count(1)),
		time_series_regression::diagnostic(Updated, sum_squared_error(SumSquaredError)),
		SumSquaredError < 1.0e-12,
		time_series_regression::valid_forecaster(Updated).

	test(time_series_regression_update_differencing_1, deterministic) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::update(Forecaster, 22, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		Forecasts =~= [24.0, 26.0].

	test(time_series_regression_update_differencing_2, deterministic) :-
		time_series_regression::learn(quadratic_trend, Forecaster, [differencing(2)]),
		time_series_regression::update(Forecaster, 81, Updated),
		time_series_regression::forecast(Updated, 2, Forecasts),
		Forecasts =~= [100.0, 121.0].

	test(time_series_regression_update_off_model_observation, true) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::update(Forecaster, 2.0, Updated),
		time_series_regression::forecast(Updated, 1, Forecasts),
		Forecasts =~= [1.66650390625],
		time_series_regression::diagnostic(Updated, sum_squared_error(SumSquaredError)),
		ResidualSquared is (2.0 - 1.33203125) ** 2,
		SumSquaredError =~= ResidualSquared.

	test(time_series_regression_update_retained_residuals, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2), retain_residuals(true)]),
		time_series_regression::update(Forecaster, 1.5, Updated),
		time_series_regression::diagnostic(Updated, residuals(Residuals)),
		length(Residuals, 79),
		time_series_regression::diagnostic(Updated, scored_count(79)).

	test(time_series_regression_update_unbound_forecaster, error(instantiation_error)) :-
		time_series_regression::update(_, 1.0, _).

	test(time_series_regression_update_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		time_series_regression::update(foo, 1.0, _).

	test(time_series_regression_update_unbound_observation_is_missing, deterministic) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, _, Updated),
		time_series_regression::valid_forecaster(Updated).

	test(time_series_regression_update_non_numeric_observation, error(type_error(number, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::update(Forecaster, foo, _).

	% prediction intervals

	test(time_series_regression_forecast_interval_zero_horizon, true(Lower-Upper == []-[])) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 0, Lower, Upper, []).

	test(time_series_regression_forecast_interval_noiseless_ar1, true) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast(Forecaster, 3, PointForecasts),
		time_series_regression::forecast_interval(Forecaster, 3, Lower, Upper, []),
		Lower =~= PointForecasts,
		Upper =~= PointForecasts.

	test(time_series_regression_forecast_interval_noisy_ar2, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast_interval(Forecaster, 5, Lower, Upper, []),
		Lower =~= [1.3930211374666417, 0.5442118493687096, 0.27931737352364405, 0.2659626606596961, 0.3114304179122329],
		Upper =~= [3.3391671617243706, 2.8529356945152164, 2.616701821839901, 2.6036916293326433, 2.65155351527206].

	test(time_series_regression_forecast_interval_brackets_point_forecast, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast(Forecaster, 6, PointForecasts),
		time_series_regression::forecast_interval(Forecaster, 6, Lower, Upper, []),
		bracketed(PointForecasts, Lower, Upper).

	test(time_series_regression_forecast_interval_widens_with_horizon, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast_interval(Forecaster, 6, Lower, Upper, []),
		widths(Lower, Upper, Widths),
		non_decreasing(Widths).

	test(time_series_regression_forecast_interval_narrower_confidence, true) :-
		time_series_regression::learn(noisy_ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast_interval(Forecaster, 4, Lower80, Upper80, [confidence(0.8)]),
		time_series_regression::forecast_interval(Forecaster, 4, Lower95, Upper95, [confidence(0.95)]),
		widths(Lower80, Upper80, Widths80),
		widths(Lower95, Upper95, Widths95),
		narrower(Widths80, Widths95).

	test(time_series_regression_forecast_interval_differencing_1, true) :-
		time_series_regression::learn(linear_trend, Forecaster, [differencing(1)]),
		time_series_regression::forecast(Forecaster, 3, PointForecasts),
		time_series_regression::forecast_interval(Forecaster, 3, Lower, Upper, []),
		Lower =~= PointForecasts,
		Upper =~= PointForecasts.

	test(time_series_regression_forecast_interval_exactly_determined_fit, error(domain_error(residual_degrees_of_freedom, _))) :-
		time_series_regression::learn(exactly_determined_series, Forecaster, [order(3)]),
		time_series_regression::diagnostic(Forecaster, scored_count(4)),
		time_series_regression::diagnostic(Forecaster, design_rank(4)),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, []).

	test(time_series_regression_forecast_interval_default_options, true) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::forecast_interval(Forecaster, 2, Lower0, Upper0, []),
		time_series_regression::forecast_interval(Forecaster, 2, Lower1, Upper1, [confidence(0.95), method(normal)]),
		Lower0 =~= Lower1,
		Upper0 =~= Upper1.

	test(time_series_regression_forecast_interval_confidence_too_high, error(domain_error(option, confidence(1.0)))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [confidence(1.0)]).

	test(time_series_regression_forecast_interval_confidence_too_low, error(domain_error(option, confidence(0.0)))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [confidence(0.0)]).

	test(time_series_regression_forecast_interval_confidence_negative, error(domain_error(option, confidence(-0.5)))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [confidence(-0.5)]).

	test(time_series_regression_forecast_interval_unsupported_method, error(domain_error(option, method(bootstrap)))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [method(bootstrap)]).

	test(time_series_regression_forecast_interval_unknown_option, error(domain_error(option, foo(1)))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [foo(1)]).

	test(time_series_regression_forecast_interval_options_not_a_list, error(type_error(list, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, foo).

	test(time_series_regression_forecast_interval_options_partial_list, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [_| _]).

	test(time_series_regression_forecast_interval_option_not_compound, error(type_error(compound, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [foo]).

	test(time_series_regression_forecast_interval_option_unbound_argument, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, 1, _, _, [confidence(_)]).

	test(time_series_regression_forecast_interval_unbound_forecaster, error(instantiation_error)) :-
		time_series_regression::forecast_interval(_, 1, _, _, []).

	test(time_series_regression_forecast_interval_invalid_forecaster, error(domain_error(forecaster, foo))) :-
		time_series_regression::forecast_interval(foo, 1, _, _, []).

	test(time_series_regression_forecast_interval_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, -1, _, _, []).

	test(time_series_regression_forecast_interval_non_integer_horizon, error(type_error(integer, foo))) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, foo, _, _, []).

	test(time_series_regression_forecast_interval_unbound_horizon, error(instantiation_error)) :-
		time_series_regression::learn(ar1_series, Forecaster),
		time_series_regression::forecast_interval(Forecaster, _, _, _, []).

	% forecaster protocol predicates

	test(time_series_regression_valid_forecaster, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::valid_forecaster(Forecaster).

	test(time_series_regression_check_forecaster_unbound, error(instantiation_error)) :-
		time_series_regression::check_forecaster(_).

	test(time_series_regression_check_forecaster_invalid, error(domain_error(forecaster, foo))) :-
		time_series_regression::check_forecaster(foo).

	test(time_series_regression_invalid_forecaster_coefficient_count, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, State, ar_parameters(Intercept, [Coefficient| _]), Diagnostics), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, State, ar_parameters(Intercept, [Coefficient]), Diagnostics)).

	test(time_series_regression_invalid_forecaster_window_length, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, ar_state([Value| _], Levels), Parameters, Diagnostics), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, ar_state([Value], Levels), Parameters, Diagnostics)).

	test(time_series_regression_invalid_forecaster_levels_length, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, ar_state(Window, _), Parameters, Diagnostics), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, ar_state(Window, [1.0]), Parameters, Diagnostics)).

	test(time_series_regression_invalid_forecaster_diagnostics, false) :-
		time_series_regression::learn(ar2_series, time_series_regression_forecaster(Model, State, Parameters, _), [order(2)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, State, Parameters, [])).

	test(time_series_regression_invalid_forecaster_nonzero_intercept_without_intercept, false) :-
		time_series_regression::learn(decay_series, time_series_regression_forecaster(Model, State, ar_parameters(_, Coefficients), Diagnostics), [intercept(false)]),
		time_series_regression::valid_forecaster(time_series_regression_forecaster(Model, State, ar_parameters(1.0, Coefficients), Diagnostics)).

	test(time_series_regression_diagnostics_2, deterministic) :-
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

	test(time_series_regression_diagnostic_2_enumeration, deterministic(Names == [model, training_series_length, options])) :-
		time_series_regression::learn(ar1_series, Forecaster),
		findall(Name, (time_series_regression::diagnostic(Forecaster, Diagnostic), functor(Diagnostic, Name, 1)), [Name1, Name2, Name3| _]),
		Names = [Name1, Name2, Name3].

	test(time_series_regression_learn_2_same_as_learn_3, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster0),
		time_series_regression::learn(ar2_series, Forecaster1, []),
		Forecaster0 == Forecaster1.

	% export and printing

	test(time_series_regression_export_to_clauses_4, deterministic) :-
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::export_to_clauses(ar2_series, Forecaster, forecaster_model, [Clause]),
		Clause == forecaster_model(Forecaster).

	test(time_series_regression_export_to_file_4_header, deterministic) :-
		^^file_path('test_output.pl', File),
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::export_to_file(ar2_series, Forecaster, forecaster_model, File),
		header_lines(File, [Line1, Line2, Line3, _, Line5]),
		Line1 == '% exported forecaster predicate: forecaster_model/1',
		Line2 == '% training dataset: ar2_series',
		Line3 == '% training series length: 10',
		Line5 == '% forecaster_model(Forecaster)'.

	test(time_series_regression_export_to_file_4_loadable, deterministic) :-
		^^file_path('test_output.pl', File),
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::export_to_file(ar2_series, Forecaster, forecaster_model, File),
		logtalk_load(File),
		{forecaster_model(Loaded)},
		time_series_regression::valid_forecaster(Loaded),
		time_series_regression::forecast(Forecaster, 2, Forecasts0),
		time_series_regression::forecast(Loaded, 2, Forecasts1),
		Forecasts1 =~= Forecasts0.

	test(time_series_regression_print_forecaster_1, deterministic) :-
		^^suppress_text_output,
		time_series_regression::learn(ar2_series, Forecaster, [order(2)]),
		time_series_regression::print_forecaster(Forecaster).

	% auxiliary predicates

	parameters(time_series_regression_forecaster(_, _, ar_parameters(Intercept, Coefficients), _), Intercept, Coefficients).

	bracketed([], [], []).
	bracketed([Point| Points], [Lower| Lowers], [Upper| Uppers]) :-
		Lower =< Point,
		Point =< Upper,
		bracketed(Points, Lowers, Uppers).

	widths([], [], []).
	widths([Lower| Lowers], [Upper| Uppers], [Width| Widths]) :-
		Width is Upper - Lower,
		widths(Lowers, Uppers, Widths).

	non_decreasing([_]) :-
		!.
	non_decreasing([Width1, Width2| Widths]) :-
		Width1 =< Width2 + 1.0e-9,
		non_decreasing([Width2| Widths]).

	narrower([], []).
	narrower([Width0| Widths0], [Width1| Widths1]) :-
		Width0 < Width1,
		narrower(Widths0, Widths1).

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

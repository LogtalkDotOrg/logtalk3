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
		comment is 'Unit tests for the "exponential_smoothing" library.'
	]).

	:- uses(lgtunit, [
		assertion/1, op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		last/2, length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	cover(exponential_smoothing).
	cover(exponential_smoothing_common).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_transformed.pl'),
		^^clean_file('test_output_box_cox_auto.pl'),
		^^clean_file('test_output_online.pl').

	% fixed-parameter models

	test(exponential_smoothing_simple_constant, deterministic(Forecasts == [5.0, 5.0, 5.0])) :-
		exponential_smoothing::learn(constant_series, Forecaster, [model(simple), alpha(0.5)]),
		exponential_smoothing::forecast(Forecaster, 3, Forecasts).

	test(exponential_smoothing_simple_linear_state, deterministic(Level =~= 18.0625)) :-
		exponential_smoothing::learn(linear_trend, exponential_smoothing_forecaster(simple, level(Level), [0.5], _Diagnostics), [model(simple), alpha(0.5)]).

	test(exponential_smoothing_simple_linear_diagnostics, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), alpha(0.5)]),
		exponential_smoothing::diagnostics(Forecaster, Diagnostics),
		assertion(memberchk(sum_squared_error(54.328125), Diagnostics)),
		assertion(memberchk(mean_squared_error(10.865625), Diagnostics)),
		assertion(memberchk(convergence(fixed_parameters), Diagnostics)),
		assertion(memberchk(iterations(0), Diagnostics)),
		assertion(memberchk(evaluations(0), Diagnostics)).

	test(exponential_smoothing_holt_linear, deterministic(Forecasts == [22.0, 24.0, 26.0])) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt), alpha(0.2), beta(0.1)]),
		exponential_smoothing::forecast(Forecaster, 3, Forecasts).

	test(exponential_smoothing_holt_regression_initialization, deterministic(Forecasts == [22.0, 24.0, 26.0])) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt), alpha(0.2), beta(0.1), initialization(regression)]),
		exponential_smoothing::forecast(Forecaster, 3, Forecasts).

	test(exponential_smoothing_holt_damped_phi_one, deterministic(DampedForecasts == HoltForecasts)) :-
		exponential_smoothing::learn(linear_trend, Holt, [model(holt), alpha(0.2), beta(0.1)]),
		exponential_smoothing::learn(linear_trend, Damped, [model(holt_damped), alpha(0.2), beta(0.1), phi(1.0)]),
		exponential_smoothing::forecast(Holt, 5, HoltForecasts),
		exponential_smoothing::forecast(Damped, 5, DampedForecasts).

	test(exponential_smoothing_holt_damped_stabilizes, deterministic(LongForecast < 30.0)) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt_damped), alpha(0.2), beta(0.1), phi(0.5)]),
		exponential_smoothing::forecast(Forecaster, 100, Forecasts),
		last(Forecasts, LongForecast).

	test(exponential_smoothing_holt_damped_automatic_phi, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt_damped), alpha(0.2), beta(0.1), optimizer_options([max_iterations(50)])]),
		Forecaster = exponential_smoothing_forecaster(holt_damped, holt_damped(_Level, _Trend, Phi), [0.2, 0.1, Phi], _Diagnostics),
		assertion(Phi > 0.0),
		assertion(Phi =< 1.0).

	test(exponential_smoothing_additive_cycle, deterministic(Forecasts == [10.0, 20.0, 15.0, 5.0, 10.0, 20.0])) :-
		exponential_smoothing::learn(seasonal_additive, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::forecast(Forecaster, 6, Forecasts).

	test(exponential_smoothing_additive_damped_phi_one, deterministic(DampedForecasts == Forecasts)) :-
		exponential_smoothing::learn(seasonal_additive, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::learn(seasonal_additive, DampedForecaster, [model(holt_winters_additive_damped), alpha(0.2), beta(0.1), gamma(0.1), phi(1.0)]),
		exponential_smoothing::forecast(Forecaster, 8, Forecasts),
		exponential_smoothing::forecast(DampedForecaster, 8, DampedForecasts).

	test(exponential_smoothing_multiplicative_cycle, deterministic) :-
		exponential_smoothing::learn(seasonal_multiplicative, Forecaster, [model(holt_winters_multiplicative), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::forecast(Forecaster, 4, [First, Second, Third, Fourth]),
		assertion(First =~= 100.0),
		assertion(Second =~= 200.0),
		assertion(Third =~= 150.0),
		assertion(Fourth =~= 50.0).

	test(exponential_smoothing_multiplicative_damped_phi_one, deterministic(DampedForecasts == Forecasts)) :-
		exponential_smoothing::learn(seasonal_multiplicative, Forecaster, [model(holt_winters_multiplicative), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::learn(seasonal_multiplicative, DampedForecaster, [model(holt_winters_multiplicative_damped), alpha(0.2), beta(0.1), gamma(0.1), phi(1.0)]),
		exponential_smoothing::forecast(Forecaster, 8, Forecasts),
		exponential_smoothing::forecast(DampedForecaster, 8, DampedForecasts).

	test(exponential_smoothing_multiplicative_damped_automatic, deterministic) :-
		exponential_smoothing::learn(seasonal_multiplicative_trend_partial, Forecaster, [model(holt_winters_multiplicative_damped), initialization(regression), optimizer_options([max_iterations(50)])]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_multiplicative_damped, holt_winters_damped(_Level, _Trend, Phi, 4, _Seasonals), [Alpha, Beta, Gamma, Phi], _Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Beta >= 0.0),
		assertion(Gamma >= 0.0),
		assertion(Phi > 0.0),
		exponential_smoothing::forecast(Forecaster, 4, [First, Second, Third, Fourth]),
		assertion(First > 0.0),
		assertion(Second > 0.0),
		assertion(Third > 0.0),
		assertion(Fourth > 0.0).

	test(exponential_smoothing_additive_trend_partial_cycle, deterministic) :-
		exponential_smoothing::learn(seasonal_additive_trend_partial, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::forecast(Forecaster, 4, [First, Second, Third, Fourth]),
		assertion(First =~= 31.0),
		assertion(Second =~= 33.0),
		assertion(Third =~= 43.0),
		assertion(Fourth =~= 41.0).

	test(exponential_smoothing_multiplicative_trend_partial_cycle, deterministic) :-
		exponential_smoothing::learn(seasonal_multiplicative_trend_partial, Forecaster, [model(holt_winters_multiplicative), alpha(0.0), beta(0.0), gamma(0.0)]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_multiplicative, holt_winters(_Level, _Trend, 4, [_NextSeasonal| _]), _Parameters, _Diagnostics),
		exponential_smoothing::forecast(Forecaster, 5, Forecasts),
		Forecasts = [First, Second, Third, Fourth, Fifth],
		assertion(First > 0.0),
		assertion(Second > 0.0),
		assertion(Third > 0.0),
		assertion(Fourth > 0.0),
		assertion(Fifth > 0.0),
		assertion(First =\= Fifth).

	test(exponential_smoothing_multiplicative_regression_initialization, deterministic) :-
		exponential_smoothing::learn(seasonal_multiplicative_trend_partial, Forecaster, [model(holt_winters_multiplicative), alpha(0.0), beta(0.0), gamma(0.0), initialization(regression)]),
		exponential_smoothing::forecast(Forecaster, 5, Forecasts),
		Forecasts = [First, Second, Third, Fourth, Fifth],
		assertion(First > 0.0),
		assertion(Second > 0.0),
		assertion(Third > 0.0),
		assertion(Fourth > 0.0),
		assertion(Fifth > 0.0),
		assertion(First =\= Fifth).

	test(exponential_smoothing_multiplicative_regression_automatic, deterministic) :-
		exponential_smoothing::learn(seasonal_multiplicative_trend_partial, Forecaster, [model(holt_winters_multiplicative), initialization(regression), optimizer_options([max_iterations(50)])]),
		exponential_smoothing::forecast(Forecaster, 4, Forecasts),
		assertion(Forecasts = [_, _, _, _]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_multiplicative, _State, [Alpha, Beta, Gamma], _Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Beta >= 0.0),
		assertion(Gamma >= 0.0).

	test(exponential_smoothing_seasonal_minimum_length, deterministic(Forecasts == [20.0, 10.0])) :-
		exponential_smoothing::learn(minimum_length_seasonal, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::forecast(Forecaster, 2, Forecasts).

	test(exponential_smoothing_unit_seasonal_frequency, error(domain_error(seasonal_frequency, 1))) :-
		exponential_smoothing::learn(unit_frequency_seasonal, _Forecaster, [model(holt_winters_additive)]).

	test(exponential_smoothing_seasonal_multiple_queue_rotations, deterministic(Forecasts == [10.0, 20.0, 10.0, 20.0])) :-
		exponential_smoothing::learn(seasonal_additive_many_cycles, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::forecast(Forecaster, 4, Forecasts).

	test(exponential_smoothing_seasonal_regression_initialization, deterministic) :-
		exponential_smoothing::learn(seasonal_additive_many_cycles, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1), initialization(regression), initial_cycles(3)]),
		exponential_smoothing::forecast(Forecaster, 4, Forecasts),
		assertion(Forecasts == [10.0, 20.0, 10.0, 20.0]),
		exponential_smoothing::forecaster_options(Forecaster, Options),
		assertion(memberchk(initialization(regression), Options)),
		assertion(memberchk(initial_cycles(3), Options)).

	test(exponential_smoothing_regression_initialization_short_series, error(domain_error(series_length, seasonal_additive_trend_partial))) :-
		exponential_smoothing::learn(seasonal_additive_trend_partial, _Forecaster, [model(holt_winters_additive), initialization(regression), initial_cycles(3)]).

	test(exponential_smoothing_optimized_simple_fixed_smoothing, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), alpha(0.5), initialization(optimized), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(simple, level(_Level), [0.5], Diagnostics),
		memberchk(options(Options), Diagnostics),
		assertion(memberchk(initialization(optimized), Options)),
		memberchk(evaluations(Evaluations), Diagnostics),
		assertion(Evaluations > 0),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_optimized_additive_zero_sum, deterministic) :-
		exponential_smoothing::learn(seasonal_additive_many_cycles, Forecaster, [model(holt_winters_additive), alpha(0.0), beta(0.0), gamma(0.0), initialization(optimized), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_additive, holt_winters(_Level, _Trend, 2, Seasonal), [0.0, 0.0, 0.0], Diagnostics),
		sum(Seasonal, SeasonalSum),
		assertion(SeasonalSum =~= 0.0),
		memberchk(evaluations(Evaluations), Diagnostics),
		assertion(Evaluations > 0).

	test(exponential_smoothing_optimized_multiplicative_mean_one, deterministic) :-
		exponential_smoothing::learn(seasonal_multiplicative_trend_partial, Forecaster, [model(holt_winters_multiplicative), alpha(0.0), beta(0.0), gamma(0.0), initialization(optimized), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_multiplicative, holt_winters(_Level, _Trend, 4, Seasonal), [0.0, 0.0, 0.0], _Diagnostics),
		sum(Seasonal, SeasonalSum),
		SeasonalMean is SeasonalSum / 4.0,
		assertion(SeasonalMean =~= 1.0).

	test(exponential_smoothing_optimized_initialization_short_series, error(domain_error(series_length, seasonal_additive_trend_partial))) :-
		exponential_smoothing::learn(seasonal_additive_trend_partial, _Forecaster, [model(holt_winters_additive), initialization(optimized), initial_cycles(3)]).

	% automatic and partially fixed fitting

	test(exponential_smoothing_simple_automatic, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(simple, _State, [Alpha], Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Alpha =< 1.0),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		assertion(SumSquaredError =< 54.328125),
		memberchk(evaluations(Evaluations), Diagnostics),
		assertion(Evaluations > 0).

	test(exponential_smoothing_holt_partially_fixed, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt), alpha(0.2), optimizer_options([max_iterations(50)])]),
		Forecaster = exponential_smoothing_forecaster(holt, _State, [Alpha, Beta], _Diagnostics),
		assertion(Alpha =~= 0.2),
		assertion(Beta >= 0.0),
		assertion(Beta =< 1.0).

	test(exponential_smoothing_seasonal_partially_fixed, deterministic) :-
		exponential_smoothing::learn(seasonal_additive_trend_partial, Forecaster, [model(holt_winters_additive), alpha(0.2), gamma(0.1), optimizer_options([max_iterations(50)])]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_additive, _State, [Alpha, Beta, Gamma], _Diagnostics),
		assertion(Alpha =~= 0.2),
		assertion(Beta >= 0.0),
		assertion(Beta =< 1.0),
		assertion(Gamma =~= 0.1).

	test(exponential_smoothing_additive_automatic, deterministic(Forecasts == [10.0, 20.0, 15.0, 5.0])) :-
		exponential_smoothing::learn(seasonal_additive, Forecaster, [model(holt_winters_additive), optimizer_options([max_iterations(100)])]),
		exponential_smoothing::forecast(Forecaster, 4, Forecasts).

	test(exponential_smoothing_learning_deterministic, deterministic(Forecaster1 == Forecaster2)) :-
		Options = [model(simple), optimizer_options([max_iterations(50)])],
		exponential_smoothing::learn(linear_trend, Forecaster1, Options),
		exponential_smoothing::learn(linear_trend, Forecaster2, Options).

	test(exponential_smoothing_optimizer_iteration_cap, deterministic) :-
		OptimizerOptions = [max_iterations(1), tol_x(0.0), tol_f(0.0)],
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), optimizer_options(OptimizerOptions)]),
		exponential_smoothing::diagnostics(Forecaster, Diagnostics),
		assertion(memberchk(convergence(maximum_iterations), Diagnostics)),
		assertion(memberchk(iterations(1), Diagnostics)).

	test(exponential_smoothing_adverse_multiplicative_automatic, deterministic) :-
		exponential_smoothing::learn(adverse_multiplicative_trajectory, Forecaster, [model(holt_winters_multiplicative), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_multiplicative, _State, [Alpha, Beta, Gamma], Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Alpha =< 1.0),
		assertion(Beta >= 0.0),
		assertion(Beta =< 1.0),
		assertion(Gamma >= 0.0),
		assertion(Gamma =< 1.0),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		assertion(SumSquaredError >= 0.0).

	% optimizer strategy selection

	test(exponential_smoothing_optimizer_default_nelder_mead, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), optimizer_options([max_iterations(50)])]),
		exponential_smoothing::diagnostics(Forecaster, Diagnostics),
		assertion(memberchk(optimizer(nelder_mead), Diagnostics)).

	test(exponential_smoothing_fixed_parameters_bypass_optimizer, deterministic) :-
		exponential_smoothing::learn(constant_series, Forecaster, [model(simple), alpha(0.5), optimizer(differential_evolution), de_options([seed(1)])]),
		exponential_smoothing::diagnostics(Forecaster, Diagnostics),
		assertion(memberchk(optimizer(differential_evolution), Diagnostics)),
		assertion(memberchk(convergence(fixed_parameters), Diagnostics)),
		assertion(memberchk(iterations(0), Diagnostics)),
		assertion(memberchk(evaluations(0), Diagnostics)).

	test(exponential_smoothing_multi_start_deterministic_repeat, deterministic(Forecaster1 == Forecaster2)) :-
		Options = [model(simple), optimizer(multi_start(4)), optimizer_options([max_iterations(50)])],
		exponential_smoothing::learn(linear_trend, Forecaster1, Options),
		exponential_smoothing::learn(linear_trend, Forecaster2, Options).

	test(exponential_smoothing_multi_start_not_worse_than_default, deterministic) :-
		exponential_smoothing::learn(linear_trend, DefaultForecaster, [model(simple), optimizer_options([max_iterations(100)])]),
		exponential_smoothing::learn(linear_trend, MultiStartForecaster, [model(simple), optimizer(multi_start(5)), optimizer_options([max_iterations(100)])]),
		exponential_smoothing::diagnostics(DefaultForecaster, DefaultDiagnostics),
		exponential_smoothing::diagnostics(MultiStartForecaster, MultiStartDiagnostics),
		memberchk(sum_squared_error(DefaultSumSquaredError), DefaultDiagnostics),
		memberchk(sum_squared_error(MultiStartSumSquaredError), MultiStartDiagnostics),
		assertion(MultiStartSumSquaredError =< DefaultSumSquaredError + 1.0e-9).

	test(exponential_smoothing_multi_start_partially_fixed, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt), alpha(0.2), optimizer(multi_start(3)), optimizer_options([max_iterations(50)])]),
		Forecaster = exponential_smoothing_forecaster(holt, _State, [Alpha, Beta], _Diagnostics),
		assertion(Alpha =~= 0.2),
		assertion(Beta >= 0.0),
		assertion(Beta =< 1.0).

	test(exponential_smoothing_multi_start_adverse_multiplicative, deterministic) :-
		exponential_smoothing::learn(adverse_multiplicative_trajectory, Forecaster, [model(holt_winters_multiplicative), optimizer(multi_start(4)), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_multiplicative, _State, [Alpha, Beta, Gamma], Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Alpha =< 1.0),
		assertion(Beta >= 0.0),
		assertion(Beta =< 1.0),
		assertion(Gamma >= 0.0),
		assertion(Gamma =< 1.0),
		assertion(memberchk(optimizer(multi_start(4)), Diagnostics)),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		assertion(SumSquaredError >= 0.0).

	test(exponential_smoothing_multi_start_valid_forecaster, deterministic(exponential_smoothing::valid_forecaster(Forecaster))) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), optimizer(multi_start(3)), optimizer_options([max_iterations(50)])]).

	test(exponential_smoothing_de_automatic, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), optimizer(differential_evolution), de_options([seed(11), population_size(10), max_generations(20)])]),
		Forecaster = exponential_smoothing_forecaster(simple, _State, [Alpha], Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Alpha =< 1.0),
		assertion(memberchk(optimizer(differential_evolution), Diagnostics)),
		memberchk(evaluations(Evaluations), Diagnostics),
		assertion(Evaluations > 0).

	test(exponential_smoothing_de_deterministic_repeat, deterministic(Forecaster1 == Forecaster2)) :-
		Options = [model(simple), optimizer(differential_evolution), de_options([seed(7), population_size(10), max_generations(15)])],
		exponential_smoothing::learn(linear_trend, Forecaster1, Options),
		exponential_smoothing::learn(linear_trend, Forecaster2, Options).

	test(exponential_smoothing_de_polish_not_worse, deterministic) :-
		exponential_smoothing::learn(linear_trend, PolishedForecaster, [model(simple), optimizer(differential_evolution), de_options([seed(11), population_size(10), max_generations(20), polish(true)])]),
		exponential_smoothing::learn(linear_trend, UnpolishedForecaster, [model(simple), optimizer(differential_evolution), de_options([seed(11), population_size(10), max_generations(20), polish(false)])]),
		exponential_smoothing::diagnostics(PolishedForecaster, PolishedDiagnostics),
		exponential_smoothing::diagnostics(UnpolishedForecaster, UnpolishedDiagnostics),
		memberchk(sum_squared_error(PolishedSumSquaredError), PolishedDiagnostics),
		memberchk(sum_squared_error(UnpolishedSumSquaredError), UnpolishedDiagnostics),
		assertion(PolishedSumSquaredError =< UnpolishedSumSquaredError + 1.0e-9).

	test(exponential_smoothing_de_valid_forecaster, deterministic(exponential_smoothing::valid_forecaster(Forecaster))) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), optimizer(differential_evolution), de_options([seed(5), population_size(10), max_generations(15)])]).

	test(exponential_smoothing_de_export_reload, deterministic(LoadedForecaster == Forecaster)) :-
		exponential_smoothing::learn(constant_series, Forecaster, [model(simple), optimizer(differential_evolution), de_options([seed(3), population_size(8), max_generations(10)])]),
		exponential_smoothing::export_to_clauses(constant_series, Forecaster, forecast_model_de, [Clause]),
		Clause = forecast_model_de(LoadedForecaster).

	% model and frequency selection

	test(exponential_smoothing_model_auto_aicc_deterministic, deterministic(Forecaster1 == Forecaster2)) :-
		Options = [model(auto), candidate_models([simple, holt]), selection_criterion(aicc), optimizer_options([max_iterations(50)])],
		exponential_smoothing::learn(long_linear_trend, Forecaster1, Options),
		exponential_smoothing::learn(long_linear_trend, Forecaster2, Options).

	test(exponential_smoothing_model_auto_aicc_selects_holt, deterministic(Method == holt)) :-
		exponential_smoothing::learn(long_linear_trend, Forecaster, [model(auto), candidate_models([simple, holt]), selection_criterion(aicc), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(Method, _State, _Parameters, Diagnostics),
		assertion(memberchk(model_selection(criterion(aicc), _Candidates, selected(holt)), Diagnostics)).

	test(exponential_smoothing_model_auto_validation, deterministic(Method == holt)) :-
		exponential_smoothing::learn(long_linear_trend, Forecaster, [model(auto), candidate_models([simple, holt]), selection_criterion(validation(2)), optimizer_options([max_iterations(100)])]),
		Forecaster = exponential_smoothing_forecaster(Method, _State, _Parameters, Diagnostics),
		assertion(memberchk(model_selection(criterion(validation(2)), _Candidates, selected(holt)), Diagnostics)).

	test(exponential_smoothing_explicit_frequency_override, deterministic) :-
		exponential_smoothing::learn(seasonal_additive_many_cycles, Forecaster, [model(holt_winters_additive), frequency(2), alpha(0.2), beta(0.1), gamma(0.1)]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_additive, holt_winters(_Level, _Trend, 2, _Seasonals), _Parameters, _Diagnostics).

	test(exponential_smoothing_automatic_frequency_known_period, deterministic(Frequency == 2)) :-
		exponential_smoothing::learn(seasonal_additive_many_cycles, Forecaster, [model(holt_winters_additive), frequency(auto), frequency_candidates([2, 3]), alpha(0.2), beta(0.1), gamma(0.1)]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_additive, holt_winters(_Level, _Trend, Frequency, _Seasonals), _Parameters, Diagnostics),
		assertion(memberchk(frequency_selection(_Candidates, selected(2)), Diagnostics)).

	test(exponential_smoothing_model_auto_invalid_candidates, error(domain_error(option, candidate_models([simple, unknown])))) :-
		exponential_smoothing::learn(linear_trend, _Forecaster, [model(auto), candidate_models([simple, unknown])]).

	test(exponential_smoothing_frequency_auto_missing_candidates, error(domain_error(missing_frequency_candidates, holt_winters_additive))) :-
		exponential_smoothing::learn(seasonal_additive, _Forecaster, [model(holt_winters_additive), frequency(auto)]).

	% protocol lifecycle

	test(exponential_smoothing_learn_2, deterministic(ground(Forecaster))) :-
		exponential_smoothing::learn(constant_series, Forecaster).

	test(exponential_smoothing_valid_forecaster, deterministic(exponential_smoothing::valid_forecaster(Forecaster))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]).

	test(exponential_smoothing_forecast_zero_horizon, deterministic(Forecasts == [])) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::forecast(Forecaster, 0, Forecasts).

	test(exponential_smoothing_diagnostics, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt), alpha(0.2), beta(0.1)]),
		findall(Diagnostic, exponential_smoothing::diagnostic(Forecaster, Diagnostic), Diagnostics),
		assertion(memberchk(model(exponential_smoothing), Diagnostics)),
		assertion(memberchk(method(holt), Diagnostics)),
		assertion(memberchk(parameters([0.2, 0.1]), Diagnostics)),
		exponential_smoothing::forecaster_options(Forecaster, Options),
		assertion(memberchk(model(holt), Options)).

	test(exponential_smoothing_export_to_clauses, deterministic(LoadedForecaster == Forecaster)) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::export_to_clauses(constant_series, Forecaster, forecast_model, [Clause]),
		Clause = forecast_model(LoadedForecaster).

	test(exponential_smoothing_export_to_file, deterministic(Forecasts == [5.0, 5.0])) :-
		^^file_path('test_output.pl', File),
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::export_to_file(constant_series, Forecaster, forecast_model_1, File),
		logtalk_load(File),
		{forecast_model_1(LoadedForecaster)},
		exponential_smoothing::forecast(LoadedForecaster, 2, Forecasts).

	test(exponential_smoothing_print_forecaster, deterministic) :-
		^^suppress_text_output,
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::print_forecaster(Forecaster).

	% immutable online updates

	test(exponential_smoothing_update_all_methods_match_batch, deterministic(length(Methods, 7))) :-
		Updates = [20, 15, 5, 10, 20, 15, 5, 10],
		findall(
			Method,
			( online_method_options(Method, Options),
				exponential_smoothing::learn(online_update_prefix, PrefixForecaster, Options),
				update_observations(Updates, PrefixForecaster, OnlineForecaster),
				exponential_smoothing::learn(online_update_full, BatchForecaster, Options),
				assert_online_batch_equivalent(Method, OnlineForecaster, BatchForecaster),
				exponential_smoothing::diagnostics(OnlineForecaster, Diagnostics),
				assertion(memberchk(training_series_length(17), Diagnostics)),
				assertion(memberchk(update_count(8), Diagnostics))
			),
			Methods
		).

	test(exponential_smoothing_update_transformed_damped_matches_batch, deterministic) :-
		Options = [model(holt_damped), alpha(0.2), beta(0.1), phi(0.9), transformation(log)],
		exponential_smoothing::learn(online_update_prefix, PrefixForecaster, Options),
		update_observations([20, 15, 5, 10, 20, 15, 5, 10], PrefixForecaster, OnlineForecaster),
		exponential_smoothing::learn(online_update_full, BatchForecaster, Options),
		assert_online_batch_equivalent(holt_damped, OnlineForecaster, BatchForecaster).

	test(exponential_smoothing_update_missing_matches_batch, deterministic) :-
		Options = [model(holt_damped), alpha(0.2), beta(0.1), phi(0.9), missing_policy(skip_update)],
		exponential_smoothing::learn(online_missing_prefix, PrefixForecaster, Options),
		exponential_smoothing::update(PrefixForecaster, missing, MissingForecaster),
		exponential_smoothing::update(MissingForecaster, 12, OnlineForecaster),
		exponential_smoothing::learn(online_missing_full, BatchForecaster, Options),
		assert_online_batch_equivalent(holt_damped, OnlineForecaster, BatchForecaster),
		exponential_smoothing::diagnostics(OnlineForecaster, Diagnostics),
		assertion(memberchk(observed_count(5), Diagnostics)),
		assertion(memberchk(missing_count(1), Diagnostics)),
		assertion(memberchk(update_count(2), Diagnostics)).

	test(exponential_smoothing_update_retains_residuals_and_options, deterministic) :-
		Options = [model(simple), alpha(0.2), retain_residuals(true)],
		exponential_smoothing::learn(online_update_prefix, PrefixForecaster, Options),
		exponential_smoothing::update(PrefixForecaster, 20, UpdatedForecaster),
		exponential_smoothing::forecaster_options(PrefixForecaster, TrainingOptions),
		exponential_smoothing::forecaster_options(UpdatedForecaster, TrainingOptions),
		exponential_smoothing::diagnostics(PrefixForecaster, PrefixDiagnostics),
		exponential_smoothing::diagnostics(UpdatedForecaster, UpdatedDiagnostics),
		memberchk(residuals(PrefixResiduals), PrefixDiagnostics),
		memberchk(residuals(UpdatedResiduals), UpdatedDiagnostics),
		length(PrefixResiduals, PrefixCount),
		length(UpdatedResiduals, UpdatedCount),
		assertion(UpdatedCount =:= PrefixCount + 1),
		assertion(memberchk(update_count(0), PrefixDiagnostics)),
		assertion(memberchk(update_count(1), UpdatedDiagnostics)).

	test(exponential_smoothing_update_export_reload, deterministic(LoadedForecaster == UpdatedForecaster)) :-
		^^file_path('test_output_online.pl', File),
		exponential_smoothing::learn(online_update_prefix, Forecaster, [model(simple), alpha(0.2)]),
		exponential_smoothing::update(Forecaster, 20, UpdatedForecaster),
		exponential_smoothing::export_to_file(online_update_prefix, UpdatedForecaster, online_model, File),
		logtalk_load(File),
		{online_model(LoadedForecaster)},
		assertion(exponential_smoothing::valid_forecaster(LoadedForecaster)).

	test(exponential_smoothing_update_deterministic, deterministic(UpdatedForecaster1 == UpdatedForecaster2)) :-
		exponential_smoothing::learn(online_update_prefix, Forecaster, [model(holt), alpha(0.2), beta(0.1)]),
		exponential_smoothing::update(Forecaster, 20, UpdatedForecaster1),
		exponential_smoothing::update(Forecaster, 20, UpdatedForecaster2).

	test(exponential_smoothing_update_variable_forecaster, error(instantiation_error)) :-
		exponential_smoothing::update(_Forecaster, 10, _UpdatedForecaster).

	test(exponential_smoothing_update_malformed_forecaster, error(domain_error(forecaster, malformed))) :-
		exponential_smoothing::update(malformed, 10, _UpdatedForecaster).

	test(exponential_smoothing_update_variable_observation, error(instantiation_error)) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::update(Forecaster, _Observation, _UpdatedForecaster).

	test(exponential_smoothing_update_invalid_observation, error(type_error(number, bad))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::update(Forecaster, bad, _UpdatedForecaster).

	test(exponential_smoothing_update_invalid_transformed_observation, error(domain_error(positive_transformation_series, 0))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5), transformation(log)]),
		exponential_smoothing::update(Forecaster, 0, _UpdatedForecaster).

	test(exponential_smoothing_update_invalid_multiplicative_observation, error(domain_error(positive_multiplicative_series, 0))) :-
		exponential_smoothing::learn(seasonal_multiplicative, Forecaster, [model(holt_winters_multiplicative), alpha(0.2), beta(0.1), gamma(0.1)]),
		exponential_smoothing::update(Forecaster, 0, _UpdatedForecaster).

	% validation and error cases

	test(exponential_smoothing_invalid_model, error(domain_error(option, model(unknown)))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [model(unknown)]).

	test(exponential_smoothing_invalid_alpha, error(domain_error(option, alpha(1.1)))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [alpha(1.1)]).

	test(exponential_smoothing_irrelevant_beta, error(domain_error(exponential_smoothing_parameter, beta(0.1)))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [model(simple), beta(0.1)]).

	test(exponential_smoothing_invalid_optimizer_option, error(domain_error(option, optimizer_options([objective(maximize)])))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [optimizer_options([objective(maximize)])]).

	test(exponential_smoothing_invalid_optimizer_selection, error(domain_error(option, optimizer(unknown)))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [optimizer(unknown)]).

	test(exponential_smoothing_invalid_multi_start_count, error(domain_error(option, optimizer(multi_start(0))))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [optimizer(multi_start(0))]).

	test(exponential_smoothing_invalid_de_option, error(domain_error(option, de_options([population_size(2)])))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [optimizer(differential_evolution), de_options([population_size(2)])]).

	test(exponential_smoothing_invalid_phi, error(domain_error(option, phi(0.0)))) :-
		exponential_smoothing::learn(linear_trend, _Forecaster, [model(holt_damped), phi(0.0)]).

	test(exponential_smoothing_holt_short_series, error(domain_error(series_length, short_series))) :-
		exponential_smoothing::learn(short_series, _Forecaster, [model(holt)]).

	test(exponential_smoothing_missing_frequency, error(domain_error(seasonal_frequency, missing_frequency_series))) :-
		exponential_smoothing::learn(missing_frequency_series, _Forecaster, [model(holt_winters_additive)]).

	test(exponential_smoothing_non_positive_multiplicative, error(domain_error(positive_multiplicative_series, 0))) :-
		exponential_smoothing::learn(non_positive_seasonal_series, _Forecaster, [model(holt_winters_multiplicative)]).

	test(exponential_smoothing_infeasible_fixed_multiplicative, error(domain_error(positive_multiplicative_level, -386.0))) :-
		exponential_smoothing::learn(adverse_multiplicative_trajectory, _Forecaster, [model(holt_winters_multiplicative), alpha(0.2), beta(0.1), gamma(0.1)]).

	test(exponential_smoothing_gap_index, error(domain_error(series_index_sequence, gap_index))) :-
		exponential_smoothing::learn(gap_index, _Forecaster).

	test(exponential_smoothing_non_numeric_value, error(type_error(number, bad))) :-
		exponential_smoothing::learn(non_numeric_value, _Forecaster).

	test(exponential_smoothing_negative_horizon, error(domain_error(non_negative_integer, -1))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::forecast(Forecaster, -1, _Forecasts).

	test(exponential_smoothing_variable_forecaster, error(instantiation_error)) :-
		exponential_smoothing::check_forecaster(_Forecaster).

	test(exponential_smoothing_invalid_forecaster_parameters, fail) :-
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options([]), method(simple), parameters([1.2])],
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, level(5.0), [1.2], Diagnostics)).

	test(exponential_smoothing_invalid_forecaster_state, fail) :-
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options([]), method(simple), parameters([0.5])],
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, holt(5.0, 0.0), [0.5], Diagnostics)).

	test(exponential_smoothing_inconsistent_forecaster_frequency, fail) :-
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options([]), method(holt_winters_additive), frequency(3), parameters([0.2, 0.1, 0.1])],
		State = holt_winters(5.0, 0.0, 2, [1.0, 1.0]),
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(holt_winters_additive, State, [0.2, 0.1, 0.1], Diagnostics)).

	test(exponential_smoothing_inconsistent_forecaster_options, fail) :-
		Options = [model(holt), alpha(0.9), beta(0.1), gamma(auto), optimizer_options([])],
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options(Options), method(simple), parameters([0.5]), sum_squared_error(1.0), mean_squared_error(0.25), convergence(fixed_parameters), iterations(0), evaluations(0)],
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, level(5.0), [0.5], Diagnostics)).

	test(exponential_smoothing_inconsistent_forecaster_convergence, fail) :-
		Options = [model(simple), alpha(auto), beta(auto), gamma(auto), optimizer_options([max_iterations(50)])],
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options(Options), method(simple), parameters([0.5]), sum_squared_error(1.0), mean_squared_error(0.25), convergence(maximum_iterations), iterations(1), evaluations(2)],
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, level(5.0), [0.5], Diagnostics)).

	test(exponential_smoothing_inconsistent_forecaster_error_statistics, fail) :-
		Options = [model(simple), alpha(0.5), beta(auto), gamma(auto), optimizer_options([])],
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options(Options), method(simple), parameters([0.5]), sum_squared_error(1.0), mean_squared_error(0.5), convergence(fixed_parameters), iterations(0), evaluations(0)],
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, level(5.0), [0.5], Diagnostics)).

	test(exponential_smoothing_non_positive_multiplicative_forecast, error(domain_error(positive_multiplicative_forecast, -1.0))) :-
		Options = [model(holt_winters_multiplicative), alpha(0.2), beta(0.1), gamma(0.1), phi(auto), initialization(two_cycles), initial_cycles(2), optimizer(nelder_mead), optimizer_options([]), de_options([]), transformation(none), bias_adjustment(none), retain_residuals(false), selection_criterion(aicc), candidate_models(default), frequency(dataset), frequency_candidates(none), missing_value(missing), missing_policy(error)],
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options(Options), method(holt_winters_multiplicative), frequency(2), parameters([0.2, 0.1, 0.1]), sum_squared_error(1.0), mean_squared_error(1.0), optimizer(nelder_mead), convergence(fixed_parameters), iterations(0), evaluations(0), observed_count(5), missing_count(0), scored_count(1), update_count(0), residuals(none)],
		State = holt_winters(1.0, -2.0, 2, [1.0, 1.0]),
		exponential_smoothing::forecast(exponential_smoothing_forecaster(holt_winters_multiplicative, State, [0.2, 0.1, 0.1], Diagnostics), 1, _Forecasts).

	% missing observations

	test(exponential_smoothing_missing_default_error, error(type_error(number, missing))) :-
		exponential_smoothing::learn(missing_nonseasonal, _Forecaster, [model(simple), alpha(0.5)]).

	test(exponential_smoothing_invalid_missing_policy, error(domain_error(option, missing_policy(impute)))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [missing_policy(impute)]).

	test(exponential_smoothing_missing_leading_consecutive_trailing, deterministic) :-
		exponential_smoothing::learn(missing_nonseasonal, Forecaster, [model(simple), alpha(0.5), missing_policy(skip_update)]),
		Forecaster = exponential_smoothing_forecaster(simple, level(Level), [0.5], Diagnostics),
		assertion(Level =~= 7.5),
		assertion(memberchk(observed_count(3), Diagnostics)),
		assertion(memberchk(missing_count(4), Diagnostics)),
		assertion(memberchk(scored_count(2), Diagnostics)),
		assertion(memberchk(sum_squared_error(61.0), Diagnostics)),
		assertion(memberchk(mean_squared_error(30.5), Diagnostics)).

	test(exponential_smoothing_missing_holt_prediction_advance, deterministic(Forecasts == [14.0])) :-
		exponential_smoothing::learn(missing_nonseasonal, Forecaster, [model(holt), alpha(0.0), beta(0.0), missing_policy(skip_update)]),
		exponential_smoothing::forecast(Forecaster, 1, Forecasts).

	test(exponential_smoothing_missing_custom_marker_transformation, deterministic) :-
		exponential_smoothing::learn(custom_missing_marker, Forecaster, [model(simple), alpha(0.5), transformation(log), missing_value(na), missing_policy(skip_update)]),
		exponential_smoothing::forecast(Forecaster, 1, [Forecast]),
		assertion(Forecast > 0.0).

	test(exponential_smoothing_missing_all_seven_methods, deterministic(length(Forecasters, 7))) :-
		findall(
			Forecaster,
			( missing_method_options(Method, Dataset, Parameters),
				exponential_smoothing::learn(Dataset, Forecaster, [model(Method), missing_policy(skip_update)| Parameters]),
				exponential_smoothing::valid_forecaster(Forecaster)
			),
			Forecasters
		).

	test(exponential_smoothing_missing_seasonal_phase_advance, deterministic) :-
		exponential_smoothing::learn(missing_seasonal, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1), missing_policy(skip_update)]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_additive, holt_winters(_Level, _Trend, 2, SeasonalQueue), _Parameters, Diagnostics),
		assertion(length(SeasonalQueue, 2)),
		assertion(memberchk(observed_count(6), Diagnostics)),
		assertion(memberchk(missing_count(3), Diagnostics)),
		assertion(memberchk(scored_count(3), Diagnostics)).

	test(exponential_smoothing_missing_initial_seasonal_phase, error(domain_error(insufficient_seasonal_phase_observations, 1))) :-
		exponential_smoothing::learn(missing_seasonal_phase, _Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1), missing_policy(skip_update)]).

	test(exponential_smoothing_missing_automatic_parameters, deterministic) :-
		exponential_smoothing::learn(missing_nonseasonal, Forecaster, [model(simple), missing_policy(skip_update), optimizer_options([max_iterations(20)])]),
		Forecaster = exponential_smoothing_forecaster(simple, _State, [Alpha], Diagnostics),
		assertion(Alpha >= 0.0),
		assertion(Alpha =< 1.0),
		assertion(memberchk(scored_count(2), Diagnostics)).

	test(exponential_smoothing_missing_automatic_model, deterministic) :-
		exponential_smoothing::learn(missing_seasonal, Forecaster, [model(auto), candidate_models([simple, holt]), missing_policy(skip_update), optimizer_options([max_iterations(20)])]),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_missing_automatic_frequency, deterministic) :-
		exponential_smoothing::learn(missing_seasonal, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1), frequency(auto), frequency_candidates([2]), missing_policy(skip_update)]),
		Forecaster = exponential_smoothing_forecaster(holt_winters_additive, holt_winters(_Level, _Trend, 2, _Seasonals), _Parameters, _Diagnostics).

	test(exponential_smoothing_missing_deterministic_repeat, deterministic(Forecaster1 == Forecaster2)) :-
		Options = [model(holt_damped), missing_policy(skip_update), optimizer_options([max_iterations(20)])],
		exponential_smoothing::learn(missing_nonseasonal, Forecaster1, Options),
		exponential_smoothing::learn(missing_nonseasonal, Forecaster2, Options).

	test(exponential_smoothing_missing_persisted_count_validation, fail) :-
		exponential_smoothing::learn(missing_nonseasonal, Forecaster, [model(simple), alpha(0.5), missing_policy(skip_update)]),
		Forecaster = exponential_smoothing_forecaster(Method, State, Parameters, Diagnostics),
		replace_missing_count(Diagnostics, InvalidDiagnostics),
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(Method, State, Parameters, InvalidDiagnostics)).

	% transformations

	test(exponential_smoothing_log_transformation_forecast, deterministic) :-
		exponential_smoothing::learn(constant_series, Forecaster, [model(simple), alpha(0.5), transformation(log)]),
		Forecaster = exponential_smoothing_forecaster(simple, transformed(log, level(Level), residual_variance(Variance)), [0.5], _Diagnostics),
		ExpectedLevel is log(5.0),
		assertion(Level =~= ExpectedLevel),
		assertion(Variance =~= 0.0),
		exponential_smoothing::forecast(Forecaster, 3, Forecasts),
		Forecasts = [First, Second, Third],
		assertion(First =~= 5.0),
		assertion(Second =~= 5.0),
		assertion(Third =~= 5.0).

	test(exponential_smoothing_box_cox_lambda_zero_equivalence, deterministic(LogForecasts == BoxCoxForecasts)) :-
		exponential_smoothing::learn(constant_series, LogForecaster, [model(simple), alpha(0.5), transformation(log)]),
		exponential_smoothing::learn(constant_series, BoxCoxForecaster, [model(simple), alpha(0.5), transformation(box_cox(0.0))]),
		exponential_smoothing::forecast(LogForecaster, 3, LogForecasts),
		exponential_smoothing::forecast(BoxCoxForecaster, 3, BoxCoxForecasts).

	test(exponential_smoothing_transformation_positivity_rejection, error(domain_error(positive_transformation_series, 0))) :-
		exponential_smoothing::learn(non_positive_seasonal_series, _Forecaster, [model(simple), transformation(log)]).

	test(exponential_smoothing_box_cox_auto_deterministic, deterministic(Forecaster1 == Forecaster2)) :-
		Options = [model(simple), alpha(0.5), transformation(box_cox(auto)), box_cox_bounds(-0.5, 1.5)],
		exponential_smoothing::learn(linear_trend, Forecaster1, Options),
		exponential_smoothing::learn(linear_trend, Forecaster2, Options).

	test(exponential_smoothing_box_cox_auto_multi_start, deterministic) :-
		Options = [model(simple), transformation(box_cox(auto)), box_cox_bounds(0.25, 0.75), optimizer(multi_start(2)), optimizer_options([max_iterations(10)])],
		exponential_smoothing::learn(linear_trend, Forecaster, Options),
		exponential_smoothing::diagnostics(Forecaster, Diagnostics),
		assertion(memberchk(optimizer(multi_start(2)), Diagnostics)),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_box_cox_auto_bounds_and_canonical_state, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), alpha(0.5), transformation(box_cox(auto)), box_cox_bounds(0.25, 0.75)]),
		Forecaster = exponential_smoothing_forecaster(simple, transformed(box_cox(Lambda), _InnerState, residual_variance(_Variance)), [0.5], Diagnostics),
		assertion(number(Lambda)),
		assertion(Lambda >= 0.25),
		assertion(Lambda =< 0.75),
		memberchk(options(EffectiveOptions), Diagnostics),
		memberchk(transformation(EffectiveTransformation), EffectiveOptions),
		assertion(EffectiveTransformation == box_cox(Lambda)),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_box_cox_auto_aicc_counts_lambda, deterministic) :-
		Options = [model(auto), candidate_models([simple]), transformation(box_cox(auto)), box_cox_bounds(0.25, 0.75), optimizer_options([max_iterations(30)])],
		exponential_smoothing::learn(linear_trend, exponential_smoothing_forecaster(simple, _State, _Parameters, Diagnostics), Options),
		memberchk(sum_squared_error(SumSquaredError), Diagnostics),
		memberchk(model_selection(criterion(aicc), candidates([candidate(simple, ok, Score, _)]), selected(simple)), Diagnostics),
		AdjustedSumSquaredError is max(SumSquaredError, 1.0e-12),
		Expected is 5 * log(AdjustedSumSquaredError / 5) + 6 + 24,
		assertion(Score =~= Expected).

	test(exponential_smoothing_box_cox_auto_missing_marker, deterministic) :-
		exponential_smoothing::learn(custom_missing_marker, Forecaster, [model(simple), alpha(0.5), transformation(box_cox(auto)), missing_value(na), missing_policy(skip_update)]),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_box_cox_auto_positivity_rejection, error(domain_error(positive_transformation_series, 0))) :-
		exponential_smoothing::learn(non_positive_seasonal_series, _Forecaster, [model(simple), transformation(box_cox(auto))]).

	test(exponential_smoothing_box_cox_auto_model_selection_update, deterministic) :-
		Options = [model(auto), candidate_models([simple, holt]), transformation(box_cox(auto)), box_cox_bounds(-0.5, 1.0), optimizer_options([max_iterations(30)]), retain_residuals(true)],
		exponential_smoothing::learn(linear_trend, Forecaster, Options),
		exponential_smoothing::update(Forecaster, 22, UpdatedForecaster),
		assertion(exponential_smoothing::valid_forecaster(UpdatedForecaster)),
		exponential_smoothing::forecast_interval(UpdatedForecaster, 2, Lower, Upper, [samples(20), seed(7)]),
		assertion(length(Lower, 2)),
		assertion(length(Upper, 2)).

	test(exponential_smoothing_box_cox_auto_frequency_selection, deterministic) :-
		Options = [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1), frequency(auto), frequency_candidates([2, 4]), transformation(box_cox(auto)), box_cox_bounds(0.25, 0.75)],
		exponential_smoothing::learn(seasonal_additive, Forecaster, Options),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_box_cox_auto_optimized_initial_state_and_bias, deterministic) :-
		Options = [model(simple), alpha(0.5), initialization(optimized), transformation(box_cox(auto)), box_cox_bounds(0.25, 0.75), bias_adjustment(delta), optimizer_options([max_iterations(30)]), retain_residuals(true)],
		exponential_smoothing::learn(linear_trend, Forecaster, Options),
		exponential_smoothing::forecast(Forecaster, 2, Forecasts),
		assertion(length(Forecasts, 2)),
		assertion(exponential_smoothing::valid_forecaster(Forecaster)).

	test(exponential_smoothing_box_cox_auto_export_reload, deterministic) :-
		^^file_path('test_output_box_cox_auto.pl', File),
		exponential_smoothing::learn(linear_trend, Forecaster, [model(simple), alpha(0.5), transformation(box_cox(auto)), box_cox_bounds(0.25, 0.75)]),
		exponential_smoothing::export_to_file(linear_trend, Forecaster, box_cox_auto_model, File),
		logtalk_load(File),
		{box_cox_auto_model(LoadedForecaster)},
		assertion(LoadedForecaster == Forecaster),
		assertion(exponential_smoothing::valid_forecaster(LoadedForecaster)).

	test(exponential_smoothing_invalid_box_cox_bounds_order, error(domain_error(option, box_cox_bounds(1.0, 1.0)))) :-
		exponential_smoothing::learn(linear_trend, _Forecaster, [transformation(box_cox(auto)), box_cox_bounds(1.0, 1.0)]).

	test(exponential_smoothing_invalid_box_cox_bounds_type, error(domain_error(option, box_cox_bounds(lower, 1.0)))) :-
		exponential_smoothing::learn(linear_trend, _Forecaster, [transformation(box_cox(auto)), box_cox_bounds(lower, 1.0)]).

	test(exponential_smoothing_irrelevant_box_cox_bounds, error(domain_error(exponential_smoothing_parameter, box_cox_bounds(-0.5, 1.5)))) :-
		exponential_smoothing::learn(linear_trend, _Forecaster, [transformation(log), box_cox_bounds(-0.5, 1.5)]).

	test(exponential_smoothing_invalid_transformation, error(domain_error(option, transformation(unknown)))) :-
		exponential_smoothing::learn(linear_trend, _Forecaster, [model(simple), transformation(unknown)]).

	test(exponential_smoothing_irrelevant_bias_adjustment, error(domain_error(exponential_smoothing_parameter, bias_adjustment(delta)))) :-
		exponential_smoothing::learn(constant_series, _Forecaster, [model(simple), alpha(0.5), bias_adjustment(delta)]).

	test(exponential_smoothing_transformation_delta_adjustment, deterministic) :-
		Options = [model(simple), alpha(0.5), transformation(log)],
		exponential_smoothing::learn(linear_trend, NoneForecaster, [bias_adjustment(none)| Options]),
		exponential_smoothing::learn(linear_trend, DeltaForecaster, [bias_adjustment(delta)| Options]),
		exponential_smoothing::forecast(NoneForecaster, 2, [NoneFirst, NoneSecond]),
		exponential_smoothing::forecast(DeltaForecaster, 2, [DeltaFirst, DeltaSecond]),
		exponential_smoothing::diagnostics(DeltaForecaster, Diagnostics),
		memberchk(mean_squared_error(Variance), Diagnostics),
		Factor is exp(Variance / 2.0),
		ExpectedFirst is NoneFirst * Factor,
		ExpectedSecond is NoneSecond * Factor,
		assertion(DeltaFirst =~= ExpectedFirst),
		assertion(DeltaSecond =~= ExpectedSecond).

	test(exponential_smoothing_valid_transformed_forecaster, deterministic(exponential_smoothing::valid_forecaster(Forecaster))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [model(simple), alpha(0.5), transformation(log)]).

	test(exponential_smoothing_inconsistent_transformation_wrapper, fail) :-
		Options = [model(simple), alpha(0.5), beta(auto), gamma(auto), phi(auto), initialization(two_cycles), initial_cycles(2), optimizer_options([]), transformation(log), bias_adjustment(none)],
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options(Options), method(simple), parameters([0.5]), sum_squared_error(0.0), mean_squared_error(0.0), convergence(fixed_parameters), iterations(0), evaluations(0)],
		State = transformed(box_cox(0.5), level(1.0), residual_variance(0.0)),
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, State, [0.5], Diagnostics)).

	test(exponential_smoothing_negative_transformation_variance, fail) :-
		Options = [model(simple), alpha(0.5), beta(auto), gamma(auto), phi(auto), initialization(two_cycles), initial_cycles(2), optimizer_options([]), transformation(log), bias_adjustment(none)],
		Diagnostics = [model(exponential_smoothing), training_series_length(5), options(Options), method(simple), parameters([0.5]), sum_squared_error(0.0), mean_squared_error(0.0), convergence(fixed_parameters), iterations(0), evaluations(0)],
		State = transformed(log, level(1.0), residual_variance(-1.0)),
		exponential_smoothing::valid_forecaster(exponential_smoothing_forecaster(simple, State, [0.5], Diagnostics)).

	test(exponential_smoothing_transformed_export_reload, deterministic) :-
		^^file_path('test_output_transformed.pl', File),
		exponential_smoothing::learn(constant_series, Forecaster, [model(simple), alpha(0.5), transformation(log)]),
		exponential_smoothing::export_to_file(constant_series, Forecaster, forecast_model_2, File),
		logtalk_load(File),
		{forecast_model_2(LoadedForecaster)},
		assertion(LoadedForecaster == Forecaster),
		exponential_smoothing::forecast(LoadedForecaster, 2, [First, Second]),
		assertion(First =~= 5.0),
		assertion(Second =~= 5.0).

	% prediction intervals

	test(exponential_smoothing_interval_deterministic_repeat, deterministic(Lower1-Upper1 == Lower2-Upper2)) :-
		exponential_smoothing::learn(adverse_multiplicative_trajectory, Forecaster, [model(holt_winters_multiplicative), optimizer_options([max_iterations(100)]), retain_residuals(true)]),
		IntervalOptions = [samples(200), seed(123)],
		exponential_smoothing::forecast_interval(Forecaster, 3, Lower1, Upper1, IntervalOptions),
		exponential_smoothing::forecast_interval(Forecaster, 3, Lower2, Upper2, IntervalOptions).

	test(exponential_smoothing_interval_confidence_option_error, error(domain_error(option, confidence(1.5)))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5), retain_residuals(true)]),
		exponential_smoothing::forecast_interval(Forecaster, 1, _Lower, _Upper, [confidence(1.5)]).

	test(exponential_smoothing_interval_method_option_error, error(domain_error(option, method(holt)))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5), retain_residuals(true)]),
		exponential_smoothing::forecast_interval(Forecaster, 1, _Lower, _Upper, [method(holt)]).

	test(exponential_smoothing_interval_samples_option_error, error(domain_error(option, samples(0)))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5), retain_residuals(true)]),
		exponential_smoothing::forecast_interval(Forecaster, 1, _Lower, _Upper, [samples(0)]).

	test(exponential_smoothing_interval_seed_option_error, error(domain_error(option, seed(-1)))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5), retain_residuals(true)]),
		exponential_smoothing::forecast_interval(Forecaster, 1, _Lower, _Upper, [seed(-1)]).

	test(exponential_smoothing_interval_horizon_zero, deterministic(Lower-Upper == []-[])) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::forecast_interval(Forecaster, 0, Lower, Upper, []).

	test(exponential_smoothing_interval_residuals_not_retained, error(domain_error(retained_residuals, Forecaster))) :-
		exponential_smoothing::learn(constant_series, Forecaster, [alpha(0.5)]),
		exponential_smoothing::forecast_interval(Forecaster, 2, _Lower, _Upper, []).

	test(exponential_smoothing_interval_transformed_model, deterministic) :-
		exponential_smoothing::learn(linear_trend, Forecaster, [model(holt), alpha(0.2), beta(0.1), transformation(log), retain_residuals(true)]),
		exponential_smoothing::forecast(Forecaster, 3, PointForecasts),
		exponential_smoothing::forecast_interval(Forecaster, 3, Lower, Upper, [samples(200), seed(7)]),
		assertion(length(Lower, 3)),
		assertion(length(Upper, 3)),
		assert_bounds_bracket_point(Lower, PointForecasts, Upper).

	test(exponential_smoothing_interval_seasonal_phase, deterministic) :-
		exponential_smoothing::learn(seasonal_additive_trend_partial, Forecaster, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1), retain_residuals(true)]),
		exponential_smoothing::forecast(Forecaster, 8, PointForecasts),
		exponential_smoothing::forecast_interval(Forecaster, 8, Lower, Upper, [samples(200), seed(11)]),
		assertion(length(Lower, 8)),
		assertion(length(Upper, 8)),
		assert_bounds_bracket_point(Lower, PointForecasts, Upper).

	test(exponential_smoothing_interval_bounds_bracket_point, deterministic) :-
		exponential_smoothing::learn(adverse_multiplicative_trajectory, Forecaster, [model(holt_winters_multiplicative), optimizer_options([max_iterations(100)]), retain_residuals(true)]),
		exponential_smoothing::forecast(Forecaster, 4, PointForecasts),
		exponential_smoothing::forecast_interval(Forecaster, 4, Lower, Upper, [samples(300), seed(99)]),
		assertion(length(Lower, 4)),
		assertion(length(Upper, 4)),
		assert_bounds_bracket_point(Lower, PointForecasts, Upper).

	% auxiliary predicates

	missing_method_options(simple, missing_nonseasonal, [alpha(0.2)]).
	missing_method_options(holt, missing_nonseasonal, [alpha(0.2), beta(0.1)]).
	missing_method_options(holt_damped, missing_nonseasonal, [alpha(0.2), beta(0.1), phi(0.9)]).
	missing_method_options(holt_winters_additive, missing_seasonal, [alpha(0.2), beta(0.1), gamma(0.1)]).
	missing_method_options(holt_winters_multiplicative, missing_seasonal, [alpha(0.2), beta(0.1), gamma(0.1)]).
	missing_method_options(holt_winters_additive_damped, missing_seasonal, [alpha(0.2), beta(0.1), gamma(0.1), phi(0.9)]).
	missing_method_options(holt_winters_multiplicative_damped, missing_seasonal, [alpha(0.2), beta(0.1), gamma(0.1), phi(0.9)]).

	replace_missing_count([missing_count(4)| Diagnostics], [missing_count(3)| Diagnostics]) :-
		!.
	replace_missing_count([Diagnostic| Diagnostics], [Diagnostic| InvalidDiagnostics]) :-
		replace_missing_count(Diagnostics, InvalidDiagnostics).

	assert_bounds_bracket_point([], [], []).
	assert_bounds_bracket_point([Lower| Lowers], [Point| Points], [Upper| Uppers]) :-
		assertion(Lower =< Point),
		assertion(Point =< Upper),
		assert_bounds_bracket_point(Lowers, Points, Uppers).

	online_method_options(simple, [model(simple), alpha(0.2)]).
	online_method_options(holt, [model(holt), alpha(0.2), beta(0.1)]).
	online_method_options(holt_damped, [model(holt_damped), alpha(0.2), beta(0.1), phi(0.9)]).
	online_method_options(holt_winters_additive, [model(holt_winters_additive), alpha(0.2), beta(0.1), gamma(0.1)]).
	online_method_options(holt_winters_multiplicative, [model(holt_winters_multiplicative), alpha(0.2), beta(0.1), gamma(0.1)]).
	online_method_options(holt_winters_additive_damped, [model(holt_winters_additive_damped), alpha(0.2), beta(0.1), gamma(0.1), phi(0.9)]).
	online_method_options(holt_winters_multiplicative_damped, [model(holt_winters_multiplicative_damped), alpha(0.2), beta(0.1), gamma(0.1), phi(0.9)]).

	update_observations([], Forecaster, Forecaster).
	update_observations([Observation| Observations], Forecaster0, Forecaster) :-
		exponential_smoothing::update(Forecaster0, Observation, Forecaster1),
		update_observations(Observations, Forecaster1, Forecaster).

	assert_online_batch_equivalent(Method, OnlineForecaster, BatchForecaster) :-
		OnlineForecaster = exponential_smoothing_forecaster(Method, OnlineState, Parameters, OnlineDiagnostics),
		BatchForecaster = exponential_smoothing_forecaster(Method, BatchState, Parameters, BatchDiagnostics),
		assertion(OnlineState == BatchState),
		memberchk(sum_squared_error(SumSquaredError), OnlineDiagnostics),
		assertion(memberchk(sum_squared_error(SumSquaredError), BatchDiagnostics)),
		memberchk(mean_squared_error(MeanSquaredError), OnlineDiagnostics),
		assertion(memberchk(mean_squared_error(MeanSquaredError), BatchDiagnostics)),
		memberchk(scored_count(ScoredCount), OnlineDiagnostics),
		assertion(memberchk(scored_count(ScoredCount), BatchDiagnostics)).

:- end_object.

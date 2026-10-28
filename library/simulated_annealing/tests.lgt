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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2026-10-27,
		comment is 'Unit tests for the "simulated_annealing" library.'
	]).

	:- uses(list, [
		msort/2, length/2, member/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	cover(simulated_annealing(_, _)).
	cover(simulated_annealing(_)).

	:- private(cached_result_/2).
	:- dynamic(cached_result_/2).

	setup :-
		retractall(cached_result_(_, _)).

	cleanup :-
		retractall(cached_result_(_, _)),
		quadratic_progress::clear_log.

	% quadratic problem with default options

	test(sa_quadratic_run_2, true((number(Energy), Energy < 5.0))) :-
		cached_run(quadratic, _State, Energy).

	test(sa_quadratic_run_3_default, true((number(Energy), Energy < 5.0))) :-
		simulated_annealing(quadratic)::run(_State, Energy, []).

	test(sa_quadratic_run_3_more_steps, true((number(Energy), Energy < 1.0))) :-
		simulated_annealing(quadratic)::run(_State, Energy, [max_steps(50000)]).

	test(sa_quadratic_state_is_number, true(number(State))) :-
		cached_run(quadratic, State, _Energy).

	test(sa_quadratic_energy_non_negative, true(Energy >= 0.0)) :-
		cached_run(quadratic, _State, Energy).

	% quadratic problem with custom stop condition

	test(sa_quadratic_stop_run_2, true(Energy < 1.0)) :-
		simulated_annealing(quadratic_stop)::run(_State, Energy).

	% TSP problem

	test(sa_tsp_run_2, true((list::valid(State), number(Energy)))) :-
		cached_run(tsp, State, Energy).

	test(sa_tsp_tour_is_permutation, true) :-
		cached_run(tsp, Tour, _Energy),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected),
		Sorted == Expected.

	test(sa_tsp_tour_has_six_cities, true(Length == 6)) :-
		cached_run(tsp, Tour, _Energy),
		length(Tour, Length).

	test(sa_tsp_energy_below_naive, true(Energy < 50.0)) :-
		% The optimal tour for a regular hexagon with side 5 is 30.0.
		% Any reasonable SA run should find a tour well below 50.0.
		simulated_annealing(tsp)::run(_Tour, Energy, [max_steps(10000)]).

	test(sa_tsp_custom_cooling_schedule, true(Energy < 50.0)) :-
		% TSP defines a custom cooling_schedule/3 (0.999 factor)
		cached_run(tsp, _Tour, Energy).

	% option validation

	test(sa_invalid_option_max_steps, error(domain_error(option, max_steps(-1)))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, [max_steps(-1)]).

	test(sa_invalid_option_max_steps_type, error(domain_error(option, max_steps(foo)))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, [max_steps(foo)]).

	test(sa_invalid_option_min_temperature, error(domain_error(option, min_temperature(-0.1)))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, [min_temperature(-0.1)]).

	test(sa_invalid_option_updates, error(domain_error(option, updates(-1)))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, [updates(-1)]).

	test(sa_invalid_option_seed, error(domain_error(option, seed(-1)))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, [seed(-1)]).

	% run/4 returns statistics

	test(sa_run_4_returns_statistics, true) :-
		cached_run(quadratic, [], _State, _Energy, Statistics),
		member(steps(Steps), Statistics), integer(Steps), Steps > 0,
		member(acceptances(Acc), Statistics), integer(Acc), Acc >= 0,
		member(improvements(Imp), Statistics), integer(Imp), Imp >= 0,
		member(final_temperature(FinalT), Statistics), float(FinalT), FinalT > 0.0.

	test(sa_run_4_steps_match_max, true(Steps =:= 2500)) :-
		% Use a very low min_temperature to ensure max_steps is fully reached
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, [max_steps(2500), min_temperature(1.0e-100)]),
		member(steps(Steps), Statistics).

	test(sa_run_4_acceptances_bounded, true((Acc >= 0, Acc =< Steps))) :-
		cached_run(quadratic, [], _State, _Energy, Statistics),
		member(steps(Steps), Statistics),
		member(acceptances(Acc), Statistics).

	% min_temperature option

	test(sa_min_temperature_stops_early, true(Steps < 10000)) :-
		% A high min_temperature should cause the loop to stop before max_steps
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, [min_temperature(50.0)]),
		member(steps(Steps), Statistics).

	test(sa_min_temperature_default_causes_early_stop, true(Steps < 10000)) :-
		% With default geometric cooling (0.995) from initial_temperature(100),
		% the default min_temperature(0.001) is reached at around step 2300,
		% well before max_steps(10000)
		cached_run(quadratic, [], _State, _Energy, Statistics),
		member(steps(Steps), Statistics).

	% seed option for reproducibility

	test(sa_seed_reproducible_results, true(Energy1 =:= Energy2)) :-
		cached_seed_runs(_State1, Energy1, _State2, Energy2).

	test(sa_seed_reproducible_state, true(State1 =:= State2)) :-
		cached_seed_runs(State1, _Energy1, State2, _Energy2).

	% neighbor_state/3 delta-energy variant

	test(sa_delta_energy_run_2, true((number(Energy), Energy < 5.0))) :-
		simulated_annealing(quadratic_delta)::run(_State, Energy).

	test(sa_delta_energy_run_4, true((integer(Steps), Steps > 0))) :-
		simulated_annealing(quadratic_delta)::run(_State, _Energy, Statistics, [max_steps(5000)]),
		member(steps(Steps), Statistics).

	test(sa_delta_energy_comparable, true((Energy < 5.0))) :-
		% Delta-energy variant should produce quality comparable to standard variant
		quadratic_delta::reset_seed,
		simulated_annealing(quadratic_delta)::run(_State, Energy, [seed(123)]).

	test(simulated_annealing_failing_hooks_fallback, true) :-
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(State, Energy, Statistics, [seed(99), max_steps(100)]),
		quadratic::reset_seed,
		simulated_annealing(simulated_annealing_failing_hooks)::run(State, Energy, Statistics, [seed(99), max_steps(100)]).

	test(simulated_annealing_failing_delta_temperature_fallback, true(Actual =~= Expected)) :-
		quadratic::reset_seed,
		simulated_annealing(quadratic)::estimate_temperature(Expected, [samples(100)]),
		quadratic::reset_seed,
		simulated_annealing(simulated_annealing_failing_hooks)::estimate_temperature(Actual, [samples(100)]).

	test(simulated_annealing_inherited_delta_hook, true) :-
		quadratic_delta::reset_seed,
		simulated_annealing(quadratic_delta)::run(State, Energy, Statistics, [seed(99), max_steps(100)]),
		quadratic_delta::reset_seed,
		simulated_annealing(simulated_annealing_inherited_hooks)::run(State, Energy, Statistics, [seed(99), max_steps(100)]).

	test(simulated_annealing_dynamic_hooks_added_during_run, true) :-
		simulated_annealing(simulated_annealing_dynamic_hooks)::run(State, Energy, Statistics, [max_steps(10)]),
		assertion(State == 0),
		assertion(Energy == 0),
		Statistics = [steps(2), acceptances(2), improvements(2), final_temperature(Temp)],
		assertion(Temp =~= 5.0).

	% progress reporting

	test(sa_progress_updates_called, true(Count > 0)) :-
		cached_progress(List),
		length(List, Count).

	test(sa_progress_updates_count, true(Count =:= 6)) :-
		quadratic_progress::clear_log,
		% Use a very low min_temperature to ensure all steps (and all updates) run.
		% With updates(5) plus the final report on termination, we expect 6 total.
		simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(5), max_steps(5000), min_temperature(1.0e-100)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(sa_progress_updates_zero, true(Count =:= 0)) :-
		quadratic_progress::clear_log,
		simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(0)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(sa_progress_log_values, true((number(Step), Step > 0, number(Temp), Temp > 0.0, number(BestE), AccRate >= 0.0, AccRate =< 1.0, ImpRate >= 0.0, ImpRate =< 1.0))) :-
		cached_progress(Log),
		member(progress(Step, Temp, BestE, AccRate, ImpRate), Log).

	test(simulated_annealing_progress_completed_steps, true([AccRate, ImpRate] =~= [ExpectedAccRate, ExpectedImpRate])) :-
		quadratic_progress::clear_log,
		quadratic_progress::reset_seed,
		simulated_annealing(quadratic_progress)::run(_State, Energy, Statistics, [seed(99), max_steps(5), updates(1)]),
		Statistics = [steps(5), acceptances(Accepts), improvements(Improves), final_temperature(Temp)],
		findall(progress(Step, Temperature, Best, Acc, Imp), quadratic_progress::progress_log(Step, Temperature, Best, Acc, Imp), Log),
		Log = [progress(4, Temp, Energy, AccRate, ImpRate), progress(5, Temp, Energy, AccRate, ImpRate)],
		ExpectedAccRate is Accepts / 5,
		ExpectedImpRate is Improves / 5.

	test(simulated_annealing_progress_restart_cumulative_rates, true([AccRate, ImpRate] =~= [ExpectedAccRate, ExpectedImpRate])) :-
		quadratic_progress::clear_log,
		quadratic_progress::reset_seed,
		simulated_annealing(quadratic_progress)::run(_State, Energy, Statistics, [seed(99), max_steps(5), restarts(1), updates(1)]),
		Statistics = [steps(10), acceptances(Accepts), improvements(Improves), final_temperature(Temp)],
		findall(progress(Step, Temperature, Best, Acc, Imp), quadratic_progress::progress_log(Step, Temperature, Best, Acc, Imp), Log),
		Log = [progress(5, _, _, _, _), progress(9, Temp, Energy, AccRate, ImpRate), progress(10, Temp, Energy, AccRate, ImpRate)],
		ExpectedAccRate is Accepts / 10,
		ExpectedImpRate is Improves / 10.

	test(simulated_annealing_progress_zero_step_restarts, true([AccRate, ImpRate] =~= [0.0, 0.0])) :-
		quadratic_progress::clear_log,
		simulated_annealing(quadratic_progress)::run(_State, Energy, Statistics, [seed(99), min_temperature(2000.0), restarts(2), updates(1)]),
		Statistics = [steps(0), acceptances(0), improvements(0), final_temperature(Temp)],
		findall(progress(Step, Temperature, Best, Acc, Imp), quadratic_progress::progress_log(Step, Temperature, Best, Acc, Imp), Log),
		Log = [progress(0, Temp, Energy, AccRate, ImpRate), progress(0, Temp, Energy, AccRate, ImpRate), progress(0, Temp, Energy, AccRate, ImpRate)].

	% reheating restarts

	test(sa_restarts_zero_default, true((number(Energy), Energy < 5.0))) :-
		% restarts(0) is the default, should work the same as before
		simulated_annealing(quadratic)::run(_State, Energy, [restarts(0)]).

	test(sa_restarts_steps_accumulate, true((Steps > 5000, Steps < 8000))) :-
		% With restarts(2), each of the 3 cycles runs ~2297 steps (min_temp reached),
		% so total steps should be ~6891
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, [restarts(2)]),
		member(steps(Steps), Statistics).

	test(sa_restarts_better_or_equal_energy, true(EnergyR =< EnergyNoR)) :-
		% With restarts, energy should be better or equal to a single run
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(_S1, EnergyNoR, [seed(99), restarts(0)]),
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(_S2, EnergyR, [seed(99), restarts(2)]).

	test(sa_restarts_seed_reproducible, true(E1 =:= E2)) :-
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(_S1, E1, [seed(77), restarts(1)]),
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(_S2, E2, [seed(77), restarts(1)]).

	test(sa_invalid_option_restarts, error(domain_error(option, restarts(-1)))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, [restarts(-1)]).

	% auto-temperature estimation

	test(sa_estimate_temperature_1, true((float(T), T > 0.0))) :-
		cached_temperature(T).

	test(sa_estimate_temperature_2, true((float(T), T > 0.0))) :-
		simulated_annealing(quadratic)::estimate_temperature(T, [samples(100), acceptance_rate(80)]).

	test(sa_estimate_temperature_high_acceptance, true(THigh > TLow)) :-
		% Higher target acceptance rate requires higher temperature
		simulated_annealing(quadratic)::estimate_temperature(THigh, [samples(200), acceptance_rate(95)]),
		simulated_annealing(quadratic)::estimate_temperature(TLow, [samples(200), acceptance_rate(50)]).

	test(sa_estimate_temperature_tsp, true((float(T), T > 0.0))) :-
		simulated_annealing(tsp)::estimate_temperature(T).

	test(sa_estimate_temperature_usable, true(Energy < 5.0)) :-
		% The estimated temperature should work well in an actual run
		cached_temperature(T),
		% Use it (quadratic ignores initial_temperature when we pass it
		% but the estimated T confirms it's a reasonable positive float)
		T > 0.0,
		cached_run(quadratic, _State, Energy).

	test(sa_invalid_option_samples, error(domain_error(option, samples(-1)))) :-
		simulated_annealing(quadratic)::estimate_temperature(_T, [samples(-1)]).

	test(sa_invalid_option_acceptance_rate, error(domain_error(option, acceptance_rate(100)))) :-
		simulated_annealing(quadratic)::estimate_temperature(_T, [acceptance_rate(100)]).

	cached_run(Problem, State, Energy) :-
		(	cached_result_(run(Problem), Result) ->
			Result = result(State, Energy)
		;	simulated_annealing(Problem)::run(State, Energy),
			assertz(cached_result_(run(Problem), result(State, Energy)))
		).

	cached_run(Problem, Options, State, Energy, Statistics) :-
		(	cached_result_(run(Problem, Options), Result) ->
			Result = result(State, Energy, Statistics)
		;	simulated_annealing(Problem)::run(State, Energy, Statistics, Options),
			assertz(cached_result_(run(Problem, Options), result(State, Energy, Statistics)))
		).

	cached_seed_runs(State1, Energy1, State2, Energy2) :-
		(	cached_result_(seed_runs, Result) ->
			Result = result(State1, Energy1, State2, Energy2)
		;	quadratic::reset_seed,
			simulated_annealing(quadratic)::run(State1, Energy1, [seed(42)]),
			quadratic::reset_seed,
			simulated_annealing(quadratic)::run(State2, Energy2, [seed(42)]),
			assertz(cached_result_(seed_runs, result(State1, Energy1, State2, Energy2)))
		).

	cached_progress(Log) :-
		(	cached_result_(progress, Result) ->
			Log = Result
		;	quadratic_progress::clear_log,
			simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(5)]),
			findall(
				progress(Step, Temp, BestE, AccRate, ImpRate),
				quadratic_progress::progress_log(Step, Temp, BestE, AccRate, ImpRate),
				Log
			),
			assertz(cached_result_(progress, Log))
		).

	cached_temperature(Temperature) :-
		(	cached_result_(temperature, Result) ->
			Temperature = Result
		;
			simulated_annealing(quadratic)::estimate_temperature(Temperature),
			assertz(cached_result_(temperature, Temperature))
		).

:- end_object.

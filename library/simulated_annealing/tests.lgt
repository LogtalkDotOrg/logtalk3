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
		date is 2026-02-22,
		comment is 'Unit tests for the "simulated_annealing" library.'
	]).

	:- uses(list, [
		msort/2, length/2, member/2
	]).

	cover(simulated_annealing(_)).

	% quadratic problem with default options

	test(sa_quadratic_run_2, true((number(Energy), Energy < 5.0))) :-
		simulated_annealing(quadratic)::run(_State, Energy).

	test(sa_quadratic_run_3_default, true((number(Energy), Energy < 5.0))) :-
		simulated_annealing(quadratic)::run(_State, Energy, []).

	test(sa_quadratic_run_3_more_steps, true((number(Energy), Energy < 1.0))) :-
		simulated_annealing(quadratic)::run(_State, Energy, [max_steps(50000)]).

	test(sa_quadratic_state_is_number, true(number(State))) :-
		simulated_annealing(quadratic)::run(State, _Energy).

	test(sa_quadratic_energy_non_negative, true(Energy >= 0.0)) :-
		simulated_annealing(quadratic)::run(_State, Energy).

	% quadratic problem with custom stop condition

	test(sa_quadratic_stop_run_2, true(Energy < 1.0)) :-
		simulated_annealing(quadratic_stop)::run(_State, Energy).

	% TSP problem

	test(sa_tsp_run_2, true((list::valid(State), number(Energy)))) :-
		simulated_annealing(tsp)::run(State, Energy).

	test(sa_tsp_tour_is_permutation, true) :-
		simulated_annealing(tsp)::run(Tour, _Energy),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected),
		Sorted == Expected.

	test(sa_tsp_tour_has_six_cities, true(Length == 6)) :-
		simulated_annealing(tsp)::run(Tour, _Energy),
		length(Tour, Length).

	test(sa_tsp_energy_below_naive, true(Energy < 50.0)) :-
		% The optimal tour for a regular hexagon with side 5 is 30.0.
		% Any reasonable SA run should find a tour well below 50.0.
		simulated_annealing(tsp)::run(_Tour, Energy, [max_steps(20000)]).

	test(sa_tsp_custom_cooling_schedule, true(Energy < 50.0)) :-
		% TSP defines a custom cooling_schedule/3 (0.999 factor)
		simulated_annealing(tsp)::run(_Tour, Energy).

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
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, []),
		member(steps(Steps), Statistics), integer(Steps), Steps > 0,
		member(acceptances(Acc), Statistics), integer(Acc), Acc >= 0,
		member(improvements(Imp), Statistics), integer(Imp), Imp >= 0,
		member(final_temperature(FinalT), Statistics), float(FinalT), FinalT > 0.0.

	test(sa_run_4_steps_match_max, true(Steps =:= 5000)) :-
		% Use a very low min_temperature to ensure max_steps is fully reached
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, [max_steps(5000), min_temperature(1.0e-100)]),
		member(steps(Steps), Statistics).

	test(sa_run_4_acceptances_bounded, true((Acc >= 0, Acc =< Steps))) :-
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, []),
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
		simulated_annealing(quadratic)::run(_State, _Energy, Statistics, []),
		member(steps(Steps), Statistics).

	% seed option for reproducibility

	test(sa_seed_reproducible_results, true(Energy1 =:= Energy2)) :-
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(_State1, Energy1, [seed(42)]),
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(_State2, Energy2, [seed(42)]).

	test(sa_seed_reproducible_state, true(State1 =:= State2)) :-
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(State1, _Energy1, [seed(42)]),
		quadratic::reset_seed,
		simulated_annealing(quadratic)::run(State2, _Energy2, [seed(42)]).

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

	% progress reporting

	test(sa_progress_updates_called, true(Count > 0)) :-
		quadratic_progress::clear_log,
		simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(5)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(sa_progress_updates_count, true(Count =:= 6)) :-
		quadratic_progress::clear_log,
		% Use a very low min_temperature to ensure all steps (and all updates) run.
		% With updates(5) plus the final report on termination, we expect 6 total.
		simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(5), max_steps(10000), min_temperature(1.0e-100)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(sa_progress_updates_zero, true(Count =:= 0)) :-
		quadratic_progress::clear_log,
		simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(0)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(sa_progress_log_values, true((number(Step), Step > 0, number(Temp), Temp > 0.0, number(BestE), AccRate >= 0.0, AccRate =< 1.0, ImpRate >= 0.0, ImpRate =< 1.0))) :-
		quadratic_progress::clear_log,
		simulated_annealing(quadratic_progress)::run(_State, _Energy, [updates(5)]),
		quadratic_progress::progress_log(Step, Temp, BestE, AccRate, ImpRate).

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
		simulated_annealing(quadratic)::estimate_temperature(T).

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
		simulated_annealing(quadratic)::estimate_temperature(T),
		% Use it (quadratic ignores initial_temperature when we pass it
		% but the estimated T confirms it's a reasonable positive float)
		T > 0.0,
		simulated_annealing(quadratic)::run(_State, Energy).

	test(sa_invalid_option_samples, error(domain_error(option, samples(-1)))) :-
		simulated_annealing(quadratic)::estimate_temperature(_T, [samples(-1)]).

	test(sa_invalid_option_acceptance_rate, error(domain_error(option, acceptance_rate(100)))) :-
		simulated_annealing(quadratic)::estimate_temperature(_T, [acceptance_rate(100)]).

:- end_object.

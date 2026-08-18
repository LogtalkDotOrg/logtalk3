%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Unit tests for the "tabu_search" library.'
	]).

	:- uses(list, [
		msort/2, length/2, memberchk/2
	]).

	cover(tabu_search(_, _)).
	cover(tabu_search(_)).

	% quadratic problem - modest step budgets keep CI fast while still
	% exercising the core search loop and energy improvement

	test(ts_quadratic_run_2, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(500)]).

	test(ts_quadratic_run_3_default, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(500)]).

	test(ts_quadratic_run_3_more_steps, deterministic((number(Energy), Energy < 1.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(2000), candidates(30)]).

	test(ts_quadratic_state_is_number, deterministic(number(State))) :-
		tabu_search(quadratic)::run(State, _Energy, [max_steps(200)]).

	test(ts_quadratic_energy_non_negative, deterministic(Energy >= 0.0)) :-
		tabu_search(quadratic)::run(_State, Energy, [max_steps(200)]).

	% TSP problem

	test(ts_tsp_run_2, deterministic((list::valid(State), number(Energy)))) :-
		tabu_search(tsp)::run(State, Energy, [max_steps(300), candidates(15)]).

	test(ts_tsp_tour_is_permutation, deterministic(Sorted == Expected)) :-
		tabu_search(tsp)::run(Tour, _Energy, [max_steps(200), candidates(10)]),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected).

	test(ts_tsp_tour_has_six_cities, deterministic(Length == 6)) :-
		tabu_search(tsp)::run(Tour, _Energy, [max_steps(100), candidates(8)]),
		length(Tour, Length).

	test(ts_tsp_energy_below_naive, deterministic(Energy < 50.0)) :-
		tabu_search(tsp)::run(_Tour, Energy, [max_steps(800), candidates(20)]).

	% option validation (no search work)

	test(ts_invalid_option_max_steps, error(domain_error(option, max_steps(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [max_steps(-1)]).

	test(ts_invalid_option_tabu_tenure, error(domain_error(option, tabu_tenure(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [tabu_tenure(-1)]).

	test(ts_invalid_option_candidates, error(domain_error(option, candidates(0)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [candidates(0)]).

	test(ts_invalid_option_seed, error(domain_error(option, seed(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [seed(-1)]).

	% run/4 returns statistics

	test(ts_run_4_returns_statistics, deterministic) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [max_steps(200)]),
		memberchk(steps(Steps), Statistics),
		^^assertion((integer(Steps), Steps > 0)),
		memberchk(acceptances(Acc), Statistics),
		^^assertion((integer(Acc), Acc >= 0)),
		memberchk(improvements(Imp), Statistics),
		^^assertion((integer(Imp), Imp >= 0)),
		memberchk(final_tabu_size(Size), Statistics),
		^^assertion((integer(Size), Size >= 0)).

	test(ts_run_4_steps_match_max, deterministic(Steps =:= 300)) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [max_steps(300)]),
		memberchk(steps(Steps), Statistics).

	test(ts_run_4_acceptances_bounded, deterministic((Acc >= 0, Acc =< Steps))) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [max_steps(200)]),
		memberchk(steps(Steps), Statistics),
		memberchk(acceptances(Acc), Statistics).

	% tabu tenure

	test(ts_tabu_tenure_respected, deterministic(Size =< 5)) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [tabu_tenure(5), max_steps(50)]),
		memberchk(final_tabu_size(Size), Statistics).

	test(ts_tabu_tenure_range_runs, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [tabu_tenure_range(3, 9), max_steps(400)]).

	test(ts_tabu_tenure_range_overrides_fixed, deterministic((number(Energy), Energy < 5.0))) :-
		% range present -> fixed tenure is ignored
		tabu_search(quadratic)::run(_State, Energy, [tabu_tenure(2), tabu_tenure_range(4, 8), max_steps(300)]).

	test(ts_tabu_tenure_range_statistics, deterministic) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [tabu_tenure_range(2, 6), max_steps(100)]),
		memberchk(steps(Steps), Statistics),
		^^assertion(Steps =:= 100),
		memberchk(final_tabu_size(Size), Statistics),
		^^assertion((integer(Size), Size >= 0)).

	test(ts_tabu_tenure_range_seed_reproducible, deterministic(E1 =:= E2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S1, E1, [seed(55), tabu_tenure_range(3, 7), max_steps(200)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S2, E2, [seed(55), tabu_tenure_range(3, 7), max_steps(200)]).

	test(ts_invalid_option_tabu_tenure_range_min, error(domain_error(option, tabu_tenure_range(0, 5)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [tabu_tenure_range(0, 5)]).

	test(ts_invalid_option_tabu_tenure_range_order, error(domain_error(option, tabu_tenure_range(9, 3)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [tabu_tenure_range(9, 3)]).

	% seed option for reproducibility

	test(ts_seed_reproducible_results, deterministic(Energy1 =:= Energy2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_State1, Energy1, [seed(42), max_steps(300)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_State2, Energy2, [seed(42), max_steps(300)]).

	test(ts_seed_reproducible_state, deterministic(State1 =:= State2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(State1, _Energy1, [seed(42), max_steps(300)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(State2, _Energy2, [seed(42), max_steps(300)]).

	% neighbor_state/3 delta-energy variant

	test(ts_delta_energy_run_2, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic_delta)::run(_State, Energy, [max_steps(500)]).

	test(ts_delta_energy_run_4, deterministic((integer(Steps), Steps > 0))) :-
		tabu_search(quadratic_delta)::run(_State, _Energy, Statistics, [max_steps(300)]),
		memberchk(steps(Steps), Statistics).

	% progress reporting

	test(ts_progress_updates_called, deterministic(Count > 0)) :-
		quadratic_progress::clear_log,
		tabu_search(quadratic_progress)::run(_State, _Energy, [updates(3), max_steps(150)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(ts_progress_updates_zero, deterministic(Count =:= 0)) :-
		quadratic_progress::clear_log,
		tabu_search(quadratic_progress)::run(_State, _Energy, [updates(0), max_steps(50)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	% restarts

	test(ts_restarts_zero_default, deterministic((number(Energy), Energy < 5.0))) :-
		tabu_search(quadratic)::run(_State, Energy, [restarts(0), max_steps(400)]).

	test(ts_restarts_steps_accumulate, deterministic(Steps > 450)) :-
		tabu_search(quadratic)::run(_State, _Energy, Statistics, [restarts(2), max_steps(200)]),
		memberchk(steps(Steps), Statistics).

	test(ts_restarts_seed_reproducible, deterministic(E1 =:= E2)) :-
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S1, E1, [seed(77), restarts(1), max_steps(200)]),
		quadratic::reset_seed,
		tabu_search(quadratic)::run(_S2, E2, [seed(77), restarts(1), max_steps(200)]).

	test(ts_invalid_option_restarts, error(domain_error(option, restarts(-1)))) :-
		tabu_search(quadratic)::run(_State, _Energy, [restarts(-1)]).

:- end_object.

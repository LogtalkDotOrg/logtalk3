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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Unit tests for the "ant_colony" library.'
	]).

	:- uses(list, [
		msort/2, length/2, memberchk/2
	]).

	cover(ant_colony(_, _)).
	cover(ant_colony(_)).

	% basic TSP run

	test(aco_tsp_run_2, deterministic((list::valid(Tour), number(Cost)))) :-
		ant_colony(tsp)::run(Tour, Cost, [max_iterations(30), ants(8)]).

	test(aco_tsp_tour_is_permutation, deterministic(Sorted == Expected)) :-
		ant_colony(tsp)::run(Tour, _Cost, [max_iterations(20), ants(6)]),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected).

	test(aco_tsp_tour_has_six_cities, deterministic(Length == 6)) :-
		ant_colony(tsp)::run(Tour, _Cost, [max_iterations(15), ants(5)]),
		length(Tour, Length).

	test(aco_tsp_cost_reasonable, deterministic(Cost =< 35.0)) :-
		ant_colony(tsp)::run(_Tour, Cost, [max_iterations(40), ants(10), beta(3.0)]).

	% asymmetric TSP

	test(aco_atsp_run_2, deterministic((list::valid(Tour), number(Cost)))) :-
		ant_colony(atsp)::run(Tour, Cost, [max_iterations(30), ants(8)]).

	test(aco_atsp_tour_is_permutation, deterministic(Sorted == Expected)) :-
		ant_colony(atsp)::run(Tour, _Cost, [max_iterations(20), ants(6)]),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected).

	test(aco_atsp_cost_positive, deterministic(Cost > 0.0)) :-
		ant_colony(atsp)::run(_Tour, Cost, [max_iterations(25), ants(6)]).

	% option validation (no search work)

	test(aco_invalid_option_max_iterations, error(domain_error(option, max_iterations(0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [max_iterations(0)]).

	test(aco_invalid_option_ants, error(domain_error(option, ants(0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [ants(0)]).

	test(aco_invalid_option_rho_high, error(domain_error(option, rho(1.5)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [rho(1.5)]).

	test(aco_invalid_option_rho_zero, error(domain_error(option, rho(0.0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [rho(0.0)]).

	test(aco_invalid_option_alpha, error(domain_error(option, alpha(-1.0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [alpha(-1.0)]).

	test(aco_invalid_option_beta, error(domain_error(option, beta(-0.5)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [beta(-0.5)]).

	test(aco_invalid_option_q, error(domain_error(option, q(0.0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [q(0.0)]).

	test(aco_invalid_option_elite, error(domain_error(option, elite(-1)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [elite(-1)]).

	test(aco_invalid_option_tau0, error(domain_error(option, tau0(0.0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [tau0(0.0)]).

	test(aco_invalid_option_tau_min, error(domain_error(option, tau_min(0.0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [tau_min(0.0)]).

	test(aco_invalid_option_tau_max, error(domain_error(option, tau_max(-1.0)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [tau_max(-1.0)]).

	test(aco_invalid_option_tau_bounds_order, error(consistency_error(tau_bounds, 10.0, 1.0))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [tau_min(10.0), tau_max(1.0)]).

	test(aco_invalid_option_seed, error(domain_error(option, seed(-1)))) :-
		ant_colony(tsp)::run(_Tour, _Cost, [seed(-1)]).

	% run/4 statistics

	test(aco_run_4_returns_statistics, deterministic) :-
		ant_colony(tsp)::run(_Tour, _Cost, Statistics, [max_iterations(20), ants(5)]),
		memberchk(iterations(Iters), Statistics),
		^^assertion((integer(Iters), Iters > 0)),
		memberchk(solutions(Sols), Statistics),
		^^assertion((integer(Sols), Sols > 0)),
		memberchk(improvements(Imp), Statistics),
		^^assertion((integer(Imp), Imp >= 0)),
		memberchk(final_best_cost(C), Statistics),
		^^assertion(number(C)).

	test(aco_run_4_iterations_match, deterministic(Iters =:= 25)) :-
		ant_colony(tsp)::run(_Tour, _Cost, Statistics, [max_iterations(25), ants(4)]),
		memberchk(iterations(Iters), Statistics).

	test(aco_run_4_solutions_accumulate, deterministic(Sols =:= 100)) :-
		ant_colony(tsp)::run(_Tour, _Cost, Statistics, [max_iterations(20), ants(5)]),
		memberchk(solutions(Sols), Statistics).

	test(aco_run_4_improvements_bounded, deterministic((Imp >= 0, Imp =< Sols))) :-
		ant_colony(tsp)::run(_Tour, _Cost, Statistics, [max_iterations(20), ants(5)]),
		memberchk(solutions(Sols), Statistics),
		memberchk(improvements(Imp), Statistics).

	% seed reproducibility

	test(aco_seed_reproducible_cost, deterministic(C1 =:= C2)) :-
		ant_colony(tsp)::run(_T1, C1, [seed(42), max_iterations(30), ants(6)]),
		ant_colony(tsp)::run(_T2, C2, [seed(42), max_iterations(30), ants(6)]).

	test(aco_seed_reproducible_tour, deterministic(T1 == T2)) :-
		ant_colony(tsp)::run(T1, _C1, [seed(99), max_iterations(25), ants(5)]),
		ant_colony(tsp)::run(T2, _C2, [seed(99), max_iterations(25), ants(5)]).

	% parameter regimes

	test(aco_high_beta_greedy, deterministic((list::valid(Tour), Cost =< 35.0))) :-
		ant_colony(tsp)::run(Tour, Cost, [seed(7), beta(5.0), alpha(0.5), max_iterations(25), ants(6)]).

	test(aco_elite_runs, deterministic((number(Cost), Cost =< 35.0))) :-
		ant_colony(tsp)::run(_Tour, Cost, [elite(2), max_iterations(30), ants(8)]).

	test(aco_elite_seed_reproducible, deterministic(C1 =:= C2)) :-
		ant_colony(tsp)::run(_T1, C1, [seed(11), elite(3), max_iterations(20), ants(5)]),
		ant_colony(tsp)::run(_T2, C2, [seed(11), elite(3), max_iterations(20), ants(5)]).

	% MAX-MIN pheromone bounds

	test(aco_tau_bounds_run, deterministic((number(Cost), Cost =< 35.0))) :-
		ant_colony(tsp)::run(_Tour, Cost, [
			tau_min(0.01), tau_max(5.0), tau0(1.0),
			max_iterations(25), ants(6), seed(13)
		]).

	test(aco_tau_bounds_seed_reproducible, deterministic(C1 =:= C2)) :-
		ant_colony(tsp)::run(_T1, C1, [
			seed(17), tau_min(0.05), tau_max(2.0),
			max_iterations(20), ants(5)
		]),
		ant_colony(tsp)::run(_T2, C2, [
			seed(17), tau_min(0.05), tau_max(2.0),
			max_iterations(20), ants(5)
		]).

	% stop condition

	test(aco_stop_condition_early_exit, deterministic(Iters < 100)) :-
		ant_colony(tsp_stop)::run(_Tour, Cost, Statistics, [
			max_iterations(100), ants(12), beta(4.0), seed(3)
		]),
		memberchk(iterations(Iters), Statistics),
		^^assertion(Cost =< 30.0001).

	test(aco_stop_condition_cost_optimal, deterministic(Cost =< 30.0001)) :-
		ant_colony(tsp_stop)::run(_Tour, Cost, [
			max_iterations(80), ants(10), beta(5.0), seed(5)
		]).

	% progress reporting

	test(aco_progress_updates_called, deterministic(Count > 0)) :-
		tsp_progress::clear_log,
		ant_colony(tsp_progress)::run(_Tour, _Cost, [updates(3), max_iterations(20), ants(4)]),
		findall(1, tsp_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(aco_progress_updates_zero, deterministic(Count =:= 0)) :-
		tsp_progress::clear_log,
		ant_colony(tsp_progress)::run(_Tour, _Cost, [updates(0), max_iterations(10), ants(3)]),
		findall(1, tsp_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

:- end_object.

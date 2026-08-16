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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Unit tests for the "genetic_algorithm" library.'
	]).

	:- uses(list, [
		msort/2, length/2, memberchk/2
	]).

	cover(genetic_algorithm(_, _)).
	cover(genetic_algorithm(_)).

	% quadratic problem

	test(ga_quadratic_run_2, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [max_generations(40), population_size(20)]).

	test(ga_quadratic_run_3_default, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [max_generations(40), population_size(20)]).

	test(ga_quadratic_more_gens, deterministic((number(Energy), Energy < 1.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [max_generations(80), population_size(30), mutation_rate(0.2)]).

	test(ga_quadratic_state_is_number, deterministic(number(State))) :-
		genetic_algorithm(quadratic)::run(State, _Energy, [max_generations(20), population_size(15)]).

	test(ga_quadratic_energy_non_negative, deterministic(Energy >= 0.0)) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [max_generations(20), population_size(15)]).

	% TSP problem

	test(ga_tsp_run, deterministic((list::valid(State), number(Energy)))) :-
		genetic_algorithm(tsp)::run(State, Energy, [max_generations(30), population_size(20)]).

	test(ga_tsp_tour_is_permutation, deterministic(Sorted == Expected)) :-
		genetic_algorithm(tsp)::run(Tour, _Energy, [max_generations(25), population_size(16)]),
		msort(Tour, Sorted),
		msort([a, b, c, d, e, f], Expected).

	test(ga_tsp_tour_has_six_cities, deterministic(Length == 6)) :-
		genetic_algorithm(tsp)::run(Tour, _Energy, [max_generations(15), population_size(12)]),
		length(Tour, Length).

	test(ga_tsp_energy_below_naive, deterministic(Energy < 50.0)) :-
		genetic_algorithm(tsp)::run(_Tour, Energy, [max_generations(50), population_size(30)]).

	% option validation

	test(ga_invalid_option_max_generations, error(domain_error(option, max_generations(0)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [max_generations(0)]).

	test(ga_invalid_option_population_size, error(domain_error(option, population_size(1)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [population_size(1)]).

	test(ga_invalid_option_crossover_rate, error(domain_error(option, crossover_rate(1.5)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [crossover_rate(1.5)]).

	test(ga_invalid_option_mutation_rate, error(domain_error(option, mutation_rate(-0.1)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [mutation_rate(-0.1)]).

	test(ga_invalid_option_seed, error(domain_error(option, seed(-1)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [seed(-1)]).

	test(ga_invalid_option_objective, error(domain_error(option, objective(sideways)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [objective(sideways)]).

	test(ga_invalid_option_selection_scheme, error(domain_error(option, selection(boltzmann)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [selection(boltzmann)]).

	test(ga_invalid_option_selection_tournament_size, error(domain_error(option, selection(tournament(0))))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [selection(tournament(0))]).

	% run/4 statistics

	test(ga_run_4_returns_statistics, deterministic) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, Statistics, [max_generations(20), population_size(15)]),
		memberchk(generations(Generations), Statistics),
		^^assertion((integer(Generations), Generations > 0)),
		memberchk(evaluations(Evaluations), Statistics),
		^^assertion((integer(Evaluations), Evaluations > 0)),
		memberchk(improvements(Improvements), Statistics),
		^^assertion((integer(Improvements), Improvements >= 0)),
		memberchk(final_population_size(FinalPopulationSize), Statistics),
		^^assertion((integer(FinalPopulationSize), FinalPopulationSize =:= 15)).

	test(ga_run_4_generations_match, deterministic(Generations =:= 25)) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, Statistics, [max_generations(25), population_size(12)]),
		memberchk(generations(Generations), Statistics).

	% elitism

	test(ga_elite_size_zero, deterministic((number(Energy), Energy < 10.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [elite_size(0), max_generations(30), population_size(20)]).

	test(ga_elite_size_preserves_best, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			elite_size(2),
			max_generations(30),
			population_size(20)
		]).

	test(ga_elite_size_full_population, deterministic((number(Energy), Energy >= 0.0))) :-
		% all individuals are elites: no breeding; still returns a valid result
		genetic_algorithm(quadratic)::run(_State, Energy, [
			elite_size(15),
			max_generations(5),
			population_size(15)
		]).

	test(ga_elite_size_clamped_to_population, deterministic((number(Energy), Energy >= 0.0))) :-
		% elite_size larger than population is clamped; must not fail
		genetic_algorithm(quadratic)::run(_State, Energy, [
			elite_size(100),
			max_generations(5),
			population_size(10)
		]).

	% selection schemes

	test(ga_selection_tournament, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			selection(tournament(5)),
			max_generations(30),
			population_size(20)
		]).

	test(ga_selection_tournament_default, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			selection(tournament(3)),
			max_generations(30),
			population_size(20)
		]).

	test(ga_selection_roulette, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			selection(roulette),
			max_generations(40),
			population_size(25)
		]).

	test(ga_selection_rank, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			selection(rank),
			max_generations(40),
			population_size(25)
		]).

	test(ga_selection_roulette_maximize, deterministic(number(Energy))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			selection(roulette),
			objective(maximize),
			max_generations(20),
			population_size(15)
		]).

	test(ga_selection_rank_maximize, deterministic(number(Energy))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			selection(rank),
			objective(maximize),
			max_generations(20),
			population_size(15)
		]).

	test(ga_selection_roulette_reproducible, deterministic(Energy1 =:= Energy2)) :-
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(_State1, Energy1, [
			seed(99),
			selection(roulette),
			max_generations(25),
			population_size(15)
		]),
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(_State2, Energy2, [
			seed(99),
			selection(roulette),
			max_generations(25),
			population_size(15)
		]).

	test(ga_selection_rank_reproducible, deterministic(Energy1 =:= Energy2)) :-
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(_State1, Energy1, [
			seed(99),
			selection(rank),
			max_generations(25),
			population_size(15)
		]),
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(_State2, Energy2, [
			seed(99),
			selection(rank),
			max_generations(25),
			population_size(15)
		]).

	% seed reproducibility

	test(ga_seed_reproducible_energy, deterministic(Energy1 =:= Energy2)) :-
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(_State1, Energy1, [seed(42), max_generations(30), population_size(20)]),
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(_State2, Energy2, [seed(42), max_generations(30), population_size(20)]).

	test(ga_seed_reproducible_state, deterministic(State1 =:= State2)) :-
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(State1, _Energy1, [seed(42), max_generations(30), population_size(20)]),
		quadratic::reset_seed,
		genetic_algorithm(quadratic)::run(State2, _Energy2, [seed(42), max_generations(30), population_size(20)]).

	% progress reporting

	test(ga_progress_updates_called, deterministic(Count > 0)) :-
		quadratic_progress::clear_log,
		genetic_algorithm(quadratic_progress)::run(_State, _Energy, [updates(3), max_generations(30), population_size(15)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).

	test(ga_progress_updates_zero, deterministic(Count =:= 0)) :-
		quadratic_progress::clear_log,
		genetic_algorithm(quadratic_progress)::run(_State, _Energy, [updates(0), max_generations(15), population_size(10)]),
		findall(1, quadratic_progress::progress_log(_, _, _, _, _), List),
		length(List, Count).


	% adaptive rate schedules

	test(ga_crossover_schedule_linear, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			crossover_schedule(linear(0.9, 0.3)),
			max_generations(30),
			population_size(20)
		]).

	test(ga_mutation_schedule_geometric, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			mutation_rate(0.3),
			mutation_schedule(geometric(0.95)),
			max_generations(30),
			population_size(20)
		]).

	test(ga_schedules_combined, deterministic((number(Energy), Energy < 5.0))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [
			crossover_rate(0.85),
			mutation_rate(0.2),
			crossover_schedule(linear(0.85, 0.4)),
			mutation_schedule(geometric(0.98)),
			max_generations(40),
			population_size(20)
		]).

	test(ga_invalid_option_crossover_schedule, error(domain_error(option, crossover_schedule(exponential)))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [crossover_schedule(exponential)]).

	test(ga_invalid_option_mutation_schedule_factor, error(domain_error(option, mutation_schedule(geometric(0))))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [mutation_schedule(geometric(0))]).

	test(ga_invalid_option_linear_bounds, error(domain_error(option, crossover_schedule(linear(-0.1, 0.5))))) :-
		genetic_algorithm(quadratic)::run(_State, _Energy, [crossover_schedule(linear(-0.1, 0.5))]).

	% objective maximize
	test(ga_objective_maximize, deterministic(number(Energy))) :-
		genetic_algorithm(quadratic)::run(_State, Energy, [objective(maximize), max_generations(20), population_size(15)]).

:- end_object.

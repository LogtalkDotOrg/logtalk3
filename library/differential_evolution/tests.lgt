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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Unit tests for the "differential_evolution" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2, member/2
	]).

	:- uses(lgtunit, [
		assertion/1, op(700, xfx, =~=), (=~=)/2
	]).

	cover(differential_evolution(_, _)).
	cover(differential_evolution(_)).

	%---------------------------------------------------------------------
	% Basic runs and strategies
	%---------------------------------------------------------------------

	test(de_sphere_run_2, deterministic((list::valid(Position), Fitness < 0.5))) :-
		differential_evolution(sphere)::run(Position, Fitness, [seed(42), max_generations(100)]).

	test(de_run_2_default_strategy, deterministic((list::valid(Position), number(Fitness)))) :-
		differential_evolution(sphere)::run(Position, Fitness).

	test(de_strategy_rand_1_bin, deterministic(Fitness < 0.5)) :-
		differential_evolution(sphere)::run(_Position, Fitness, [strategy(rand/1/bin), seed(42), max_generations(100)]).

	test(de_strategy_rand_1_exp, deterministic(Fitness < 0.5)) :-
		differential_evolution(sphere)::run(_Position, Fitness, [strategy(rand/1/exp), seed(42), max_generations(100)]).

	test(de_strategy_best_1_bin, deterministic(Fitness < 1.0)) :-
		differential_evolution(sphere)::run(_Position, Fitness, [strategy(best/1/bin), seed(42), max_generations(100)]).

	test(de_strategy_current_to_best_1_bin, deterministic(Fitness < 0.5)) :-
		differential_evolution(sphere)::run(_Position, Fitness, [strategy(current-to-best/1/bin), seed(42), max_generations(100)]).

	%---------------------------------------------------------------------
	% Reproducibility
	%---------------------------------------------------------------------

	test(de_seed_reproducible, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_generations(50)]),
		differential_evolution(sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_generations(50)]).

	test(de_seed_reproducible_rand_1_bin, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [strategy(rand/1/bin), seed(42), max_generations(40)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [strategy(rand/1/bin), seed(42), max_generations(40)]).

	test(de_seed_reproducible_rand_1_exp, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [strategy(rand/1/exp), seed(42), max_generations(40)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [strategy(rand/1/exp), seed(42), max_generations(40)]).

	test(de_seed_reproducible_best, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [strategy(best/1/bin), seed(42), max_generations(40)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [strategy(best/1/bin), seed(42), max_generations(40)]).

	test(de_seed_reproducible_current_to_best, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [strategy(current-to-best/1/bin), seed(42), max_generations(40)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [strategy(current-to-best/1/bin), seed(42), max_generations(40)]).

	%---------------------------------------------------------------------
	% Objective (minimize / maximize)
	%---------------------------------------------------------------------

	test(de_explicit_minimize, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [seed(42), max_generations(40)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [seed(42), max_generations(40), objective(minimize)]).

	test(de_maximize, deterministic(Fitness > -0.5)) :-
		differential_evolution(negative_sphere)::run(_Position, Fitness, [seed(42), max_generations(100), objective(maximize)]).

	test(de_maximize_seed_reproducible, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		differential_evolution(negative_sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_generations(50), objective(maximize)]),
		differential_evolution(negative_sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_generations(50), objective(maximize)]).

	%---------------------------------------------------------------------
	% Statistics
	%---------------------------------------------------------------------

	test(de_statistics, deterministic) :-
		differential_evolution(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_generations(30)]),
		memberchk(generations(Generations), Statistics),
		^^assertion(Generations =:= 30),
		memberchk(improvements(Improvements), Statistics),
		^^assertion((0 =< Improvements, Improvements =< Generations)),
		memberchk(final_mean_fitness(MeanFitness), Statistics),
		^^assertion((number(MeanFitness), MeanFitness >= 0.0)),
		memberchk(final_diversity(Diversity), Statistics),
		^^assertion((number(Diversity), Diversity >= 0.0)).

	test(de_evaluation_count, deterministic(Evaluations =:= 30 * 31)) :-
		% population_size(30) default, max_generations(30) -> 30 * (30+1)
		differential_evolution(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_generations(30)]),
		memberchk(evaluations(Evaluations), Statistics).

	test(de_max_generations, deterministic(Generations =:= 20)) :-
		differential_evolution(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_generations(20)]),
		memberchk(generations(Generations), Statistics).

	%---------------------------------------------------------------------
	% Position dimensions and bounds
	%---------------------------------------------------------------------

	test(de_position_dimensions_and_bounds, deterministic) :-
		differential_evolution(sphere)::run(Position, _Fitness, [seed(42), max_generations(20)]),
		length(Position, Length),
		^^assertion(Length =:= 2),
		Position = [X, Y],
		^^assertion((X >= -5.0, X =< 5.0, Y >= -5.0, Y =< 5.0)).

	%---------------------------------------------------------------------
	% Custom random algorithm
	%---------------------------------------------------------------------

	test(de_explicit_random_algorithm, deterministic(Fitness < 0.05)) :-
		differential_evolution(sphere, as183)::run(_Position, Fitness, [seed(42), max_generations(60)]).

	%---------------------------------------------------------------------
	% Target fitness stopping
	%---------------------------------------------------------------------

	test(de_target_fitness_minimize, deterministic((Generations =< 100, Fitness =< 0.5))) :-
		differential_evolution(sphere)::run(_Position, Fitness, Statistics, [seed(42), max_generations(100), target_fitness(0.5)]),
		memberchk(generations(Generations), Statistics).

	test(de_target_fitness_unreachable, deterministic(Generations =:= 20)) :-
		differential_evolution(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_generations(20), target_fitness(-1.0)]),
		memberchk(generations(Generations), Statistics).

	test(de_target_fitness_none, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [seed(42), max_generations(20)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [seed(42), max_generations(20), target_fitness(none)]).

	%---------------------------------------------------------------------
	% Stagnation stopping
	%---------------------------------------------------------------------

	test(de_stagnation_generations, deterministic((Generations =< 20, Improvements =:= 0))) :-
		differential_evolution(constant_fitness)::run(_Position, _Fitness, Statistics, [seed(42), max_generations(20), stagnation_generations(3)]),
		memberchk(generations(Generations), Statistics),
		memberchk(improvements(Improvements), Statistics).

	test(de_stagnation_zero_disabled, deterministic((Position1 == Position2, Fitness1 =:= Fitness2))) :-
		differential_evolution(sphere)::run(Position1, Fitness1, [seed(42), max_generations(20)]),
		differential_evolution(sphere)::run(Position2, Fitness2, [seed(42), max_generations(20), stagnation_generations(0)]).

	test(de_stagnation_seed_reproducible, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		differential_evolution(constant_fitness)::run(Position1, Fitness1, Statistics1, [seed(42), max_generations(20), stagnation_generations(3)]),
		differential_evolution(constant_fitness)::run(Position2, Fitness2, Statistics2, [seed(42), max_generations(20), stagnation_generations(3)]).

	%---------------------------------------------------------------------
	% Custom stop condition
	%---------------------------------------------------------------------

	test(de_custom_stop_condition, deterministic(Generations =:= 5)) :-
		differential_evolution(sphere_stop)::run(_Position, _Fitness, Statistics, [seed(42), max_generations(100)]),
		memberchk(generations(Generations), Statistics).

	%---------------------------------------------------------------------
	% Progress reporting
	%---------------------------------------------------------------------

	test(de_progress_updates_zero, deterministic(Count =:= 0)) :-
		sphere_progress::clear_log,
		differential_evolution(sphere_progress)::run(_Position, _Fitness, [seed(42), max_generations(50), updates(0)]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(de_progress_updates_count, deterministic(Count =:= 5)) :-
		sphere_progress::clear_log,
		differential_evolution(sphere_progress)::run(_Position, _Fitness, [seed(42), max_generations(50), updates(5)]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(de_progress_values, deterministic) :-
		sphere_progress::clear_log,
		differential_evolution(sphere_progress)::run(_Position, _Fitness, [seed(42), max_generations(50), updates(5)]),
		once(sphere_progress::progress_log(Generation, BestPosition, BestFitness, MeanFitness, Diversity)),
		^^assertion(Generation > 0),
		^^assertion(list::valid(BestPosition)),
		^^assertion(number(BestFitness)),
		^^assertion(number(MeanFitness)),
		^^assertion(Diversity >= 0.0).

	%---------------------------------------------------------------------
	% Option validation
	%---------------------------------------------------------------------

	test(de_invalid_option_strategy, error(domain_error(option, strategy(rand/2/bin)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [strategy(rand/2/bin)]).

	test(de_invalid_option_max_generations, error(domain_error(option, max_generations(0)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [max_generations(0)]).

	test(de_invalid_option_population_size, error(domain_error(option, population_size(3)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [population_size(3)]).

	test(de_invalid_option_crossover_probability, error(domain_error(option, crossover_probability(1.5)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [crossover_probability(1.5)]).

	test(de_invalid_option_differential_weight, error(domain_error(option, differential_weight(0.0)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [differential_weight(0.0)]).

	test(de_invalid_option_objective, error(domain_error(option, objective(optimize)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [objective(optimize)]).

	test(de_invalid_option_target_fitness, error(domain_error(option, target_fitness(best)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [target_fitness(best)]).

	test(de_invalid_option_stagnation_generations, error(domain_error(option, stagnation_generations(-1)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [stagnation_generations(-1)]).

	test(de_invalid_option_updates, error(domain_error(option, updates(-1)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [updates(-1)]).

	test(de_invalid_option_seed, error(domain_error(option, seed(0)))) :-
		differential_evolution(sphere)::run(_Position, _Fitness, [seed(0)]).

	%---------------------------------------------------------------------
	% Problem validation
	%---------------------------------------------------------------------

	test(de_invalid_bounds, error(domain_error(position_bounds, [1.0-(-1.0), (-5.0)-5.0]))) :-
		differential_evolution(malformed_problem(invalid_bounds))::run(_Position, _Fitness).

	test(differential_evolution_random_donors, deterministic) :-
		fast_random(xoshiro128pp)::randomize(42),
		findall(
			A-B-C,
			(	integer::between(1, 64, _),
				integer::between(1, 8, Forbidden),
				differential_evolution(sphere, xoshiro128pp)<<random_distinct_indices_3(8, Forbidden, A, B, C),
				assertion(A =\= Forbidden),
				assertion(B =\= Forbidden),
				assertion(C =\= Forbidden),
				assertion(A =\= B),
				assertion(A =\= C),
				assertion(B =\= C)
			),
			Donors
		),
		assertion(member(8-_-_, Donors)),
		fast_random(xoshiro128pp)::randomize(42),
		findall(
			A-B-C,
			(	integer::between(1, 64, _),
				integer::between(1, 8, Forbidden),
				differential_evolution(sphere, xoshiro128pp)<<random_distinct_indices_3(8, Forbidden, A, B, C)
			),
			Repeated
		),
		assertion(Donors == Repeated).

	test(differential_evolution_minimum_population_donors, deterministic) :-
		fast_random(xoshiro128pp)::randomize(42),
		forall(
			integer::between(1, 4, Forbidden),
			(	differential_evolution(sphere, xoshiro128pp)<<random_distinct_indices_3(4, Forbidden, A, B, C),
				assertion(A =\= Forbidden),
				assertion(B =\= Forbidden),
				assertion(C =\= Forbidden),
				assertion(A =\= B),
				assertion(A =\= C),
				assertion(B =\= C),
				differential_evolution(sphere, xoshiro128pp)<<random_distinct_indices_2(4, Forbidden, D, E),
				assertion(D =\= Forbidden),
				assertion(E =\= Forbidden),
				assertion(D =\= E)
			)
		).

	test(differential_evolution_crossover_endpoints, deterministic) :-
		forall(
			member(Kind, [bin, exp]),
			(	fast_random(xoshiro128pp)::randomize(42),
				findall(
					Trial,
					(	integer::between(1, 32, _),
						differential_evolution(sphere, xoshiro128pp)<<rand_trial(Kind, [0,0], [1,1], [0,0], [0,0], [0-1,0-1], 0, 1, 2, Trial)
					),
					Trials
				),
				assertion(member([1,0], Trials)),
				assertion(member([0,1], Trials)),
				forall(member(Trial, Trials), memberchk(Trial, [[1,0], [0,1]])),
				differential_evolution(sphere, xoshiro128pp)<<rand_trial(Kind, [0,0], [1,1], [0,0], [0,0], [0-1,0-1], 1, 1, 2, Full),
				assertion(Full == [1,1]),
				differential_evolution(sphere, xoshiro128pp)<<rand_trial(Kind, [0], [1], [0], [0], [0-1], 0, 1, 1, Single),
				assertion(Single == [1])
			)
		).

	test(differential_evolution_malformed_bounds, deterministic) :-
		forall(
			member(Case, [empty, variable, open, improper, pair, numeric, inverted]),
			(	differential_evolution_fixture(Case)::position_bounds(Bounds),
				copy_term(Bounds, Original),
				catch(differential_evolution(differential_evolution_fixture(Case))::run(_, _), Error, true),
				assertion(nonvar(Error)),
				Error = error(domain_error(position_bounds, Reported), _),
				assertion(lgtunit::variant(Reported, Bounds)),
				assertion(lgtunit::variant(Original, Bounds))
			)
		).

	test(differential_evolution_continuous_initialization, deterministic) :-
		forall(
			member(Bounds, [[0-1], [0.0-1.0], [0-1.0], [0.0-1], [(-2)-(-1)], [3-3]]),
			(	fast_random(xoshiro128pp)::randomize(42),
				differential_evolution(sphere, xoshiro128pp)<<random_individual(1, Bounds, [X]),
				Bounds = [Low-High],
				assertion(X >= Low),
				assertion(X =< High),
				(	Low < High ->
					assertion(X > Low),
					assertion(X < High),
					assertion(float(X))
				;	assertion(X =:= Low)
				)
			)
		).

	test(differential_evolution_supplied_positions, deterministic) :-
		differential_evolution(differential_evolution_fixture(supplied))::run(Position, Fitness, Statistics, [population_size(4), target_fitness(1)]),
		assertion(Position == [0]),
		assertion(Fitness =:= 1),
		memberchk(generations(0), Statistics),
		memberchk(evaluations(4), Statistics).

	test(differential_evolution_invalid_positions, deterministic) :-
		forall(
			member(Case, [positions_empty, positions_count, positions_improper, positions_variable, positions_dimension, positions_numeric, positions_bounds]),
			(	differential_evolution_fixture(Case)::initial_positions(Positions),
				catch(differential_evolution(differential_evolution_fixture(Case))::run(_, _, [population_size(4)]), Error, true),
				assertion(nonvar(Error)),
				Error = error(domain_error(initial_positions, Reported), _),
				assertion(lgtunit::variant(Reported, Positions))
			)
		).

	test(differential_evolution_invalid_initial_fitness, error(domain_error(fitness, invalid))) :-
		differential_evolution(differential_evolution_fixture(fitness))::run(_, _, [population_size(4)]).

	test(differential_evolution_invalid_later_fitness, error(domain_error(fitness, invalid))) :-
		differential_evolution(differential_evolution_fixture(later_fitness), xoshiro128pp)<<evaluate_population([[0], [1]], minimize, _, _, _).

	test(differential_evolution_invalid_trial_fitness, error(domain_error(fitness, invalid))) :-
		differential_evolution(differential_evolution_fixture(fitness), xoshiro128pp)<<select_population([[0]-1], [[1]], minimize, _, _, _).

	test(differential_evolution_selection_ordering, deterministic) :-
		forall(
			member(Objective, [minimize, maximize]),
			(	forall(
					member(Value, [0, 1, 2]),
					(	differential_evolution(differential_evolution_fixture(mapped), xoshiro128pp)<<select_population([[1]-1], [[Value]], Objective, Selected, _, _),
						(	(Objective == minimize, Value =< 1; Objective == maximize, Value >= 1) ->
							assertion(Selected == [[Value]-Value])
						;	assertion(Selected == [[1]-1])
						)
					)
				),
				differential_evolution(differential_evolution_fixture(supplied), xoshiro128pp)<<select_population([[0]-1.0], [[1]], Objective, Tied, _, _),
				assertion(Tied == [[1]-1])
			)
		).

	test(differential_evolution_plateau_stagnation, deterministic) :-
		differential_evolution(differential_evolution_fixture(supplied))::run(Position, _, Statistics, [seed(42), population_size(4), max_generations(20), stagnation_generations(3)]),
		assertion(Position == [0]),
		memberchk(generations(3), Statistics),
		memberchk(improvements(0), Statistics),
		memberchk(evaluations(16), Statistics).

	test(differential_evolution_maximize_initial_target, deterministic) :-
		differential_evolution(differential_evolution_fixture(supplied))::run(_, _, Statistics, [population_size(4), objective(maximize), target_fitness(1)]),
		memberchk(generations(0), Statistics),
		memberchk(evaluations(4), Statistics).

	test(differential_evolution_report_schedules, all(Actual == Expected)) :-
		member(Max-Updates-Expected, [3-2-[2,3], 5-2-[3,5], 5-1-[5], 3-9-[1,2,3], 1-1-[1], 50-5-[10,20,30,40,50], 3-0-[]]),
		sphere_progress::clear_log,
		differential_evolution(sphere_progress)::run(_, _, [seed(42), max_generations(Max), updates(Updates)]),
		findall(Generation, sphere_progress::progress_log(Generation, _, _, _, _), Actual).

	test(differential_evolution_early_reports, all(Actual == Expected)) :-
		member(Options-Expected, [
			[max_generations(10), updates(5), stagnation_generations(3)]-[2,3],
			[max_generations(10), updates(5), target_fitness(1)]-[0],
			[max_generations(10), updates(5)]-[2,3]
		]),
		differential_evolution_reporting::clear_log,
		differential_evolution(differential_evolution_reporting)::run(_, _, [seed(42)| Options]),
		findall(Generation, differential_evolution_reporting::progress_log(Generation, _, _, _, _), Actual).

	test(differential_evolution_undefined_progress, deterministic) :-
		differential_evolution(sphere)::run(_, _, [seed(42), max_generations(3), updates(2)]).

	test(differential_evolution_mutation_formulas, deterministic) :-
		differential_evolution(sphere, xoshiro128pp)<<mutate_rand([1,2], [3,4], [5,6], 2, [(-10)-10,(-10)-10], Rand),
		assertion(Rand == [-3,-2]),
		differential_evolution(sphere, xoshiro128pp)<<best_bin(1, [0,0], [1,2], [3,4], [5,6], [(-10)-10,(-10)-10], 1, 2, 1, Best),
		assertion(Best == [-3,-2]),
		differential_evolution(sphere, xoshiro128pp)<<current_bin(1, [1,2], [3,4], [5,6], [7,8], [(-10)-10,(-10)-10], 1, 2, 1, Current),
		assertion(Current == [1,2]).

	test(differential_evolution_exponential_segments, all(Trial == Expected)) :-
		member(Start-Length-Expected, [1-1-[1,0,0,0], 2-2-[0,1,1,0], 4-2-[1,0,0,1], 3-4-[1,1,1,1]]),
		differential_evolution(sphere, xoshiro128pp)<<crossover_segment(1, [0,0,0,0], [1,1,1,1], Start, Length, 4, Trial).

	test(differential_evolution_corrected_reference, deterministic) :-
		forall(
			(	member(Strategy, [rand/1/bin,rand/1/exp,best/1/bin,current-to-best/1/bin]),
				member(Objective, [minimize,maximize])
			),
			(	corrected_reference(Strategy, Objective, Position, Fitness, State),
				differential_evolution(sphere)::run(ActualPosition, ActualFitness, [seed(42), strategy(Strategy), objective(Objective), population_size(4), max_generations(5), crossover_probability(0.9)]),
				fast_random(xoshiro128pp)::get_seed(ActualState),
				assertion(ActualPosition == Position),
				assertion(ActualFitness =:= Fitness),
				assertion(ActualState == State)
			)
		).

	test(differential_evolution_population_index, deterministic) :-
		forall(
			member(NP, [4,16,17,256,257]),
			(	findall([I]-I, integer::between(1, NP, I), Population),
				differential_evolution(sphere, xoshiro128pp)<<population_index(Population, Index),
				forall(
					integer::between(1, NP, I),
					(	differential_evolution(sphere, xoshiro128pp)<<indexed_position(Index, I, Position),
						assertion(Position == [I])
					)
				)
			)
		).

	test(differential_evolution_fused_bounds, deterministic) :-
		differential_evolution(sphere, xoshiro128pp)<<rand_bin(1, [0,0,0], [-2,0,2], [0,0,0], [0,0,0], [(-1)-1,0-0,(-1)-1], 1, 1, 1, Rand),
		assertion(Rand == [-1,0,1]),
		differential_evolution(sphere, xoshiro128pp)<<best_bin(1, [0,0,0], [-2,0,2], [0,0,0], [0,0,0], [(-1)-1,0-0,(-1)-1], 1, 1, 1, Best),
		assertion(Best == [-1,0,1]),
		differential_evolution(sphere, xoshiro128pp)<<current_bin(1, [0,0,0], [-2,0,2], [0,0,0], [0,0,0], [(-1)-1,0-0,(-1)-1], 1, 1, 1, Current),
		assertion(Current == [-1,0,1]).

	test(differential_evolution_selection_best, deterministic) :-
		forall(
			member(Objective-Expected, [minimize-0, maximize-2]),
			(	differential_evolution(differential_evolution_fixture(mapped), xoshiro128pp)<<select_population([[1]-1,[1]-1,[1]-1], [[0],[2],[0]], Objective, _, Position, Fitness),
				assertion(Position == [Expected]),
				assertion(Fitness =:= Expected),
				differential_evolution(differential_evolution_fixture(supplied), xoshiro128pp)<<select_population([[0]-1,[0]-1], [[1],[0]], Objective, _, Tied, _),
				assertion(Tied == [1])
			)
		).

	test(differential_evolution_actual_evaluations, deterministic) :-
		differential_evolution_benchmark(2, counted)::clear_log,
		differential_evolution(differential_evolution_benchmark(2, counted))::run(_, _, Statistics, [seed(42), population_size(4), max_generations(5)]),
		differential_evolution_benchmark(2, counted)::evaluations(Calls),
		length(Calls, Count),
		memberchk(evaluations(Count), Statistics),
		assertion(Count =:= 24),
		check_evaluation_batches(Calls).

	test(differential_evolution_metrics_values, deterministic) :-
		differential_evolution(sphere, xoshiro128pp)<<population_metrics([[0,0]-1, [2,0]-3], Mean, Diversity),
		assertion(Mean =~= 2),
		assertion(Diversity =~= 1),
		differential_evolution(sphere, xoshiro128pp)<<population_metrics([[3,4]-2, [3,4]-2], SameMean, SameDiversity),
		assertion(SameMean =~= 2),
		assertion(SameDiversity =~= 0).

	test(differential_evolution_no_discarded_metrics, deterministic) :-
		differential_evolution(differential_evolution_fixture(large_fitness))::run(_, _),
		differential_evolution(differential_evolution_fixture(large_fitness))::run(_, _, [target_fitness(1.0e308)]).

	test(differential_evolution_final_metrics_shared, deterministic) :-
		sphere_progress::clear_log,
		differential_evolution(sphere_progress)::run(Position, Fitness, Statistics, [seed(42), max_generations(3), updates(2)]),
		memberchk(final_mean_fitness(Mean), Statistics),
		memberchk(final_diversity(Diversity), Statistics),
		findall(G, sphere_progress::progress_log(G, _, _, _, _), Generations),
		assertion(Generations == [2,3]),
		sphere_progress::progress_log(3, Position, Fitness, ReportedMean, ReportedDiversity),
		assertion(Mean =~= ReportedMean),
		assertion(Diversity =~= ReportedDiversity).

	test(differential_evolution_two_donor_rng, deterministic) :-
		fast_random(xoshiro128pp)::randomize(42),
		fast_random(xoshiro128pp)::get_seed(Before),
		findall(
			A-B,
			(	integer::between(1, 64, _),
				differential_evolution(sphere, xoshiro128pp)<<random_distinct_indices_2(8, 1, A, B),
				assertion(A >= 2),
				assertion(A =< 8),
				assertion(B >= 2),
				assertion(B =< 8),
				assertion(A =\= B)
			),
			Donors
		),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before \== After),
		assertion(member(8-_, Donors)),
		assertion(member(_-8, Donors)).

	test(differential_evolution_open_bounds_preserved, variant(Reported, Bounds)) :-
		Bounds = [0-1| Tail],
		catch(differential_evolution(sphere, xoshiro128pp)<<validate_bounds(Bounds), Error, true),
		assertion(nonvar(Error)),
		assertion(var(Tail)),
		Error = error(domain_error(position_bounds, Reported), _).

	test(differential_evolution_unbound_options, error(instantiation_error)) :-
		differential_evolution(sphere)::run(_, _, _).

	test(differential_evolution_options_list_type, error(type_error(list, invalid))) :-
		differential_evolution(sphere)::run(_, _, invalid).

	test(differential_evolution_option_type, error(type_error(compound, invalid))) :-
		differential_evolution(sphere)::run(_, _, [invalid]).

	test(differential_evolution_run_arities, deterministic) :-
		forall(
			member(Strategy, [rand/1/bin,rand/1/exp,best/1/bin,current-to-best/1/bin]),
			(	Options = [seed(42),strategy(Strategy),population_size(4),max_generations(5)],
				differential_evolution(sphere)::run(Position, Fitness, Statistics, Options),
				fast_random(xoshiro128pp)::get_seed(State),
				differential_evolution(sphere)::run(OtherPosition, OtherFitness, Options),
				fast_random(xoshiro128pp)::get_seed(OtherState),
				assertion(Position == OtherPosition),
				assertion(Fitness =:= OtherFitness),
				assertion(State == OtherState),
				memberchk(evaluations(24), Statistics)
			)
		).

	test(differential_evolution_termination_report_boundary, deterministic(Actual == [3])) :-
		differential_evolution_reporting::clear_log,
		differential_evolution(differential_evolution_reporting)::run(_, _, [seed(42),max_generations(6),updates(2)]),
		findall(Generation, differential_evolution_reporting::progress_log(Generation, _, _, _, _), Actual).

	% auxiliary predicates

	corrected_reference(rand/1/bin, minimize, [0.01255232244729998,-0.28452965896576643], 0.08111468762999635, seed(xoshiro128pp,s(3920786907,2001352116,3570734577,2545528888))).
	corrected_reference(rand/1/bin, maximize, [-4.999819677323103,-5.0], 49.9981968057473, seed(xoshiro128pp,s(3920786907,2001352116,3570734577,2545528888))).
	corrected_reference(rand/1/exp, minimize, [-0.125326543688774,-0.09465178327634938], 0.024665702630367187, seed(xoshiro128pp,s(4003496685,885762629,2904628508,869883251))).
	corrected_reference(rand/1/exp, maximize, [-4.999819677323103,-5.0], 49.9981968057473, seed(xoshiro128pp,s(4003496685,885762629,2904628508,869883251))).
	corrected_reference(best/1/bin, minimize, [-0.32286506740376353,-0.05190603379160175], 0.10693608809361167, seed(xoshiro128pp,s(2892108366,612155686,341760389,462332415))).
	corrected_reference(best/1/bin, maximize, [-5.0,-5.0], 50.0, seed(xoshiro128pp,s(2892108366,612155686,341760389,462332415))).
	corrected_reference(current-to-best/1/bin, minimize, [0.36078100251033896,-0.28452965896576643], 0.21112005860354055, seed(xoshiro128pp,s(2892108366,612155686,341760389,462332415))).
	corrected_reference(current-to-best/1/bin, maximize, [-5.0,-5.0], 50.0, seed(xoshiro128pp,s(2892108366,612155686,341760389,462332415))).

	check_evaluation_batches([]).
	check_evaluation_batches([_-State, _-State1, _-State2, _-State3| Calls]) :-
		assertion(State == State1),
		assertion(State == State2),
		assertion(State == State3),
		check_evaluation_batches(Calls).

:- end_object.

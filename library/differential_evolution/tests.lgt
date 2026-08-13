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
		date is 2026-08-13,
		comment is 'Unit tests for the "differential_evolution" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
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
		% population_size(30) default, max_generations(30) → 30 * (30+1)
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

	test(de_progress_updates_count, deterministic(Count =:= 6)) :-
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

:- end_object.

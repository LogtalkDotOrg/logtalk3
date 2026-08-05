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
		date is 2026-08-05,
		comment is 'Unit tests for the "particle_swarm_optimization" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(particle_swarm_optimization(_, _)).
	cover(particle_swarm_optimization(_)).

	test(pso_sphere_run_2, deterministic((list::valid(Position), Fitness < 0.001))) :-
		particle_swarm_optimization(sphere)::run(Position, Fitness, [seed(42), max_iterations(100)]).

	test(pso_maximize_initial_best, deterministic((Position == [0.5, -0.5], Fitness =:= -0.5))) :-
		particle_swarm_optimization(negative_sphere_stop)::run(Position, Fitness, [seed(42), objective(maximize)]).

	test(pso_maximize, deterministic(Fitness > -0.001)) :-
		particle_swarm_optimization(negative_sphere)::run(_Position, Fitness, [seed(42), max_iterations(100), objective(maximize)]).

	test(pso_maximize_seed_reproducible, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(negative_sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(100), objective(maximize)]),
		particle_swarm_optimization(negative_sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(100), objective(maximize)]).

	test(pso_seed_reproducible, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(100)]),
		particle_swarm_optimization(sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(100)]).

	test(pso_explicit_minimize, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(100)]),
		particle_swarm_optimization(sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(100), objective(minimize)]).

	test(pso_evaluation_count, deterministic(Evaluations =:= 404)) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(100)]),
		memberchk(evaluations(Evaluations), Statistics).

	test(pso_run_2, deterministic((list::valid(Position), number(Fitness)))) :-
		particle_swarm_optimization(sphere)::run(Position, Fitness).

	test(pso_explicit_random_algorithm, deterministic(Fitness < 0.01)) :-
		particle_swarm_optimization(sphere, as183)::run(_Position, Fitness, [seed(42), max_iterations(100)]).

	test(pso_supplied_velocity_movement, deterministic((Position == [0.5], Fitness =:= 0.5))) :-
		particle_swarm_optimization(supplied_velocity(movement))::run(Position, Fitness, [seed(42), objective(maximize), max_iterations(1), inertia_weight(1.0), cognitive_coefficient(0.0), social_coefficient(0.0)]).

	test(pso_supplied_velocity_seed_independent, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		Options = [objective(maximize), max_iterations(1), inertia_weight(1.0), cognitive_coefficient(0.0), social_coefficient(0.0)],
		particle_swarm_optimization(supplied_velocity(movement))::run(Position1, Fitness1, Statistics1, [seed(1)| Options]),
		particle_swarm_optimization(supplied_velocity(movement))::run(Position2, Fitness2, Statistics2, [seed(2)| Options]).

	test(pso_supplied_velocity_lower_limit, deterministic(Position == [0.0])) :-
		particle_swarm_optimization(supplied_velocity(lower_limit))::run(Position, _Fitness, [max_iterations(1), inertia_weight(1.0), cognitive_coefficient(0.0), social_coefficient(0.0)]).

	test(pso_supplied_velocity_upper_limit, deterministic(Position == [1.0])) :-
		particle_swarm_optimization(supplied_velocity(upper_limit))::run(Position, _Fitness, [objective(maximize), max_iterations(1), inertia_weight(1.0), cognitive_coefficient(0.0), social_coefficient(0.0)]).

	test(pso_failing_velocity_fallback, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(random_velocity_fallback)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(10)]),
		particle_swarm_optimization(failing_velocity_fallback)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(10)]).

	test(pso_position_dimensions_and_bounds, deterministic((Length =:= 2, X >= -5.0, X =< 5.0, Y >= -5.0, Y =< 5.0))) :-
		particle_swarm_optimization(sphere)::run(Position, _Fitness, [seed(42), max_iterations(20)]),
		length(Position, Length),
		Position = [X, Y].

	test(pso_max_iterations, deterministic(Iterations =:= 20)) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(20)]),
		memberchk(iterations(Iterations), Statistics).

	test(pso_custom_stop_condition, deterministic((Iterations =:= 5, Evaluations =:= 24))) :-
		particle_swarm_optimization(sphere_stop)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(100)]),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics).

	test(pso_stagnation_iterations, deterministic((Iterations =:= 3, Evaluations =:= 16, Improvements =:= 0))) :-
		particle_swarm_optimization(constant_fitness)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(20), stagnation_iterations(3)]),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics),
		memberchk(improvements(Improvements), Statistics).

	test(pso_stagnation_one_maximize, deterministic((Iterations =:= 1, Evaluations =:= 8))) :-
		particle_swarm_optimization(constant_fitness)::run(_Position, _Fitness, Statistics, [seed(42), objective(maximize), max_iterations(20), stagnation_iterations(1)]),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics).

	test(pso_stagnation_zero_disabled, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(20)]),
		particle_swarm_optimization(sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(20), stagnation_iterations(0)]).

	test(pso_stagnation_at_max_iterations, deterministic(Iterations =:= 5)) :-
		particle_swarm_optimization(constant_fitness)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(5), stagnation_iterations(5)]),
		memberchk(iterations(Iterations), Statistics).

	test(pso_stagnation_seed_reproducible, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(constant_fitness)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(20), stagnation_iterations(3)]),
		particle_swarm_optimization(constant_fitness)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(20), stagnation_iterations(3)]).

	test(pso_target_fitness_minimize_initial, deterministic((Iterations =:= 0, Evaluations =:= 4, Fitness =:= 0.5))) :-
		particle_swarm_optimization(sphere)::run(_Position, Fitness, Statistics, [seed(42), target_fitness(0.5)]),
		memberchk(iterations(Iterations), Statistics),
		memberchk(evaluations(Evaluations), Statistics).

	test(pso_target_fitness_maximize_initial, deterministic((Iterations =:= 0, Fitness =:= -0.5))) :-
		particle_swarm_optimization(negative_sphere)::run(_Position, Fitness, Statistics, [seed(42), objective(maximize), target_fitness(-0.5)]),
		memberchk(iterations(Iterations), Statistics).

	test(pso_target_fitness_minimize, deterministic((Iterations < 100, Fitness =< 0.001))) :-
		particle_swarm_optimization(sphere)::run(_Position, Fitness, Statistics, [seed(42), max_iterations(100), target_fitness(0.001)]),
		memberchk(iterations(Iterations), Statistics).

	test(pso_target_fitness_unreachable, deterministic(Iterations =:= 20)) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(20), target_fitness(-1.0)]),
		memberchk(iterations(Iterations), Statistics).

	test(pso_target_fitness_none, deterministic((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(20)]),
		particle_swarm_optimization(sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(20), target_fitness(none)]).

	test(pso_target_fitness_progress_final, deterministic(Iteration =:= 0)) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), target_fitness(0.5), updates(1)]),
		sphere_progress::progress_log(Iteration, _, _, _, _).

	test(pso_statistics, deterministic((Iterations =:= 20, Improvements >= 0, Improvements =< Iterations, number(MeanFitness), MeanFitness >= 0.0, number(Diversity), Diversity >= 0.0))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(20)]),
		memberchk(iterations(Iterations), Statistics),
		memberchk(improvements(Improvements), Statistics),
		memberchk(final_mean_fitness(MeanFitness), Statistics),
		memberchk(final_diversity(Diversity), Statistics).

	test(pso_progress_updates_zero, deterministic(Count =:= 0)) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), max_iterations(100), updates(0)]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(pso_progress_updates_count, deterministic(Count =:= 6)) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), max_iterations(100), updates(5)]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(pso_progress_values, deterministic((Iteration > 0, list::valid(BestPosition), number(BestFitness), number(MeanFitness), MeanFitness >= BestFitness, Diversity >= 0.0))) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), max_iterations(100), updates(5)]),
		once(sphere_progress::progress_log(Iteration, BestPosition, BestFitness, MeanFitness, Diversity)).

	test(pso_boundary_clamps_and_zeroes_velocity, deterministic((BestPosition == [0.0], MeanFitness1 =:= 0.0, MeanFitness2 =:= 0.0))) :-
		boundary_progress::clear_log,
		particle_swarm_optimization(boundary_progress)::run(BestPosition, _BestFitness, [seed(42), max_iterations(2), updates(2)]),
		boundary_progress::progress_log(1, _, _, MeanFitness1, _),
		boundary_progress::progress_log(2, _, _, MeanFitness2, _).

	test(pso_invalid_option_max_iterations, error(domain_error(option, max_iterations(0)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [max_iterations(0)]).

	test(pso_invalid_option_stagnation_iterations_negative, error(domain_error(option, stagnation_iterations(-1)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [stagnation_iterations(-1)]).

	test(pso_invalid_option_stagnation_iterations_float, error(domain_error(option, stagnation_iterations(1.0)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [stagnation_iterations(1.0)]).

	test(pso_invalid_option_objective, error(domain_error(option, objective(optimize)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [objective(optimize)]).

	test(pso_invalid_option_target_fitness, error(domain_error(option, target_fitness(best)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [target_fitness(best)]).

	test(pso_invalid_option_inertia_weight, error(domain_error(option, inertia_weight(-0.1)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [inertia_weight(-0.1)]).

	test(pso_invalid_option_cognitive_coefficient, error(domain_error(option, cognitive_coefficient(-0.1)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [cognitive_coefficient(-0.1)]).

	test(pso_invalid_option_social_coefficient, error(domain_error(option, social_coefficient(-0.1)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [social_coefficient(-0.1)]).

	test(pso_invalid_option_updates, error(domain_error(option, updates(-1)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [updates(-1)]).

	test(pso_invalid_option_seed, error(domain_error(option, seed(0)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [seed(0)]).

	test(pso_empty_positions, error(domain_error(initial_positions, []))) :-
		particle_swarm_optimization(malformed_problem(empty))::run(_Position, _Fitness).

	test(pso_inconsistent_positions, error(domain_error(initial_positions, [[0.0, 0.0], [0.0]]))) :-
		particle_swarm_optimization(malformed_problem(inconsistent))::run(_Position, _Fitness).

	test(pso_out_of_bounds_positions, error(domain_error(initial_positions, [[6.0, 0.0]]))) :-
		particle_swarm_optimization(malformed_problem(out_of_bounds))::run(_Position, _Fitness).

	test(pso_invalid_bounds, error(domain_error(position_bounds, [1.0-(-1.0), (-5.0)-5.0]))) :-
		particle_swarm_optimization(malformed_problem(invalid_bounds))::run(_Position, _Fitness).

	test(pso_empty_initial_velocities, error(domain_error(initial_velocities, []))) :-
		particle_swarm_optimization(supplied_velocity(empty))::run(_Position, _Fitness).

	test(pso_extra_initial_velocities, error(domain_error(initial_velocities, [[0.0], [0.0]]))) :-
		particle_swarm_optimization(supplied_velocity(extra))::run(_Position, _Fitness).

	test(pso_initial_velocity_dimension, error(domain_error(initial_velocities, [[0.0, 0.0]]))) :-
		particle_swarm_optimization(supplied_velocity(dimension))::run(_Position, _Fitness).

	test(pso_initial_velocity_nonnumeric, error(domain_error(initial_velocities, [[a]]))) :-
		particle_swarm_optimization(supplied_velocity(nonnumeric))::run(_Position, _Fitness).

	test(pso_initial_velocity_below_limit, error(domain_error(initial_velocities, [[-1.1]]))) :-
		particle_swarm_optimization(supplied_velocity(below_limit))::run(_Position, _Fitness).

	test(pso_initial_velocity_above_limit, error(domain_error(initial_velocities, [[1.1]]))) :-
		particle_swarm_optimization(supplied_velocity(above_limit))::run(_Position, _Fitness).

	test(pso_initial_velocity_nonground, error(domain_error(initial_velocities, [[_]]))) :-
		particle_swarm_optimization(supplied_velocity(nonground))::run(_Position, _Fitness).

:- end_object.

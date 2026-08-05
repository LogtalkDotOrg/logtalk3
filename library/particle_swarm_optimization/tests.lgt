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
		length/2, member/2
	]).

	cover(particle_swarm_optimization(_, _)).
	cover(particle_swarm_optimization(_)).

	test(pso_sphere_run_2, true((list::valid(Position), Fitness < 0.001))) :-
		particle_swarm_optimization(sphere)::run(Position, Fitness, [seed(42), max_iterations(100)]).

	test(pso_seed_reproducible, true((Position1 == Position2, Fitness1 =:= Fitness2, Statistics1 == Statistics2))) :-
		particle_swarm_optimization(sphere)::run(Position1, Fitness1, Statistics1, [seed(42), max_iterations(100)]),
		particle_swarm_optimization(sphere)::run(Position2, Fitness2, Statistics2, [seed(42), max_iterations(100)]).

	test(pso_evaluation_count, true(Evaluations =:= 404)) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(100)]),
		member(evaluations(Evaluations), Statistics).

	test(pso_run_2, true((list::valid(Position), number(Fitness)))) :-
		particle_swarm_optimization(sphere)::run(Position, Fitness).

	test(pso_explicit_random_algorithm, true(Fitness < 0.01)) :-
		particle_swarm_optimization(sphere, as183)::run(_Position, Fitness, [seed(42), max_iterations(100)]).

	test(pso_position_dimensions_and_bounds, true((Length =:= 2, X >= -5.0, X =< 5.0, Y >= -5.0, Y =< 5.0))) :-
		particle_swarm_optimization(sphere)::run(Position, _Fitness, [seed(42), max_iterations(20)]),
		length(Position, Length),
		Position = [X, Y].

	test(pso_max_iterations, true(Iterations =:= 20)) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(20)]),
		member(iterations(Iterations), Statistics).

	test(pso_custom_stop_condition, true((Iterations =:= 5, Evaluations =:= 24))) :-
		particle_swarm_optimization(sphere_stop)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(100)]),
		member(iterations(Iterations), Statistics),
		member(evaluations(Evaluations), Statistics).

	test(pso_statistics, true((Iterations =:= 20, Improvements >= 0, Improvements =< Iterations, number(MeanFitness), MeanFitness >= 0.0, number(Diversity), Diversity >= 0.0))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, Statistics, [seed(42), max_iterations(20)]),
		member(iterations(Iterations), Statistics),
		member(improvements(Improvements), Statistics),
		member(final_mean_fitness(MeanFitness), Statistics),
		member(final_diversity(Diversity), Statistics).

	test(pso_progress_updates_zero, true(Count =:= 0)) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), max_iterations(100), updates(0)]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(pso_progress_updates_count, true(Count =:= 6)) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), max_iterations(100), updates(5)]),
		findall(1, sphere_progress::progress_log(_, _, _, _, _), Logs),
		length(Logs, Count).

	test(pso_progress_values, true((Iteration > 0, list::valid(BestPosition), number(BestFitness), number(MeanFitness), MeanFitness >= BestFitness, Diversity >= 0.0))) :-
		sphere_progress::clear_log,
		particle_swarm_optimization(sphere_progress)::run(_Position, _Fitness, [seed(42), max_iterations(100), updates(5)]),
		sphere_progress::progress_log(Iteration, BestPosition, BestFitness, MeanFitness, Diversity).

	test(pso_boundary_clamps_and_zeroes_velocity, true((BestPosition == [0.0], MeanFitness1 =:= 0.0, MeanFitness2 =:= 0.0))) :-
		boundary_progress::clear_log,
		particle_swarm_optimization(boundary_progress)::run(BestPosition, _BestFitness, [seed(42), max_iterations(2), updates(2)]),
		boundary_progress::progress_log(1, _, _, MeanFitness1, _),
		boundary_progress::progress_log(2, _, _, MeanFitness2, _).

	test(pso_invalid_option_max_iterations, error(domain_error(option, max_iterations(0)))) :-
		particle_swarm_optimization(sphere)::run(_Position, _Fitness, [max_iterations(0)]).

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

:- end_object.

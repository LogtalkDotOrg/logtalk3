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


:- object(particle_swarm_optimization(_Problem_, _RandomAlgorithm_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-05,
		comment is 'Continuous bounded global-best particle swarm optimization algorithm. Parameterized by a problem object implementing the ``particle_swarm_optimization_protocol`` protocol and by a random number generator algorithm for the ``fast_random`` library. The algorithm minimizes the fitness function defined by the problem.',
		parameters is [
			'Problem' - 'Problem object implementing ``particle_swarm_optimization_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library.'
		],
		remarks is [
			'Algorithm' - 'Uses synchronous global-best particle swarm optimization. Every particle update in an iteration uses the global best from the start of that iteration.',
			'Boundary handling' - 'Velocities are limited to plus or minus the range of each dimension. A position crossing a bound is clamped to that bound and its velocity component is set to zero.',
			'Progress reporting' - 'If the problem object defines ``progress/5``, it is called periodically and once when the loop terminates.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.'
		],
		see_also is [particle_swarm_optimization(_), particle_swarm_optimization_protocol]
	]).

	:- public(run/2).
	:- mode(run(-list(number), -number), one).
	:- info(run/2, [
		comment is 'Runs the particle swarm optimization algorithm using default options and returns the best position and fitness found.',
		argnames is ['BestPosition', 'BestFitness']
	]).

	:- public(run/3).
	:- mode(run(-list(number), -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the particle swarm optimization algorithm using the given options and returns the best position and fitness found.',
		argnames is ['BestPosition', 'BestFitness', 'Options'],
		remarks is [
			'``max_iterations(N)`` option' - 'Maximum number of swarm iterations (default: ``1000``).',
			'``inertia_weight(W)`` option' - 'Velocity inertia weight (default: ``0.7298``).',
			'``cognitive_coefficient(C)`` option' - 'Personal-best acceleration coefficient (default: ``1.49618``).',
			'``social_coefficient(C)`` option' - 'Global-best acceleration coefficient (default: ``1.49618``).',
			'``updates(N)`` option' - 'Number of progress reports during the run; zero disables reporting (default: ``0``).',
			'``seed(S)`` option' - 'Positive integer random seed for reproducible runs.'
		]
	]).

	:- public(run/4).
	:- mode(run(-list(number), -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the particle swarm optimization algorithm using the given options and returns the best position, best fitness, and run statistics.',
		argnames is ['BestPosition', 'BestFitness', 'Statistics', 'Options'],
		remarks is [
			'Statistics list' - 'A list containing ``iterations(N)``, ``evaluations(E)``, ``improvements(I)``, ``final_mean_fitness(M)``, and ``final_diversity(D)``.'
		]
	]).

	:- uses(_Problem_, [
		initial_positions/1, position_bounds/1, fitness/2, stop_condition/3, progress/5
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		random/1, random/3, randomize/1
	]).

	:- uses(type, [
		valid/2
	]).

	run(BestPosition, BestFitness) :-
		run(BestPosition, BestFitness, _Statistics, []).

	run(BestPosition, BestFitness, UserOptions) :-
		run(BestPosition, BestFitness, _Statistics, UserOptions).

	run(BestPosition, BestFitness, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		( 	^^option(seed(Seed), Options) ->
			randomize(Seed)
		;	true
		),
		initial_positions(Positions),
		position_bounds(Bounds),
		validate_problem(Positions, Bounds),
		initialize_swarm(Positions, Bounds, Swarm0, InitialBestPosition, InitialBestFitness, SwarmSize),
		^^option(max_iterations(MaxIterations), Options),
		^^option(inertia_weight(InertiaWeight), Options),
		^^option(cognitive_coefficient(CognitiveCoefficient), Options),
		^^option(social_coefficient(SocialCoefficient), Options),
		^^option(updates(Updates), Options),
		( 	Updates > 0 ->
			UpdateInterval is max(1, (MaxIterations - 1) // Updates)
		;	UpdateInterval is 0
		),
		loop(
			0, MaxIterations, UpdateInterval, Bounds,
			InertiaWeight, CognitiveCoefficient, SocialCoefficient,
			Swarm0, InitialBestPosition, InitialBestFitness, 0,
			FinalSwarm, BestPosition, BestFitness, Iterations, Improvements
		),
		swarm_metrics(FinalSwarm, MeanFitness, Diversity),
		Evaluations is SwarmSize * (Iterations + 1),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			improvements(Improvements),
			final_mean_fitness(MeanFitness),
			final_diversity(Diversity)
		].

	validate_problem(Positions, Bounds) :-
		( 	valid_bounds(Bounds) ->
			true
		;	throw(error(domain_error(position_bounds, Bounds), run/4))
		),
		( 	valid_positions(Positions, Bounds) ->
			true
		;	throw(error(domain_error(initial_positions, Positions), run/4))
		).

	valid_bounds([Lower-Upper| Bounds]) :-
		number(Lower),
		number(Upper),
		Lower =< Upper,
		valid_remaining_bounds(Bounds).

	valid_remaining_bounds([]).
	valid_remaining_bounds([Lower-Upper| Bounds]) :-
		number(Lower),
		number(Upper),
		Lower =< Upper,
		valid_remaining_bounds(Bounds).

	valid_positions([Position| Positions], Bounds) :-
		valid_position(Position, Bounds),
		valid_remaining_positions(Positions, Bounds).

	valid_remaining_positions([], _Bounds).
	valid_remaining_positions([Position| Positions], Bounds) :-
		valid_position(Position, Bounds),
		valid_remaining_positions(Positions, Bounds).

	valid_position([], []).
	valid_position([Value| Position], [Lower-Upper| Bounds]) :-
		number(Value),
		Value >= Lower,
		Value =< Upper,
		valid_position(Position, Bounds).

	initialize_swarm([Position| Positions], Bounds, [Particle| Particles], BestPosition, BestFitness, SwarmSize) :-
		initialize_particle(Position, Bounds, Particle),
		Particle = particle(_, _, Fitness, _, _),
		initialize_remaining_swarm(Positions, Bounds, Particles, Position, Fitness, BestPosition, BestFitness, 1, SwarmSize).

	initialize_remaining_swarm([], _Bounds, [], BestPosition, BestFitness, BestPosition, BestFitness, SwarmSize, SwarmSize).
	initialize_remaining_swarm([Position| Positions], Bounds, [Particle| Particles], BestPosition0, BestFitness0, BestPosition, BestFitness, SwarmSize0, SwarmSize) :-
		initialize_particle(Position, Bounds, Particle),
		Particle = particle(_, _, Fitness, _, _),
		( 	Fitness < BestFitness0 ->
			BestPosition1 = Position,
			BestFitness1 = Fitness
		;	BestPosition1 = BestPosition0,
			BestFitness1 = BestFitness0
		),
		SwarmSize1 is SwarmSize0 + 1,
		initialize_remaining_swarm(Positions, Bounds, Particles, BestPosition1, BestFitness1, BestPosition, BestFitness, SwarmSize1, SwarmSize).

	initialize_particle(Position, Bounds, particle(Position, Velocity, Fitness, Position, Fitness)) :-
		initial_velocity(Bounds, Velocity),
		fitness(Position, Fitness),
		( 	number(Fitness) ->
			true
		;	throw(error(domain_error(fitness, Fitness), run/4))
		).

	initial_velocity([], []).
	initial_velocity([Lower-Upper| Bounds], [Velocity| Velocities]) :-
		Range is Upper - Lower,
		MinimumVelocity is -1.0 * Range,
		MaximumVelocity is 1.0 * Range,
		random(MinimumVelocity, MaximumVelocity, Velocity),
		initial_velocity(Bounds, Velocities).

	loop(Iteration, MaxIterations, UpdateInterval, _Bounds, _InertiaWeight, _CognitiveCoefficient, _SocialCoefficient,
			Swarm, BestPosition, BestFitness, Improvements,
			Swarm, BestPosition, BestFitness, Iteration, Improvements) :-
		Iteration >= MaxIterations,
		!,
		report_final(Iteration, UpdateInterval, Swarm, BestPosition, BestFitness).
	loop(Iteration, _MaxIterations, UpdateInterval, _Bounds, _InertiaWeight, _CognitiveCoefficient, _SocialCoefficient,
			Swarm, BestPosition, BestFitness, Improvements,
			Swarm, BestPosition, BestFitness, Iteration, Improvements) :-
		stop_condition(Iteration, BestPosition, BestFitness),
		!,
		report_final(Iteration, UpdateInterval, Swarm, BestPosition, BestFitness).
	loop(Iteration, MaxIterations, UpdateInterval, Bounds, InertiaWeight, CognitiveCoefficient, SocialCoefficient,
			Swarm0, BestPosition0, BestFitness0, Improvements0,
			FinalSwarm, BestPosition, BestFitness, Iterations, Improvements) :-
		update_swarm(Swarm0, Bounds, BestPosition0, InertiaWeight, CognitiveCoefficient, SocialCoefficient, Swarm1),
		swarm_best(Swarm1, CandidatePosition, CandidateFitness),
		( 	CandidateFitness < BestFitness0 ->
			BestPosition1 = CandidatePosition,
			BestFitness1 = CandidateFitness,
			Improvements1 is Improvements0 + 1
		;	BestPosition1 = BestPosition0,
			BestFitness1 = BestFitness0,
			Improvements1 = Improvements0
		),
		Iteration1 is Iteration + 1,
		report_progress(Iteration1, UpdateInterval, Swarm1, BestPosition1, BestFitness1),
		loop(Iteration1, MaxIterations, UpdateInterval, Bounds, InertiaWeight, CognitiveCoefficient, SocialCoefficient,
			Swarm1, BestPosition1, BestFitness1, Improvements1,
			FinalSwarm, BestPosition, BestFitness, Iterations, Improvements).

	update_swarm([], _Bounds, _GlobalBest, _InertiaWeight, _CognitiveCoefficient, _SocialCoefficient, []).
	update_swarm([Particle0| Particles0], Bounds, GlobalBest, InertiaWeight, CognitiveCoefficient, SocialCoefficient, [Particle| Particles]) :-
		update_particle(Particle0, Bounds, GlobalBest, InertiaWeight, CognitiveCoefficient, SocialCoefficient, Particle),
		update_swarm(Particles0, Bounds, GlobalBest, InertiaWeight, CognitiveCoefficient, SocialCoefficient, Particles).

	update_particle(particle(Position0, Velocity0, _Fitness0, PersonalBest0, PersonalBestFitness0), Bounds, GlobalBest, InertiaWeight, CognitiveCoefficient, SocialCoefficient,
			particle(Position, Velocity, Fitness, PersonalBest, PersonalBestFitness)) :-
		update_components(Position0, Velocity0, PersonalBest0, GlobalBest, Bounds, InertiaWeight, CognitiveCoefficient, SocialCoefficient, Position, Velocity),
		fitness(Position, Fitness),
		( 	number(Fitness) ->
			true
		;	throw(error(domain_error(fitness, Fitness), run/4))
		),
		( 	Fitness < PersonalBestFitness0 ->
			PersonalBest = Position,
			PersonalBestFitness = Fitness
		;	PersonalBest = PersonalBest0,
			PersonalBestFitness = PersonalBestFitness0
		).

	update_components([], [], [], [], [], _InertiaWeight, _CognitiveCoefficient, _SocialCoefficient, [], []).
	update_components([Value0| Position0], [Velocity0| Velocities0], [PersonalBest| PersonalBests], [GlobalBest| GlobalBests], [Lower-Upper| Bounds], InertiaWeight, CognitiveCoefficient, SocialCoefficient,
			[Value| Position], [Velocity| Velocities]) :-
		random(CognitiveRandom),
		random(SocialRandom),
		RawVelocity is InertiaWeight * Velocity0 + CognitiveCoefficient * CognitiveRandom * (PersonalBest - Value0) + SocialCoefficient * SocialRandom * (GlobalBest - Value0),
		Range is Upper - Lower,
		clamp(RawVelocity, -Range, Range, BoundedVelocity),
		RawValue is Value0 + BoundedVelocity,
		bound_position(RawValue, BoundedVelocity, Lower, Upper, Value, Velocity),
		update_components(Position0, Velocities0, PersonalBests, GlobalBests, Bounds, InertiaWeight, CognitiveCoefficient, SocialCoefficient, Position, Velocities).

	clamp(Value, Lower, _Upper, Lower) :-
		Value < Lower,
		!.
	clamp(Value, _Lower, Upper, Upper) :-
		Value > Upper,
		!.
	clamp(Value, _Lower, _Upper, Value).

	bound_position(Value, _Velocity, Lower, _Upper, Lower, 0.0) :-
		Value < Lower,
		!.
	bound_position(Value, _Velocity, _Lower, Upper, Upper, 0.0) :-
		Value > Upper,
		!.
	bound_position(Value, Velocity, _Lower, _Upper, Value, Velocity).

	swarm_best([particle(_, _, _, PersonalBest, PersonalBestFitness)| Particles], BestPosition, BestFitness) :-
		swarm_best(Particles, PersonalBest, PersonalBestFitness, BestPosition, BestFitness).

	swarm_best([], BestPosition, BestFitness, BestPosition, BestFitness).
	swarm_best([particle(_, _, _, PersonalBest, PersonalBestFitness)| Particles], BestPosition0, BestFitness0, BestPosition, BestFitness) :-
		( 	PersonalBestFitness < BestFitness0 ->
			BestPosition1 = PersonalBest,
			BestFitness1 = PersonalBestFitness
		;	BestPosition1 = BestPosition0,
			BestFitness1 = BestFitness0
		),
		swarm_best(Particles, BestPosition1, BestFitness1, BestPosition, BestFitness).

	report_progress(Iteration, UpdateInterval, Swarm, BestPosition, BestFitness) :-
		UpdateInterval > 0,
		Iteration > 0,
		Iteration mod UpdateInterval =:= 0,
		!,
		call_progress(Iteration, Swarm, BestPosition, BestFitness).
	report_progress(_Iteration, _UpdateInterval, _Swarm, _BestPosition, _BestFitness).

	report_final(Iteration, UpdateInterval, Swarm, BestPosition, BestFitness) :-
		UpdateInterval > 0,
		!,
		call_progress(Iteration, Swarm, BestPosition, BestFitness).
	report_final(_Iteration, _UpdateInterval, _Swarm, _BestPosition, _BestFitness).

	call_progress(Iteration, Swarm, BestPosition, BestFitness) :-
		swarm_metrics(Swarm, MeanFitness, Diversity),
		ignore(progress(Iteration, BestPosition, BestFitness, MeanFitness, Diversity)).

	swarm_metrics(Swarm, MeanFitness, Diversity) :-
		sum_fitnesses(Swarm, 0.0, FitnessSum, 0, Count),
		MeanFitness is FitnessSum / Count,
		Swarm = [particle(FirstPosition, _, _, _, _)| _],
		zero_vector(FirstPosition, Zeros),
		sum_positions(Swarm, Zeros, PositionSums),
		divide_vector(PositionSums, Count, Centroid),
		sum_distances(Swarm, Centroid, 0.0, DistanceSum),
		Diversity is DistanceSum / Count.

	sum_fitnesses([], Sum, Sum, Count, Count).
	sum_fitnesses([particle(_, _, Fitness, _, _)| Particles], Sum0, Sum, Count0, Count) :-
		Sum1 is Sum0 + Fitness,
		Count1 is Count0 + 1,
		sum_fitnesses(Particles, Sum1, Sum, Count1, Count).

	zero_vector([], []).
	zero_vector([_| Values], [0.0| Zeros]) :-
		zero_vector(Values, Zeros).

	sum_positions([], Sums, Sums).
	sum_positions([particle(Position, _, _, _, _)| Particles], Sums0, Sums) :-
		add_vectors(Position, Sums0, Sums1),
		sum_positions(Particles, Sums1, Sums).

	add_vectors([], [], []).
	add_vectors([Value| Values], [Sum0| Sums0], [Sum| Sums]) :-
		Sum is Sum0 + Value,
		add_vectors(Values, Sums0, Sums).

	divide_vector([], _Divisor, []).
	divide_vector([Value| Values], Divisor, [Quotient| Quotients]) :-
		Quotient is Value / Divisor,
		divide_vector(Values, Divisor, Quotients).

	sum_distances([], _Centroid, Sum, Sum).
	sum_distances([particle(Position, _, _, _, _)| Particles], Centroid, Sum0, Sum) :-
		squared_distance(Position, Centroid, 0.0, SquaredDistance),
		Distance is sqrt(SquaredDistance),
		Sum1 is Sum0 + Distance,
		sum_distances(Particles, Centroid, Sum1, Sum).

	squared_distance([], [], Sum, Sum).
	squared_distance([Value| Values], [CentroidValue| CentroidValues], Sum0, Sum) :-
		Difference is Value - CentroidValue,
		Sum1 is Sum0 + Difference * Difference,
		squared_distance(Values, CentroidValues, Sum1, Sum).

	default_option(max_iterations(1000)).
	default_option(inertia_weight(0.7298)).
	default_option(cognitive_coefficient(1.49618)).
	default_option(social_coefficient(1.49618)).
	default_option(updates(0)).

	valid_option(max_iterations(N)) :-
		valid(positive_integer, N).
	valid_option(inertia_weight(Weight)) :-
		valid(non_negative_float, Weight).
	valid_option(cognitive_coefficient(Coefficient)) :-
		valid(non_negative_float, Coefficient).
	valid_option(social_coefficient(Coefficient)) :-
		valid(non_negative_float, Coefficient).
	valid_option(updates(N)) :-
		valid(non_negative_integer, N).
	valid_option(seed(Seed)) :-
		valid(positive_integer, Seed).

:- end_object.


:- object(particle_swarm_optimization(_Problem_),
	extends(particle_swarm_optimization(_Problem_, xoshiro128pp))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-05,
		comment is 'Particle swarm optimization algorithm using the Xoshiro128++ random number generator.',
		parameters is [
			'Problem' - 'Problem object implementing ``particle_swarm_optimization_protocol``.'
		],
		see_also is [particle_swarm_optimization(_, _), particle_swarm_optimization_protocol]
	]).

:- end_object.

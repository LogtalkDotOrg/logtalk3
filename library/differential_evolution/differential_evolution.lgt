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


:- object(differential_evolution(_Problem_, _RandomAlgorithm_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Differential Evolution (DE/rand/1/bin, DE/best/1/bin, and DE/current-to-best/1/bin) metaheuristic for continuous bounded optimization.',
		parameters is [
			'Problem' - 'Problem object implementing ``differential_evolution_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library.'
		],
		remarks is [
			'Strategies' - 'The ``strategy(rand/1/bin)`` (default), ``strategy(best/1/bin)``, and ``strategy(current-to-best/1/bin)`` options select the classic mutation strategies.',
			'Optimization objective' - 'The ``objective(minimize|maximize)`` option selects the fitness ordering.',
			'Boundary handling' - 'Trial vectors that leave the bounds are clamped to the nearest bound.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.'
		],
		see_also is [differential_evolution(_), differential_evolution_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-list(number), -number), one).
	:- info(run/2, [
		comment is 'Runs Differential Evolution with default options and returns the best position and fitness found.',
		argnames is ['BestPosition', 'BestFitness']
	]).

	:- public(run/3).
	:- mode(run(-list(number), -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs Differential Evolution with the given options and returns the best position and fitness found.',
		argnames is ['BestPosition', 'BestFitness', 'Options']
	]).

	:- public(run/4).
	:- mode(run(-list(number), -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs Differential Evolution with the given options and returns the best position, best fitness, and run statistics.',
		argnames is ['BestPosition', 'BestFitness', 'Statistics', 'Options'],
		remarks is [
			'Statistics list' - 'Contains ``generations(N)``, ``evaluations(E)``, ``improvements(I)``, ``final_mean_fitness(M)``, and ``final_diversity(D)``.'
		]
	]).

	:- uses(_Problem_, [
		position_bounds/1, fitness/2, initial_positions/1, stop_condition/3, progress/5
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		random/1, random/3, randomize/1
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(list, [
		length/2, nth1/3
	]).

	%---------------------------------------------------------------------
	% Public entry points
	%---------------------------------------------------------------------

	run(BestPosition, BestFitness) :-
		run(BestPosition, BestFitness, _Statistics, []).

	run(BestPosition, BestFitness, UserOptions) :-
		run(BestPosition, BestFitness, _Statistics, UserOptions).

	run(BestPosition, BestFitness, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	^^option(seed(Seed), Options) ->
			randomize(Seed)
		;	true
		),
		position_bounds(Bounds),
		validate_bounds(Bounds),
		length(Bounds, Dimension),
		^^option(population_size(NP), Options),
		^^option(max_generations(MaxGenerations), Options),
		^^option(crossover_probability(CR), Options),
		^^option(differential_weight(F), Options),
		^^option(strategy(Strategy), Options),
		^^option(objective(Objective), Options),
		^^option(target_fitness(TargetFitness), Options),
		^^option(stagnation_generations(StagnationGenerations), Options),
		^^option(updates(Updates), Options),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxGenerations - 1) // Updates)
		;	UpdateInterval = 0
		),
		initial_population(NP, Dimension, Bounds, Population0),
		evaluate_population(Population0, Objective, Evaluated0, BestPosition0, BestFitness0),
		loop(
			0, MaxGenerations, UpdateInterval, Dimension, Bounds,
			Strategy, CR, F, Objective, TargetFitness, StagnationGenerations, 0,
			Evaluated0, BestPosition0, BestFitness0, 0,
			FinalPopulation, BestPosition, BestFitness, Generations, Improvements
		),
		population_metrics(FinalPopulation, MeanFitness, Diversity),
		Evaluations is NP * (Generations + 1),
		Statistics = [
			generations(Generations),
			evaluations(Evaluations),
			improvements(Improvements),
			final_mean_fitness(MeanFitness),
			final_diversity(Diversity)
		].

	%---------------------------------------------------------------------
	% Main generational loop
	%---------------------------------------------------------------------

	loop(
		Generation, MaxGenerations, UpdateInterval, _Dimension, _Bounds,
		_Strategy, _CR, _F, _Objective, _Target, _Stagnation, _StagCount,
		Population, BestPosition, BestFitness, Improvements,
		Population, BestPosition, BestFitness, Generation, Improvements
	) :-
		Generation >= MaxGenerations,
		!,
		report_final(Generation, UpdateInterval, Population, BestPosition, BestFitness).

	loop(
		Generation, _MaxGenerations, UpdateInterval, _Dimension, _Bounds,
		_Strategy, _CR, _F, Objective, TargetFitness, _Stagnation, _StagCount,
		Population, BestPosition, BestFitness, Improvements,
		Population, BestPosition, BestFitness, Generation, Improvements
	) :-
		target_fitness_reached(Objective, BestFitness, TargetFitness),
		!,
		report_final(Generation, UpdateInterval, Population, BestPosition, BestFitness).

	loop(
		Generation, _MaxGenerations, UpdateInterval, _Dimension, _Bounds,
		_Strategy, _CR, _F, _Objective, _Target, StagnationGenerations, StagCount,
		Population, BestPosition, BestFitness, Improvements,
		Population, BestPosition, BestFitness, Generation, Improvements
	) :-
		StagnationGenerations > 0,
		StagCount >= StagnationGenerations,
		!,
		report_final(Generation, UpdateInterval, Population, BestPosition, BestFitness).

	loop(
		Generation, _MaxGenerations, UpdateInterval, _Dimension, _Bounds,
		_Strategy, _CR, _F, _Objective, _Target, _Stagnation, _StagCount,
		Population, BestPosition, BestFitness, Improvements,
		Population, BestPosition, BestFitness, Generation, Improvements
	) :-
		stop_condition(Generation, BestPosition, BestFitness),
		!,
		report_final(Generation, UpdateInterval, Population, BestPosition, BestFitness).

	loop(
		Generation, MaxGenerations, UpdateInterval, Dimension, Bounds,
		Strategy, CR, F, Objective, TargetFitness, StagnationGenerations, StagCount0,
		Population0, BestPosition0, BestFitness0, Improvements0,
		FinalPopulation, BestPosition, BestFitness, Generations, Improvements
	) :-
		generation_step(Strategy, Population0, Dimension, Bounds, CR, F, BestPosition0, Trials),
		select_population(Population0, Trials, Objective, Population1),
		population_best(Population1, Objective, CandidatePosition, CandidateFitness),
		(	better_fitness(Objective, CandidateFitness, BestFitness0) ->
			BestPosition1 = CandidatePosition,
			BestFitness1 = CandidateFitness,
			Improvements1 is Improvements0 + 1,
			StagCount1 = 0
		;	BestPosition1 = BestPosition0,
			BestFitness1 = BestFitness0,
			Improvements1 = Improvements0,
			StagCount1 is StagCount0 + 1
		),
		Generation1 is Generation + 1,
		report_progress(Generation1, UpdateInterval, Population1, BestPosition1, BestFitness1),
		loop(Generation1, MaxGenerations, UpdateInterval, Dimension, Bounds,
			Strategy, CR, F, Objective, TargetFitness, StagnationGenerations, StagCount1,
			Population1, BestPosition1, BestFitness1, Improvements1,
			FinalPopulation, BestPosition, BestFitness, Generations, Improvements).

	%---------------------------------------------------------------------
	% Generation step – strategy dispatch
	%---------------------------------------------------------------------

	generation_step(rand/1/bin, Population, Dimension, Bounds, CR, F, _Best, Trials) :-
		!,
		length(Population, NP),
		generation_rand(1, NP, Population, Dimension, Bounds, CR, F, Trials).
	generation_step(best/1/bin, Population, Dimension, Bounds, CR, F, Best, Trials) :-
		!,
		length(Population, NP),
		generation_best(1, NP, Population, Dimension, Bounds, CR, F, Best, Trials).
	generation_step(current-to-best/1/bin, Population, Dimension, Bounds, CR, F, Best, Trials) :-
		length(Population, NP),
		generation_current_to_best(1, NP, Population, Dimension, Bounds, CR, F, Best, Trials).

	%----- DE/rand/1/bin --------------------------------------------------

	generation_rand(I, NP, _Population, _Dimension, _Bounds, _CR, _F, []) :-
		I > NP,
		!.
	generation_rand(I, NP, Population, Dimension, Bounds, CR, F, [Trial| Trials]) :-
		nth1(I, Population, Target-_),
		random_distinct_indices_3(NP, I, R1, R2, R3),
		nth1(R1, Population, Xr1-_),
		nth1(R2, Population, Xr2-_),
		nth1(R3, Population, Xr3-_),
		mutate_rand(Xr1, Xr2, Xr3, F, Dimension, Mutant0),
		repair_bounds(Mutant0, Bounds, Mutant),
		random(1, Dimension, Jrand),
		crossover(Target, Mutant, CR, Jrand, Dimension, Trial),
		I1 is I + 1,
		generation_rand(I1, NP, Population, Dimension, Bounds, CR, F, Trials).

	mutate_rand(Xr1, Xr2, Xr3, F, Dimension, Mutant) :-
		mutate_rand_(1, Dimension, Xr1, Xr2, Xr3, F, Mutant).

	mutate_rand_(J, Dimension, _, _, _, _, []) :-
		J > Dimension,
		!.
	mutate_rand_(J, Dimension, Xr1, Xr2, Xr3, F, [V| Vs]) :-
		nth1(J, Xr1, A),
		nth1(J, Xr2, B),
		nth1(J, Xr3, C),
		V is A + F * (B - C),
		J1 is J + 1,
		mutate_rand_(J1, Dimension, Xr1, Xr2, Xr3, F, Vs).

	%----- DE/best/1/bin --------------------------------------------------

	generation_best(I, NP, _Population, _Dimension, _Bounds, _CR, _F, _Best, []) :-
		I > NP,
		!.
	generation_best(I, NP, Population, Dimension, Bounds, CR, F, Best, [Trial| Trials]) :-
		nth1(I, Population, Target-_),
		random_distinct_indices_2(NP, I, R1, R2),
		nth1(R1, Population, Xr1-_),
		nth1(R2, Population, Xr2-_),
		mutate_best(Best, Xr1, Xr2, F, Dimension, Mutant0),
		repair_bounds(Mutant0, Bounds, Mutant),
		random(1, Dimension, Jrand),
		crossover(Target, Mutant, CR, Jrand, Dimension, Trial),
		I1 is I + 1,
		generation_best(I1, NP, Population, Dimension, Bounds, CR, F, Best, Trials).

	mutate_best(Best, Xr1, Xr2, F, Dimension, Mutant) :-
		mutate_best_(1, Dimension, Best, Xr1, Xr2, F, Mutant).

	mutate_best_(J, Dimension, _, _, _, _, []) :-
		J > Dimension,
		!.
	mutate_best_(J, Dimension, Best, Xr1, Xr2, F, [V| Vs]) :-
		nth1(J, Best, B),
		nth1(J, Xr1, A),
		nth1(J, Xr2, C),
		V is B + F * (A - C),
		J1 is J + 1,
		mutate_best_(J1, Dimension, Best, Xr1, Xr2, F, Vs).

	%----- DE/current-to-best/1/bin ---------------------------------------

	generation_current_to_best(I, NP, _Population, _Dimension, _Bounds, _CR, _F, _Best, []) :-
		I > NP,
		!.
	generation_current_to_best(I, NP, Population, Dimension, Bounds, CR, F, Best, [Trial| Trials]) :-
		nth1(I, Population, Target-_),
		random_distinct_indices_2(NP, I, R1, R2),
		nth1(R1, Population, Xr1-_),
		nth1(R2, Population, Xr2-_),
		mutate_current_to_best(Target, Best, Xr1, Xr2, F, Dimension, Mutant0),
		repair_bounds(Mutant0, Bounds, Mutant),
		random(1, Dimension, Jrand),
		crossover(Target, Mutant, CR, Jrand, Dimension, Trial),
		I1 is I + 1,
		generation_current_to_best(I1, NP, Population, Dimension, Bounds, CR, F, Best, Trials).

	% v = x_i + F*(x_best - x_i) + F*(x_r1 - x_r2)
	mutate_current_to_best(Xi, Best, Xr1, Xr2, F, Dimension, Mutant) :-
		mutate_current_to_best_(1, Dimension, Xi, Best, Xr1, Xr2, F, Mutant).

	mutate_current_to_best_(J, Dimension, _, _, _, _, _, []) :-
		J > Dimension,
		!.
	mutate_current_to_best_(J, Dimension, Xi, Best, Xr1, Xr2, F, [V| Vs]) :-
		nth1(J, Xi,   XiJ),
		nth1(J, Best, BestJ),
		nth1(J, Xr1,  R1J),
		nth1(J, Xr2,  R2J),
		V is XiJ + F*(BestJ - XiJ) + F*(R1J - R2J),
		J1 is J + 1,
		mutate_current_to_best_(J1, Dimension, Xi, Best, Xr1, Xr2, F, Vs).

	%---------------------------------------------------------------------
	% Binary crossover
	%---------------------------------------------------------------------

	crossover(Target, Mutant, CR, Jrand, Dimension, Trial) :-
		crossover_(1, Dimension, Target, Mutant, CR, Jrand, Trial).

	crossover_(J, Dimension, _, _, _, _, []) :-
		J > Dimension,
		!.
	crossover_(J, Dimension, Target, Mutant, CR, Jrand, [Uj| Ujs]) :-
		nth1(J, Target, Xj),
		nth1(J, Mutant, Vj),
		random(R),
		(	(R < CR ; J =:= Jrand) ->
			Uj = Vj
		;	Uj = Xj
		),
		J1 is J + 1,
		crossover_(J1, Dimension, Target, Mutant, CR, Jrand, Ujs).

	%---------------------------------------------------------------------
	% Selection (one-to-one)
	%---------------------------------------------------------------------

	select_population([], [], _Objective, []).
	select_population([Target-TargetFit| Targets], [Trial| Trials], Objective, [Selected| Selecteds]) :-
		fitness(Trial, TrialFit),
		(	number(TrialFit) ->
			true
		;	domain_error(fitness, TrialFit)
		),
		(	better_fitness(Objective, TrialFit, TargetFit) ->
			Selected = Trial-TrialFit
		;	Selected = Target-TargetFit
		),
		select_population(Targets, Trials, Objective, Selecteds).

	%---------------------------------------------------------------------
	% Population helpers
	%---------------------------------------------------------------------

	initial_population(NP, Dimension, Bounds, Population) :-
		(	initial_positions(Positions) ->
			(	valid_positions(Positions, Bounds, NP) ->
				Population = Positions
			;	domain_error(initial_positions, Positions)
			)
		;	random_population(NP, Dimension, Bounds, Population)
		).

	random_population(0, _, _, []) :-
		!.
	random_population(N, Dimension, Bounds, [Individual| Individuals]) :-
		random_individual(Dimension, Bounds, Individual),
		N1 is N - 1,
		random_population(N1, Dimension, Bounds, Individuals).

	random_individual(Dimension, Bounds, Individual) :-
		random_individual_(1, Dimension, Bounds, Individual).

	random_individual_(J, Dimension, _, []) :-
		J > Dimension,
		!.
	random_individual_(J, Dimension, Bounds, [X| Xs]) :-
		nth1(J, Bounds, Low-High),
		random(Low, High, X),
		J1 is J + 1,
		random_individual_(J1, Dimension, Bounds, Xs).

	evaluate_population([Individual| Individuals], Objective, [Individual-Fitness| Rest], BestPos, BestFit) :-
		fitness(Individual, Fitness),
		(	number(Fitness) ->
			true
		;	domain_error(fitness, Fitness)
		),
		evaluate_remaining(Individuals, Objective, Rest, Individual, Fitness, BestPos, BestFit).

	evaluate_remaining([], _Objective, [], BestPos, BestFit, BestPos, BestFit).
	evaluate_remaining([Individual| Individuals], Objective, [Individual-Fitness| Rest], BestPos0, BestFit0, BestPos, BestFit) :-
		fitness(Individual, Fitness),
		(	number(Fitness) ->
			true
		;	domain_error(fitness, Fitness)
		),
		(	better_fitness(Objective, Fitness, BestFit0) ->
			BestPos1 = Individual, BestFit1 = Fitness
		;	BestPos1 = BestPos0, BestFit1 = BestFit0
		),
		evaluate_remaining(Individuals, Objective, Rest, BestPos1, BestFit1, BestPos, BestFit).

	population_best([Individual-Fitness| Rest], Objective, BestPos, BestFit) :-
		population_best_(Rest, Objective, Individual, Fitness, BestPos, BestFit).

	population_best_([], _Objective, BestPos, BestFit, BestPos, BestFit).
	population_best_([Individual-Fitness| Rest], Objective, BestPos0, BestFit0, BestPos, BestFit) :-
		(	better_fitness(Objective, Fitness, BestFit0) ->
			population_best_(Rest, Objective, Individual, Fitness, BestPos, BestFit)
		;	population_best_(Rest, Objective, BestPos0, BestFit0, BestPos, BestFit)
		).

	%---------------------------------------------------------------------
	% Utilities
	%---------------------------------------------------------------------

	repair_bounds([], [], []).
	repair_bounds([X| Xs], [Low-High| Bounds], [Y| Ys]) :-
		(	X < Low -> Y = Low
		;	X > High -> Y = High
		;	Y = X
		),
		repair_bounds(Xs, Bounds, Ys).

	random_distinct_indices_3(NP, Forbidden, R1, R2, R3) :-
		repeat,
			between(1, NP, R1), R1 =\= Forbidden,
			between(1, NP, R2), R2 =\= Forbidden, R2 =\= R1,
			between(1, NP, R3), R3 =\= Forbidden, R3 =\= R1, R3 =\= R2,
		!.

	random_distinct_indices_2(NP, Forbidden, R1, R2) :-
		repeat,
			between(1, NP, R1), R1 =\= Forbidden,
			between(1, NP, R2), R2 =\= Forbidden, R2 =\= R1,
		!.

	better_fitness(minimize, Fitness, Reference) :-
		Fitness < Reference.
	better_fitness(maximize, Fitness, Reference) :-
		Fitness > Reference.

	target_fitness_reached(minimize, Fitness, Target) :-
		number(Target), Fitness =< Target.
	target_fitness_reached(maximize, Fitness, Target) :-
		number(Target), Fitness >= Target.

	validate_bounds([]) :-
		!.
	validate_bounds([Low-High| Bounds]) :-
		number(Low), number(High), Low =< High,
		!,
		validate_bounds(Bounds).
	validate_bounds(Bounds) :-
		domain_error(position_bounds, Bounds).

	valid_positions(Positions, Bounds, NP) :-
		length(Positions, NP),
		valid_positions_(Positions, Bounds).

	valid_positions_([], _).
	valid_positions_([Position| Positions], Bounds) :-
		valid_position(Position, Bounds),
		valid_positions_(Positions, Bounds).

	valid_position([], []).
	valid_position([V| Vs], [Low-High| Bounds]) :-
		number(V), V >= Low, V =< High,
		valid_position(Vs, Bounds).

	%----- Metrics & progress ---------------------------------------------

	population_metrics(Population, MeanFitness, Diversity) :-
		sum_fitnesses(Population, 0.0, FitSum, 0, Count),
		MeanFitness is FitSum / Count,
		Population = [First-_| _],
		zero_vector(First, Zeros),
		sum_positions(Population, Zeros, Sums),
		divide_vector(Sums, Count, Centroid),
		sum_distances(Population, Centroid, 0.0, DistSum),
		Diversity is DistSum / Count.

	sum_fitnesses([], S, S, C, C).
	sum_fitnesses([_-F| Ps], S0, S, C0, C) :-
		S1 is S0 + F,
		C1 is C0 + 1,
		sum_fitnesses(Ps, S1, S, C1, C).

	zero_vector([], []).
	zero_vector([_| Vs], [0.0| Zs]) :-
		zero_vector(Vs, Zs).

	sum_positions([], Sums, Sums).
	sum_positions([Position-_| Positions], Sums0, Sums) :-
		add_vectors(Position, Sums0, Sums1),
		sum_positions(Positions, Sums1, Sums).

	add_vectors([], [], []).
	add_vectors([A| As], [B| Bs], [C| Cs]) :-
		C is A + B,
		add_vectors(As, Bs, Cs).

	divide_vector([], _, []).
	divide_vector([V| Vs], D, [Q| Qs]) :-
		Q is V / D,
		divide_vector(Vs, D, Qs).

	sum_distances([], _, Sum, Sum).
	sum_distances([Position-_| Positions], Centroid, Sum0, Sum) :-
		squared_distance(Position, Centroid, 0.0, SquaredDistance),
		Distance is sqrt(SquaredDistance),
		Sum1 is Sum0 + Distance,
		sum_distances(Positions, Centroid, Sum1, Sum).

	squared_distance([], [], SquaredDistance, SquaredDistance).
	squared_distance([A| As], [B| Bs], SquaredDistance0, SquaredDistance) :-
		Distance is A - B,
		SquaredDistance1 is SquaredDistance0 + Distance*Distance,
		squared_distance(As, Bs, SquaredDistance1, SquaredDistance).

	report_progress(Generation, UpdateInterval, Population, BestPos, BestFit) :-
		UpdateInterval > 0,
		Generation > 0,
		Generation mod UpdateInterval =:= 0,
		!,
		call_progress(Generation, Population, BestPos, BestFit).
	report_progress(_, _, _, _, _).

	report_final(Generation, UpdateInterval, Population, BestPos, BestFit) :-
		UpdateInterval > 0,
		!,
		call_progress(Generation, Population, BestPos, BestFit).
	report_final(_, _, _, _, _).

	call_progress(Generation, Population, BestPos, BestFit) :-
		population_metrics(Population, MeanFitness, Diversity),
		ignore(progress(Generation, BestPos, BestFit, MeanFitness, Diversity)).

	%---------------------------------------------------------------------
	% Options
	%---------------------------------------------------------------------

	default_option(strategy(rand/1/bin)).
	default_option(objective(minimize)).
	default_option(target_fitness(none)).
	default_option(population_size(30)).
	default_option(max_generations(100)).
	default_option(crossover_probability(0.9)).
	default_option(differential_weight(0.8)).
	default_option(stagnation_generations(0)).
	default_option(updates(0)).

	valid_option(strategy(Strategy)) :-
		once((Strategy == rand/1/bin; Strategy == best/1/bin; Strategy == current-to-best/1/bin)).
	valid_option(objective(Objective)) :-
		once((Objective == minimize; Objective == maximize)).
	valid_option(target_fitness(Target)) :-
		once((Target == none ; number(Target))).
	valid_option(population_size(N)) :-
		valid(positive_integer, N), N >= 4.		% need at least 4 for rand/1/bin
	valid_option(max_generations(N)) :-
		valid(positive_integer, N).
	valid_option(crossover_probability(CR)) :-
		number(CR), CR >= 0.0, CR =< 1.0.
	valid_option(differential_weight(F)) :-
		number(F), F > 0.0.
	valid_option(stagnation_generations(N)) :-
		valid(non_negative_integer, N).
	valid_option(updates(N)) :-
		valid(non_negative_integer, N).
	valid_option(seed(Seed)) :-
		valid(positive_integer, Seed).

:- end_object.


:- object(differential_evolution(_Problem_),
	extends(differential_evolution(_Problem_, xoshiro128pp))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-13,
		comment is 'Differential Evolution using the Xoshiro128++ random number generator.',
		parameters is [
			'Problem' - 'Problem object implementing ``differential_evolution_protocol``.'
		],
		see_also is [differential_evolution(_, _), differential_evolution_problem_protocol]
	]).

:- end_object.

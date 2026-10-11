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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Differential Evolution (DE/rand/1/bin, DE/rand/1/exp, DE/best/1/bin, and DE/current-to-best/1/bin) metaheuristic for continuous bounded optimization.',
		parameters is [
			'Problem' - 'Problem object implementing ``differential_evolution_problem_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library.'
		],
		remarks is [
			'Strategies' - 'The ``strategy(rand/1/bin)`` (default), ``strategy(rand/1/exp)``, ``strategy(best/1/bin)``, and ``strategy(current-to-best/1/bin)`` options select the classic mutation strategies.',
			'Optimization objective' - 'The ``objective(minimize|maximize)`` option selects the fitness ordering.',
			'Boundary handling' - 'Trial vectors that leave the bounds are clamped to the nearest bound.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.'
		],
		see_also is [differential_evolution(_), differential_evolution_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-list(number), -number), one_or_error).
	:- info(run/2, [
		comment is 'Runs Differential Evolution with default options and returns the best position and fitness found.',
		argnames is ['BestPosition', 'BestFitness'],
		exceptions is [
			'``Bounds`` returned by ``Problem::position_bounds/1`` is a variable' - instantiation_error,
			'``Positions`` returned by ``Problem::initial_positions/1`` is a variable' - instantiation_error,
			'``Fitness`` returned by ``Problem::fitness/2`` is a variable' - instantiation_error,
			'``Bounds`` returned by ``Problem::position_bounds/1`` is not a valid list of position bounds' - domain_error(position_bounds, 'Bounds'),
			'``Positions`` returned by ``Problem::initial_positions/1`` is not a valid initial population' - domain_error(initial_positions, 'Positions'),
			'``Fitness`` returned by ``Problem::fitness/2`` is not a number' - domain_error(fitness, 'Fitness')
		]
	]).

	:- public(run/3).
	:- mode(run(-list(number), -number, +list(compound)), one_or_error).
	:- info(run/3, [
		comment is 'Runs Differential Evolution with the given options and returns the best position and fitness found.',
		argnames is ['BestPosition', 'BestFitness', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'``Bounds`` returned by ``Problem::position_bounds/1`` is a variable' - instantiation_error,
			'``Positions`` returned by ``Problem::initial_positions/1`` is a variable' - instantiation_error,
			'``Fitness`` returned by ``Problem::fitness/2`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Bounds`` returned by ``Problem::position_bounds/1`` is not a valid list of position bounds' - domain_error(position_bounds, 'Bounds'),
			'``Positions`` returned by ``Problem::initial_positions/1`` is not a valid initial population' - domain_error(initial_positions, 'Positions'),
			'``Fitness`` returned by ``Problem::fitness/2`` is not a number' - domain_error(fitness, 'Fitness')
		]
	]).

	:- public(run/4).
	:- mode(run(-list(number), -number, -list(compound), +list(compound)), one_or_error).
	:- info(run/4, [
		comment is 'Runs Differential Evolution with the given options and returns the best position, best fitness, and run statistics.',
		argnames is ['BestPosition', 'BestFitness', 'Statistics', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'``Bounds`` returned by ``Problem::position_bounds/1`` is a variable' - instantiation_error,
			'``Positions`` returned by ``Problem::initial_positions/1`` is a variable' - instantiation_error,
			'``Fitness`` returned by ``Problem::fitness/2`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Bounds`` returned by ``Problem::position_bounds/1`` is not a valid list of position bounds' - domain_error(position_bounds, 'Bounds'),
			'``Positions`` returned by ``Problem::initial_positions/1`` is not a valid initial population' - domain_error(initial_positions, 'Positions'),
			'``Fitness`` returned by ``Problem::fitness/2`` is not a number' - domain_error(fitness, 'Fitness')
		],
		remarks is [
			'Statistics list' - 'Contains ``generations(N)``, ``evaluations(E)``, ``improvements(I)``, ``final_mean_fitness(M)``, and ``final_diversity(D)``.'
		]
	]).

	:- uses(_Problem_, [
		position_bounds/1, fitness/2, initial_positions/1, stop_condition/3, progress/5
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		random/1, randomize/1, between/3
	]).

	:- uses(linear_algebra, [
		add_vectors/3, new_vector_like/2
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(numberlist, [
		rescale/3
	]).

	:- uses(type, [
		valid/2
	]).

	% public entry points

	run(BestPosition, BestFitness) :-
		run_engine(false, BestPosition, BestFitness, _Statistics, []).

	run(BestPosition, BestFitness, UserOptions) :-
		run_engine(false, BestPosition, BestFitness, _Statistics, UserOptions).

	run(BestPosition, BestFitness, Statistics, UserOptions) :-
		run_engine(true, BestPosition, BestFitness, Statistics, UserOptions).

	run_engine(StatisticsRequired, BestPosition, BestFitness, Statistics, UserOptions) :-
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
		ReportCount is min(Updates, MaxGenerations),
		UpdateInterval = reports(MaxGenerations, ReportCount, StatisticsRequired, FinalMetrics),
		initial_population(NP, Dimension, Bounds, Population0),
		evaluate_population(Population0, Objective, Evaluated0, BestPosition0, BestFitness0),
		loop(
			0, MaxGenerations, UpdateInterval, size(NP, Dimension), Bounds,
			Strategy, CR, F, Objective, TargetFitness, StagnationGenerations, 0,
			Evaluated0, BestPosition0, BestFitness0, 0,
			_FinalPopulation, BestPosition, BestFitness, Generations, Improvements
		),
		(	StatisticsRequired == true ->
			FinalMetrics = metrics(MeanFitness, Diversity),
			Evaluations is NP * (Generations + 1),
			Statistics = [
				generations(Generations),
				evaluations(Evaluations),
				improvements(Improvements),
				final_mean_fitness(MeanFitness),
				final_diversity(Diversity)
			]
		;	Statistics = []
		).

	% main generational loop

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
		report_progress(Generation, UpdateInterval, Population0, BestPosition0, BestFitness0),
		generation_step(Strategy, Population0, Dimension, Bounds, CR, F, BestPosition0, Trials),
		select_population(Population0, Trials, Objective, Population1, CandidatePosition, CandidateFitness),
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
		loop(Generation1, MaxGenerations, UpdateInterval, Dimension, Bounds,
			Strategy, CR, F, Objective, TargetFitness, StagnationGenerations, StagCount1,
			Population1, BestPosition1, BestFitness1, Improvements1,
			FinalPopulation, BestPosition, BestFitness, Generations, Improvements).

	% generation step - strategy dispatch

	generation_step(Strategy, Population, size(NP, Dimension), Bounds, CR, F, Best, Trials) :-
		population_index(Population, Index),
		generate_trials(Strategy, Population, NP, Index, Dimension, Bounds, CR, F, Best, Trials).

	generate_trials(rand/1/bin, Population, NP, Index, Dimension, Bounds, CR, F, _Best, Trials) :-
		!,
		generation_rand(1, NP, Population, Index, Dimension, Bounds, CR, F, bin, Trials).
	generate_trials(rand/1/exp, Population, NP, Index, Dimension, Bounds, CR, F, _Best, Trials) :-
		!,
		generation_rand(1, NP, Population, Index, Dimension, Bounds, CR, F, exp, Trials).
	generate_trials(best/1/bin, Population, NP, Index, Dimension, Bounds, CR, F, Best, Trials) :-
		!,
		generation_best(1, NP, Population, Index, Dimension, Bounds, CR, F, Best, Trials).
	generate_trials(current-to-best/1/bin, Population, NP, Index, Dimension, Bounds, CR, F, Best, Trials) :-
		generation_current_to_best(1, NP, Population, Index, Dimension, Bounds, CR, F, Best, Trials).

	% DE/rand/1/bin and DE/rand/1/exp

	generation_rand(_, _, [], _, _, _, _, _, _, []) :-
		!.
	generation_rand(I, NP, [Target-_| Population], Index, Dimension, Bounds, CR, F, Xover, [Trial| Trials]) :-
		random_distinct_indices_3(NP, I, R1, R2, R3),
		indexed_position(Index, R1, Xr1),
		indexed_position(Index, R2, Xr2),
		indexed_position(Index, R3, Xr3),
		rand_trial(Xover, Target, Xr1, Xr2, Xr3, Bounds, CR, F, Dimension, Trial),
		I1 is I + 1,
		generation_rand(I1, NP, Population, Index, Dimension, Bounds, CR, F, Xover, Trials).

	rand_trial(bin, Target, Xr1, Xr2, Xr3, Bounds, CR, F, Dimension, Trial) :-
		!,
		between(1, Dimension, Forced),
		rand_bin(1, Target, Xr1, Xr2, Xr3, Bounds, CR, F, Forced, Trial).
	rand_trial(exp, Target, Xr1, Xr2, Xr3, Bounds, CR, F, Dimension, Trial) :-
		mutate_rand(Xr1, Xr2, Xr3, F, Bounds, Mutant),
		crossover(exp, Target, Mutant, CR, Dimension, Trial).

	mutate_rand([], [], [], _, [], []) :-
		!.
	mutate_rand([A| As], [B| Bs], [C| Cs], F, [Low-High| Bounds], [V| Vs]) :-
		Value is A + F * (B - C),
		clamp_coordinate(Value, Low, High, V),
		mutate_rand(As, Bs, Cs, F, Bounds, Vs).

	rand_bin(_, [], [], [], [], [], _, _, _, []) :-
		!.
	rand_bin(J, [X| Target], [A| As], [B| Bs], [C| Cs], [Low-High| Bounds], CR, F, Forced, [U| Trial]) :-
		Value is A + F * (B - C),
		clamp_coordinate(Value, Low, High, V),
		choose_coordinate(J, X, V, CR, Forced, U),
		J1 is J + 1,
		rand_bin(J1, Target, As, Bs, Cs, Bounds, CR, F, Forced, Trial).

	% DE/best/1/bin

	generation_best(_, _, [], _, _, _, _, _, _, []) :-
		!.
	generation_best(I, NP, [Target-_| Population], Index, Dimension, Bounds, CR, F, Best, [Trial| Trials]) :-
		random_distinct_indices_2(NP, I, R1, R2),
		indexed_position(Index, R1, Xr1),
		indexed_position(Index, R2, Xr2),
		between(1, Dimension, Forced),
		best_bin(1, Target, Best, Xr1, Xr2, Bounds, CR, F, Forced, Trial),
		I1 is I + 1,
		generation_best(I1, NP, Population, Index, Dimension, Bounds, CR, F, Best, Trials).

	best_bin(_, [], [], [], [], [], _, _, _, []) :-
		!.
	best_bin(J, [X| Target], [B| Best], [A| As], [C| Cs], [Low-High| Bounds], CR, F, Forced, [U| Trial]) :-
		Value is B + F * (A - C),
		clamp_coordinate(Value, Low, High, V),
		choose_coordinate(J, X, V, CR, Forced, U),
		J1 is J + 1,
		best_bin(J1, Target, Best, As, Cs, Bounds, CR, F, Forced, Trial).

	% DE/current-to-best/1/bin

	generation_current_to_best(_, _, [], _, _, _, _, _, _, []) :-
		!.
	generation_current_to_best(I, NP, [Target-_| Population], Index, Dimension, Bounds, CR, F, Best, [Trial| Trials]) :-
		random_distinct_indices_2(NP, I, R1, R2),
		indexed_position(Index, R1, Xr1),
		indexed_position(Index, R2, Xr2),
		between(1, Dimension, Forced),
		current_bin(1, Target, Best, Xr1, Xr2, Bounds, CR, F, Forced, Trial),
		I1 is I + 1,
		generation_current_to_best(I1, NP, Population, Index, Dimension, Bounds, CR, F, Best, Trials).

	% v = x_i + F*(x_best - x_i) + F*(x_r1 - x_r2)
	current_bin(_, [], [], [], [], [], _, _, _, []) :-
		!.
	current_bin(J, [X| Target], [B| Best], [A| As], [C| Cs], [Low-High| Bounds], CR, F, Forced, [U| Trial]) :-
		Value is X + F*(B - X) + F*(A - C),
		clamp_coordinate(Value, Low, High, V),
		choose_coordinate(J, X, V, CR, Forced, U),
		J1 is J + 1,
		current_bin(J1, Target, Best, As, Cs, Bounds, CR, F, Forced, Trial).

	% Crossover (binomial and exponential)

	crossover(exp, Target, Mutant, CR, Dimension, Trial) :-
		between(1, Dimension, Jstart),
		crossover_length(1, Dimension, CR, Length),
		crossover_segment(1, Target, Mutant, Jstart, Length, Dimension, Trial).

	% exponential
	%
	% starting at Jstart, take consecutive components from the mutant
	% while rand < CR (and not a full cycle); at least one component is taken

	crossover_length(Length, Dimension, _, Length) :-
		Length >= Dimension,
		!.
	crossover_length(Length0, Dimension, CR, Length) :-
		random(R),
		(	R < CR ->
			Length1 is Length0 + 1,
			crossover_length(Length1, Dimension, CR, Length)
		;	Length = Length0
		).

	crossover_segment(_, [], [], _, _, _, []) :-
		!.
	crossover_segment(J, [X| Target], [V| Mutant], Start, Length, Dimension, [U| Trial]) :-
		Offset is (J - Start + Dimension) mod Dimension,
		(	Offset < Length ->
			U = V
		;	U = X
		),
		J1 is J + 1,
		crossover_segment(J1, Target, Mutant, Start, Length, Dimension, Trial).

	% selection (one-to-one)

	select_population([Target| Targets], [Trial| Trials], Objective, [Selected| Selecteds], BestPosition, BestFitness) :-
		select_individual(Target, Trial, Objective, Selected),
		Selected = Position-Fitness,
		select_remaining(Targets, Trials, Objective, Selecteds, Position, Fitness, BestPosition, BestFitness).

	select_remaining([], [], _, [], BestPosition, BestFitness, BestPosition, BestFitness) :-
		!.
	select_remaining([Target| Targets], [Trial| Trials], Objective, [Selected| Selecteds], BestPosition0, BestFitness0, BestPosition, BestFitness) :-
		select_individual(Target, Trial, Objective, Selected),
		Selected = Position-Fitness,
		(	better_fitness(Objective, Fitness, BestFitness0) ->
			BestPosition1 = Position,
			BestFitness1 = Fitness
		;	BestPosition1 = BestPosition0,
			BestFitness1 = BestFitness0
		),
		select_remaining(Targets, Trials, Objective, Selecteds, BestPosition1, BestFitness1, BestPosition, BestFitness).

	select_individual(Target-TargetFit, Trial, Objective, Selected) :-
		fitness(Trial, TrialFit),
		(	number(TrialFit) ->
			true
		;	domain_error(fitness, TrialFit)
		),
		(	acceptable_fitness(Objective, TrialFit, TargetFit) ->
			Selected = Trial-TrialFit
		;	Selected = Target-TargetFit
		).

	% population predicates

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

	random_individual(_Dimension, Bounds, Individual) :-
		random_coordinates(Bounds, Individual).

	random_coordinates([], []).
	random_coordinates([Low-High| Bounds], [X| Xs]) :-
		random(R),
		Value is Low + R * (High - Low),
		X is max(Low, min(High, Value)),
		random_coordinates(Bounds, Xs).

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

	% auxiliary predicates

	population_index(Population, index(Span, Root)) :-
		index_level(Population, 1, Span, Root).

	index_level(Entries, Span0, Span, Root) :-
		index_nodes(Entries, Nodes),
		(	Nodes = [Root] ->
			Span = Span0
		;	Span1 is Span0 * 16,
			index_level(Nodes, Span1, Span, Root)
		).

	index_nodes([], []) :-
		!.
	index_nodes(Entries, [Node| Nodes]) :-
		index_chunk(16, Entries, Chunk, Rest),
		Node =.. [items| Chunk],
		index_nodes(Rest, Nodes).

	index_chunk(0, Entries, [], Entries) :-
		!.
	index_chunk(_, [], [], []) :-
		!.
	index_chunk(Count, [Entry| Entries], [Entry| Chunk], Rest) :-
		Count1 is Count - 1,
		index_chunk(Count1, Entries, Chunk, Rest).

	indexed_position(index(1, Node), I, Position) :-
		!,
		arg(I, Node, Position-_).
	indexed_position(index(Span, Node), I, Position) :-
		Slot is (I - 1) // Span + 1,
		Offset is (I - 1) mod Span + 1,
		arg(Slot, Node, Child),
		ChildSpan is Span // 16,
		indexed_position(index(ChildSpan, Child), Offset, Position).

	clamp_coordinate(Value, Low, High, Clamped) :-
		(	Value < Low ->
			Clamped = Low
		;	Value > High ->
			Clamped = High
		;	Clamped = Value
		).

	choose_coordinate(J, Target, Mutant, CR, Forced, Value) :-
		random(R),
		(	(R < CR; J =:= Forced) ->
			Value = Mutant
		;	Value = Target
		).

	random_distinct_indices_3(NP, Forbidden, R1, R2, R3) :-
		repeat,
			between(1, NP, R1),
			R1 =\= Forbidden,
			between(1, NP, R2),
			R2 =\= Forbidden,
			R2 =\= R1,
			between(1, NP, R3),
			R3 =\= Forbidden,
			R3 =\= R1,
			R3 =\= R2,
		!.

	random_distinct_indices_2(NP, Forbidden, R1, R2) :-
		repeat,
			between(1, NP, R1),
			R1 =\= Forbidden,
			between(1, NP, R2),
			R2 =\= Forbidden,
			R2 =\= R1,
		!.

	better_fitness(minimize, Fitness, Reference) :-
		Fitness < Reference.
	better_fitness(maximize, Fitness, Reference) :-
		Fitness > Reference.

	acceptable_fitness(minimize, Fitness, Reference) :-
		Fitness =< Reference.
	acceptable_fitness(maximize, Fitness, Reference) :-
		Fitness >= Reference.

	target_fitness_reached(minimize, Fitness, Target) :-
		number(Target), Fitness =< Target.
	target_fitness_reached(maximize, Fitness, Target) :-
		number(Target), Fitness >= Target.

	validate_bounds(Bounds) :-
		(	ground(Bounds),
			Bounds = [_| _],
			valid_bounds(Bounds) ->
			true
		;	domain_error(position_bounds, Bounds)
		).

	valid_bounds([]).
	valid_bounds([Low-High| Bounds]) :-
		number(Low),
		number(High),
		Low =< High,
		valid_bounds(Bounds).

	valid_positions(Positions, Bounds, NP) :-
		ground(Positions),
		valid(list, Positions),
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

	% metrics and progress

	population_metrics(Population, MeanFitness, Diversity) :-
		Population = [First-_| _],
		new_vector_like(First, Zeros),
		sum_population(Population, Zeros, 0.0, 0, Sums, FitSum, Count),
		MeanFitness is FitSum / Count,
		Scale is 1.0 / Count,
		rescale(Sums, Scale, Centroid),
		sum_distances(Population, Centroid, 0.0, DistSum),
		Diversity is DistSum / Count.

	sum_population([], Sums, FitSum, Count, Sums, FitSum, Count) :-
		!.
	sum_population([Position-Fitness| Positions], Sums0, FitSum0, Count0, Sums, FitSum, Count) :-
		add_vectors(Position, Sums0, Sums1),
		FitSum1 is FitSum0 + Fitness,
		Count1 is Count0 + 1,
		sum_population(Positions, Sums1, FitSum1, Count1, Sums, FitSum, Count).

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

	report_progress(Generation, reports(MaxGenerations, Count, _, _), Population, BestPos, BestFit) :-
		Count > 0,
		Generation > 0,
		Generation < MaxGenerations,
		Generation * Count // MaxGenerations > (Generation - 1) * Count // MaxGenerations,
		!,
		call_progress(Generation, Population, BestPos, BestFit).
	report_progress(_, _, _, _, _).

	report_final(Generation, reports(_, Count, StatisticsRequired, Metrics), Population, BestPos, BestFit) :-
		(	(Count > 0; StatisticsRequired == true) ->
			population_metrics(Population, MeanFitness, Diversity),
			Metrics = metrics(MeanFitness, Diversity),
			(	Count > 0 ->
				ignore(progress(Generation, BestPos, BestFit, MeanFitness, Diversity))
			;	true
			)
		;	Metrics = none
		).

	call_progress(Generation, Population, BestPos, BestFit) :-
		population_metrics(Population, MeanFitness, Diversity),
		ignore(progress(Generation, BestPos, BestFit, MeanFitness, Diversity)).

	% options

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
		once((	Strategy == rand/1/bin
			;	Strategy == rand/1/exp
			;	Strategy == best/1/bin
			;	Strategy == current-to-best/1/bin
		)).
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
			'Problem' - 'Problem object implementing ``differential_evolution_problem_protocol``.'
		],
		see_also is [differential_evolution(_, _), differential_evolution_problem_protocol]
	]).

:- end_object.

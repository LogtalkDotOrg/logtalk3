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


:- object(genetic_algorithm(_Problem_, _RandomAlgorithm_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Genetic algorithm meta-heuristic optimizer. Parameterized by a problem object implementing the ``genetic_algorithm_problem_protocol`` protocol and by a random number generator algorithm for the ``fast_random`` library. The algorithm minimizes the energy (cost) function defined by the problem by default; maximization is supported via options. Custom stop conditions, diversity measures, progress reporting, and selection pressure can be defined by the problem object or configured via options; suitable defaults are used otherwise.',
		parameters is [
			'Problem' - 'Problem object implementing ``genetic_algorithm_problem_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library (e.g. ``xoshiro128pp``, ``xoshiro256ss``, ``well512a``, ...).'
		],
		remarks is [
			'Algorithm' - 'A generational genetic algorithm that maintains a population of individuals. Each generation applies selection, crossover, and mutation to produce the next population. Optional elitism preserves the best individuals across generations.',
			'Selection' - 'Parent selection is controlled by the ``selection/1`` option. Supported schemes are ``tournament(K)`` (default ``tournament(3)``), ``roulette``, and ``rank``. Tournament samples K individuals and keeps the best; roulette selects proportionally to fitness derived from energy; rank selects proportionally to rank after sorting by objective.',
			'Crossover and mutation' - 'Both operators are defined by the problem object. Crossover is applied with probability ``crossover_rate``; mutation is applied independently to each offspring with probability ``mutation_rate``. Rates may be held constant or adapted each generation via ``crossover_schedule/1`` and ``mutation_schedule/1`` options, or via optional problem hooks ``crossover_rate/4`` and ``mutation_rate/4`` (hooks take precedence over schedules).',
			'Elitism' - 'When ``elite_size(N)`` is greater than zero, the best N individuals of the current population (clamped to the population size) are copied unchanged into the next generation. This preserves the best solutions found so far against disruption by crossover and mutation. Set ``elite_size(0)`` to disable elitism.',
			'Best individual tracking' - 'The algorithm tracks the best individual found across all generations, not just the final population.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.',
			'Progress reporting' - 'If the problem object defines ``progress/5``, it is called periodically with the current generation, best individual, best energy, mean population energy, and diversity. A final report is always produced when the loop terminates when updates are enabled.'
		],
		see_also is [genetic_algorithm(_), genetic_algorithm_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-nonvar, -number), one).
	:- info(run/2, [
		comment is 'Runs the genetic algorithm using default options and returns the best individual found and its energy.',
		argnames is ['BestIndividual', 'BestEnergy']
	]).

	:- public(run/3).
	:- mode(run(-nonvar, -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the genetic algorithm using the given options and returns the best individual found and its energy.',
		argnames is ['BestIndividual', 'BestEnergy', 'Options'],
		remarks is [
			'``max_generations(N)`` option' - 'Maximum number of generations (default: ``200``).',
			'``population_size(N)`` option' - 'Number of individuals in the population; must be at least 2 (default: ``50``).',
			'``crossover_rate(P)`` option' - 'Initial probability of applying crossover to a selected pair of parents (default: ``0.8``). Used as the constant rate when the schedule is ``constant``, and as the starting value for adaptive schedules.',
			'``mutation_rate(P)`` option' - 'Initial probability of mutating each offspring (default: ``0.1``). Used as the constant rate when the schedule is ``constant``, and as the starting value for adaptive schedules.',
			'``crossover_schedule(Schedule)`` option' - 'How the crossover rate evolves across generations: ``constant`` (default), ``linear(Initial, Final)`` interpolates from ``Initial`` to ``Final``, or ``geometric(Factor)`` multiplies the rate by ``Factor`` each generation (clamped to ``[0.0, 1.0]``). Overridden when the problem defines ``crossover_rate/4``.',
			'``mutation_schedule(Schedule)`` option' - 'How the mutation rate evolves across generations: ``constant`` (default), ``linear(Initial, Final)`` interpolates from ``Initial`` to ``Final``, or ``geometric(Factor)`` multiplies the rate by ``Factor`` each generation (clamped to ``[0.0, 1.0]``). Overridden when the problem defines ``mutation_rate/4``.',
			'``selection(Scheme)`` option' - 'Parent selection scheme: ``tournament(K)`` with positive integer K (default: ``tournament(3)``), ``roulette``, or ``rank``.',
			'``elite_size(N)`` option' - 'Number of best individuals preserved unchanged into the next generation (default: ``1``). Set to ``0`` to disable elitism.',
			'``objective(Direction)`` option' - 'Optimization direction: ``minimize`` (default) or ``maximize``.',
			'``updates(N)`` option' - 'Number of progress reports during the run. Set to ``0`` to disable. Progress is reported by calling ``progress/5`` on the problem object (default: ``0``).',
			'``seed(S)`` option' - 'Positive integer seed for the random number generator, enabling reproducible runs (default: none).'
		]
	]).

	:- public(run/4).
	:- mode(run(-nonvar, -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the genetic algorithm using the given options, returns the best individual found and its energy, and returns run statistics.',
		argnames is ['BestIndividual', 'BestEnergy', 'Statistics', 'Options'],
		remarks is [
			'Statistics list' - 'A list of ``Key(Value)`` pairs: ``generations(N)`` is the number of generations executed, ``evaluations(E)`` is the total number of fitness evaluations, ``improvements(I)`` is the number of generations that improved the best energy, and ``final_population_size(S)`` is the size of the final population.'
		]
	]).

	:- uses(_Problem_, [
		random_individual/1, initial_population/1, state_energy/2, crossover/4, mutate/2, stop_condition/3,
		progress/5, diversity/2, crossover_rate/4, mutation_rate/4
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		between/3, random/1, randomize/1
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth1/3, take/3
	]).

	:- uses(numberlist, [
		sum/2, min/2, max/2
	]).

	:- uses(type, [
		valid/2
	]).

	run(BestIndividual, BestEnergy) :-
		run(BestIndividual, BestEnergy, _Statistics, []).

	run(BestIndividual, BestEnergy, UserOptions) :-
		run(BestIndividual, BestEnergy, _Statistics, UserOptions).

	run(BestIndividual, BestEnergy, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		% handle seed option
		(	^^option(seed(Seed), Options) ->
			randomize(Seed)
		;	true
		),
		^^option(max_generations(MaxGenerations), Options),
		^^option(population_size(PopulationSize), Options),
		^^option(crossover_rate(CrossoverRate0), Options),
		^^option(mutation_rate(MutationRate0), Options),
		^^option(crossover_schedule(CrossoverSchedule), Options),
		^^option(mutation_schedule(MutationSchedule), Options),
		^^option(selection(Selection), Options),
		^^option(elite_size(EliteSize), Options),
		^^option(objective(Objective), Options),
		^^option(updates(Updates), Options),
		% initial population
		init_population(PopulationSize, Population0),
		evaluate_population(Population0, Evaluated0, 0, Evaluations0),
		best_of(Evaluated0, Objective, Best0, BestEnergy0),
		% update interval
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxGenerations - 1) // Updates)
		;	UpdateInterval is 0
		),
		% main generational loop
		generational_loop(
			0, MaxGenerations, PopulationSize,
			CrossoverRate0, MutationRate0, CrossoverSchedule, MutationSchedule,
			Selection, EliteSize, Objective, UpdateInterval,
			Evaluated0, Best0, BestEnergy0, Evaluations0, 0,
			FinalPopulation, BestIndividual, BestEnergy, FinalGenerations, FinalEvaluations, FinalImprovements
		),
		length(FinalPopulation, FinalPopulationSize),
		Statistics = [
			generations(FinalGenerations),
			evaluations(FinalEvaluations),
			improvements(FinalImprovements),
			final_population_size(FinalPopulationSize)
		].

	% initial population

	init_population(PopulationSize, Population) :-
		(	initial_population(Given),
			length(Given, Length),
			Length > 0 ->
			!,
			(	Length >= PopulationSize ->
				take(PopulationSize, Given, Population)
			;	Need is PopulationSize - Length,
				generate_random_individuals(Need, Extra),
				append(Given, Extra, Population)
			)
		;	generate_random_individuals(PopulationSize, Population)
		).

	generate_random_individuals(0, []) :-
		!.
	generate_random_individuals(Count, [Individual| Individuals]) :-
		Count > 0,
		random_individual(Individual),
		Count1 is Count - 1,
		generate_random_individuals(Count1, Individuals).

	% evaluation

	evaluate_population([], [], Evaluations, Evaluations).
	evaluate_population([Individual| Individuals], [Individual-Energy| Evaluated], Evaluations0, Evaluations) :-
		state_energy(Individual, Energy),
		Evaluations1 is Evaluations0 + 1,
		evaluate_population(Individuals, Evaluated, Evaluations1, Evaluations).

	% best selection according to objective

	best_of([Individual-Energy| Rest], Objective, BestIndividual, BestEnergy) :-
		best_of(Rest, Objective, Individual, Energy, BestIndividual, BestEnergy).

	best_of([], _, BestIndividual, BestEnergy, BestIndividual, BestEnergy).
	best_of([Individual-Energy| Rest], Objective, BestIndividual0, BestEnergy0, BestIndividual, BestEnergy) :-
		(	better(Objective, Energy, BestEnergy0) ->
			best_of(Rest, Objective, Individual, Energy, BestIndividual, BestEnergy)
		;	best_of(Rest, Objective, BestIndividual0, BestEnergy0, BestIndividual, BestEnergy)
		).

	better(minimize, Energy1, Energy2) :-
		Energy1 < Energy2.
	better(maximize, Energy1, Energy2) :-
		Energy1 > Energy2.

	% generational loop

	generational_loop(
		Generation, MaxGenerations, _PopulationSize,
		_CrossoverRate, _MutationRate, _CrossoverSchedule, _MutationSchedule,
		_Selection, _EliteSize, _Objective, UpdateInterval,
		Population, BestIndividual, BestEnergy, Evaluations, Improvements,
		Population, BestIndividual, BestEnergy, Generation, Evaluations, Improvements
	) :-
		Generation >= MaxGenerations,
		!,
		report_final(Generation, UpdateInterval, Population, BestIndividual, BestEnergy).
	generational_loop(
		Generation, _MaxGenerations, _PopulationSize,
		_CrossoverRate, _MutationRate, _CrossoverSchedule, _MutationSchedule,
		_Selection, _EliteSize, _Objective, UpdateInterval,
		Population, BestIndividual, BestEnergy, Evaluations, Improvements,
		Population, BestIndividual, BestEnergy, Generation, Evaluations, Improvements
	) :-
		stop_condition(Generation, BestIndividual, BestEnergy),
		!,
		report_final(Generation, UpdateInterval, Population, BestIndividual, BestEnergy).
	generational_loop(
		Generation, MaxGenerations, PopulationSize,
		CrossoverRate0, MutationRate0, CrossoverSchedule, MutationSchedule,
		Selection, EliteSize, Objective, UpdateInterval,
		Population, BestIndividual, BestEnergy, Evaluations, Improvements,
		FinalPopulation, FinalBestIndividual, FinalBestEnergy, FinalGenerations, FinalEvaluations, FinalImprovements
	) :-
		% adapt rates once per generation (hooks override schedules)
		update_rate(crossover, Generation, MaxGenerations, CrossoverRate0, CrossoverSchedule, CrossoverRate),
		update_rate(mutation, Generation, MaxGenerations, MutationRate0, MutationSchedule, MutationRate),
		% produce next generation
		next_generation(
			Population, PopulationSize, CrossoverRate, MutationRate, Selection, EliteSize, Objective,
			NextPopulation, NewEvaluations
		),
		Evaluations1 is Evaluations + NewEvaluations,
		best_of(NextPopulation, Objective, CandidateBest, CandidateBestEnergy),
		(	better(Objective, CandidateBestEnergy, BestEnergy) ->
			NewBestIndividual = CandidateBest,
			NewBestEnergy = CandidateBestEnergy,
			Improvements1 is Improvements + 1
		;	NewBestIndividual = BestIndividual,
			NewBestEnergy = BestEnergy,
			Improvements1 is Improvements
		),
		% progress
		report_progress(Generation, UpdateInterval, NextPopulation, NewBestIndividual, NewBestEnergy),
		Generation1 is Generation + 1,
		generational_loop(
			Generation1, MaxGenerations, PopulationSize,
			CrossoverRate, MutationRate, CrossoverSchedule, MutationSchedule,
			Selection, EliteSize, Objective, UpdateInterval,
			NextPopulation, NewBestIndividual, NewBestEnergy, Evaluations1, Improvements1,
			FinalPopulation, FinalBestIndividual, FinalBestEnergy, FinalGenerations, FinalEvaluations, FinalImprovements
		).

	% adaptive rate update
	%
	% Priority: problem hook (if defined and succeeds) > schedule option > constant.

	update_rate(crossover, Generation, MaxGenerations, CurrentRate, Schedule, NewRate) :-
		(	crossover_rate(Generation, MaxGenerations, CurrentRate, HookRate) ->
			clamp_rate(HookRate, NewRate)
		;	apply_schedule(Schedule, Generation, MaxGenerations, CurrentRate, NewRate)
		).
	update_rate(mutation, Generation, MaxGenerations, CurrentRate, Schedule, NewRate) :-
		(	mutation_rate(Generation, MaxGenerations, CurrentRate, HookRate) ->
			clamp_rate(HookRate, NewRate)
		;	apply_schedule(Schedule, Generation, MaxGenerations, CurrentRate, NewRate)
		).

	apply_schedule(constant, _Generation, _MaxGenerations, CurrentRate, CurrentRate).
	apply_schedule(linear(InitialRate, FinalRate), Generation, MaxGenerations, _CurrentRate, NewRate) :-
		(	MaxGenerations =< 1 ->
			clamp_rate(FinalRate, NewRate)
		;	Fraction is Generation / (MaxGenerations - 1),
			Raw is InitialRate + (FinalRate - InitialRate) * Fraction,
			clamp_rate(Raw, NewRate)
		).
	apply_schedule(geometric(_Factor), 0, _MaxGenerations, CurrentRate, CurrentRate) :-
		!.
	apply_schedule(geometric(Factor), _Generation, _MaxGenerations, CurrentRate, NewRate) :-
		Raw is CurrentRate * Factor,
		clamp_rate(Raw, NewRate).

	clamp_rate(Rate, Clamped) :-
		(	Rate < 0.0 ->
			Clamped is 0.0
		;	Rate > 1.0 ->
			Clamped is 1.0
		;	Clamped is Rate
		).

	% next generation construction

	% next generation with elitism preservation
	%
	% The best EffectiveEliteSize individuals of the current population
	% (sorted by objective) are copied unchanged into the next generation.
	% EffectiveEliteSize is min(EliteSize, PopulationSize). The remaining
	% slots are filled by selection, crossover, and mutation. When
	% EliteSize is 0, elitism is disabled and the whole next generation
	% is produced by breeding.

	next_generation(Population, PopulationSize, CrossoverRate, MutationRate, Selection, EliteSize, Objective, NextPopulation, NewEvaluations) :-
		EffectiveEliteSize is min(EliteSize, PopulationSize),
		(	EffectiveEliteSize > 0 ->
			sort_population(Objective, Population, Sorted),
			take(EffectiveEliteSize, Sorted, Elites),
			Need is PopulationSize - EffectiveEliteSize
		;	Elites = [],
			Need is PopulationSize
		),
		(	Need > 0 ->
			breed(Need, Population, CrossoverRate, MutationRate, Selection, Objective, Offspring, OffspringEvaluations)
		;	Offspring = [],
			OffspringEvaluations = 0
		),
		append(Elites, Offspring, NextPopulation),
		NewEvaluations is OffspringEvaluations.

	% sort so that better individuals come first
	sort_population(minimize, Population, Sorted) :-
		add_keys_asc(Population, Keyed),
		keysort(Keyed, SortedKeyed),
		strip_keys(SortedKeyed, Sorted).
	sort_population(maximize, Population, Sorted) :-
		add_keys_desc(Population, Keyed),
		keysort(Keyed, SortedKeyed),
		strip_keys(SortedKeyed, Sorted).

	add_keys_asc([], []).
	add_keys_asc([Individual-Energy| Rest], [Energy-Individual-Energy| Keyed]) :-
		add_keys_asc(Rest, Keyed).

	add_keys_desc([], []).
	add_keys_desc([Individual-Energy| Rest], [Key-Individual-Energy| Keyed]) :-
		Key is -Energy,
		add_keys_desc(Rest, Keyed).

	strip_keys([], []).
	strip_keys([_-Individual-Energy| Rest], [Individual-Energy| Stripped]) :-
		strip_keys(Rest, Stripped).

	% breeding (selection, crossover, mutation)

	breed(0, _, _, _, _, _, [], 0) :-
		!.
	breed(Need, Population, CrossoverRate, MutationRate, Selection, Objective, Offspring, Evaluations) :-
		Need > 0,
		% select two parents
		select_parent(Selection, Population, Objective, Parent1),
		select_parent(Selection, Population, Objective, Parent2),
		% crossover or copy
		random(Random),
		(	Random < CrossoverRate ->
			crossover(Parent1, Parent2, Child1a, Child2a)
		;	Child1a = Parent1,
			Child2a = Parent2
		),
		% mutation
		maybe_mutate(Child1a, MutationRate, Child1),
		maybe_mutate(Child2a, MutationRate, Child2),
		% evaluate
		state_energy(Child1, Energy1),
		state_energy(Child2, Energy2),
		(	Need =:= 1 ->
			Offspring = [Child1-Energy1],
			Evaluations is 1
		;	Need1 is Need - 2,
			breed(Need1, Population, CrossoverRate, MutationRate, Selection, Objective, Rest, RestEvaluations),
			Offspring = [Child1-Energy1, Child2-Energy2| Rest],
			Evaluations is RestEvaluations + 2
		).

	maybe_mutate(Individual, Rate, Mutated) :-
		random(Random),
		(	Random < Rate ->
			mutate(Individual, Mutated)
		;	Mutated = Individual
		).

	% parent selection dispatch

	select_parent(tournament(TournamentSize), Population, Objective, Winner) :-
		tournament_select(Population, TournamentSize, Objective, Winner).
	select_parent(roulette, Population, Objective, Winner) :-
		roulette_select(Population, Objective, Winner).
	select_parent(rank, Population, Objective, Winner) :-
		rank_select(Population, Objective, Winner).

	% tournament selection
	%
	% Sample TournamentSize individuals uniformly at random from the
	% population (with replacement). Return the best individual among
	% the sample according to the optimization objective.

	tournament_select(Population, TournamentSize, Objective, Winner) :-
		length(Population, PopulationLength),
		tournament_sample(TournamentSize, PopulationLength, Population, [], Sample),
		best_of(Sample, Objective, Winner, _).

	tournament_sample(0, _, _, Sample, Sample) :-
		!.
	tournament_sample(Remaining, PopulationLength, Population, Sample0, Sample) :-
		Remaining > 0,
		between(1, PopulationLength, Index),
		nth1(Index, Population, IndividualEnergy),
		Remaining1 is Remaining - 1,
		tournament_sample(Remaining1, PopulationLength, Population, [IndividualEnergy| Sample0], Sample).

	% roulette-wheel selection
	%
	% Convert energies to non-negative fitness values according to the
	% objective, build a cumulative wheel, and select proportionally.

	roulette_select(Population, Objective, Winner) :-
		energies_to_fitness(Objective, Population, FitnessPairs),
		findall(Fitness, member(_-Fitness, FitnessPairs), FitnessValues),
		sum(FitnessValues, TotalFitness),
		(	TotalFitness =< 0.0 ->
			% all fitness zero: fall back to uniform choice
			length(Population, PopulationLength),
			between(1, PopulationLength, Index),
			nth1(Index, Population, Winner-_)
		;	random(Random),
			Spin is Random * TotalFitness,
			spin_wheel(FitnessPairs, Spin, 0.0, Winner)
		).

	energies_to_fitness(minimize, Population, FitnessPairs) :-
		findall(Energy, member(_-Energy, Population), Energies),
		max(Energies, MaxEnergy),
		% fitness = max - energy + epsilon so the worst still has a small chance
		Epsilon = 1.0e-12,
		findall(
			Individual-(MaxEnergy - Energy + Epsilon),
			member(Individual-Energy, Population),
			FitnessPairs
		).
	energies_to_fitness(maximize, Population, FitnessPairs) :-
		findall(Energy, member(_-Energy, Population), Energies),
		min(Energies, MinEnergy),
		% shift so the lowest energy maps to a small positive fitness
		Epsilon = 1.0e-12,
		findall(
			Individual-(Energy - MinEnergy + Epsilon),
			member(Individual-Energy, Population),
			FitnessPairs
		).

	spin_wheel([Individual-_| _], Spin, Cumulative, Individual) :-
		Spin =< Cumulative,
		!.
	spin_wheel([Individual-_], _, _, Individual) :-
		!.
	spin_wheel([Individual-Fitness| Rest], Spin, Cumulative0, Winner) :-
		Cumulative1 is Cumulative0 + Fitness,
		(	Spin =< Cumulative1 ->
			Winner = Individual
		;	spin_wheel(Rest, Spin, Cumulative1, Winner)
		).

	% rank selection
	%
	% Sort the population by objective (best first). Assign linear rank
	% weights N, N-1, ..., 1 and select proportionally to those weights.

	rank_select(Population, Objective, Winner) :-
		sort_population(Objective, Population, Sorted),
		length(Sorted, PopulationLength),
		rank_weights(Sorted, PopulationLength, Weighted),
		findall(Weight, member(_-Weight, Weighted), Weights),
		sum(Weights, TotalWeight),
		random(Random),
		Spin is Random * TotalWeight,
		spin_wheel(Weighted, Spin, 0.0, Winner).

	rank_weights([], _, []).
	rank_weights([Individual-_| Rest], Rank, [Individual-Rank| WeightedRest]) :-
		Rank1 is Rank - 1,
		rank_weights(Rest, Rank1, WeightedRest).

	% progress reporting

	report_progress(Generation, UpdateInterval, Population, BestIndividual, BestEnergy) :-
		UpdateInterval > 0,
		Generation > 0,
		Generation mod UpdateInterval =:= 0,
		!,
		call_progress(Generation, Population, BestIndividual, BestEnergy).
	report_progress(_, _, _, _, _).

	report_final(Generation, UpdateInterval, Population, BestIndividual, BestEnergy) :-
		UpdateInterval > 0,
		!,
		call_progress(Generation, Population, BestIndividual, BestEnergy).
	report_final(_, _, _, _, _).

	call_progress(Generation, Population, BestIndividual, BestEnergy) :-
		mean_energy(Population, MeanEnergy),
		findall(Individual, member(Individual-_, Population), Individuals),
		(	diversity(Individuals, Diversity) ->
			true
		;	Diversity = 0.0
		),
		ignore(progress(Generation, BestIndividual, BestEnergy, MeanEnergy, Diversity)).

	mean_energy(Population, Mean) :-
		findall(Energy, member(_-Energy, Population), Energies),
		length(Energies, Count),
		(	Count > 0 ->
			sum(Energies, Sum),
			Mean is Sum / Count
		;	Mean is 0.0
		).

	% default options

	default_option(max_generations(200)).
	default_option(population_size(50)).
	default_option(crossover_rate(0.8)).
	default_option(mutation_rate(0.1)).
	default_option(crossover_schedule(constant)).
	default_option(mutation_schedule(constant)).
	default_option(selection(tournament(3))).
	default_option(elite_size(1)).
	default_option(objective(minimize)).
	default_option(updates(0)).

	% option validation

	valid_option(max_generations(MaxGenerations)) :-
		valid(positive_integer, MaxGenerations).
	valid_option(population_size(PopulationSize)) :-
		valid(positive_integer, PopulationSize),
		PopulationSize >= 2.
	valid_option(crossover_rate(CrossoverRate)) :-
		number(CrossoverRate),
		CrossoverRate >= 0.0, CrossoverRate =< 1.0.
	valid_option(mutation_rate(MutationRate)) :-
		number(MutationRate),
		MutationRate >= 0.0, MutationRate =< 1.0.
	valid_option(selection(Selection)) :-
		once((
			Selection == roulette
		;	Selection == rank
		;	Selection = tournament(TournamentSize),
			valid(positive_integer, TournamentSize)
		)).
	valid_option(crossover_schedule(Schedule)) :-
		once((
			Schedule == constant
		;	Schedule = linear(InitialRate, FinalRate),
			number(InitialRate), InitialRate >= 0.0, InitialRate =< 1.0,
			number(FinalRate), FinalRate >= 0.0, FinalRate =< 1.0
		;	Schedule = geometric(Factor),
			number(Factor), Factor > 0.0
		)).
	valid_option(mutation_schedule(Schedule)) :-
		once((
			Schedule == constant
		;	Schedule = linear(InitialRate, FinalRate),
			number(InitialRate), InitialRate >= 0.0, InitialRate =< 1.0,
			number(FinalRate), FinalRate >= 0.0, FinalRate =< 1.0
		;	Schedule = geometric(Factor),
			number(Factor), Factor > 0.0
		)).
	valid_option(elite_size(EliteSize)) :-
		valid(non_negative_integer, EliteSize).
	valid_option(objective(Objective)) :-
		once((Objective == minimize; Objective == maximize)).
	valid_option(updates(Updates)) :-
		valid(non_negative_integer, Updates).
	valid_option(seed(Seed)) :-
		valid(positive_integer, Seed).

:- end_object.


:- object(genetic_algorithm(_Problem_),
	extends(genetic_algorithm(_Problem_, xoshiro128pp))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Genetic algorithm meta-heuristic optimizer using the Xoshiro128++ random number generator. Convenience object that extends ``genetic_algorithm/2`` with the random algorithm bound to ``xoshiro128pp``.',
		parameters is [
			'Problem' - 'Problem object implementing ``genetic_algorithm_problem_protocol``.'
		],
		see_also is [genetic_algorithm(_, _), genetic_algorithm_problem_protocol]
	]).

:- end_object.

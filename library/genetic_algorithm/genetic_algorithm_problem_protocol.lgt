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


:- protocol(genetic_algorithm_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Protocol for genetic algorithm problem definitions. A problem object must define the required predicates for individuals, fitness (energy), crossover, and mutation, and may optionally define initial population generation, stopping, progress reporting, diversity, and adaptive rate hooks.',
		see_also is [genetic_algorithm(_), genetic_algorithm(_, _)]
	]).

	:- public(random_individual/1).
	:- mode(random_individual(-nonvar), one).
	:- info(random_individual/1, [
		comment is 'Generates a random individual (candidate solution). Used to build the initial population when ``initial_population/1`` is not defined and to replace individuals when needed.',
		argnames is ['Individual']
	]).

	:- public(initial_population/1).
	:- mode(initial_population(-list(nonvar)), zero_or_one).
	:- info(initial_population/1, [
		comment is 'Optionally returns a non-empty list of initial individuals. When not defined, the algorithm generates a random initial population of the requested size by repeated calls to ``random_individual/1``.',
		argnames is ['Population']
	]).

	:- public(state_energy/2).
	:- mode(state_energy(+nonvar, -number), one).
	:- info(state_energy/2, [
		comment is 'Computes the energy (cost / fitness) of the given individual. The algorithm minimizes this value by default; use the ``objective(maximize)`` option to maximize it instead.',
		argnames is ['Individual', 'Energy']
	]).

	:- public(crossover/4).
	:- mode(crossover(+nonvar, +nonvar, -nonvar, -nonvar), one).
	:- info(crossover/4, [
		comment is 'Recombines two parent individuals into two offspring. This is the most problem-specific operator after representation and strongly influences search quality.',
		argnames is ['Parent1', 'Parent2', 'Offspring1', 'Offspring2']
	]).

	:- public(mutate/2).
	:- mode(mutate(+nonvar, -nonvar), one).
	:- info(mutate/2, [
		comment is 'Produces a mutated version of the given individual. Mutation introduces diversity and helps escape local optima.',
		argnames is ['Individual', 'Mutated']
	]).

	:- public(stop_condition/3).
	:- mode(stop_condition(+non_negative_integer, +nonvar, +number), zero_or_one).
	:- info(stop_condition/3, [
		comment is 'True when the search should stop given the current generation, best individual found so far, and its energy. Optional. When not defined by the problem, the search runs until the maximum number of generations is reached.',
		argnames is ['Generation', 'BestIndividual', 'BestEnergy']
	]).

	:- public(progress/5).
	:- mode(progress(+non_negative_integer, +nonvar, +number, +number, +number), zero_or_one).
	:- info(progress/5, [
		comment is 'Called periodically to report optimization progress. Optional. When not defined by the problem, progress reporting is skipped. The mean energy and diversity are population statistics; diversity is problem-defined when ``diversity/2`` is present, otherwise a simple placeholder.',
		argnames is ['Generation', 'BestIndividual', 'BestEnergy', 'MeanEnergy', 'Diversity']
	]).

	:- public(diversity/2).
	:- mode(diversity(+list(nonvar), -number), zero_or_one).
	:- info(diversity/2, [
		comment is 'Optionally computes a numeric diversity measure for the current population. When not defined, progress reporting uses 0.0 for diversity.',
		argnames is ['Population', 'Diversity']
	]).

	:- public(crossover_rate/4).
	:- mode(crossover_rate(+non_negative_integer, +positive_integer, +float, -float), zero_or_one).
	:- info(crossover_rate/4, [
		comment is 'Optional adaptive crossover-rate hook. Called once per generation with the current generation index, the maximum number of generations, and the current crossover rate. When defined and successful, the returned rate (clamped to ``[0.0, 1.0]``) is used for that generation and overrides any ``crossover_schedule/1`` option. When not defined or when it fails, the configured schedule is applied instead.',
		argnames is ['Generation', 'MaxGenerations', 'CurrentRate', 'NewRate']
	]).

	:- public(mutation_rate/4).
	:- mode(mutation_rate(+non_negative_integer, +positive_integer, +float, -float), zero_or_one).
	:- info(mutation_rate/4, [
		comment is 'Optional adaptive mutation-rate hook. Called once per generation with the current generation index, the maximum number of generations, and the current mutation rate. When defined and successful, the returned rate (clamped to ``[0.0, 1.0]``) is used for that generation and overrides any ``mutation_schedule/1`` option. When not defined or when it fails, the configured schedule is applied instead.',
		argnames is ['Generation', 'MaxGenerations', 'CurrentRate', 'NewRate']
	]).

:- end_protocol.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(quadratic_progress,
	implements(genetic_algorithm_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Quadratic problem that records progress/5 calls for testing.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		random/3
	]).

	:- public([
		clear_log/0,
		progress_log/5
	]).

	:- private(log_/5).
	:- dynamic(log_/5).

	clear_log :-
		retractall(log_(_, _, _, _, _)).

	progress_log(Generation, BestIndividual, BestEnergy, MeanEnergy, Diversity) :-
		log_(Generation, BestIndividual, BestEnergy, MeanEnergy, Diversity).

	random_individual(Individual) :-
		random(-50.0, 50.0, Individual).

	state_energy(Individual, Energy) :-
		Energy is (Individual - 3.0) * (Individual - 3.0).

	crossover(Parent1, Parent2, Offspring1, Offspring2) :-
		Offspring1 is 0.5*Parent1 + 0.5*Parent2,
		Offspring2 is 0.5*Parent2 + 0.5*Parent1.

	mutate(Individual, Mutated) :-
		random(-2.0, 2.0, Delta),
		Mutated is Individual + Delta.

	progress(Generation, BestIndividual, BestEnergy, MeanEnergy, Diversity) :-
		assertz(log_(Generation, BestIndividual, BestEnergy, MeanEnergy, Diversity)).

:- end_object.

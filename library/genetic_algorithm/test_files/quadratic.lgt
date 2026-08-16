%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(quadratic,
	implements(genetic_algorithm_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Simple quadratic minimization problem for testing the genetic algorithm.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		random/3, randomize/1
	]).

	:- public(reset_seed/0).

	reset_seed :-
		randomize(12345).

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

:- end_object.

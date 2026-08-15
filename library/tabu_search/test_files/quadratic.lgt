%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(quadratic,
	implements(tabu_search_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Simple quadratic minimization problem for testing tabu search.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		random/3, randomize/1
	]).

	:- public(reset_seed/0).

	reset_seed :-
		randomize(12345).

	initial_state(50.0).

	neighbor_state(X, Y) :-
		random(-5.0, 5.0, Delta),
		Y is X + Delta.

	state_energy(X, E) :-
		E is (X - 3.0) * (X - 3.0).

:- end_object.

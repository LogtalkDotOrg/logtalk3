%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(quadratic_delta,
	implements(tabu_search_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Quadratic minimization using the delta-energy neighbor variant.'
	]).

	:- uses(fast_random(xoshiro128pp), [
		random/3, randomize/1
	]).

	:- public(reset_seed/0).

	reset_seed :-
		randomize(12345).

	initial_state(50.0).

	neighbor_state(X, Y, DeltaE) :-
		random(-5.0, 5.0, Delta),
		Y is X + Delta,
		% Exact delta for the quadratic (X-3)^2
		DeltaE is (Y - 3.0)*(Y - 3.0) - (X - 3.0)*(X - 3.0).

	neighbor_state(X, Y) :-
		neighbor_state(X, Y, _).

	state_energy(X, E) :-
		E is (X - 3.0) * (X - 3.0).

:- end_object.

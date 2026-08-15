%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(quadratic_progress,
	implements(tabu_search_problem_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Quadratic minimization that records progress calls.'
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

	progress_log(Step, BestE, CurrE, AccRate, ImpRate) :-
		log_(Step, BestE, CurrE, AccRate, ImpRate).

	initial_state(50.0).

	neighbor_state(X, Y) :-
		random(-5.0, 5.0, Delta),
		Y is X + Delta.

	state_energy(X, E) :-
		E is (X - 3.0) * (X - 3.0).

	progress(Step, BestE, CurrE, AccRate, ImpRate) :-
		assertz(log_(Step, BestE, CurrE, AccRate, ImpRate)).

:- end_object.

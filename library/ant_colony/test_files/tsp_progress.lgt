%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tsp_progress,
	extends(tsp)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'TSP test problem that records progress/5 calls for unit testing.'
	]).

	:- public([
		clear_log/0,
		progress_log/5
	]).

	:- private(log_/5).
	:- dynamic(log_/5).

	clear_log :-
		retractall(log_(_, _, _, _, _)).

	progress_log(Iter, Best, IterBest, Acc, Imp) :-
		log_(Iter, Best, IterBest, Acc, Imp).

	progress(Iter, Best, IterBest, Acc, Imp) :-
		assertz(log_(Iter, Best, IterBest, Acc, Imp)).

:- end_object.

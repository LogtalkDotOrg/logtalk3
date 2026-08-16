%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tsp_stop,
	extends(tsp)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'TSP test problem that stops early once the best cost reaches the known optimum for the regular-hexagon instance (side length 5 → optimal tour length 30).'
	]).

	% Optimal tour of the unit-side-scaled hexagon has length 6 * 5 = 30.
	% Allow a tiny floating-point tolerance.
	stop_condition(_Iteration, BestCost, _IterationBestCost) :-
		BestCost =< 30.0001.

:- end_object.

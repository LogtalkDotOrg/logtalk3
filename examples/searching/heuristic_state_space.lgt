%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(heuristic_state_space,
	instantiates(class),
	specializes(state_space)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 1998/3/23,
		comment is 'Heuristic state space.'
	]).

	:- public(next_state/3).
	:- mode(next_state(+nonvar, -nonvar, -number), zero_or_more).
	:- info(next_state/3, [
		comment is 'Generates a state sucessor.',
		argnames is ['State', 'Next', 'Cost']
	]).

	:- public(heuristic/2).
	:- mode(heuristic(+nonvar, -number), one).
	:- info(heuristic/2, [
		comment is 'Estimates state distance to a goal state.',
		argnames is ['State', 'Estimate']
	]).

	next_state(Prev, Next) :-
		::next_state(Prev, Next, _).

:- end_object.

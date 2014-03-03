%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% fsm(Transitions, Initial, Final)
%
% fsm(-list, -nonvar, -list)

fsm([red-0-red, red-1-green, red-2-red,		% a simple finite state machine example
	 yellow-0-red, yellow-1-green, yellow-2-red,
	 green-0-yellow, green-1-yellow, green-2-red],
	red,
	[red]).


:- object(fsm(_Transitions, _Initial, _Final),
	imports(private::assignvars)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2005/1/8,
		comment is 'A simple implementation of finite-state machines using assignable variables and parametric objects. Adapted from a similar example by Nobukuni Kino.',
		parnames is ['Transitions', 'Initial state', 'Final states']
	]).

	:- public(recognise/1).
	:- mode(recognise(+list), zero_or_more).
	:- info(recognise/1, [
		comment is 'Recognise a list of events.',
		argnames is ['Events']
	]).

	recognise(Events) :-
		parameter(2, Initial),
		::assignable(Current, Initial),
		recognise(Events, Current).

	recognise([], State) :-
		::State => Current,
		final_state(Current).
	recognise([Event| Events], State) :-
		::State => Current,
		transition(Event, Current, Next),
		(	write(Current-Event-Next), nl
		;	write('backtracking...'), nl,
			fail
		),
		::State <= Next,
		recognise(Events, State).

	transition(Event, Current, Next) :-
		parameter(1, Transitions),
		transition(Transitions, Event, Current, Next).

	transition([Current-Event-Next| _], Event, Current, Next).
	transition([_| Transitions], Event, Current, Next):-
		transition(Transitions, Event, Current, Next).

	final_state(State) :-
		parameter(3, Final),
		final_state(Final, State).

	final_state([State| _], State).
	final_state([_| States], State) :-
		final_state(States, State).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


% fsm(Transitions, Initial, Final)
%
% fsm(-list, -nonvar, -list)

fsm([red-0-red, red-1-green, red-2-red,		% a simple finite state machine example
	yellow-0-red, yellow-1-green, yellow-2-red,
	green-0-yellow, green-1-yellow, green-2-red],
	red,
	[red]
).


:- object(fsm(_Transitions, _Initial, _Final)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/08/02,
		comment is 'A simple implementation of finite-state machines using assignable variables and parametric objects. Adapted from a similar example by Nobukuni Kino.',
		parnames is ['Transitions', 'Initial state', 'Final states']
	]).

	:- uses(assignvars, [
		assignable/2, (=>)/2, (<=)/2, op(100, xfx, '=>'), op(100, xfx, '<=')
	]).

	:- public(recognise/1).
	:- mode(recognise(+list), zero_or_more).
	:- info(recognise/1, [
		comment is 'Recognise a list of events.',
		argnames is ['Events']
	]).

	recognise(Events) :-
		parameter(2, Initial),
		assignable(Current, Initial),
		recognise(Events, Current).

	recognise([], State) :-
		State => Current,
		final_state(Current).
	recognise([Event| Events], State) :-
		State => Current,
		transition(Event, Current, Next),
		(	write(Current-Event-Next), nl
		;	write('backtracking...'), nl,
			fail
		),
		State <= Next,
		recognise(Events, State).

	transition(Event, Current, Next) :-
		parameter(1, Transitions),
		transition(Transitions, Event, Current, Next).

	transition([Current-Event-Next| _], Event, Current, Next).
	transition([_| Transitions], Event, Current, Next) :-
		transition(Transitions, Event, Current, Next).

	final_state(State) :-
		parameter(3, Final),
		final_state(Final, State).

	final_state([State| _], State).
	final_state([_| States], State) :-
		final_state(States, State).

:- end_object.

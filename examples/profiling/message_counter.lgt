%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(message_counter,
	implements(monitoring),
	imports(monitor)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-11-22,
		comment is 'Message counter monitor.'
	]).

	:- public(counts/3).
	:- mode(counts(?object_identifier, ?non_negative_integer, ?non_negative_integer), zero_or_more).
	:- info(counts/3, [
		comment is 'Returns current calls and exits message counts for an object. Used for testing.',
		argnames is ['Object', 'Calls', 'Exits']
	]).

	:- public(counts/4).
	:- mode(counts(?object_identifier, ?predicate_indicator, ?non_negative_integer, ?non_negative_integer), zero_or_more).
	:- info(counts/4, [
		comment is 'Returns current calls and exits message counts for an object predicate. Used for testing.',
		argnames is ['Object', 'Predicate', 'Calls', 'Exits']
	]).

	:- public(report/0).
	:- mode(report, one).
	:- info(report/0, [
		comment is 'Reports current calls and exits message counts.'
	]).

	:- public(stop/0).
	:- mode(stop, one).
	:- info(stop/0, [
		comment is 'Stops message counting.'
	]).

	:- private(calls/2).
	:- dynamic(calls/2).
	:- mode(calls(?object, ?integer), zero_or_more).

	:- private(calls/3).
	:- dynamic(calls/3).
	:- mode(calls(?object, ?predicate_indicator,?integer), zero_or_more).

	:- private(exits/2).
	:- dynamic(exits/2).
	:- mode(exits(?object, ?integer), zero_or_more).

	:- private(exits/3).
	:- dynamic(exits/3).
	:- mode(exits(?object, ?predicate_indicator,?integer), zero_or_more).

	counts(Object, Calls, Exits) :-
		calls(Object, Calls),
		(	exits(Object, Exits) ->
			true
		;	Exits = 0
		).

	counts(Object, Predicate, Calls, Exits) :-
		calls(Object, Predicate, Calls),
		(	exits(Object, Predicate, Exits) ->
			true
		;	Exits = 0
		).

	report :-
		forall(
			calls(Object, Calls),
			(	writeq(Object), nl,
				write('  total of calls: '), write(Calls), nl,
				write('  total of exits: '),
				(	exits(Object, Exits) ->
					write(Exits), nl, nl
				;	write(0), nl, nl
				),
				forall(
					calls(Object, Functor/Arity, Calls2),
					(	write('  '), writeq(Functor/Arity), nl,
						write('    calls: '), write(Calls2), nl,
						write('    exits: '),
						(	exits(Object, Functor/Arity, Exits2) ->
							write(Exits2), nl, nl
						;	write(0), nl, nl
						)
					)
				)
			)
		).

	stop :-
		retractall(calls(_, _)),
		retractall(exits(_, _)),
		retractall(calls(_, _, _)),
		retractall(exits(_, _, _)),
		^^reset_monitor.

	before(Object, Message, _) :-
		(	retract(calls(Object, Old)) ->
			New is Old + 1
		;	New = 1
		),
		assertz(calls(Object, New)),
		functor(Message, Functor, Arity),
		(	retract(calls(Object, Functor/Arity, Old2)) ->
			New2 is Old2 + 1
		;	New2 = 1
		),
		assertz(calls(Object, Functor/Arity, New2)).

	after(Object, Message, _) :-
		(	retract(exits(Object, Old)) ->
			New is Old + 1
		;	New = 1
		),
		assertz(exits(Object, New)),
		functor(Message, Functor, Arity),
		(	retract(exits(Object, Functor/Arity, Old2)) ->
			New2 is Old2 + 1
		;	New2 = 1
		),
		assertz(exits(Object, Functor/Arity, New2)).

:- end_object.

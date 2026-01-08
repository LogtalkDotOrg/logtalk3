%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(sequence_grammars).

	:- info([
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2025-10-06,
		comment is 'Sequence grammars.'
	]).

	:- public(zero_or_more//2).
	:- meta_non_terminal(zero_or_more(1, *)).
	:- mode_non_terminal(zero_or_more(+callable, -list(atomic)), one).
	:- info(zero_or_more//2, [
		comment is 'Eagerly collect zero or more terminals that satisfy the given closure.',
		argnames is ['Closure', 'Terminals']
	]).

	:- public(one_or_more//2).
	:- meta_non_terminal(one_or_more(1, *)).
	:- mode_non_terminal(one_or_more(+callable, -list(atomic)), zero_or_one).
	:- info(one_or_more//2, [
		comment is 'Eagerly collect one or more terminals that satisfy the given closure.',
		argnames is ['Closure', 'Terminals']
	]).

	:- public(zero_or_more//1).
	:- mode_non_terminal(zero_or_more(-list(atomic)), one).
	:- info(zero_or_more//1, [
		comment is 'Eagerly collect zero or more terminals.',
		argnames is ['Terminals']
	]).

	:- public(one_or_more//1).
	:- mode_non_terminal(one_or_more(-list(atomic)), zero_or_one).
	:- info(one_or_more//1, [
		comment is 'Eagerly collect one or more terminals.',
		argnames is ['Terminals']
	]).

	:- public(zero_or_more//0).
	:- mode_non_terminal(zero_or_more, one).
	:- info(zero_or_more//0, [
		comment is 'Eagerly parse zero or more terminals.'
	]).
	:- public(one_or_more//0).
	:- mode_non_terminal(one_or_more, zero_or_one).
	:- info(one_or_more//0, [
		comment is 'Eagerly parse one or more terminals.'
	]).

	:- public(without//2).
	:- mode_non_terminal(without(+list(atomic), -list(atomic)), one).
	:- info(without//2, [
		comment is 'Collects input terminals until one of the stop terminals is found. The stop terminals are excluded from the collected terminals.',
		argnames is ['StopTerminals', 'Terminals']
	]).

	zero_or_more(Closure, [Terminal| Terminals]) -->
		call(Closure, Terminal), !, zero_or_more(Closure, Terminals).
	zero_or_more(_, []) -->
		[].

	one_or_more(Closure, [Terminal| Terminals]) -->
		call(Closure, Terminal), !, zero_or_more(Closure, Terminals).

	zero_or_more([Terminal| Terminals]) -->
		[Terminal], !, zero_or_more(Terminals).
	zero_or_more([]) -->
		[].

	one_or_more([Terminal| Terminals]) -->
		[Terminal], !, zero_or_more(Terminals).

	zero_or_more -->
		[_], !, zero_or_more.
	zero_or_more -->
		[].

	one_or_more -->
		[_], !, zero_or_more.

	without(StopTerminals, [Terminal| Terminals]) -->
		[Terminal],
		{ \+ member(Terminal, StopTerminals) },
		!,
		without(StopTerminals, Terminals).
	without(_, []) -->
		[].

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.

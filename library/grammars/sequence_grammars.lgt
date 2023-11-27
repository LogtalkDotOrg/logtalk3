%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2023-11-27,
		comment is 'Sequence grammars.'
	]).

	:- public(zero_or_more//1).
	:- mode(zero_or_more(-list(atomic)), zero_or_one).
	:- info(zero_or_more//1, [
		comment is 'Describes a sequence of zero or more terminals.',
		argnames is ['Terminals']
	]).

	:- public(one_or_more//1).
	:- mode(one_or_more(-list(atomic)), zero_or_one).
	:- info(one_or_more//1, [
		comment is 'Describes a sequence of one or more terminals.',
		argnames is ['Terminals']
	]).

	:- public(zero_or_more//0).
	:- mode(zero_or_more, one).
	:- info(zero_or_more//0, [
		comment is 'Describes a sequence of zero or more terminals.'
	]).
	:- public(one_or_more//0).
	:- mode(one_or_more, one).
	:- info(one_or_more//0, [
		comment is 'Describes a sequence of one or more terminals.'
	]).

	:- public(lazy_without//2).
	:- mode(lazy_without(+list(atomic), -list(atomic)), zero_or_more).
	:- info(lazy_without//2, [
		comment is 'Lazily collect input terminals until one of the stop terminals is found. The stop terminals are excluded from the collected terminals.',
		argnames is ['StopTerminals', 'Terminals']
	]).

	:- public(greedy_without//2).
	:- mode(greedy_without(+list(atomic), -list(atomic)), one).
	:- info(greedy_without//2, [
		comment is 'Greedly collect input terminals until one of the stop terminals is found. The stop terminals are excluded from the collected terminals.',
		argnames is ['StopTerminals', 'Terminals']
	]).

	:- public(rest//1).
	:- mode(rest(-list(atomic)), zero_or_one).
	:- info(rest//1, [
		comment is 'Rest of the input terminals.',
		argnames is ['Terminals']
	]).

	zero_or_more([]) -->
		[].
	zero_or_more([Terminal| Terminals]) -->
		[Terminal], zero_or_more(Terminals).

	one_or_more([Terminal| Terminals]) -->
		[Terminal], zero_or_more(Terminals).

	zero_or_more -->
		[].
	zero_or_more -->
		[_], zero_or_more.

	one_or_more -->
		[_], zero_or_more.

	lazy_without(_, []) -->
		[].
	lazy_without(StopTerminals, [Terminal| Terminals]) -->
		[Terminal],
		{ \+ member(Terminal, StopTerminals) },
		lazy_without(StopTerminals, Terminals).

	greedy_without(StopTerminals, [Terminal| Terminals]) -->
		[Terminal],
		{ \+ member(Terminal, StopTerminals) },
		!,
		greedy_without(StopTerminals, Terminals).
	greedy_without(_, []) -->
		[].

	rest([Terminal| Terminals]) -->
		[Terminal],
		rest(Terminals).
	rest([]) -->
		[].

	member(Head, [Head| _]).
	member(Head, [_| Tail]) :-
		member(Head, Tail).

:- end_object.

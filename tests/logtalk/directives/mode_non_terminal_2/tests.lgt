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


:- set_logtalk_flag(source_data, on).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-10-06,
		comment is 'Unit tests for the mode_non_terminal/2 built-in directive.'
	]).

	:- public(foo//6).
	:- mode_non_terminal(foo(+, -, ?, @, ++, --), zero).

	test(mode_non_terminal_2_01) :-
		predicate_property(foo(_,_,_,_,_,_,_,_), non_terminal(foo//6)).

	test(mode_non_terminal_2_02) :-
		predicate_property(foo(_,_,_,_,_,_,_,_), mode(Template, Solutions)),
		Template == foo(+, -, ?, @, ++, --, ?list, ?list),
		Solutions == zero.

	:- public(bar//6).
	:- mode_non_terminal(bar(+integer, -float, ?atom, @list, ++list, --stream), zero_or_one).

	test(mode_non_terminal_2_03) :-
		predicate_property(bar(_,_,_,_,_,_,_,_), non_terminal(bar//6)).

	test(mode_non_terminal_2_04) :-
		predicate_property(bar(_,_,_,_,_,_,_,_), mode(Template, Solutions)),
		Template == bar(+integer, -float, ?atom, @list, ++list, --stream, ?list, ?list),
		Solutions == zero_or_one.

	:- public(baz//1).
	:- mode_non_terminal(baz(@list(atom)), one).

	test(mode_non_terminal_2_05) :-
		predicate_property(baz(_,_,_), non_terminal(baz//1)).

	test(mode_non_terminal_2_06) :-
		predicate_property(baz(_,_,_), mode(Template, Solutions)),
		Template == baz(@list(atom), ?list, ?list),
		Solutions == one.

	:- public(qux//1).
	:- mode_non_terminal(qux(-positive_integer), zero_or_more).

	test(mode_non_terminal_2_07) :-
		predicate_property(qux(_,_,_), non_terminal(qux//1)).

	test(mode_non_terminal_2_08) :-
		predicate_property(qux(_,_,_), mode(Template, Solutions)),
		Template == qux(-positive_integer, ?list, ?list),
		Solutions == zero_or_more.

	:- public(quux//1).
	:- mode_non_terminal(quux(+string), one_or_more).

	test(mode_non_terminal_2_09) :-
		predicate_property(quux(_,_,_), non_terminal(quux//1)).

	test(mode_non_terminal_2_10) :-
		predicate_property(quux(_,_,_), mode(Template, Solutions)),
		Template == quux(+string, ?list, ?list),
		Solutions == one_or_more.

:- end_object.

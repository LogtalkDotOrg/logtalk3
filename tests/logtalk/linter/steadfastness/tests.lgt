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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-13,
		comment is 'Unit tests for the ``steadfastness`` linter flag.'
	]).

	:- private(steadfastness/5).
	:- dynamic(steadfastness/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [steadfastness(warning)]).

	cleanup :-
		retractall(steadfastness(_, _, _, _, _)).

	test(steadfastness_linter_flag_01, exists(Term == max/3)) :-
		steadfastness(_, _, object, steadfastness, Term).

	test(steadfastness_linter_flag_02, exists(Term == nt//2)) :-
		steadfastness(_, _, object, steadfastness, Term).

	test(steadfastness_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(possible_non_steadfast_non_terminal(file, 1-2, object, steadfastness, foo//1), core), Tokens).

	test(steadfastness_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(possible_non_steadfast_predicate(file, 1-2, object, steadfastness, foo/1), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(possible_non_steadfast_non_terminal(File, Lines, Type, Entity, Term), warning(steadfastness), core, _) :-
		assertz(steadfastness(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(possible_non_steadfast_predicate(File, Lines, Type, Entity, Term), warning(steadfastness), core, _) :-
		assertz(steadfastness(File, Lines, Type, Entity, Term)).

:- end_object.

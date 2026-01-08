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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-12-10,
		comment is 'Unit tests for the ``left_recursion`` linter flag.'
	]).

	:- private(left_recursion/5).
	:- dynamic(left_recursion/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [left_recursion(warning)]).

	cleanup :-
		retractall(left_recursion(_, _, _, _, _)).

	test(left_recursion_linter_flag_01, exists(Term == (a --> a, b))) :-
		left_recursion(_, _, object, left_recursion, Term).

	test(left_recursion_linter_flag_02, exists(Term == (d, [1] --> d, f))) :-
		left_recursion(_, _, object, left_recursion, Term).

	test(left_recursion_linter_flag_03, exists(Term == (p :- p, q))) :-
		left_recursion(_, _, object, left_recursion, Term).

	test(left_recursion_linter_flag_04, false) :-
		left_recursion(_, _, object, no_left_recursion, _).

	test(left_recursion_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(left_recursion(file, 1-2, object, left_recursion, (a :- a, b)), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(left_recursion(File, Lines, Type, Entity, Term), warning(left_recursion), core, _) :-
		assertz(left_recursion(File, Lines, Type, Entity, Term)).

:- end_object.

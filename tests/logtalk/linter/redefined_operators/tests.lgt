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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-13,
		comment is 'Unit tests for the ``redefined_operators`` linter flag.'
	]).

	:- private(redefined_operator/4).
	:- dynamic(redefined_operator/4).

	:- private(redefined_operator/6).
	:- dynamic(redefined_operator/6).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [redefined_built_ins(warning)]).

	cleanup :-
		retractall(redefined_operator(_, _, _, _)),
		retractall(redefined_operator(_, _, _, _, _, _)).

	test(redefined_operators_linter_flag_01, exists(Term == op(600, fx, ^^))) :-
		redefined_operator(_, _, _, Term).

	test(redefined_operators_linter_flag_02, exists(Term == op(777, xfy, ::))) :-
		redefined_operator(_, _, object, redefined_operators, _, Term).

	test(redefined_operators_linter_flag_03, exists(Term == op(123, xfx, @>))) :-
		redefined_operator(_, _, object, redefined_operators, _, Term).

	test(redefined_operators_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(redefined_operator(file, 1-2, op(600, xfy, ::), op(777, xfy, ::)), core), Tokens).

	test(redefined_operators_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(redefined_operator(file, 1-2, object, redefined_operators, op(600, xfy, ::), op(777, xfy, ::)), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(redefined_operator(File, Lines, OriginalOperator, Operator), warning(redefined_operators), core, _) :-
		assertz(redefined_operator(File, Lines, OriginalOperator, Operator)).
	logtalk::message_hook(redefined_operator(File, Lines, Type, Entity, OriginalOperator, Operator), warning(redefined_operators), core, _) :-
		assertz(redefined_operator(File, Lines, Type, Entity, OriginalOperator, Operator)).

:- end_object.

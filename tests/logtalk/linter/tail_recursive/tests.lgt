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
		comment is 'Unit tests for the ``tail_recursive`` linter flag.'
	]).

	:- private(tail_recursive/5).
	:- dynamic(tail_recursive/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [tail_recursive(warning)]).

	cleanup :-
		retractall(tail_recursive(_, _, _, _, _)).

	test(tail_recursive_linter_flag_01, exists(Term == sum_list/2)) :-
		tail_recursive(_, _, object, tail_recursive, Term).

	test(tail_recursive_linter_flag_02, exists(Term == foo//1)) :-
		tail_recursive(_, _, object, tail_recursive, Term).

	test(tail_recursive_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_tail_recursive_non_terminal(file, 1-2, object, tail_recursive, foo//1), core), Tokens).

	test(tail_recursive_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(non_tail_recursive_predicate(file, 1-2, object, tail_recursive, foo/1), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(non_tail_recursive_non_terminal(File, Lines, Type, Entity, Term), warning(tail_recursive), core, _) :-
		assertz(tail_recursive(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(non_tail_recursive_predicate(File, Lines, Type, Entity, Term), warning(tail_recursive), core, _) :-
		assertz(tail_recursive(File, Lines, Type, Entity, Term)).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-09-02,
		comment is 'Unit tests for the ``trivial_goal_fails`` linter flag.'
	]).

	:- private(trivial_goal_fails/5).
	:- dynamic(trivial_goal_fails/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [trivial_goal_fails(warning)]).

	cleanup :-
		retractall(trivial_goal_fails(_, _, _, _, _)).

	test(trivial_goal_fails_linter_flag_01, exists(Term == bar(1))) :-
		trivial_goal_fails(_, _, object, trivial_fails, Term).

	test(trivial_goal_fails_linter_flag_02, exists(Term == bar(a, 1))) :-
		trivial_goal_fails(_, _, object, trivial_fails, Term).

	test(trivial_goal_fails_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(no_matching_clause_for_predicate_goal(file, 1-2, object, unknown_entities, foo), core), Tokens).

	test(trivial_goal_fails_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(no_matching_clause_for_non_terminal_goal(file, 1-2, object, unknown_entities, foo), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(no_matching_clause_for_predicate_goal(File, Lines, Type, Entity, Term), warning(trivial_goal_fails), core, _) :-
		assertz(trivial_goal_fails(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(no_matching_clause_for_non_terminal_goal(File, Lines, Type, Entity, Term), warning(trivial_goal_fails), core, _) :-
		assertz(trivial_goal_fails(File, Lines, Type, Entity, Term)).

:- end_object.

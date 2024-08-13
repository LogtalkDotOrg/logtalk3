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
		date is 2024-08-13,
		comment is 'Unit tests for the ``undefined_predicates`` linter flag.'
	]).

	:- private(undefined_predicate/5).
	:- dynamic(undefined_predicate/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [undefined_predicates(warning)]).

	cleanup :-
		retractall(undefined_predicate(_, _, _, _, _)).

	test(undefined_predicates_linter_flag_01, exists(Term == foo/0)) :-
		undefined_predicate(_, _, object, undefined_predicates, Term).

	test(undefined_predicates_linter_flag_02, exists(Term == quux//0)) :-
		undefined_predicate(_, _, object, undefined_predicates, Term).

	test(undefined_predicates_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(declared_static_predicate_called_but_not_defined(file, 1-2, object, undefined_predicates, foo/1), core), Tokens).

	test(undefined_predicates_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(declared_static_non_terminal_called_but_not_defined(file, 1-2, object, undefined_predicates, foo//1), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(declared_static_predicate_called_but_not_defined(File, Lines, Type, Entity, Term), warning(undefined_predicates), core, _) :-
		assertz(undefined_predicate(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(declared_static_non_terminal_called_but_not_defined(File, Lines, Type, Entity, Term), warning(undefined_predicates), core, _) :-
		assertz(undefined_predicate(File, Lines, Type, Entity, Term)).

:- end_object.

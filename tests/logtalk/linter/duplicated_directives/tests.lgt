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
		comment is 'Unit tests for the ``duplicated_directives`` linter flag.'
	]).

	:- private(duplicated_directive/5).
	:- dynamic(duplicated_directive/5).

	:- private(duplicated_directive/7).
	:- dynamic(duplicated_directive/7).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [duplicated_directives(warning)]).

	cleanup :-
		retractall(duplicated_directive(_, _, _, _, _)),
		retractall(duplicated_directive(_, _, _, _, _, _, _)).

	test(duplicated_directives_linter_flag_01, exists(Term == public(foo/1))) :-
		duplicated_directive(_, _, object, duplicated_scope_directive, Term, _, _).

	test(duplicated_directives_linter_flag_02, exists(Term == multifile(foo/1))) :-
		duplicated_directive(_, _, object, duplicated_multifile_directive, Term, _, _).

	test(duplicated_directives_linter_flag_03, exists(Term == dynamic(foo/1))) :-
		duplicated_directive(_, _, object, duplicated_dynamic_directive, Term, _, _).

	test(duplicated_directives_linter_flag_04, exists(Term == discontiguous(foo/1))) :-
		duplicated_directive(_, _, object, duplicated_discontiguous_directive, Term, _, _).

	test(duplicated_directives_linter_flag_05, exists(Term == meta_predicate(foo(0)))) :-
		duplicated_directive(_, _, object, duplicated_meta_predicate_directive, Term, _, _).

	test(duplicated_directives_linter_flag_06, exists(Term == meta_non_terminal(foo(0)))) :-
		duplicated_directive(_, _, object, duplicated_meta_non_terminal_directive, Term, _, _).

	test(duplicated_directives_linter_flag_07, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(duplicated_directive(file, 1-2, object, duplicated_directives, (:- dynamic(a/1))), core), Tokens).

	test(duplicated_directives_linter_flag_08, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(duplicated_directive(file, 1-2, object, duplicated_directives, (:- dynamic(a/1)), original, 3-4), core), Tokens).

	test(duplicated_directives_linter_flag_09, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(duplicated_predicate_reference(file, 1-2, object, duplicated_directives, a/1, original, 3-4), core), Tokens).

	test(duplicated_directives_linter_flag_10, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(duplicated_non_terminal_reference(file, 1-2, object, duplicated_directives, a//1, original, 3-4), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(duplicated_directive(File, Lines, Type, Entity, Directive), warning(duplicated_directives), core, _) :-
		assertz(duplicated_directive(File, Lines, Type, Entity, Directive)).
	logtalk::message_hook(duplicated_directive(File, Lines, Type, Entity, Directive, OriginalFile, OriginalLines), warning(duplicated_directives), core, _) :-
		assertz(duplicated_directive(File, Lines, Type, Entity, Directive, OriginalFile, OriginalLines)).
	logtalk::message_hook(duplicated_predicate_reference(File, Lines, Type, Entity, Predicate, OriginalFile, OriginalLines), warning(duplicated_directives), core, _) :-
		assertz(duplicated_predicate_reference(File, Lines, Type, Entity, Predicate, OriginalFile, OriginalLines)).
	logtalk::message_hook(duplicated_non_terminal_reference(File, Lines, Type, Entity, NonTerminal, OriginalFile, OriginalLines), warning(duplicated_directives), core, _) :-
		assertz(duplicated_non_terminal_reference(File, Lines, Type, Entity, NonTerminal, OriginalFile, OriginalLines)).

:- end_object.

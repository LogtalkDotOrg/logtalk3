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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-09-02,
		comment is 'Unit tests for the ``missing_directives`` linter flag.'
	]).

	:- private(missing_directive/6).
	:- dynamic(missing_directive/6).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [missing_directives(warning)]).

	cleanup :-
		retractall(missing_directive(_, _, _, _, _, _)).

	test(missing_directives_linter_flag_01, exists(Directive-Term == (multifile)/1-m/2)) :-
		missing_directive(_, _, object, missing_directives, Directive, Term).

	test(missing_directives_linter_flag_02, exists(Directive-Term == (meta_predicate)/1-foo/1)) :-
		missing_directive(_, _, object, missing_directives, Directive, Term).

	test(missing_directives_linter_flag_03, exists(Directive-Term == meta_non_terminal/1-foo//1)) :-
		missing_directive(_, _, object, missing_directives, Directive, Term).

	test(missing_directives_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(missing_scope_directive(file, 1-2, object, missing_directives, (dynamic)/1, a/1), core), Tokens).

	test(missing_directives_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(missing_scope_directive(file, 1-2, object, missing_directives, (dynamic)/1, a//1), core), Tokens).

	test(missing_directives_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(missing_predicate_directive(file, 1-2, object, missing_directives, (dynamic)/1, a), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(missing_scope_directive(File, Lines, Type, Entity, Directive, Term), warning(missing_directives), core, _) :-
		assertz(missing_directive(File, Lines, Type, Entity, Directive, Term)).
	logtalk::message_hook(missing_predicate_directive(File, Lines, Type, Entity, Directive, Term), warning(missing_directives), core, _) :-
		assertz(missing_directive(File, Lines, Type, Entity, Directive, Term)).

:- end_object.

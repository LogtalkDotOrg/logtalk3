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
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2024-11-02,
		comment is 'Unit tests for the ``deprecated`` linter flag.'
	]).

	:- private(deprecated_construct/6).
	:- dynamic(deprecated_construct/6).

	:- private(deprecated_predicate/6).
	:- dynamic(deprecated_predicate/6).

	:- private(deprecated_predicate/5).
	:- dynamic(deprecated_predicate/5).

	:- private(deprecated_function/6).
	:- dynamic(deprecated_function/6).

	:- private(deprecated_function/5).
	:- dynamic(deprecated_function/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [deprecated(warning)]).

	cleanup :-
		retractall(deprecated_construct(_, _, _, _, _, _)),
		retractall(deprecated_predicate(_, _, _, _, _, _)),
		retractall(deprecated_predicate(_, _, _, _, _)).

	test(deprecated_linter_flag_01, exists(Term == assert/1), [condition(predicate_property(assert(_), built_in))]) :-
		deprecated_predicate(_, _, object, deprecated, Term, _).

	test(deprecated_linter_flag_02, exists(Term == (not)/1), [condition(predicate_property(not(_), built_in))]) :-
		deprecated_predicate(_, _, object, deprecated, Term, _).

	test(deprecated_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(deprecated_construct(file, 1-2, object, deprecated, foo, bar), core), Tokens).

	test(deprecated_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(deprecated_predicate(file, 1-2, object, deprecated, foo, bar), core), Tokens).

	test(deprecated_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(deprecated_predicate(file, 1-2, object, deprecated, foo), core), Tokens).

	test(deprecated_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(deprecated_function(file, 1-2, object, deprecated, foo, bar), core), Tokens).

	test(deprecated_linter_flag_07, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(deprecated_function(file, 1-2, object, deprecated, foo), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(deprecated_construct(File, Lines, Type, Entity, Culprit, Alternative), warning(deprecated), core, _) :-
		assertz(deprecated_construct(File, Lines, Type, Entity, Culprit, Alternative)).
	logtalk::message_hook(deprecated_predicate(File, Lines, Type, Entity, Culprit, Alternative), warning(deprecated), core, _) :-
		assertz(deprecated_predicate(File, Lines, Type, Entity, Culprit, Alternative)).
	logtalk::message_hook(deprecated_predicate(File, Lines, Type, Entity, Culprit), warning(deprecated), core, _) :-
		assertz(deprecated_predicate(File, Lines, Type, Entity, Culprit)).
	logtalk::message_hook(deprecated_function(File, Lines, Type, Entity, Culprit, Alternative), warning(deprecated), core, _) :-
		assertz(deprecated_function(File, Lines, Type, Entity, Culprit, Alternative)).
	logtalk::message_hook(deprecated_function(File, Lines, Type, Entity, Culprit), warning(deprecated), core, _) :-
		assertz(deprecated_function(File, Lines, Type, Entity, Culprit)).

:- end_object.

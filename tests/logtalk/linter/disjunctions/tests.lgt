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
		date is 2024-08-14,
		comment is 'Unit tests for the ``disjunctions`` linter flag.'
	]).

	:- private(disjunction_as_body/6).
	:- dynamic(disjunction_as_body/6).

	:- private(suspicious_cut_in_disjunction/5).
	:- dynamic(suspicious_cut_in_disjunction/5).

	:- private(suspicious_cut_in_disjunction/6).
	:- dynamic(suspicious_cut_in_disjunction/6).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [disjunctions(warning)]).

	cleanup :-
		retractall(disjunction_as_body(_, _, _, _, _, _)),
		retractall(suspicious_cut_in_disjunction(_, _, _, _, _)),
		retractall(suspicious_cut_in_disjunction(_, _, _, _, _, _)).

	test(disjunctions_linter_flag_01, exists(Head == foo)) :-
		disjunction_as_body(_, _, object, disjunctions, Head, _).

	test(disjunctions_linter_flag_02, exists(Disjunction == (!, a(1); a(2)))) :-
		suspicious_cut_in_disjunction(_, _, object, disjunctions, _, Disjunction).

	test(disjunctions_linter_flag_03, exists(Head == quux)) :-
		disjunction_as_body(_, _, object, disjunctions, Head, _).

	test(disjunctions_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(disjunction_as_body(file, 1-2, object, disjunctions, foo, (a;b)), core), Tokens).

	test(disjunctions_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_cut_in_disjunction(file, 1-2, object, disjunctions, foo), core), Tokens).

	test(disjunctions_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_cut_in_disjunction(file, 1-2, object, disjunctions, foo, (a;b)), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(disjunction_as_body(File, Lines, Type, Entity, Head, Disjunction), warning(disjunctions), core, _) :-
		assertz(disjunction_as_body(File, Lines, Type, Entity, Head, Disjunction)).
	logtalk::message_hook(suspicious_cut_in_disjunction(File, Lines, Type, Entity, Head), warning(disjunctions), core, _) :-
		assertz(suspicious_cut_in_disjunction(File, Lines, Type, Entity, Head)).
	logtalk::message_hook(suspicious_cut_in_disjunction(File, Lines, Type, Entity, Head, Disjunction), warning(disjunctions), core, _) :-
		assertz(suspicious_cut_in_disjunction(File, Lines, Type, Entity, Head, Disjunction)).

:- end_object.

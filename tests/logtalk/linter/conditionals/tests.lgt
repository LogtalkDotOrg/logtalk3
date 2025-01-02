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
		date is 2024-08-14,
		comment is 'Unit tests for the ``conditionals`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(suspicious_cut_in_if_then_else/5).
	:- dynamic(suspicious_cut_in_if_then_else/5).

	:- private(suspicious_cut_in_if_then_else/6).
	:- dynamic(suspicious_cut_in_if_then_else/6).

	:- private(suspicious_cut_in_soft_cut/5).
	:- dynamic(suspicious_cut_in_soft_cut/5).

	:- private(suspicious_cut_in_soft_cut/6).
	:- dynamic(suspicious_cut_in_soft_cut/6).

	:- private(suspicious_if_then_else_test/6).
	:- dynamic(suspicious_if_then_else_test/6).

	:- private(suspicious_soft_cut_test/6).
	:- dynamic(suspicious_soft_cut_test/6).

	:- private(missing_else_part/5).
	:- dynamic(missing_else_part/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [conditionals(warning)]).

	cleanup :-
		retractall(suspicious_cut_in_if_then_else(_, _, _, _, _)),
		retractall(suspicious_cut_in_if_then_else(_, _, _, _, _, _)),
		retractall(suspicious_cut_in_soft_cut(_, _, _, _, _)),
		retractall(suspicious_cut_in_soft_cut(_, _, _, _, _, _)),
		retractall(suspicious_if_then_else_test(_, _, _, _, _, _)),
		retractall(suspicious_soft_cut_test(_, _, _, _, _, _)),
		retractall(missing_else_part(_, _, _, _, _)).

	test(conditionals_linter_flag_01, exists(Term == (b -> c))) :-
		missing_else_part(_, _, object, conditionals, Term).

	test(conditionals_linter_flag_02, exists(Term == p)) :-
		suspicious_cut_in_if_then_else(_, _, object, conditionals, Term).

	test(conditionals_linter_flag_03, exists(Term == qux)) :-
		suspicious_cut_in_if_then_else(_, _, object, conditionals, Term, _).

	test(conditionals_linter_flag_04, exists(variant(Term, quux(_)))) :-
		suspicious_if_then_else_test(_, _, object, conditionals, Term, _).

	test(conditionals_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_cut_in_if_then_else(file, 1-2, object, conditionals, a), core), Tokens).

	test(conditionals_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_cut_in_if_then_else(file, 1-2, object, conditionals, a, b), core), Tokens).

	test(conditionals_linter_flag_07, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_cut_in_soft_cut(file, 1-2, object, conditionals, a), core), Tokens).

	test(conditionals_linter_flag_08, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_cut_in_soft_cut(file, 1-2, object, conditionals, a, b), core), Tokens).

	test(conditionals_linter_flag_09, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_if_then_else_test(file, 1-2, object, conditionals, a, b), core), Tokens).

	test(conditionals_linter_flag_10, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_soft_cut_test(file, 1-2, object, conditionals, a, b), core), Tokens).

	test(conditionals_linter_flag_11, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(missing_else_part(file, 1-2, object, conditionals, a), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(suspicious_cut_in_if_then_else(File, Lines, Type, Entity, Head), warning(conditionals), core, _) :-
		assertz(suspicious_cut_in_if_then_else(File, Lines, Type, Entity, Head)).
	logtalk::message_hook(suspicious_cut_in_if_then_else(File, Lines, Type, Entity, Head, If), warning(conditionals), core, _) :-
		assertz(suspicious_cut_in_if_then_else(File, Lines, Type, Entity, Head, If)).
	logtalk::message_hook(suspicious_cut_in_soft_cut(File, Lines, Type, Entity, Head), warning(conditionals), core, _) :-
		assertz(suspicious_cut_in_soft_cut(File, Lines, Type, Entity, Head)).
	logtalk::message_hook(suspicious_cut_in_soft_cut(File, Lines, Type, Entity, Head, If), warning(conditionals), core, _) :-
		assertz(suspicious_cut_in_soft_cut(File, Lines, Type, Entity, Head, If)).
	logtalk::message_hook(suspicious_if_then_else_test(File, Lines, Type, Entity, Head, If), warning(conditionals), core, _) :-
		assertz(suspicious_if_then_else_test(File, Lines, Type, Entity, Head, If)).
	logtalk::message_hook(suspicious_soft_cut_test(File, Lines, Type, Entity, Head, If), warning(conditionals), core, _) :-
		assertz(suspicious_soft_cut_test(File, Lines, Type, Entity, Head, If)).
	logtalk::message_hook(missing_else_part(File, Lines, Type, Entity, IfThen), warning(conditionals), core, _) :-
		assertz(missing_else_part(File, Lines, Type, Entity, IfThen)).

:- end_object.

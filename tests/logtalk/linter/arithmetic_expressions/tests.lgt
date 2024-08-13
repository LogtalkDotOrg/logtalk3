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
		comment is 'Unit tests for the ``arithmetic_expressions`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(suspicious_call/5).
	:- dynamic(suspicious_call/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [arithmetic_expressions(warning)]).

	cleanup :-
		retractall(suspicious_call(_, _, _, _, _)).

	test(arithmetic_expressions_linter_flag_01, exists(variant(Term, A == 2.0))) :-
		suspicious_call(_, _, object, arithmetic_expressions, Term).

	test(arithmetic_expressions_linter_flag_02, exists(variant(Term, 2.0 =:= A))) :-
		suspicious_call(_, _, object, arithmetic_expressions, Term).

	test(arithmetic_expressions_linter_flag_03, exists(variant(Term, sqrt(_) =:= sin(_)))) :-
		suspicious_call(_, _, object, arithmetic_expressions, Term).

	test(arithmetic_expressions_linter_flag_04, exists(variant(Term, 3.0 \= 3))) :-
		suspicious_call(_, _, object, arithmetic_expressions, Term).

	test(arithmetic_expressions_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, arithmetic_expressions, 1, reason(comparing_numbers_using_unification)), core), Tokens).

	test(arithmetic_expressions_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, arithmetic_expressions, 1, reason(float_comparison)), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(suspicious_call(File, Lines, Type, Entity, Term, reason(comparing_numbers_using_unification)), warning(arithmetic_expressions), core, _) :-
		assertz(suspicious_call(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(suspicious_call(File, Lines, Type, Entity, Term, reason(float_comparison)), warning(arithmetic_expressions), core, _) :-
		assertz(suspicious_call(File, Lines, Type, Entity, Term)).

:- end_object.

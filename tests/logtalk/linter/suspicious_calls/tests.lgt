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
		date is 2024-08-16,
		comment is 'Unit tests for the ``suspicious_calls`` linter flag.'
	]).

	:- private(suspicious_call/6).
	:- dynamic(suspicious_call/6).

	setup :-
		cleanup.

	cleanup :-
		retractall(suspicious_call(_, _, _, _, _, _)).

	test(suspicious_calls_linter_flag_01, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, a, [b, c]), core), Tokens).

	test(suspicious_calls_linter_flag_02, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, repeat, reason(repeat(head))), core), Tokens).

	test(suspicious_calls_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, 1 = _, reason(comparing_numbers_using_unification)), core), Tokens).

	test(suspicious_calls_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, !, reason(multifile(head))), core), Tokens).

	test(suspicious_calls_linter_flag_05, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, _ = _, reason(cyclic_terms)), core), Tokens).

	test(suspicious_calls_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, _ = _, reason(no_variable_bindings_after_unification)), core), Tokens).

	test(suspicious_calls_linter_flag_07, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, _ = _, reason(comparing_numbers_using_unification)), core), Tokens).

	test(suspicious_calls_linter_flag_08, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, call, reason(no_shared_variables(call))), core), Tokens).

	test(suspicious_calls_linter_flag_09, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, qgoal, reason(existential_variables([_, _, _],goal))), core), Tokens).

	test(suspicious_calls_linter_flag_10, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, qgoal, reason(singleton_variables(foo/3,qgoal,[_, _, _]))), core), Tokens).

	test(suspicious_calls_linter_flag_11, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, Term is _, reason(shared_variable(Term))), core), Tokens).

	test(suspicious_calls_linter_flag_12, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, _ =:= _, reason(float_comparison)), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(suspicious_call(File, Lines, Type, Entity, Goal, Term), warning(suspicious_calls), core, _) :-
		assertz(catchall_catch(File, Lines, Type, Entity, Goal, Term)).

:- end_object.

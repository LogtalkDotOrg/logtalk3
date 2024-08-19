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
		date is 2024-08-19,
		comment is 'Unit tests for the ``suspicious_calls`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(suspicious_call/6).
	:- dynamic(suspicious_call/6).

	setup :-
%		cleanup,
		logtalk_compile(test_entities, [suspicious_calls(warning)]).

%	cleanup :-
%		retractall(suspicious_call(_, _, _, _, _, _)).

	test(suspicious_calls_linter_flag_01, exists(variant(Call-Alternatives, (::recursive(A))-[recursive(A), ^^recursive(A)]))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Alternatives).

	test(suspicious_calls_linter_flag_02, exists(variant(Call-Alternatives, (_::bar)-[::bar]))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Alternatives).

	test(suspicious_calls_linter_flag_03, exists(variant(Call-Alternatives, (_::baz)-[baz]))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Alternatives).

	test(suspicious_calls_linter_flag_04, exists(Call-Reason == !-reason(multifile(multi)))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_05, exists(Call-Reason == !-reason(multifile(logtalk::message_prefix_stream(comment, foo, ':> ', user_error))))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_06, exists(variant(Call-Alternatives, findall(_,a(A),_)-[(a(A),fail;true)]))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Alternatives).

	test(suspicious_calls_linter_flag_07, exists(variant(Call-Reason, bagof(_,a(_),_)-reason(no_shared_variables(bagof))))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_08, exists(variant(Call-Reason, a(A)-reason(singleton_variables(bagof/3,a(A),[A]))))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_09, exists(variant(Call-Reason, (a(1,A)=a(1,A))-reason(no_variable_bindings_after_unification)))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_10, exists(variant(Call-Reason, a(A)-reason(singleton_variables(bagof/3,a(A),[A]))))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_11, exists(variant(Call-Reason, repeat-reason(repeat(foo(_)))))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_12, exists(variant(Call-Reason, (A is _*A)-reason(shared_variable(A))))) :-
		suspicious_call(_, _, object, suspicious_calls, Call, Reason).

	test(suspicious_calls_linter_flag_13, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, a, [b, c]), core), Tokens).

	test(suspicious_calls_linter_flag_14, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, repeat, reason(repeat(head))), core), Tokens).

	test(suspicious_calls_linter_flag_15, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, 1 = _, reason(comparing_numbers_using_unification)), core), Tokens).

	test(suspicious_calls_linter_flag_16, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, !, reason(multifile(head))), core), Tokens).

	test(suspicious_calls_linter_flag_17, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, _ = _, reason(cyclic_terms)), core), Tokens).

	test(suspicious_calls_linter_flag_18, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, _ = _, reason(no_variable_bindings_after_unification)), core), Tokens).

	test(suspicious_calls_linter_flag_19, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, call, reason(no_shared_variables(call))), core), Tokens).

	test(suspicious_calls_linter_flag_20, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, qgoal, reason(existential_variables([_, _, _],goal))), core), Tokens).

	test(suspicious_calls_linter_flag_21, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, qgoal, reason(singleton_variables(foo/3,qgoal,[_, _, _]))), core), Tokens).

	test(suspicious_calls_linter_flag_22, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(suspicious_call(file, 1-2, object, suspicious_calls, Term is _, reason(shared_variable(Term))), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(suspicious_call(File, Lines, Type, Entity, Goal, Term), warning(suspicious_calls), core, _) :-
		assertz(suspicious_call(File, Lines, Type, Entity, Goal, Term)).

:- end_object.

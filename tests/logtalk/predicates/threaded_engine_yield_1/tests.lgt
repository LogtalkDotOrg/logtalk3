%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-08-20,
		comment is 'Unit tests for the threaded_engine_yield/1 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% calls outside the context of an engine fail
	test(threaded_engine_yield_1_01, false) :-
		{threaded_engine_yield(_)}.

	% no restrictions on the kind of terms that can be returned as answers

	test(threaded_engine_yield_1_02, true(Answer == foo)) :-
		{threaded_engine_create(none, tests::return_atom, test_engine_1),
		 threaded_engine_next(test_engine_1, Answer)}.

	test(threaded_engine_yield_1_03, variant(Answer, f(X,_,X))) :-
		{threaded_engine_create(none, tests::return_compound, test_engine_2),
		 threaded_engine_next(test_engine_2, Answer)}.

	test(threaded_engine_yield_1_04, true(var(Answer))) :-
		{threaded_engine_create(none, tests::return_var, test_engine_3),
		 threaded_engine_next(test_engine_3, Answer)}.

	% auxiliary predicates

	:- public(return_atom/0).
	return_atom :-
		{threaded_engine_yield(foo)}.

	:- public(return_compound/0).
	return_compound :-
		{threaded_engine_yield(f(X,_,X))}.

	:- public(return_var/0).
	return_var :-
		{threaded_engine_yield(_)}.

:- end_object.

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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-08-20,
		comment is 'Unit tests for the threaded_engine_next_reified/2 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% engine name must be bound at runtime (but no error at compile time)
	test(threaded_engine_next_reified_2_01, ball(error(instantiation_error, logtalk(threaded_engine_next_reified(_,_), _)))) :-
		{threaded_engine_next_reified(_, _)}.

	% engine must exist
	test(threaded_engine_next_reified_2_02, ball(error(existence_error(engine,foo), logtalk(threaded_engine_next_reified(foo,_), _)))) :-
		{threaded_engine_next_reified(foo, _)}.

	% create engine for the following tests
	test(threaded_engine_next_reified_2_03, true) :-
		{threaded_engine_create(X, tests::a(X), test_engine_1)}.

	% all solutions must be retrievable
	test(threaded_engine_next_reified_2_04, true(xyz(X,Y,Z) == xyz(the(1),the(2),the(3)))) :-
		{threaded_engine_next_reified(test_engine_1, X),
		 threaded_engine_next_reified(test_engine_1, Y),
		 threaded_engine_next_reified(test_engine_1, Z)}.

	% no more answers
	test(threaded_engine_next_reified_2_05, true(Answer == no)) :-
		{threaded_engine_next_reified(test_engine_1, Answer)}.

	% no more answers (must keep failing)
	test(threaded_engine_next_reified_2_06, true(Answer == no)) :-
		{threaded_engine_next_reified(test_engine_1, Answer)}.

	% engine with no goal solutions
	test(threaded_engine_next_reified_2_07, true(Answer == no)) :-
		{threaded_engine_create(_, fail, test_engine_2),
		 threaded_engine_next_reified(test_engine_2, Answer)}.

	% engine with a goal that throws an exception
	test(threaded_engine_next_reified_2_08, true(Answer == exception(error))) :-
		{threaded_engine_create(_, throw(error), test_engine_3),
		 threaded_engine_next_reified(test_engine_3, Answer)}.

	% after the exception, there cannot be any solutions
	test(threaded_engine_next_reified_2_09, true(Answer == no)) :-
		{threaded_engine_next_reified(test_engine_3, Answer)}.

	% auxiliary predicates

	:- public(a/1).
	a(1).
	a(2).
	a(3).

:- end_object.

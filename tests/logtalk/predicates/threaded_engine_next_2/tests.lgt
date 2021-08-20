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
		comment is 'Unit tests for the threaded_engine_next/2 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% engine name must be bound at runtime (but no error at compile time)
	test(threaded_engine_next_2_01, ball(error(instantiation_error, logtalk(threaded_engine_next(_,_), _)))) :-
		{threaded_engine_next(_, _)}.

	% engine must exist
	test(threaded_engine_next_2_02, ball(error(existence_error(engine,foo), logtalk(threaded_engine_next(foo,_), _)))) :-
		{threaded_engine_next(foo, _)}.

	% create engine for the following tests
	test(threaded_engine_next_2_03, true) :-
		{threaded_engine_create(X, tests::a(X), test_engine_1)}.

	% all solutions must be retrievable
	test(threaded_engine_next_2_04, true(xyz(X,Y,Z) == xyz(1,2,3))) :-
		{threaded_engine_next(test_engine_1, X),
		 threaded_engine_next(test_engine_1, Y),
		 threaded_engine_next(test_engine_1, Z)}.

	% no more answers
	test(threaded_engine_next_2_05, false) :-
		{threaded_engine_next(test_engine_1, _)}.

	% no more answers (must keep failing)
	test(threaded_engine_next_2_06, false) :-
		{threaded_engine_next(test_engine_1, _)}.

	% engine with no goal solutions
	test(threaded_engine_next_2_07, false) :-
		{threaded_engine_create(_, fail, test_engine_2),
		 threaded_engine_next(test_engine_2, _)}.

	% engine with a goal that throws an exception
	test(threaded_engine_next_2_08, ball(error(error, logtalk(threaded_engine_next(_,_), _)))) :-
		{threaded_engine_create(_, throw(error), test_engine_3),
		 threaded_engine_next(test_engine_3, _)}.

	% after the exception, there cannot be any solutions
	test(threaded_engine_next_2_09, false) :-
		{threaded_engine_next(test_engine_3, _)}.

	% auxiliary predicates

	:- public(a/1).
	a(1).
	a(2).
	a(3).

:- end_object.

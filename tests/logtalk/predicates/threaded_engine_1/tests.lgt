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
		comment is 'Unit tests for the threaded_engine/1 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% no engines exist
	test(threaded_engine_1_01, false) :-
		{threaded_engine(_)}.

	% no engine named "foo" exists
	test(threaded_engine_1_02, false) :-
		{threaded_engine(foo)}.

	% engine with a single solution
	test(threaded_engine_1_03, true) :-
		{threaded_engine_create(none, true, Engine),
		 threaded_engine(Engine),
		 threaded_engine_destroy(Engine)}.

	% engine with multiple solutions
	test(threaded_engine_1_04, true) :-
		{threaded_engine_create(none, repeat, Engine),
		 threaded_engine(Engine),
		 threaded_engine_destroy(Engine)}.

	% engine with no solutions
	test(threaded_engine_1_05, true) :-
		{threaded_engine_create(none, fail, Engine),
		 threaded_engine(Engine),
		 threaded_engine_destroy(Engine)}.

	% the predicate must also work as expected
	% when called with an unbound argument
	test(threaded_engine_1_06, true(ReturnedEngine == Engine)) :-
		{threaded_engine_create(none, true, Engine),
		 threaded_engine(ReturnedEngine),
		 threaded_engine_destroy(Engine)}.

	% all existing engines must be returned
	test(threaded_engine_1_07, true(Engines == [test_engine_1, test_engine_2, test_engine_3])) :-
		{threaded_engine_create(none, true, test_engine_1),
		 threaded_engine_create(none, true, test_engine_2),
		 threaded_engine_create(none, true, test_engine_3),
		 setof(Engine, threaded_engine(Engine), Engines)}.

:- end_object.

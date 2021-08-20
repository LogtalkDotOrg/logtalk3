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
		comment is 'Unit tests for the threaded_engine_self/1 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% test calling threaded_engine_self/1 with unbound argument
	test(threaded_engine_self_1_01, true(Engine == test_engine_1)) :-
		{threaded_engine_create(none, tests::return, test_engine_1),
		 threaded_engine_next(test_engine_1, Engine)}.

	% test calling threaded_engine_self/1 with bound and correct argument
	test(threaded_engine_self_1_02, true(Answer == none)) :-
		{threaded_engine_create(none, tests::correct, test_engine_2),
		 threaded_engine_next(test_engine_2, Answer)}.

	% test calling threaded_engine_self/1 with bound but incorrect argument
	test(threaded_engine_self_1_03, false) :-
		{threaded_engine_create(none, tests::wrong, test_engine_3),
		 threaded_engine_next(test_engine_3, _)}.

	% calls outside the context of an engine must fail
	test(threaded_engine_self_1_04, false) :-
		{threaded_engine_self(_)}.

	% auxiliary predicates

	:- public(return/0).
	return :-
		{threaded_engine_self(Engine),
		 threaded_engine_yield(Engine)}.

	:- public(correct/0).
	correct :-
		{threaded_engine_self(test_engine_2)}.

	:- public(wrong/0).
	wrong :-
		{threaded_engine_self(wrong)}.

:- end_object.

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
		comment is 'Unit tests for the threaded_engine_fetch/1 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% must be able to fetch a posted term
	test(threaded_engine_fetch_1_01, true(Term == term)) :-
		{threaded_engine_create(none, tests::boomerang, test_engine_1),
		 threaded_engine_post(test_engine_1, term),
		 threaded_engine_next(test_engine_1, Term)}.

	% engine term queue must be, well, a queue
	test(threaded_engine_fetch_1_02, true(t(Term1,Term2,Term3) == t(term1,term2,term3))) :-
		{threaded_engine_create(none, tests::loop, test_engine_2),
		 threaded_engine_post(test_engine_2, term1),
		 threaded_engine_post(test_engine_2, term2),
		 threaded_engine_post(test_engine_2, term3),
		 threaded_engine_next(test_engine_2, Term1),
		 threaded_engine_next(test_engine_2, Term2),
		 threaded_engine_next(test_engine_2, Term3)}.

	% calls outside the context of an engine fail
	test(threaded_engine_fetch_1_03, false) :-
		{threaded_engine_fetch(_)}.

	% auxiliary predicates

	:- public(boomerang/0).
	boomerang :-
		{threaded_engine_fetch(Term),
		 threaded_engine_yield(Term)}.

	:- public(loop/0).
	loop :-
		{threaded_engine_fetch(Term),
		 threaded_engine_yield(Term)},
		loop.

:- end_object.

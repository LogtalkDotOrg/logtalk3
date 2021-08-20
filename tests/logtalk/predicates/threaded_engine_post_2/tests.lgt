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
		comment is 'Unit tests for the threaded_engine_post/2 built-in predicate.'
	]).

	condition :-
		current_logtalk_flag(engines, supported).

	% engine argument must be bound at runtime (but no error at compile time)
	test(threaded_engine_post_2_01, ball(error(instantiation_error, logtalk(threaded_engine_post(_,_),_)))) :-
		{threaded_engine_post(_, _)}.

	% engine must exist
	test(threaded_engine_post_2_02, ball(error(existence_error(engine,foo), logtalk(threaded_engine_post(foo,_),_)))) :-
		{threaded_engine_post(foo, _)}.

	% posting terms to an engine term queue is independent of
	% the status of the engine goal and its solutions if any

	test(threaded_engine_post_2_03, true) :-
		{threaded_engine_create(none, repeat, test_engine_1),
		 threaded_engine_post(test_engine_1, term)}.

	test(threaded_engine_post_2_04, true) :-
		{threaded_engine_create(none, true, test_engine_2),
		 threaded_engine_post(test_engine_1, term)}.

	test(threaded_engine_post_2_05, true) :-
		{threaded_engine_create(none, fail, test_engine_3),
		 threaded_engine_post(test_engine_1, term)}.

	test(threaded_engine_post_2_06, true) :-
		{threaded_engine_create(none, throw(error), test_engine_4),
		 threaded_engine_post(test_engine_1, term)}.

:- end_object.

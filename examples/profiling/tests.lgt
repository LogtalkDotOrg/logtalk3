%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		date is 2023-11-22,
		comment is 'Unit tests for the "profiling" example.'
	]).

	:- set_logtalk_flag(events, allow).

	setup :-
		message_counter::stop,
		message_counter::set_spy_point(_, list, _, _),
		message_counter::activate_monitor.

	test(profiling_message_counter_object, true(Calls-Exits == 2-4)) :-
		(	list::empty([]),
			list::member(_, [1, 2, 3]),
			fail
		;	message_counter::counts(list, Calls, Exits)
		).

	test(profiling_message_counter_object_predicate_1, true(Calls-Exits == 1-1)) :-
		message_counter::counts(list, empty/1, Calls, Exits).

	test(profiling_message_counter_object_predicate_2, true(Calls-Exits == 1-3)) :-
		message_counter::counts(list, member/2, Calls, Exits).

	cleanup :-
		message_counter::stop.

:- end_object.

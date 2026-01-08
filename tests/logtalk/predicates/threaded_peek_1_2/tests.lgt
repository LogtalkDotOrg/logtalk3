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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2023-01-30,
		comment is 'Unit tests for the threaded_peek/1-2 built-in predicates.'
	]).

	:- threaded.

	% threaded_peek/1 tests

	test(threaded_peek_1_01, error(existence_error(thread,tests))) :-
		threaded_peek(true).

	test(threaded_peek_1_02, error(instantiation_error)) :-
		threaded_call(true),
		threaded_peek(_).

	test(threaded_peek_1_03, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_call(true),
		threaded_peek(Int).

	% threaded_peek/2 tests

	test(threaded_peek_2_01, error(instantiation_error)) :-
		threaded_peek(a, _).

	test(threaded_peek_2_02, error(instantiation_error)) :-
		threaded_call(true, Tag),
		threaded_peek(_, Tag).

	test(threaded_peek_2_03, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_call(true, Tag),
		threaded_peek(Int, Tag).

	test(threaded_peek_2_04, true) :-
		threaded_call(true, Tag),
		thread_sleep(1),
		threaded_peek(true, Tag).

	test(threaded_peek_2_05, true) :-
		threaded_once(true, Tag),
		thread_sleep(1),
		threaded_peek(true, Tag).

	% auxiliary predicates

	int(1).

	a.

:- end_object.

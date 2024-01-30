%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-01-30,
		comment is 'Unit tests for the threaded_exit/1-2 built-in predicates.'
	]).

	:- threaded.

	% threaded_exit/1 tests

	test(threaded_exit_1_01, error(instantiation_error)) :-
		threaded_call(true),
		threaded_exit(_).

	test(threaded_exit_1_02, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_call(true),
		threaded_exit(Int).

	% threaded_exit/2 tests

	test(threaded_exit_2_01, error(instantiation_error)) :-
		threaded_exit(a, _).

	test(threaded_exit_2_02, error(instantiation_error)) :-
		threaded_call(true, Tag),
		threaded_exit(_, Tag).

	test(threaded_exit_2_03, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_call(true, Tag),
		threaded_exit(Int, Tag).

	test(threaded_exit_2_04, true) :-
		threaded_call(true, Tag),
		threaded_exit(true, Tag).

	test(threaded_exit_2_05, true(L == [1,2,3])) :-
		threaded_call(list::member(X,[1,2,3]), Tag),
		findall(X, threaded_exit(list::member(X,[1,2,3]), Tag), L).

	test(threaded_exit_2_06, true) :-
		threaded_once(true, Tag),
		threaded_exit(true, Tag).

	% auxiliary predicates

	int(1).

	a.

:- end_object.

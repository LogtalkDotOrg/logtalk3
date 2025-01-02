%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the threaded_once/1-2 built-in predicates.'
	]).

	:- threaded.

	% threaded_once/1 tests

	test(threaded_once_1_01, error(instantiation_error)) :-
		% delay the error to runtime
		variable(Var),
		threaded_once(Var).

	test(threaded_once_1_02, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_once(Int).

	% threaded_once/2 tests

	test(threaded_once_2_01, error(instantiation_error)) :-
		% delay the error to runtime
		variable(Var),
		threaded_once(Var, _).

	test(threaded_once_2_02, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_once(Int, _).

	test(threaded_once_2_03, error(uninstantiation_error(Tag))) :-
		% delay the error to runtime
		bind(Tag),
		threaded_once(true, Tag).

	% auxiliary predicates

	variable(_).

	int(1).

	bind(tag).

:- end_object.

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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-01-30,
		comment is 'Unit tests for the threaded_ignore/1 built-in predicate.'
	]).

	:- threaded.

	test(threaded_ignore_1_01, error(instantiation_error)) :-
		threaded_ignore(_).

	test(threaded_ignore_1_02, error(type_error(callable, Int))) :-
		% delay the error to runtime
		int(Int),
		threaded_ignore(Int).

	test(threaded_ignore_1_03, true) :-
		threaded_ignore(true).

	test(threaded_ignore_1_04, true) :-
		threaded_ignore(fail).

	test(threaded_ignore_1_05, true) :-
		threaded_ignore(throw(err)).

	% auxiliary predicates

	int(1).

:- end_object.

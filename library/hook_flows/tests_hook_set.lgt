%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests_hook_set,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2024-09-27,
		comment is 'Unit tests for the "hook_flows" library hook_set/1 object.'
	]).

	% tests for terms and goals that should be expanded

	test(hook_set_1_01, true(N == 42)) :-
		test_object_2::bb(N).

	test(hook_set_1_02, true(N == 60)) :-
		test_object_2::p(N).

	test(hook_set_1_03, true(X == c)) :-
		phrase(test_object_2::g(a,X), []).

	% tests for terms and goals that shouldn't be expanded

	test(hook_pipeline_1_05, true) :-
		test_object_1::foo.

	test(hook_pipeline_1_06, true) :-
		test_object_1::bar.

	test(hook_pipeline_1_07, true) :-
		phrase(test_object_1::qux, [quux]).

:- end_object.

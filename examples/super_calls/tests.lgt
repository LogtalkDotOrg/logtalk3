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
		date is 2020-12-24,
		comment is 'Unit tests for the "super_calls" example.'
	]).

	cover(parent).
	cover(prototype).
	cover(top).
	cover(middle).
	cover(bottom).

	test(super_calls_01, true(Local == parent)) :-
		parent::get_local(Local).

	test(super_calls_02, true(Local == prototype)) :-
		prototype::get_local(Local).

	test(super_calls_03, true(Local == prototype)) :-
		prototype::correct(Local).

	test(super_calls_04, true(Local == parent)) :-
		prototype::wrong(Local).

	test(super_calls_05, true(Value == parent)) :-
		bottom::value(Value).

	test(super_calls_06, true(Value == middle)) :-
		middle::assertz(d(middle)),
		bottom::value(Value).

	test(super_calls_07, true(Value == parent)) :-
		middle::retractall(d(_)),
		bottom::value(Value).

:- end_object.

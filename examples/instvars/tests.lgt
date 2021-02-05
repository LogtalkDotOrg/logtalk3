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
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-25,
		comment is 'Unit tests for the "instvars" example.'
	]).

	cover(root).
	cover(instance1).
	cover(instance2).
	cover(instance3).

	test(instvars_01, true(Value1 == 0)) :-
		instance1::ivar(Value1).

	test(instvars_02, true(Value2 == 0)) :-
		instance2::ivar(Value2).

	test(instvars_03, true(Value3 == 0)) :-
		instance3::ivar(Value3).

	test(instvars_04) :-
		instance1::set_ivar(1).

	test(instvars_05, true(Value1 == 1)) :-
		instance1::ivar(Value1).

	test(instvars_06, true(Value2 == 0)) :-
		instance2::ivar(Value2).

	test(instvars_07, true(Value3 == 0)) :-
		instance3::ivar(Value3).

:- end_object.

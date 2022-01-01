%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Parker Jones and Paulo Moura',
		date is 2021-06-12,
		comment is 'Unit tests for the "threads/functions" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(functions_01, true(Zero =~= 2.0)) :-
		bisection::find_root(f1, 1.0, 2.3, 1.0e-15, Zero).

	test(functions_02, true(Zero =~= 2.0)) :-
		newton::find_root(f1, 1.0, 2.3, 1.0e-15, Zero).

	test(functions_03, true(Zero =~= 2.0)) :-
		muller::find_root(f1, 1.0, 2.3, 1.0e-15, Zero).

	test(functions_04, true(Zero =~= 1.25809265664599)) :-
		bisection::find_root(f2, 1.0, 1.3, 1.0e-15, Zero).

	test(functions_05, true(Zero =~= 1.25809265664599)) :-
		newton::find_root(f2, 1.0, 1.3, 1.0e-15, Zero).

	test(functions_06, true(Zero =~= 1.25809265664599)) :-
		muller::find_root(f2, 1.0, 1.3, 1.0e-15, Zero).

	test(functions_07, false) :-
		bisection::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	test(functions_08, true(Zero =~= 1.29954968258)) :-
		muller::find_root(humps, -1.0, 2.0, 1.0e-15, Zero).

	test(functions_09, error(evaluation_error(float_overflow))) :-
		newton::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	test(functions_10, true(Zero =~= 2.0)) :-
		function_root::find_root(f1, 1.0, 2.3, 1.0e-15, Zero, _).

	test(functions_11, true(Zero =~= 1.25809265664599)) :-
		function_root::find_root(f2, 1.0, 1.3, 1.0e-15, Zero, _).

	test(functions_12, true(Zero =~= 1.4142135623731)) :-
		function_root::find_root(f3, 0.0, 3.0, 1.0e-15, Zero, _).

	test(functions_13, true(Zero =~= -8.88178419700125e-16)) :-
		function_root::find_root(f4, -1.0, 2.0, 1.0e-15, Zero, _).

	test(functions_14, true(Zero =~= 1.29954968258482)) :-
		function_root::find_root(humps, -1.0, 2.0, 1.0e-15, Zero, _).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/functions" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	:- discontiguous(succeeds/1).

	succeeds(functions_01) :-
		bisection::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_02) :-
		newton::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_03) :-
		muller::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_04) :-
		bisection::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	succeeds(functions_05) :-
		newton::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	succeeds(functions_06) :-
		muller::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	fails(functions_07) :-
		bisection::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	succeeds(functions_08) :-
		muller::find_root(humps, -1.0, 2.0, 1.0e-15, Zero),
		Zero =~= 1.29954968258.

	throws(functions_09, error(evaluation_error(float_overflow), _)) :-
		newton::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	succeeds(functions_10) :-
		function_root::find_root(f1, 1.0, 2.3, 1.0e-15, Zero, _),
		Zero =~= 2.0.

	succeeds(functions_11) :-
		function_root::find_root(f2, 1.0, 1.3, 1.0e-15, Zero, _),
		Zero =~= 1.25809265664599.

	succeeds(functions_12) :-
		function_root::find_root(f3, 0.0, 3.0, 1.0e-15, Zero, _),
		Zero =~= 1.4142135623731.

	succeeds(functions_13) :-
		function_root::find_root(f4, -1.0, 2.0, 1.0e-15, Zero, _),
		Zero =~= -8.88178419700125e-16.

	succeeds(functions_14) :-
		function_root::find_root(humps, -1.0, 2.0, 1.0e-15, Zero, _),
		Zero =~= 1.29954968258482.

:- end_object.

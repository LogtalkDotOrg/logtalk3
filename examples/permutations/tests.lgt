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


:- object(tests(_Size_, _Repetitions_, _Clock_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paul Tarau; adapted to Logtalk by Paulo Moura',
		date is 2024-12-06,
		comment is 'Unit tests for the "permutations" example.'
	]).

	:- uses(lgtunit, [
		benchmark/4
	]).

	test(backtracking, true) :-
		benchmark(permutations::backtracking(_Size_), _Repetitions_, _Clock_, _).

	test(list, true) :-
		benchmark(permutations::list(_Size_, _), _Repetitions_, _Clock_, _).

	test(all, true) :-
		benchmark(permutations::all(_Size_, _), _Repetitions_, _Clock_, _).

	test(map, true) :-
		benchmark(permutations::map(_Size_, _), _Repetitions_, _Clock_, _).

	test(copy, true) :-
		benchmark(permutations::copy(_Size_), _Repetitions_, _Clock_, _).

:- end_object.

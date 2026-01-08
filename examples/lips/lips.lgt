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


:- object(lips).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-03-19,
		comment is 'Classical naive list reverse benchmark. Computes million of logical inferences per second.'
	]).

	:- public(mlips/1).
	:- mode(mlips(-integer), one).
	:- info(mlips/1, [
		comment is 'Runs the benchmark 100000 times with the default list of 30 elements.',
		argnames is ['MLIPS']
	]).

	:- public(mlips/2).
	:- mode(mlips(+integer, -integer), one).
	:- info(mlips/2, [
		comment is 'Runs the benchmark the given number of times with the default list of 30 elements.',
		argnames is ['Repetitions', 'MLIPS']
	]).

	:- uses(os, [cpu_time/1]).
	:- uses(integer, [between/3, sequence/3]).

	mlips(MLIPS) :-
		mlips(100000, MLIPS).

	mlips(Repetitions, MLIPS) :-
		sequence(1, 30, List),
		cpu_time(Time0),
		dummy_loop(Repetitions, List),
		cpu_time(Time1),
		reverse_loop(Repetitions, List),
		cpu_time(Time2),
		Time is (Time2 - Time1) - (Time1 - Time0),
		MLIPS is round(((496 * Repetitions) / Time) / 1000000).

	dummy_loop(Repetitions, List) :-
		between(1, Repetitions, _),
		dummy(List, _),
		fail.
	dummy_loop(_, _).

	reverse_loop(Repetitions, List) :-
		between(1, Repetitions, _),
		reverse(List, _),
		fail.
	reverse_loop(_, _).

	dummy(_, _).

	reverse([], []).
	reverse([X|L0],L) :-
		reverse(L0, L1),
		append(L1, [X], L).

	append([], L, L).
	append([X|L1], L2, [X|L3]) :-
		append(L1, L2, L3).

:- end_object.

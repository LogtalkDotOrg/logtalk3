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


:- protocol(tabu_search_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Protocol for tabu search problem definitions. A problem object must define the three required predicates and may optionally define predicates to override neighbor generation with delta energy, full neighborhood enumeration, stopping, and progress reporting defaults.',
		see_also is [tabu_search(_)]
	]).

	:- public(initial_state/1).
	:- mode(initial_state(-nonvar), one).
	:- info(initial_state/1, [
		comment is 'Returns an initial state for the optimization problem.',
		argnames is ['State']
	]).

	:- public(neighbor_state/2).
	:- mode(neighbor_state(+nonvar, -nonvar), one).
	:- info(neighbor_state/2, [
		comment is 'Generates a neighboring state from the given state. This is the most problem-specific predicate and its definition determines the quality of the search. Used both for candidate sampling and (when ``neighbors/2`` is not defined) as the sole neighborhood operator.',
		argnames is ['State', 'Neighbor']
	]).

	:- public(neighbor_state/3).
	:- mode(neighbor_state(+nonvar, -nonvar, -number), one).
	:- info(neighbor_state/3, [
		comment is 'Generates a neighboring state and returns the energy change (delta) directly, avoiding a full energy recomputation. Optional. When not defined by the problem, the algorithm calls ``neighbor_state/2`` and ``state_energy/2`` instead.',
		argnames is ['State', 'Neighbor', 'DeltaEnergy']
	]).

	:- public(neighbors/2).
	:- mode(neighbors(+nonvar, -list(nonvar)), zero_or_one).
	:- info(neighbors/2, [
		comment is 'Optionally returns the complete list of neighboring states. When defined, the algorithm uses this list (or a random sample of it controlled by the ``candidates(N)`` option) instead of repeated calls to ``neighbor_state/2``. Useful for small, enumerable neighborhoods.',
		argnames is ['State', 'Neighbors']
	]).

	:- public(state_energy/2).
	:- mode(state_energy(+nonvar, -number), one).
	:- info(state_energy/2, [
		comment is 'Computes the energy (cost) of the given state. The algorithm minimizes this value.',
		argnames is ['State', 'Energy']
	]).

	:- public(stop_condition/3).
	:- mode(stop_condition(+non_negative_integer, +number, +number), zero_or_one).
	:- info(stop_condition/3, [
		comment is 'True when the search should stop given the current step, best energy found so far, and current energy. Optional. When not defined by the problem, the search runs until the maximum number of steps is reached.',
		argnames is ['Step', 'BestEnergy', 'CurrentEnergy']
	]).

	:- public(progress/5).
	:- mode(progress(+non_negative_integer, +number, +number, +number, +number), zero_or_one).
	:- info(progress/5, [
		comment is 'Called periodically to report optimization progress. Optional. When not defined by the problem, progress reporting is skipped. The acceptance and improvement rates are values between 0.0 and 1.0 computed over the interval since the last progress report.',
		argnames is ['Step', 'BestEnergy', 'CurrentEnergy', 'AcceptanceRate', 'ImprovementRate']
	]).

:- end_protocol.

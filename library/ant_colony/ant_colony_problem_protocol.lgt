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


:- protocol(ant_colony_problem_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Protocol for ant colony optimization problem definitions. A problem object must define the required predicates for constructive solution building on a construction graph (typically a complete graph over a set of nodes, as in the TSP). Optional predicates allow custom stopping and progress reporting.',
		see_also is [ant_colony(_)]
	]).

	:- public(nodes/1).
	:- mode(nodes(-list), one).
	:- info(nodes/1, [
		comment is 'Returns the ordered list of nodes that form the construction graph. Solutions are permutations (or paths) over these nodes.',
		argnames is ['Nodes']
	]).

	:- public(heuristic/3).
	:- mode(heuristic(+nonvar, +nonvar, -number), one).
	:- info(heuristic/3, [
		comment is 'Returns the heuristic desirability ``Eta`` of moving from ``From`` to ``To``. Typically the reciprocal of a distance or cost. Must be strictly positive for every pair that can appear in a solution.',
		argnames is ['From', 'To', 'Eta']
	]).

	:- public(solution_cost/2).
	:- mode(solution_cost(+list, -number), one).
	:- info(solution_cost/2, [
		comment is 'Computes the cost (energy) of a complete solution (tour/path). The algorithm minimizes this value.',
		argnames is ['Solution', 'Cost']
	]).

	:- public(stop_condition/3).
	:- mode(stop_condition(+non_negative_integer, +number, +number), zero_or_one).
	:- info(stop_condition/3, [
		comment is 'True when the search should stop given the current iteration, best cost found so far, and the cost of the iteration-best solution. Optional. When not defined by the problem, the search runs until the maximum number of iterations is reached.',
		argnames is ['Iteration', 'BestCost', 'IterationBestCost']
	]).

	:- public(progress/5).
	:- mode(progress(+non_negative_integer, +number, +number, +number, +number), zero_or_one).
	:- info(progress/5, [
		comment is 'Called periodically to report optimization progress. Optional. When not defined by the problem, progress reporting is skipped. The rates are values between 0.0 and 1.0.',
		argnames is ['Iteration', 'BestCost', 'IterationBestCost', 'AcceptanceRate', 'ImprovementRate']
	]).

:- end_protocol.

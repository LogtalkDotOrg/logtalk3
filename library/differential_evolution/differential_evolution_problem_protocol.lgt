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


:- protocol(differential_evolution_problem_protocol).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-11,
		comment is 'Protocol for continuous bounded Differential Evolution problem definitions.',
		see_also is [differential_evolution(_), differential_evolution(_, _)]
	]).

	:- public(position_bounds/1).
	:- mode(position_bounds(-list(pair)), one).
	:- info(position_bounds/1, [
		comment is 'Returns a ground, non-empty proper list of ``Lower-Upper`` numeric bound pairs, with ``Lower =< Upper``. Fixed dimensions are allowed.',
		argnames is ['Bounds']
	]).

	:- public(fitness/2).
	:- mode(fitness(+list(number), -number), one).
	:- info(fitness/2, [
		comment is 'Computes the fitness of a candidate solution. The optimization direction is selected by the ``objective/1`` option.',
		argnames is ['Position', 'Fitness']
	]).

	:- public(initial_positions/1).
	:- mode(initial_positions(-list(list(number))), zero_or_one).
	:- info(initial_positions/1, [
		comment is 'Optional. Returns exactly the configured population size in ground initial positions inside the bounds. When not defined, the algorithm generates a continuous random initial population.',
		argnames is ['Positions']
	]).

	:- public(stop_condition/3).
	:- mode(stop_condition(+non_negative_integer, +list(number), +number), zero_or_one).
	:- info(stop_condition/3, [
		comment is 'Optional. True when the search should stop given the completed generation, best position, and best fitness.',
		argnames is ['Generation', 'BestPosition', 'BestFitness']
	]).

	:- public(progress/5).
	:- mode(progress(+non_negative_integer, +list(number), +number, +number, +number), zero_or_one).
	:- info(progress/5, [
		comment is 'Optional. Called at most ``updates(N)`` times, including one final report, with generation, best position, best fitness, mean fitness, and mean Euclidean distance from the centroid.',
		argnames is ['Generation', 'BestPosition', 'BestFitness', 'MeanFitness', 'Diversity']
	]).

:- end_protocol.

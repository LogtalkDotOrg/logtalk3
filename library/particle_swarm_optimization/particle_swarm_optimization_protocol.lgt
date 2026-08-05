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


:- protocol(particle_swarm_optimization_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-05,
		comment is 'Protocol for continuous bounded particle swarm optimization problem definitions. A problem object must define the three required predicates and may optionally define predicates for stopping and progress reporting.',
		see_also is [particle_swarm_optimization(_)]
	]).

	:- public(initial_positions/1).
	:- mode(initial_positions(-list(list(number))), one).
	:- info(initial_positions/1, [
		comment is 'Returns a non-empty list of initial particle positions. All positions must be non-empty lists of numbers with the same dimensions as the position bounds.',
		argnames is ['Positions']
	]).

	:- public(position_bounds/1).
	:- mode(position_bounds(-list(pair)), one).
	:- info(position_bounds/1, [
		comment is 'Returns one ``Lower-Upper`` numeric bound pair per position dimension, with ``Lower =< Upper``.',
		argnames is ['Bounds']
	]).

	:- public(fitness/2).
	:- mode(fitness(+list(number), -number), one).
	:- info(fitness/2, [
		comment is 'Computes the fitness of a particle position. The algorithm minimizes this value.',
		argnames is ['Position', 'Fitness']
	]).

	:- public(stop_condition/3).
	:- mode(stop_condition(+non_negative_integer, +list(number), +number), zero_or_one).
	:- info(stop_condition/3, [
		comment is 'True when the search should stop given the completed iteration count, best position, and best fitness. Optional. When not defined, the search runs for the configured maximum number of iterations.',
		argnames is ['Iteration', 'BestPosition', 'BestFitness']
	]).

	:- public(progress/5).
	:- mode(progress(+non_negative_integer, +list(number), +number, +number, +number), zero_or_one).
	:- info(progress/5, [
		comment is 'Called periodically to report the completed iteration count, best position, best fitness, mean swarm fitness, and swarm diversity. Diversity is the mean Euclidean distance from the swarm centroid. Optional.',
		argnames is ['Iteration', 'BestPosition', 'BestFitness', 'MeanFitness', 'Diversity']
	]).

:- end_protocol.

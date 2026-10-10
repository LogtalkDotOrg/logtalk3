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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-10,
		comment is 'Protocol for tabu search problems, with optional tabu keys, tenure, aspiration, restart diversification, delta-energy generation, neighborhood enumeration, stopping, and progress reporting.',
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
		comment is 'Optionally returns the complete neighborhood. With ``exhaustive(true)``, an implementation must succeed for every visited state, including empty neighborhoods, and all candidates are evaluated in source order. Otherwise the candidate limit controls sampling and failure falls back to neighbor generation.',
		argnames is ['State', 'Neighbors'],
		exceptions is [
			'Exhaustive mode has no implemented enumeration predicate' - existence_error(procedure, 'Problem'::neighbors/2),
			'Enumeration fails in exhaustive mode' - domain_error(tabu_hook_result, neighbors/2)
		]
	]).

	:- public(tabu_key/2).
	:- mode(tabu_key(+nonvar, -nonvar), one).
	:- info(tabu_key/2, [
		comment is 'Optionally returns a stable tabu-equivalence key without instantiating the state. Absent implementations use the state itself. Keys are compared using strict term identity; shared variables retain identity, while fresh variables do not provide stable equivalence.',
		argnames is ['State', 'Key'],
		exceptions is [
			'An implemented hook fails' - domain_error(tabu_hook_result, tabu_key/2),
			'An implemented hook returns an unbound key' - domain_error(tabu_hook_result, tabu_key/2-'Key')
		]
	]).

	:- public(aspiration/3).
	:- mode(aspiration(+nonvar, +number, +number), zero_or_one).
	:- info(aspiration/3, [
		comment is 'Optionally permits an otherwise tabu contender. Success permits the move; failure rejects it even when it improves the global best. Absent implementations use strict best-so-far aspiration. Called only for tabu candidates that could win selection; implementations should be pure and must not instantiate the candidate.',
		argnames is ['CandidateState', 'CandidateEnergy', 'BestEnergy']
	]).

	:- public(tabu_tenure/4).
	:- mode(tabu_tenure(+non_negative_integer, +number, +number, -non_negative_integer), one).
	:- info(tabu_tenure/4, [
		comment is 'Optionally returns tenure once per accepted move, using the zero-based global selection step and pre-move best and current energies. Overrides fixed and ranged options. Zero skips insertion without erasing earlier active entries; existing expiration steps never change.',
		argnames is ['Step', 'BestEnergy', 'CurrentEnergy', 'Tenure'],
		exceptions is [
			'An implemented hook fails' - domain_error(tabu_hook_result, tabu_tenure/4),
			'An implemented hook returns an invalid tenure' - domain_error(tabu_hook_result, tabu_tenure/4-'Tenure')
		]
	]).

	:- public(restart_state/2).
	:- mode(restart_state(+nonvar, -nonvar), one).
	:- info(restart_state/2, [
		comment is 'Optionally returns a restart state without instantiating the global best state. Called only between cycles. Its energy is recomputed and may update the best, without incrementing move statistics. Absent implementations restart from the best with its cached energy.',
		argnames is ['BestState', 'RestartState'],
		exceptions is [
			'An implemented hook fails' - domain_error(tabu_hook_result, restart_state/2),
			'An implemented hook returns an unbound state' - domain_error(tabu_hook_result, restart_state/2-'RestartState')
		]
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
		comment is 'Called with completed global steps and the actual current energy. Optional. The acceptance and improvement rates are computed since the preceding report, or since the cycle began for its first report. A zero-step interval has zero rates. When reporting is enabled, each cycle has a final report that replaces any coincident periodic report.',
		argnames is ['Step', 'BestEnergy', 'CurrentEnergy', 'AcceptanceRate', 'ImprovementRate']
	]).

:- end_protocol.

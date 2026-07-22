%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(mutator_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Mutator protocol.'
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets any mutator internal state used while expanding terms.'
	]).

	:- public(mutation/2).
	:- mode(mutation(@callable, @callable), zero_or_more).
	:- info(mutation/2, [
		comment is 'Generates by backtracking zero or more mutations for a given term.',
		argnames is ['Term', 'Mutation']
	]).

	:- public(coverage_clause_mutator/0).
	:- mode(coverage_clause_mutator, zero_or_one).
	:- info(coverage_clause_mutator/0, [
		comment is 'True when mutation occurrences map directly to predicate clause numbers and can use baseline clause coverage for skipping uncovered mutants.'
	]).

:- end_protocol.

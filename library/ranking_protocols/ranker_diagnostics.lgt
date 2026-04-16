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


:- category(ranker_diagnostics).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-16,
		comment is 'Shared predicates for accessing ranker diagnostics in a representation-independent way.'
	]).

	:- uses(list, [
		member/2
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+compound, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns the diagnostics metadata associated with a learned ranker.',
		argnames is ['Ranker', 'Diagnostics']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+compound, ?compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Tests or enumerates individual diagnostics metadata terms for a learned ranker.',
		argnames is ['Ranker', 'Diagnostic']
	]).

	:- public(ranker_options/2).
	:- mode(ranker_options(+compound, -list(compound)), zero_or_one).
	:- info(ranker_options/2, [
		comment is 'Returns the effective training options recorded in a learned ranker diagnostics list.',
		argnames is ['Ranker', 'Options']
	]).

	:- protected(ranker_diagnostics_data/2).
	:- mode(ranker_diagnostics_data(+compound, -list(compound)), one).
	:- info(ranker_diagnostics_data/2, [
		comment is 'Hook predicate that importing ranker implementations must define in order to expose diagnostics metadata.',
		argnames is ['Ranker', 'Diagnostics']
	]).

	diagnostics(Ranker, Diagnostics) :-
		::ranker_diagnostics_data(Ranker, Diagnostics).

	diagnostic(Ranker, Diagnostic) :-
		diagnostics(Ranker, Diagnostics),
		member(Diagnostic, Diagnostics).

	ranker_options(Ranker, Options) :-
		diagnostic(Ranker, options(Options)).

:- end_category.

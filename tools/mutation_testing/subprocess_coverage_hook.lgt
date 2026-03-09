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


% This file is loaded only inside baseline coverage subprocesses via
% an initialization goal and thus not loaded by the main loader.

:- object(subprocess_coverage_hook).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Subprocess coverage hook object. Loaded in baseline coverage subprocesses to capture lgtunit coverage and persist it to a file.'
	]).

	:- public(load_coverage_config/1).
	:- mode(load_coverage_config(+atom), one).
	:- info(load_coverage_config/1, [
		comment is 'Loads a baseline coverage config file and enables coverage collection in the subprocess.',
		argnames is ['ConfigFile']
	]).

	:- private(coverage_file_/1).
	:- dynamic(coverage_file_/1).
	:- mode(coverage_file_(?atom), zero_or_more).
	:- info(coverage_file_/1, [
		comment is 'Holds the baseline coverage output file path for the current subprocess.',
		argnames is ['CoverageFile']
	]).

	:- private(coverage_entry_/4).
	:- dynamic(coverage_entry_/4).
	:- mode(coverage_entry_(?entity_identifier, ?predicate_indicator, ?list(integer), ?integer), zero_or_more).
	:- info(coverage_entry_/4, [
		comment is 'Captured lgtunit coverage for one entity predicate: covered clauses and total clauses.',
		argnames is ['Entity', 'Predicate', 'CoveredClauses', 'TotalClauses']
	]).

	load_coverage_config(ConfigFile) :-
		logtalk_load(ConfigFile),
		{baseline_coverage_file(CoverageFile)},
		retractall(coverage_file_(_)),
		retractall(coverage_entry_(_, _, _, _)),
		assertz(coverage_file_(CoverageFile)).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(entity_predicate_coverage(Entity, Predicate, _Covered, Total, _Percentage, Clauses0), _, lgtunit, _) :-
		(   valid_clauses(Clauses0, Clauses), integer(Total) ->
			retractall(coverage_entry_(Entity, Predicate, _, _)),
			assertz(coverage_entry_(Entity, Predicate, Clauses, Total))
		;   true
		),
		fail.

	logtalk::message_hook(tests_ended, _, lgtunit, _) :-
		write_coverage_file,
		fail.

	write_coverage_file :-
		coverage_file_(CoverageFile),
		open(CoverageFile, write, Stream),
		forall(
			coverage_entry_(Entity, Predicate, Clauses, Total),
			(	writeq(Stream, baseline_coverage(Entity, Predicate, Clauses, Total)),
				write(Stream, '.'),
				nl(Stream)
			)
		),
		close(Stream).

	valid_clauses([], []).
	valid_clauses([Clause| Clauses], [Clause| ValidClauses]) :-
		integer(Clause),
		Clause >= 1,
		!,
		valid_clauses(Clauses, ValidClauses).
	valid_clauses([_| Clauses], ValidClauses) :-
		valid_clauses(Clauses, ValidClauses).

:- end_object.

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


% This file is loaded only inside mutation testing subprocesses via
% an initialization goal and thus not loaded by the main loader.

:- object(subprocess_mutation_hook).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Subprocess mutation hook object. Loaded in mutation testing subprocesses to set up the mutator hook and record test results to a status file.'
	]).

	:- public(load_config/1).
	:- mode(load_config(+atom), one).
	:- info(load_config/1, [
		comment is 'Loads a mutation config file and sets up the mutator hook for the subprocess.',
		argnames is ['ConfigFile']
	]).

	:- private(status_file_/1).
	:- dynamic(status_file_/1).
	:- mode(status_file_(?atom), zero_or_more).
	:- info(status_file_/1, [
		comment is 'Holds the status file path for the current subprocess.',
		argnames is ['StatusFile']
	]).

	:- private(failed_count_/1).
	:- dynamic(failed_count_/1).
	:- mode(failed_count_(?integer), zero_or_more).
	:- info(failed_count_/1, [
		comment is 'Accumulates the number of failed tests reported by lgtunit.',
		argnames is ['Failed']
	]).

	:- uses(fast_random(xoshiro128pp), [
		reset_seed/0
	]).

	load_config(ConfigFile) :-
		% load the config file as Prolog facts
		logtalk_load(ConfigFile),
		% retrieve config facts
		{mutation_entity(Entity)},
		{mutation_predicate(Predicate)},
		{mutation_mutator(Mutator)},
		(   {mutation_clause_index(ClauseIndex)} ->
			true
		;   {mutation_occurrence(ClauseIndex)}
		),
		{mutation_occurrence(Occurrence)},
		{mutation_mutator_file(MutatorFile)},
		{mutation_status_file(StatusFile)},
		% store status file path
		retractall(status_file_(_)),
		assertz(status_file_(StatusFile)),
		% initialize failed counter
		retractall(failed_count_(_)),
		assertz(failed_count_(0)),
		% load mutator dependency (absolute paths)
		logtalk_load(MutatorFile, [reload(skip)]),
		% construct and set up the hook; printing of mutated terms
		% is handled by the main process, not the subprocess;
		% reset the random seed to ensure the mutation matches
		% the one printed by the main process
		reset_seed,
		Hook =.. [Mutator, Entity, Predicate, ClauseIndex, Occurrence, false],
		Hook::reset,
		set_logtalk_flag(hook, Hook).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(tests_results_summary(_, _, _, _, Failed, _, _), _, lgtunit, _) :-
		integer(Failed),
		Failed > 0,
		retract(failed_count_(N0)),
		N is N0 + Failed,
		assertz(failed_count_(N)),
		fail.

	logtalk::message_hook(tests_ended, _, lgtunit, _) :-
		write_status_file,
		fail.

	write_status_file :-
		status_file_(StatusFile),
		failed_count_(Failed),
		(	Failed > 0 ->
			Status = killed
		;	Status = survived
		),
		open(StatusFile, write, Stream),
		write(Stream, Status),
		write(Stream, '.'),
		nl(Stream),
		close(Stream).

:- end_object.

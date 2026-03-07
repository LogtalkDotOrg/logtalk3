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


:- category(mutation_testing_messages).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Default message translations for the ``mutation_testing`` tool.'
	]).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, mutation_testing, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(comment,     '% ',     user_output).
	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).
	message_prefix_stream(silent,      '',       user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(mutated_term(Mutator, Original, Mutation, Variables, File, Line), mutation_testing) -->
		['Mutated term using ~q at ~q:~d'-[Mutator, File, Line], nl],
		['  Original: '-[], term(Original, [quoted(true), variable_names(Variables)]), nl],
		['  Mutation: '-[], term(Mutation, [quoted(true), variable_names(Variables)]), nl].

	logtalk::message_tokens(campaign_started(Entity), mutation_testing) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Mutation testing campaign started for ~q'-[GroundEntity], nl].

	logtalk::message_tokens(campaign_ended(_Entity), mutation_testing) -->
		[].

	logtalk::message_tokens(phase_mutations_discovery, mutation_testing) -->
		['Phase 1: Mutations discovery'-[], nl].

	logtalk::message_tokens(phase_mutations_testing, mutation_testing) -->
		['Phase 2: Mutations testing'-[], nl].

	logtalk::message_tokens(testing_mutation(Mutant), mutation_testing) -->
		['Testing mutation: ~q'-[Mutant], nl].

	logtalk::message_tokens(running_mutant(Index, Mutant), mutation_testing) -->
		['Running mutant #~d ~q'-[Index, Mutant], nl].

	logtalk::message_tokens(mutant_result(Index, Mutant, Status), mutation_testing) -->
		['Mutant #~d ~q => ~q'-[Index, Mutant, Status], nl, nl].

	logtalk::message_tokens(subprocess_command(Command), mutation_testing) -->
		['Subprocess command: ~w'-[Command], nl].

	logtalk::message_tokens(subprocess_exit_status(ExitStatus), mutation_testing) -->
		['Subprocess exit status: ~q'-[ExitStatus], nl].

	logtalk::message_tokens(mutant_reproduction(ApplyCommand, RevertCommand), mutation_testing) -->
		['  Reproduce apply: ~q'-[ApplyCommand], nl],
		['  Reproduce revert: ~q'-[RevertCommand], nl].

	logtalk::message_tokens(summary(Entity, Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors, Score, Threshold), mutation_testing) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Summary for ~q: total=~d, killed=~d, survived=~d, untested=~d, timeout=~d, no_coverage=~d, errors=~d'-[GroundEntity, Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors], nl],
		['Score=~2f, threshold=~2f'-[Score, Threshold], nl].

	logtalk::message_tokens(threshold_passed(Entity, Score, Threshold), mutation_testing) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Threshold passed for ~q (~2f >= ~2f)'-[GroundEntity, Score, Threshold], nl].

	logtalk::message_tokens(threshold_failed(Entity, Score, Threshold), mutation_testing) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Threshold failed for ~q (~2f < ~2f)'-[GroundEntity, Score, Threshold], nl].

	logtalk::message_tokens(unknown(entity, Entity), mutation_testing) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Entity not loaded: ~q'-[GroundEntity], nl].

	logtalk::message_tokens(unknown(predicate, Entity, Predicate), mutation_testing) -->
		{ground_term_copy(Entity, GroundEntity)},
		['Predicate not defined by ~q: ~q'-[GroundEntity, Predicate], nl].

	logtalk::message_tokens(unknown(library, Library), mutation_testing) -->
		['Unknown library: ~q'-[Library], nl].

	logtalk::message_tokens(unknown(directory, Directory), mutation_testing) -->
		['Unknown directory: ~q'-[Directory], nl].

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

:- end_category.

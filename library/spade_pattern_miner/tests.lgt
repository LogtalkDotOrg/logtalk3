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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "spade_pattern_miner" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(spade_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(spade_mine_2_structure, deterministic(functor(PatternMiner, spade_pattern_miner, 3))) :-
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]).

	test(spade_mine_3_same_event_and_next_event_extensions, deterministic([SameEventSupport, NextEventSupport] == [3, 3])) :-
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[a, b]], SameEventSupport), Patterns),
		memberchk(sequence_pattern([[a], [b]], NextEventSupport), Patterns).

	test(spade_mine_3_same_event_and_next_event_complete_pattern_set, deterministic(Patterns == [
		sequence_pattern([[a]], 5),
		sequence_pattern([[b]], 5),
		sequence_pattern([[c]], 2),
		sequence_pattern([[a], [b]], 3),
		sequence_pattern([[a], [c]], 2),
		sequence_pattern([[a, b]], 3),
		sequence_pattern([[b], [c]], 2)
	])) :-
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(spade_mine_3_repeated_embedding_support, deterministic(Support == 2)) :-
		spade_pattern_miner::mine(repeated_embedding_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]),
		memberchk(sequence_pattern([[a], [b], [c]], Support), Patterns).

	test(spade_mine_3_repeated_embedding_support_counts, deterministic([TwoStepSupport, ThreeStepSupport] == [2, 2])) :-
		spade_pattern_miner::mine(repeated_embedding_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]),
		memberchk(sequence_pattern([[a], [b]], TwoStepSupport), Patterns),
		memberchk(sequence_pattern([[a], [b], [c]], ThreeStepSupport), Patterns).

	test(spade_mine_3_self_sequence_join, deterministic(Support == 2)) :-
		spade_pattern_miner::mine(self_join_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[a], [a]], Support), Patterns).

	test(spade_mine_3_dense_overlap_support, deterministic(Support == 6)) :-
		spade_pattern_miner::mine(dense_overlap_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[b], [d]], Support), Patterns).

	test(spade_diagnostics_2, deterministic([Model, SearchStrategy, CandidateGeneration, SupportLayout] == [spade_pattern_miner, lattice_depth_first_growth, equivalence_class_temporal_joins, vertical_occurrence_lists])) :-
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade_pattern_miner::diagnostics(PatternMiner, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(search_strategy(SearchStrategy), Diagnostics),
		memberchk(candidate_generation(CandidateGeneration), Diagnostics),
		memberchk(support_layout(SupportLayout), Diagnostics).

	test(spade_valid_pattern_miner_1, deterministic) :-
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade_pattern_miner::valid_pattern_miner(PatternMiner).

	test(spade_invalid_pattern_miner_1, fail) :-
		PatternMiner = spade_pattern_miner([a], [sequence_pattern([[a]], foo)], [minimum_support(0.5)]),
		spade_pattern_miner::valid_pattern_miner(PatternMiner).

	test(spade_export_to_file_4, deterministic(Support == 3)) :-
		^^file_path('test_output.pl', File),
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade_pattern_miner::export_to_file(same_event_vs_next_event_sequences, PatternMiner, spade_exported_patterns, File),
		logtalk_load(File),
		{spade_exported_patterns(_ItemDomain, Patterns, _Options)},
		memberchk(sequence_pattern([[a, b]], Support), Patterns).

	test(spade_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		spade_pattern_miner::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade_pattern_miner::print_pattern_miner(PatternMiner).

	test(spade_mine_2_invalid_dataset, error(domain_error(canonical_itemset, [b, a]))) :-
		spade_pattern_miner::mine(invalid_unsorted_itemset_sequences, _PatternMiner).

	test(spade_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_sequence_ids, [1, 1]))) :-
		spade_pattern_miner::mine(invalid_duplicate_id_sequences, _PatternMiner).

:- end_object.

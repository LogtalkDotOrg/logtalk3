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
		date is 2026-04-29,
		comment is 'Unit tests for the "spade" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(spade).

	cleanup :-
		^^clean_file('test_output.pl').

	test(spade_mine_2_structure, deterministic(functor(PatternMiner, spade_pattern_miner, 3))) :-
		spade::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]).

	test(spade_mine_3_same_event_and_next_event_extensions, deterministic((memberchk(sequence_pattern([[a, b]], 3), Patterns), memberchk(sequence_pattern([[a], [b]], 3), Patterns)))) :-
		spade::mine(same_event_vs_next_event_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(spade_mine_3_same_event_and_next_event_complete_pattern_set, deterministic(Patterns == [
		sequence_pattern([[a]], 5),
		sequence_pattern([[b]], 5),
		sequence_pattern([[c]], 2),
		sequence_pattern([[a], [b]], 3),
		sequence_pattern([[a], [c]], 2),
		sequence_pattern([[a, b]], 3),
		sequence_pattern([[b], [c]], 2)
	])) :-
		spade::mine(same_event_vs_next_event_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(spade_mine_3_repeated_embedding_support, deterministic(memberchk(sequence_pattern([[a], [b], [c]], 2), Patterns))) :-
		spade::mine(repeated_embedding_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(spade_mine_3_repeated_embedding_support_counts, deterministic((memberchk(sequence_pattern([[a], [b]], 2), Patterns), memberchk(sequence_pattern([[a], [b], [c]], 2), Patterns)))) :-
		spade::mine(repeated_embedding_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(spade_mine_3_self_sequence_join, deterministic(memberchk(sequence_pattern([[a], [a]], 2), Patterns))) :-
		spade::mine(self_join_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(spade_mine_3_dense_overlap_support, deterministic(memberchk(sequence_pattern([[b], [d]], 6), Patterns))) :-
		spade::mine(dense_overlap_sequences, spade_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(spade_diagnostics_2, deterministic((memberchk(model(spade), Diagnostics), memberchk(search_strategy(lattice_depth_first_growth), Diagnostics), memberchk(candidate_generation(equivalence_class_temporal_joins), Diagnostics), memberchk(support_layout(vertical_occurrence_lists), Diagnostics)))) :-
		spade::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade::diagnostics(PatternMiner, Diagnostics).

	test(spade_valid_pattern_miner_1, deterministic) :-
		spade::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade::valid_pattern_miner(PatternMiner).

	test(spade_invalid_pattern_miner_1, fail) :-
		PatternMiner = spade_pattern_miner([a], [sequence_pattern([[a]], foo)], [minimum_support(0.5)]),
		spade::valid_pattern_miner(PatternMiner).

	test(spade_export_to_file_4, deterministic(memberchk(sequence_pattern([[a, b]], 3), Patterns))) :-
		^^file_path('test_output.pl', File),
		spade::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade::export_to_file(same_event_vs_next_event_sequences, PatternMiner, spade_exported_patterns, File),
		logtalk_load(File),
		{spade_exported_patterns(_ItemDomain, Patterns, _Options)}.

	test(spade_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		spade::mine(same_event_vs_next_event_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(2)]),
		spade::print_pattern_miner(PatternMiner).

	test(spade_mine_2_invalid_dataset, error(domain_error(canonical_itemset, [b, a]))) :-
		spade::mine(invalid_unsorted_itemset_sequences, _PatternMiner).

	test(spade_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_sequence_ids, [1, 1]))) :-
		spade::mine(invalid_duplicate_id_sequences, _PatternMiner).

:- end_object.

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
		comment is 'Unit tests for the "prefix_span_pattern_miner" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(prefix_span_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(prefix_span_mine_2_clickstream_sequences, deterministic(ground(PatternMiner))) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, PatternMiner).

	test(prefix_span_mine_2_structure, deterministic(functor(PatternMiner, prefix_span_pattern_miner, 3))) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, PatternMiner).

	test(prefix_span_mine_3_singleton_patterns, deterministic(Patterns == [sequence_pattern([[home]], 5), sequence_pattern([[login]], 5), sequence_pattern([[logout]], 5), sequence_pattern([[search]], 5), sequence_pattern([[view]], 5)])) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(5), maximum_pattern_length(1)]).

	test(prefix_span_mine_3_default_contains_long_prefix, deterministic(Support == 5)) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options)),
		memberchk(sequence_pattern([[home], [login], [search], [view]], Support), Patterns).

	test(prefix_span_mine_3_support_count_threshold, deterministic(Support == 3)) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[view], [cart]], Support), Patterns).

	test(prefix_span_mine_3_itemset_extension, deterministic(Support == 1)) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(1), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[browse, search]], Support), Patterns).

	test(prefix_span_mine_3_prefix_ladder_baseline, deterministic(Patterns == [sequence_pattern([[a]], 5), sequence_pattern([[b]], 4), sequence_pattern([[a], [b]], 4)])) :-
		prefix_span_pattern_miner::mine(prefix_ladder_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_minimum_pattern_length_filter, deterministic(Patterns == [sequence_pattern([[a], [b]], 4)])) :-
		prefix_span_pattern_miner::mine(prefix_ladder_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), maximum_pattern_length(2), minimum_pattern_length(2)]).

	test(prefix_span_mine_3_same_event_and_next_event_extensions, deterministic([SameEventSupport, NextEventSupport] == [3, 3])) :-
		prefix_span_pattern_miner::mine(same_event_vs_next_event_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[a, b]], SameEventSupport), Patterns),
		memberchk(sequence_pattern([[a], [b]], NextEventSupport), Patterns).

	test(prefix_span_mine_3_late_i_extension_support, deterministic(Support == 2)) :-
		prefix_span_pattern_miner::mine(late_i_extension_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[a, c]], Support), Patterns).

	test(prefix_span_mine_3_repeated_embedding_support, deterministic([TwoStepSupport, ThreeStepSupport] == [2, 2])) :-
		prefix_span_pattern_miner::mine(repeated_embedding_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]),
		memberchk(sequence_pattern([[a], [b]], TwoStepSupport), Patterns),
		memberchk(sequence_pattern([[a], [b], [c]], ThreeStepSupport), Patterns).

	test(prefix_span_mine_3_border_threshold_patterns, deterministic(Patterns == [sequence_pattern([[a]], 4), sequence_pattern([[b]], 4), sequence_pattern([[c]], 3), sequence_pattern([[a], [b]], 3)])) :-
		prefix_span_pattern_miner::mine(border_threshold_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_closure_fixture_contains_supersequence, deterministic(Support == 2)) :-
		prefix_span_pattern_miner::mine(closure_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]),
		memberchk(sequence_pattern([[a], [b], [c]], Support), Patterns).

	test(prefix_span_mine_3_dense_overlap_support, deterministic(Support == 6)) :-
		prefix_span_pattern_miner::mine(dense_overlap_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[b], [d]], Support), Patterns).

	test(prefix_span_mine_3_branching_patterns, deterministic([BranchASupport, BranchBSupport] == [3, 3])) :-
		prefix_span_pattern_miner::mine(branching_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]),
		memberchk(sequence_pattern([[start], [a], [end]], BranchASupport), Patterns),
		memberchk(sequence_pattern([[start], [b], [end]], BranchBSupport), Patterns).

	test(prefix_span_diagnostics_2, deterministic([Model, ExtensionModes, SupportLayout] == [prefix_span_pattern_miner, [itemset, sequence], projected_database])) :-
		prefix_span_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		prefix_span_pattern_miner::diagnostics(PatternMiner, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(extension_modes(ExtensionModes), Diagnostics),
		memberchk(support_layout(SupportLayout), Diagnostics).

	test(prefix_span_valid_pattern_miner_1, deterministic) :-
		prefix_span_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		prefix_span_pattern_miner::valid_pattern_miner(PatternMiner).

	test(prefix_span_invalid_pattern_miner_1, fail) :-
		PatternMiner = prefix_span_pattern_miner([a], [sequence_pattern([[a]], foo)], [minimum_support(0.5)]),
		prefix_span_pattern_miner::valid_pattern_miner(PatternMiner).

	test(prefix_span_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		prefix_span_pattern_miner::mine(clickstream_sequences, PatternMiner, [minimum_support_count(5), maximum_pattern_length(1)]),
		prefix_span_pattern_miner::export_to_clauses(clickstream_sequences, PatternMiner, mined_patterns, [Clause]).

	test(prefix_span_export_to_file_4, deterministic(Support == 5)) :-
		^^file_path('test_output.pl', File),
		prefix_span_pattern_miner::mine(clickstream_sequences, PatternMiner, [minimum_support_count(5), maximum_pattern_length(1)]),
		prefix_span_pattern_miner::export_to_file(clickstream_sequences, PatternMiner, prefix_span_exported_patterns, File),
		logtalk_load(File),
		{prefix_span_exported_patterns(_ItemDomain, Patterns, _Options)},
		memberchk(sequence_pattern([[home]], Support), Patterns).

	test(prefix_span_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		prefix_span_pattern_miner::mine(clickstream_sequences, PatternMiner, [minimum_support_count(5), maximum_pattern_length(1)]),
		prefix_span_pattern_miner::print_pattern_miner(PatternMiner).

	test(prefix_span_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, c))) :-
		prefix_span_pattern_miner::mine(invalid_undeclared_item_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_unsorted_itemset_dataset, error(domain_error(canonical_itemset, [b, a]))) :-
		prefix_span_pattern_miner::mine(invalid_unsorted_itemset_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_itemset, [a, a]))) :-
		prefix_span_pattern_miner::mine(invalid_duplicate_item_in_event_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_sequence_ids, [1, 1]))) :-
		prefix_span_pattern_miner::mine(invalid_duplicate_id_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_empty_itemset_dataset, error(domain_error(non_empty_itemset, []))) :-
		prefix_span_pattern_miner::mine(invalid_empty_event_sequences, _PatternMiner).

:- end_object.

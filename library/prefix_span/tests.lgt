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
		comment is 'Unit tests for the "prefix_span" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(prefix_span).

	cleanup :-
		^^clean_file('test_output.pl').

	test(prefix_span_mine_2_clickstream_sequences, deterministic(ground(PatternMiner))) :-
		prefix_span::mine(clickstream_sequences, PatternMiner).

	test(prefix_span_mine_2_structure, deterministic(functor(PatternMiner, prefix_span_pattern_miner, 3))) :-
		prefix_span::mine(clickstream_sequences, PatternMiner).

	test(prefix_span_mine_3_singleton_patterns, deterministic(Patterns == [sequence_pattern([[home]], 5), sequence_pattern([[login]], 5), sequence_pattern([[logout]], 5), sequence_pattern([[search]], 5), sequence_pattern([[view]], 5)])) :-
		prefix_span::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(5), maximum_pattern_length(1)]).

	test(prefix_span_mine_3_default_contains_long_prefix, deterministic(memberchk(sequence_pattern([[home], [login], [search], [view]], 5), Patterns))) :-
		prefix_span::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options)).

	test(prefix_span_mine_3_support_count_threshold, deterministic(memberchk(sequence_pattern([[view], [cart]], 3), Patterns))) :-
		prefix_span::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_itemset_extension, deterministic(memberchk(sequence_pattern([[browse, search]], 1), Patterns))) :-
		prefix_span::mine(clickstream_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(1), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_prefix_ladder_baseline, deterministic(Patterns == [sequence_pattern([[a]], 5), sequence_pattern([[b]], 4), sequence_pattern([[a], [b]], 4)])) :-
		prefix_span::mine(prefix_ladder_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_minimum_pattern_length_filter, deterministic(Patterns == [sequence_pattern([[a], [b]], 4)])) :-
		prefix_span::mine(prefix_ladder_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), maximum_pattern_length(2), minimum_pattern_length(2)]).

	test(prefix_span_mine_3_same_event_and_next_event_extensions, deterministic((memberchk(sequence_pattern([[a, b]], 3), Patterns), memberchk(sequence_pattern([[a], [b]], 3), Patterns)))) :-
		prefix_span::mine(same_event_vs_next_event_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_late_i_extension_support, deterministic(memberchk(sequence_pattern([[a, c]], 2), Patterns))) :-
		prefix_span::mine(late_i_extension_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_repeated_embedding_support, deterministic((memberchk(sequence_pattern([[a], [b]], 2), Patterns), memberchk(sequence_pattern([[a], [b], [c]], 2), Patterns)))) :-
		prefix_span::mine(repeated_embedding_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(prefix_span_mine_3_border_threshold_patterns, deterministic(Patterns == [sequence_pattern([[a]], 4), sequence_pattern([[b]], 4), sequence_pattern([[c]], 3), sequence_pattern([[a], [b]], 3)])) :-
		prefix_span::mine(border_threshold_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_closure_fixture_contains_supersequence, deterministic(memberchk(sequence_pattern([[a], [b], [c]], 2), Patterns))) :-
		prefix_span::mine(closure_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(prefix_span_mine_3_dense_overlap_support, deterministic(memberchk(sequence_pattern([[b], [d]], 6), Patterns))) :-
		prefix_span::mine(dense_overlap_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(prefix_span_mine_3_branching_patterns, deterministic((memberchk(sequence_pattern([[start], [a], [end]], 3), Patterns), memberchk(sequence_pattern([[start], [b], [end]], 3), Patterns)))) :-
		prefix_span::mine(branching_sequences, prefix_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(prefix_span_diagnostics_2, deterministic((memberchk(model(prefix_span), Diagnostics), memberchk(extension_modes([itemset, sequence]), Diagnostics), memberchk(support_layout(projected_database), Diagnostics)))) :-
		prefix_span::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		prefix_span::diagnostics(PatternMiner, Diagnostics).

	test(prefix_span_valid_pattern_miner_1, deterministic) :-
		prefix_span::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		prefix_span::valid_pattern_miner(PatternMiner).

	test(prefix_span_invalid_pattern_miner_1, fail) :-
		PatternMiner = prefix_span_pattern_miner([a], [sequence_pattern([[a]], foo)], [minimum_support(0.5)]),
		prefix_span::valid_pattern_miner(PatternMiner).

	test(prefix_span_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		prefix_span::mine(clickstream_sequences, PatternMiner, [minimum_support_count(5), maximum_pattern_length(1)]),
		prefix_span::export_to_clauses(clickstream_sequences, PatternMiner, mined_patterns, [Clause]).

	test(prefix_span_export_to_file_4, deterministic(memberchk(sequence_pattern([[home]], 5), Patterns))) :-
		^^file_path('test_output.pl', File),
		prefix_span::mine(clickstream_sequences, PatternMiner, [minimum_support_count(5), maximum_pattern_length(1)]),
		prefix_span::export_to_file(clickstream_sequences, PatternMiner, prefix_span_exported_patterns, File),
		logtalk_load(File),
		{prefix_span_exported_patterns(_ItemDomain, Patterns, _Options)}.

	test(prefix_span_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		prefix_span::mine(clickstream_sequences, PatternMiner, [minimum_support_count(5), maximum_pattern_length(1)]),
		prefix_span::print_pattern_miner(PatternMiner).

	test(prefix_span_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, c))) :-
		prefix_span::mine(invalid_undeclared_item_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_unsorted_itemset_dataset, error(domain_error(canonical_itemset, [b, a]))) :-
		prefix_span::mine(invalid_unsorted_itemset_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_itemset, [a, a]))) :-
		prefix_span::mine(invalid_duplicate_item_in_event_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_sequence_ids, [1, 1]))) :-
		prefix_span::mine(invalid_duplicate_id_sequences, _PatternMiner).

	test(prefix_span_mine_2_invalid_empty_itemset_dataset, error(domain_error(non_empty_itemset, []))) :-
		prefix_span::mine(invalid_empty_event_sequences, _PatternMiner).

:- end_object.

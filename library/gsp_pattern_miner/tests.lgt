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
		date is 2026-05-06,
		comment is 'Unit tests for the "gsp_pattern_miner" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(gsp_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(gsp_mine_2_structure, deterministic(functor(PatternMiner, gsp_pattern_miner, 3))) :-
		gsp_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]).

	test(gsp_mine_3_prefix_ladder_baseline, deterministic(Patterns == [sequence_pattern([[a]], 5), sequence_pattern([[b]], 4), sequence_pattern([[a], [b]], 4)])) :-
		gsp_pattern_miner::mine(prefix_ladder_sequences, gsp_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), maximum_pattern_length(2)]).

	test(gsp_mine_3_same_event_and_next_event_extensions, deterministic((memberchk(sequence_pattern([[a, b]], 3), Patterns), memberchk(sequence_pattern([[a], [b]], 3), Patterns)))) :-
		gsp_pattern_miner::mine(same_event_vs_next_event_sequences, gsp_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]).

	test(gsp_mine_3_repeated_embedding_support, deterministic((memberchk(sequence_pattern([[a], [b]], 2), Patterns), memberchk(sequence_pattern([[a], [b], [c]], 2), Patterns)))) :-
		gsp_pattern_miner::mine(repeated_embedding_sequences, gsp_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(gsp_mine_3_dense_overlap_support, deterministic(memberchk(sequence_pattern([[b], [d]], 6), Patterns))) :-
		gsp_pattern_miner::mine(dense_overlap_sequences, gsp_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(gsp_mine_3_branching_patterns, deterministic((memberchk(sequence_pattern([[start], [a], [end]], 3), Patterns), memberchk(sequence_pattern([[start], [b], [end]], 3), Patterns)))) :-
		gsp_pattern_miner::mine(branching_sequences, gsp_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(gsp_diagnostics_2, deterministic((memberchk(model(gsp_pattern_miner), Diagnostics), memberchk(candidate_generation(sequential_join_prune), Diagnostics), memberchk(support_layout(horizontal_sequences), Diagnostics)))) :-
		gsp_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		gsp_pattern_miner::diagnostics(PatternMiner, Diagnostics).

	test(gsp_valid_pattern_miner_1, deterministic) :-
		gsp_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		gsp_pattern_miner::valid_pattern_miner(PatternMiner).

	test(gsp_invalid_pattern_miner_1, fail) :-
		PatternMiner = gsp_pattern_miner([a], [sequence_pattern([[a]], foo)], [minimum_support(0.5)]),
		gsp_pattern_miner::valid_pattern_miner(PatternMiner).

	test(gsp_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		gsp_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		gsp_pattern_miner::export_to_clauses(prefix_ladder_sequences, PatternMiner, mined_patterns, [Clause]).

	test(gsp_export_to_file_4, deterministic(memberchk(sequence_pattern([[a]], 5), Patterns))) :-
		^^file_path('test_output.pl', File),
		gsp_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		gsp_pattern_miner::export_to_file(prefix_ladder_sequences, PatternMiner, gsp_exported_patterns, File),
		logtalk_load(File),
		{gsp_exported_patterns(_ItemDomain, Patterns, _Options)}.

	test(gsp_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		gsp_pattern_miner::mine(prefix_ladder_sequences, PatternMiner, [minimum_support_count(4), maximum_pattern_length(2)]),
		gsp_pattern_miner::print_pattern_miner(PatternMiner).

	test(gsp_mine_2_invalid_dataset, error(domain_error(item, c))) :-
		gsp_pattern_miner::mine(invalid_undeclared_item_sequences, _PatternMiner).

	test(gsp_mine_2_invalid_unsorted_itemset_dataset, error(domain_error(canonical_itemset, [b, a]))) :-
		gsp_pattern_miner::mine(invalid_unsorted_itemset_sequences, _PatternMiner).

	test(gsp_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_itemset, [a, a]))) :-
		gsp_pattern_miner::mine(invalid_duplicate_item_in_event_sequences, _PatternMiner).

	test(gsp_mine_2_invalid_empty_itemset_dataset, error(domain_error(non_empty_itemset, []))) :-
		gsp_pattern_miner::mine(invalid_empty_event_sequences, _PatternMiner).

	test(gsp_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_sequence_ids, [1, 1]))) :-
		gsp_pattern_miner::mine(invalid_duplicate_id_sequences, _PatternMiner).

:- end_object.

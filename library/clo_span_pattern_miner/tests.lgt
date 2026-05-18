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
		date is 2026-05-18,
		comment is 'Unit tests for the "clo_span_pattern_miner" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	cover(clo_span_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(clo_span_mine_2_structure, deterministic(functor(PatternMiner, clo_span_pattern_miner, 3))) :-
		clo_span_pattern_miner::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]).

	test(clo_span_mine_3_closed_patterns, deterministic(Patterns == [sequence_pattern([[a], [b]], 5), sequence_pattern([[a], [b], [c]], 2)])) :-
		clo_span_pattern_miner::mine(closure_sequences, clo_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]).

	test(clo_span_mine_3_prefix_ladder_closed_pattern, deterministic(Support == 2)) :-
		clo_span_pattern_miner::mine(prefix_ladder_sequences, clo_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(3)]),
		memberchk(sequence_pattern([[a], [b], [c]], Support), Patterns).

	test(clo_span_mine_3_same_event_and_next_event_extensions, deterministic([SameEventSupport, NextEventSupport] == [3, 3])) :-
		clo_span_pattern_miner::mine(same_event_vs_next_event_sequences, clo_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(2), maximum_pattern_length(2)]),
		memberchk(sequence_pattern([[a, b]], SameEventSupport), Patterns),
		memberchk(sequence_pattern([[a], [b]], NextEventSupport), Patterns).

	test(clo_span_mine_3_minimum_pattern_length_filter, deterministic(Patterns == [sequence_pattern([[a], [b]], 4)])) :-
		clo_span_pattern_miner::mine(prefix_ladder_sequences, clo_span_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), maximum_pattern_length(2), minimum_pattern_length(2)]).

	test(clo_span_matches_prefix_span_closed_subset, deterministic(ClosedPatterns == FilteredPatterns)) :-
		Options = [minimum_support_count(2), maximum_pattern_length(3)],
		clo_span_pattern_miner::mine(closure_sequences, clo_span_pattern_miner(_ClosedDomain, ClosedPatterns, _ClosedOptions), Options),
		prefix_span_pattern_miner::mine(closure_sequences, prefix_span_pattern_miner(_PrefixDomain, PrefixPatterns, _PrefixOptions), Options),
		filter_closed_patterns(PrefixPatterns, FilteredPatterns).

	test(clo_span_prunes_projected_database_equivalent_branch, deterministic((
		[PrefixSingleSupport, PrefixClosedSupport, ClosedSupport] == [5, 5, 5],
		SignatureB == SignatureAB,
		\+ member(sequence_pattern([[b]], 5), ClosedPatterns)
	))) :-
		Options = [minimum_support_count(2), maximum_pattern_length(3)],
		prefix_span_pattern_miner::mine(closure_sequences, prefix_span_pattern_miner(_PrefixDomain, PrefixPatterns, _PrefixOptions), Options),
		memberchk(sequence_pattern([[b]], PrefixSingleSupport), PrefixPatterns),
		memberchk(sequence_pattern([[a], [b]], PrefixClosedSupport), PrefixPatterns),
		memberchk(sequence_pattern([[a], [b]], ClosedSupport), ClosedPatterns),
		projected_signature(closure_sequences, [[b]], SignatureB),
		projected_signature(closure_sequences, [[a], [b]], SignatureAB),
		clo_span_pattern_miner::mine(closure_sequences, clo_span_pattern_miner(_ClosedDomain, ClosedPatterns, _ClosedOptions), Options).

	test(clo_span_matches_prefix_span_dense_overlap_closed_subset, deterministic(ClosedPatterns == FilteredPatterns)) :-
		Options = [minimum_support_count(3), maximum_pattern_length(2)],
		clo_span_pattern_miner::mine(dense_overlap_sequences, clo_span_pattern_miner(_ClosedDomain, ClosedPatterns, _ClosedOptions), Options),
		prefix_span_pattern_miner::mine(dense_overlap_sequences, prefix_span_pattern_miner(_PrefixDomain, PrefixPatterns, _PrefixOptions), Options),
		filter_closed_patterns(PrefixPatterns, FilteredPatterns).

	test(clo_span_diagnostics_2, deterministic([Model, BackwardPruning, ClosureFilter, SupportLayout] == [clo_span_pattern_miner, projected_database_equivalence, projected_database_equivalence_and_same_support_frontier_pruning, projected_database])) :-
		clo_span_pattern_miner::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]),
		clo_span_pattern_miner::diagnostics(PatternMiner, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(backward_pruning(BackwardPruning), Diagnostics),
		memberchk(closure_filter(ClosureFilter), Diagnostics),
		memberchk(support_layout(SupportLayout), Diagnostics).

	test(clo_span_valid_pattern_miner_1, deterministic) :-
		clo_span_pattern_miner::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]),
		clo_span_pattern_miner::valid_pattern_miner(PatternMiner).

	test(clo_span_invalid_pattern_miner_1, fail) :-
		PatternMiner = clo_span_pattern_miner([a], [sequence_pattern([[a]], foo)], [minimum_support(0.5)]),
		clo_span_pattern_miner::valid_pattern_miner(PatternMiner).

	test(clo_span_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		clo_span_pattern_miner::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]),
		clo_span_pattern_miner::export_to_clauses(closure_sequences, PatternMiner, mined_patterns, [Clause]).

	test(clo_span_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		clo_span_pattern_miner::mine(closure_sequences, PatternMiner, [minimum_support_count(2), maximum_pattern_length(3)]),
		clo_span_pattern_miner::print_pattern_miner(PatternMiner).

	test(clo_span_mine_2_invalid_dataset, error(domain_error(non_empty_itemset, []))) :-
		clo_span_pattern_miner::mine(invalid_empty_event_sequences, _PatternMiner).

	test(clo_span_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, c))) :-
		clo_span_pattern_miner::mine(invalid_undeclared_item_sequences, _PatternMiner).

	test(clo_span_mine_2_invalid_unsorted_itemset_dataset, error(domain_error(canonical_itemset, [b, a]))) :-
		clo_span_pattern_miner::mine(invalid_unsorted_itemset_sequences, _PatternMiner).

	test(clo_span_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_itemset, [a, a]))) :-
		clo_span_pattern_miner::mine(invalid_duplicate_item_in_event_sequences, _PatternMiner).

	test(clo_span_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_sequence_ids, [1, 1]))) :-
		clo_span_pattern_miner::mine(invalid_duplicate_id_sequences, _PatternMiner).

	% auxiliary predicates

	projected_signature(Dataset, Pattern, Signature) :-
		findall(projected(RemainingItemset, RemainingSequence),
			(
				Dataset::sequence(_Id, Sequence),
				project_pattern(Pattern, Sequence, projected(RemainingItemset, RemainingSequence))
			),
			Signature
		).

	project_pattern([[Item]], Sequence, projected(RemainingItemset, RemainingSequence)) :-
		project_singleton_item(Item, Sequence, RemainingItemset, RemainingSequence).
	project_pattern([[Item]| Pattern], Sequence, Projection) :-
		match_singleton_item(Item, Sequence, RemainingSequence),
		project_pattern(Pattern, RemainingSequence, Projection).

	match_singleton_item(Item, [Itemset| Sequence], Sequence) :-
		memberchk(Item, Itemset),
		!.
	match_singleton_item(Item, [_Itemset| Sequence0], Sequence) :-
		match_singleton_item(Item, Sequence0, Sequence).

	project_singleton_item(Item, [Itemset| Sequence], RemainingItemset, Sequence) :-
		itemset_suffix_after_item(Item, Itemset, RemainingItemset),
		!.
	project_singleton_item(Item, [_Itemset| Sequence0], RemainingItemset, Sequence) :-
		project_singleton_item(Item, Sequence0, RemainingItemset, Sequence).

	itemset_suffix_after_item(Item, [Item| Itemset], Itemset) :-
		!.
	itemset_suffix_after_item(Item, [_| Itemset0], Itemset) :-
		itemset_suffix_after_item(Item, Itemset0, Itemset).

	filter_closed_patterns([], []).
	filter_closed_patterns([sequence_pattern(Pattern, Support)| Patterns], FilteredPatterns) :-
		(	pattern_is_closed(Pattern, Support, Patterns) ->
			FilteredPatterns = [sequence_pattern(Pattern, Support)| RestFilteredPatterns]
		;	FilteredPatterns = RestFilteredPatterns
		),
		filter_closed_patterns(Patterns, RestFilteredPatterns).

	pattern_is_closed(_Pattern, _Support, []).
	pattern_is_closed(Pattern, Support, [sequence_pattern(OtherPattern, OtherSupport)| Patterns]) :-
		(	Support =:= OtherSupport,
			Pattern \== OtherPattern,
			strict_superpattern(OtherPattern, Pattern) ->
			fail
		;	pattern_is_closed(Pattern, Support, Patterns)
		).

	strict_superpattern(Superpattern, Pattern) :-
		pattern_length(Superpattern, SuperpatternLength),
		pattern_length(Pattern, PatternLength),
		SuperpatternLength > PatternLength,
		pattern_in_pattern(Pattern, Superpattern).

	pattern_in_pattern([], _Superpattern).
	pattern_in_pattern([Itemset| Pattern], Superpattern) :-
		select_matching_itemset(Itemset, Superpattern, RestSuperpattern),
		pattern_in_pattern(Pattern, RestSuperpattern).

	select_matching_itemset(Itemset, [OtherItemset| Superpattern], Superpattern) :-
		itemset_subset(Itemset, OtherItemset),
		!.
	select_matching_itemset(Itemset, [_OtherItemset| Superpattern0], Superpattern) :-
		select_matching_itemset(Itemset, Superpattern0, Superpattern).

	itemset_subset([], _OtherItemset).
	itemset_subset([Item| Items], OtherItemset) :-
		memberchk(Item, OtherItemset),
		itemset_subset(Items, OtherItemset).

	pattern_length(Pattern, PatternLength) :-
		pattern_length(Pattern, 0, PatternLength).

	pattern_length([], PatternLength, PatternLength).
	pattern_length([Itemset| Pattern], PatternLength0, PatternLength) :-
		length(Itemset, ItemsetLength),
		PatternLength1 is PatternLength0 + ItemsetLength,
		pattern_length(Pattern, PatternLength1, PatternLength).

:- end_object.

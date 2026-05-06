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


:- object(clo_span_pattern_miner,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Closed sequential pattern miner for sequence datasets using closure-aware projected-database search.',
		see_also is [pattern_miner_protocol, sequence_dataset_protocol, prefix_span_pattern_miner, gsp_pattern_miner, spade_pattern_miner]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	mine(Dataset, PatternMiner, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::items(ItemDomain),
		^^check_item_domain(ItemDomain),
		findall(Id-Sequence, Dataset::sequence(Id, Sequence), Sequences),
		^^check_sequences(Dataset, ItemDomain, Sequences, MaxSequenceLength),
		length(Sequences, SequenceCount),
		^^effective_support_count(SequenceCount, Options, SupportCount),
		^^effective_maximum_pattern_length(MaxSequenceLength, Options, MaximumPatternLength),
		initial_projected_database(Sequences, ProjectedDatabase),
		mine_closed_patterns(ProjectedDatabase, SupportCount, MaximumPatternLength, [], ClosedPatterns0),
		strip_closed_patterns(ClosedPatterns0, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = clo_span_pattern_miner(ItemDomain, Patterns, Options).

	initial_projected_database([], []).
	initial_projected_database([_Id-Sequence| Sequences], [projected([], Sequence)| ProjectedDatabase]) :-
		initial_projected_database(Sequences, ProjectedDatabase).

	mine_closed_patterns(ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		frequent_i_extensions(ProjectedDatabase, SupportCount, FrequentIExtensions),
		mine_closed_i_extensions(FrequentIExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, IPatterns),
		frequent_s_extensions(ProjectedDatabase, SupportCount, FrequentSExtensions),
		(   IPatterns == [] ->
			mine_closed_s_extensions(FrequentSExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns)
		;   mine_closed_s_extensions(FrequentSExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, IPatterns, Patterns)
		).

	mine_closed_i_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		mine_closed_i_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, [], Patterns).

	mine_closed_i_extensions([], _ProjectedDatabase, _SupportCount, _MaximumPatternLength, _Prefix, Patterns, Patterns).
	mine_closed_i_extensions([item_support(Item, Support)| ItemSupports], ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns0, Patterns) :-
		extend_prefix_itemset(Prefix, Item, ExtendedPrefix),
		^^pattern_length(ExtendedPrefix, PatternLength),
		(	PatternLength =< MaximumPatternLength ->
			project_i_extension(ProjectedDatabase, Item, ProjectedExtensionDatabase),
			projected_database_signature(ProjectedExtensionDatabase, Signature),
			(	equivalent_projected_superpattern(ExtendedPrefix, Support, Signature, Patterns0) ->
				Patterns1 = Patterns0
			;	mine_closed_extension(ProjectedExtensionDatabase, SupportCount, MaximumPatternLength, ExtendedPrefix, Support, ExtensionPatterns),
				merge_closed_patterns(ExtensionPatterns, Patterns0, Patterns1)
			)
		;	Patterns1 = Patterns0
		),
		mine_closed_i_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns1, Patterns).

	mine_closed_s_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		mine_closed_s_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, [], Patterns).

	mine_closed_s_extensions([], _ProjectedDatabase, _SupportCount, _MaximumPatternLength, _Prefix, Patterns, Patterns).
	mine_closed_s_extensions([item_support(Item, Support)| ItemSupports], ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns0, Patterns) :-
		extend_prefix_sequence(Prefix, Item, ExtendedPrefix),
		^^pattern_length(ExtendedPrefix, PatternLength),
		(	PatternLength =< MaximumPatternLength ->
			project_s_extension(ProjectedDatabase, Item, ProjectedExtensionDatabase),
			projected_database_signature(ProjectedExtensionDatabase, Signature),
			(	equivalent_projected_superpattern(ExtendedPrefix, Support, Signature, Patterns0) ->
				Patterns1 = Patterns0
			;	mine_closed_extension(ProjectedExtensionDatabase, SupportCount, MaximumPatternLength, ExtendedPrefix, Support, ExtensionPatterns),
				merge_closed_patterns(ExtensionPatterns, Patterns0, Patterns1)
			)
		;	Patterns1 = Patterns0
		),
		mine_closed_s_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns1, Patterns).

	mine_closed_extension(ProjectedDatabase, _SupportCount, MaximumPatternLength, Pattern, Support, [closed_pattern(Pattern, Support, Signature)]) :-
		^^pattern_length(Pattern, PatternLength),
		PatternLength >= MaximumPatternLength,
		projected_database_signature(ProjectedDatabase, Signature),
		!.
	mine_closed_extension(ProjectedDatabase, SupportCount, MaximumPatternLength, Pattern, Support, Patterns) :-
		projected_database_signature(ProjectedDatabase, Signature),
		frequent_i_extensions(ProjectedDatabase, SupportCount, FrequentIExtensions),
		mine_closed_i_extensions(FrequentIExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Pattern, IPatterns),
		frequent_s_extensions(ProjectedDatabase, SupportCount, FrequentSExtensions),
		mine_closed_s_extensions(FrequentSExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Pattern, IPatterns, ExtensionPatterns),
		insert_closed_pattern(closed_pattern(Pattern, Support, Signature), ExtensionPatterns, Patterns).

	projected_database_signature(ProjectedDatabase, Signature) :-
		Signature = ProjectedDatabase.

	strip_closed_patterns([], []).
	strip_closed_patterns([closed_pattern(Pattern, Support, _Signature)| ClosedPatterns], [sequence_pattern(Pattern, Support)| Patterns]) :-
		strip_closed_patterns(ClosedPatterns, Patterns).

	merge_closed_patterns([], Patterns, Patterns).
	merge_closed_patterns([Pattern| PendingPatterns], Patterns0, Patterns) :-
		insert_closed_pattern(Pattern, Patterns0, Patterns1),
		merge_closed_patterns(PendingPatterns, Patterns1, Patterns).

	insert_closed_pattern(closed_pattern(Pattern, Support, Signature), Patterns0, Patterns) :-
		(	member(closed_pattern(Pattern, Support, Signature), Patterns0) ->
			Patterns = Patterns0
		;	equivalent_projected_superpattern(Pattern, Support, Signature, Patterns0) ->
			Patterns = Patterns0
		;	same_support_superpattern(Pattern, Support, Patterns0) ->
			Patterns = Patterns0
		;	remove_equivalent_projected_subpatterns(Patterns0, Pattern, Support, Signature, Patterns1),
			remove_same_support_subpatterns(Patterns1, Pattern, Support, Patterns2),
			Patterns = [closed_pattern(Pattern, Support, Signature)| Patterns2]
		).

	equivalent_projected_superpattern(Pattern, Support, Signature, [closed_pattern(OtherPattern, OtherSupport, OtherSignature)| _Patterns]) :-
		Support =:= OtherSupport,
		Signature == OtherSignature,
		strict_superpattern(OtherPattern, Pattern),
		!.
	equivalent_projected_superpattern(Pattern, Support, Signature, [_Pattern| Patterns]) :-
		equivalent_projected_superpattern(Pattern, Support, Signature, Patterns).

	same_support_superpattern(Pattern, Support, [closed_pattern(OtherPattern, OtherSupport, _OtherSignature)| _Patterns]) :-
		Support =:= OtherSupport,
		strict_superpattern(OtherPattern, Pattern),
		!.
	same_support_superpattern(Pattern, Support, [_Pattern| Patterns]) :-
		same_support_superpattern(Pattern, Support, Patterns).

	remove_equivalent_projected_subpatterns([], _Pattern, _Support, _Signature, []).
	remove_equivalent_projected_subpatterns([closed_pattern(OtherPattern, OtherSupport, OtherSignature)| Patterns0], Pattern, Support, Signature, Patterns) :-
		(	Support =:= OtherSupport,
			Signature == OtherSignature,
			strict_superpattern(Pattern, OtherPattern) ->
			Patterns = RestPatterns
		;	Patterns = [closed_pattern(OtherPattern, OtherSupport, OtherSignature)| RestPatterns]
		),
		remove_equivalent_projected_subpatterns(Patterns0, Pattern, Support, Signature, RestPatterns).

	remove_same_support_subpatterns([], _Pattern, _Support, []).
	remove_same_support_subpatterns([closed_pattern(OtherPattern, OtherSupport, OtherSignature)| Patterns0], Pattern, Support, Patterns) :-
		(	Support =:= OtherSupport,
			strict_superpattern(Pattern, OtherPattern) ->
			Patterns = RestPatterns
		;	Patterns = [closed_pattern(OtherPattern, OtherSupport, OtherSignature)| RestPatterns]
		),
		remove_same_support_subpatterns(Patterns0, Pattern, Support, RestPatterns).

	strict_superpattern(Superpattern, Pattern) :-
		^^pattern_length(Superpattern, SuperpatternLength),
		^^pattern_length(Pattern, PatternLength),
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

	frequent_i_extensions(ProjectedDatabase, SupportCount, FrequentItemSupports) :-
		count_i_extension_supports(ProjectedDatabase, [], ItemCounts0),
		^^select_frequent_item_supports(ItemCounts0, SupportCount, FrequentItemSupports0),
		^^sort_item_supports(FrequentItemSupports0, FrequentItemSupports).

	frequent_s_extensions(ProjectedDatabase, SupportCount, FrequentItemSupports) :-
		count_s_extension_supports(ProjectedDatabase, [], ItemCounts0),
		^^select_frequent_item_supports(ItemCounts0, SupportCount, FrequentItemSupports0),
		^^sort_item_supports(FrequentItemSupports0, FrequentItemSupports).

	count_i_extension_supports([], ItemCounts, ItemCounts).
	count_i_extension_supports([projected(CurrentItemset, _Sequence)| ProjectedDatabase], ItemCounts0, ItemCounts) :-
		^^count_items(CurrentItemset, 1, ItemCounts0, ItemCounts1),
		count_i_extension_supports(ProjectedDatabase, ItemCounts1, ItemCounts).

	count_s_extension_supports([], ItemCounts, ItemCounts).
	count_s_extension_supports([projected(_CurrentItemset, Sequence)| ProjectedDatabase], ItemCounts0, ItemCounts) :-
		sequence_available_items(Sequence, AvailableItems),
		^^count_items(AvailableItems, 1, ItemCounts0, ItemCounts1),
		count_s_extension_supports(ProjectedDatabase, ItemCounts1, ItemCounts).

	sequence_available_items(Sequence, AvailableItems) :-
		sequence_available_items(Sequence, [], AvailableItems0),
		sort(AvailableItems0, AvailableItems).

	sequence_available_items([], AvailableItems, AvailableItems).
	sequence_available_items([Itemset| Sequence], AvailableItems0, AvailableItems) :-
		append(AvailableItems0, Itemset, AvailableItems1),
		sequence_available_items(Sequence, AvailableItems1, AvailableItems).

	extend_prefix_itemset(Prefix, Item, ExtendedPrefix) :-
		append(LeadingItemsets, [LastItemset], Prefix),
		append(LastItemset, [Item], ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], ExtendedPrefix),
		!.

	extend_prefix_sequence(Prefix, Item, ExtendedPrefix) :-
		append(Prefix, [[Item]], ExtendedPrefix).

	project_i_extension([], _Item, []).
	project_i_extension([projected(CurrentItemset, Sequence)| ProjectedDatabase], Item, ProjectedExtensionDatabase) :-
		(	itemset_suffix_after_item(Item, CurrentItemset, RemainingItemset) ->
			ProjectedExtensionDatabase = [projected(RemainingItemset, Sequence)| RestProjectedExtensionDatabase]
		;	ProjectedExtensionDatabase = RestProjectedExtensionDatabase
		),
		project_i_extension(ProjectedDatabase, Item, RestProjectedExtensionDatabase).

	project_s_extension([], _Item, []).
	project_s_extension([projected(_CurrentItemset, Sequence)| ProjectedDatabase], Item, ProjectedExtensionDatabase) :-
		(	sequence_suffix_after_item(Item, Sequence, RemainingItemset, RemainingSequence) ->
			ProjectedExtensionDatabase = [projected(RemainingItemset, RemainingSequence)| RestProjectedExtensionDatabase]
		;	ProjectedExtensionDatabase = RestProjectedExtensionDatabase
		),
		project_s_extension(ProjectedDatabase, Item, RestProjectedExtensionDatabase).

	sequence_suffix_after_item(Item, [Itemset| Sequence], RemainingItemset, Sequence) :-
		itemset_suffix_after_item(Item, Itemset, RemainingItemset),
		!.
	sequence_suffix_after_item(Item, [_Itemset| Sequence0], RemainingItemset, Sequence) :-
		sequence_suffix_after_item(Item, Sequence0, RemainingItemset, Sequence).

	itemset_suffix_after_item(Item, [Item| Itemset], Itemset) :-
		!.
	itemset_suffix_after_item(Item, [_| Itemset0], Itemset) :-
		itemset_suffix_after_item(Item, Itemset0, Itemset).

	pattern_miner_diagnostics_data(clo_span_pattern_miner(ItemDomain, Patterns, Options), Diagnostics) :-
		^^pattern_miner_diagnostics(clo_span_pattern_miner, ItemDomain, Patterns, Options, [
			search_strategy(depth_first_projected_growth),
			extension_modes([itemset, sequence]),
			backward_pruning(projected_database_equivalence),
			closure_filter(projected_database_equivalence_and_same_support_frontier_pruning),
			support_layout(projected_database)
		], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(   PatternMiner = clo_span_pattern_miner(ItemDomain, Patterns, Options),
			^^valid_sequence_patterns(ItemDomain, Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(clo_span_pattern_miner, ItemDomain, Patterns, Options, Diagnostics),
			memberchk(search_strategy(depth_first_projected_growth), Diagnostics),
			memberchk(extension_modes([itemset, sequence]), Diagnostics),
			memberchk(backward_pruning(projected_database_equivalence), Diagnostics),
			memberchk(closure_filter(projected_database_equivalence_and_same_support_frontier_pruning), Diagnostics),
			memberchk(support_layout(projected_database), Diagnostics) ->
			true
		;   domain_error(clo_span_pattern_miner, PatternMiner)
		).

	pattern_miner_export_template(_Dataset, clo_span_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(clo_span_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('CloSpan Pattern Miner~n', []),
		format('====================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

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


:- object(prefix_span_pattern_miner,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'PrefixSpan sequential pattern miner for sequence datasets using recursive projected databases.',
		see_also is [pattern_miner_protocol, sequence_dataset_protocol]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_list/2 as dictionary_as_list/2,
		insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
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
		mine_patterns(ProjectedDatabase, SupportCount, MaximumPatternLength, [], Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = prefix_span_pattern_miner(ItemDomain, Patterns, Options).

	initial_projected_database([], []).
	initial_projected_database([Id-Sequence| Sequences], [projected(Id, [], Sequence)| ProjectedDatabase]) :-
		initial_projected_database(Sequences, ProjectedDatabase).

	mine_patterns(ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		frequent_extensions(ProjectedDatabase, SupportCount, FrequentIExtensions, FrequentSExtensions),
		mine_i_extensions(FrequentIExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, IPatterns),
		mine_s_extensions(FrequentSExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, SPatterns),
		append(IPatterns, SPatterns, Patterns).

	frequent_extensions(ProjectedDatabase, SupportCount, FrequentIExtensions, FrequentSExtensions) :-
		count_extension_supports(ProjectedDatabase, IItemCounts, SItemCounts),
		select_frequent_dictionary_item_supports(IItemCounts, SupportCount, FrequentIExtensions0),
		select_frequent_dictionary_item_supports(SItemCounts, SupportCount, FrequentSExtensions0),
		^^sort_item_supports(FrequentIExtensions0, FrequentIExtensions),
		^^sort_item_supports(FrequentSExtensions0, FrequentSExtensions).

	count_extension_supports(ProjectedDatabase, IItemCounts, SItemCounts) :-
		dictionary_new(IItemCounts0),
		dictionary_new(SItemCounts0),
		dictionary_new(ISeenSequenceItems0),
		dictionary_new(SSeenSequenceItems0),
		count_extension_supports(ProjectedDatabase, ISeenSequenceItems0, IItemCounts0, IItemCounts, SSeenSequenceItems0, SItemCounts0, SItemCounts).

	count_extension_supports([], _ISeenSequenceItems, IItemCounts, IItemCounts, _SSeenSequenceItems, SItemCounts, SItemCounts).
	count_extension_supports([projected(Id, CurrentItemset, Sequence)| ProjectedDatabase], ISeenSequenceItems0, IItemCounts0, IItemCounts, SSeenSequenceItems0, SItemCounts0, SItemCounts) :-
		increment_sequence_items(CurrentItemset, Id, ISeenSequenceItems0, ISeenSequenceItems1, IItemCounts0, IItemCounts1),
		increment_sequence_itemsets(Sequence, Id, SSeenSequenceItems0, SSeenSequenceItems1, SItemCounts0, SItemCounts1),
		count_extension_supports(ProjectedDatabase, ISeenSequenceItems1, IItemCounts1, IItemCounts, SSeenSequenceItems1, SItemCounts1, SItemCounts).

	select_frequent_dictionary_item_supports(ItemCounts, SupportCount, FrequentItemSupports) :-
		dictionary_as_list(ItemCounts, ItemCountPairs),
		^^select_frequent_item_supports(ItemCountPairs, SupportCount, FrequentItemSupports).

	increment_sequence_itemsets([], _Id, SeenSequenceItems, SeenSequenceItems, ItemCounts, ItemCounts).
	increment_sequence_itemsets([Itemset| Sequence], Id, SeenSequenceItems0, SeenSequenceItems, ItemCounts0, ItemCounts) :-
		increment_sequence_items(Itemset, Id, SeenSequenceItems0, SeenSequenceItems1, ItemCounts0, ItemCounts1),
		increment_sequence_itemsets(Sequence, Id, SeenSequenceItems1, SeenSequenceItems, ItemCounts1, ItemCounts).

	increment_sequence_items([], _Id, SeenSequenceItems, SeenSequenceItems, ItemCounts, ItemCounts).
	increment_sequence_items([Item| Items], Id, SeenSequenceItems0, SeenSequenceItems, ItemCounts0, ItemCounts) :-
		increment_sequence_item(Item, Id, SeenSequenceItems0, SeenSequenceItems1, ItemCounts0, ItemCounts1),
		increment_sequence_items(Items, Id, SeenSequenceItems1, SeenSequenceItems, ItemCounts1, ItemCounts).

	increment_sequence_item(Item, Id, SeenSequenceItems0, SeenSequenceItems, ItemCounts0, ItemCounts) :-
		Key = Id-Item,
		(   dictionary_lookup(Key, true, SeenSequenceItems0) ->
			SeenSequenceItems = SeenSequenceItems0,
			ItemCounts = ItemCounts0
		;   dictionary_insert(SeenSequenceItems0, Key, true, SeenSequenceItems),
			increment_dictionary_count(Item, ItemCounts0, ItemCounts)
		).

	increment_dictionary_count(Key, Dictionary0, Dictionary) :-
		(   dictionary_lookup(Key, Count0, Dictionary0) ->
			Count is Count0 + 1
		;   Count = 1
		),
		dictionary_insert(Dictionary0, Key, Count, Dictionary).

	mine_i_extensions([], _ProjectedDatabase, _SupportCount, _MaximumPatternLength, _Prefix, []).
	mine_i_extensions([item_support(Item, Support)| ItemSupports], ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		extend_prefix_itemset(Prefix, Item, ExtendedPrefix),
		^^pattern_length(ExtendedPrefix, PatternLength),
		(   PatternLength =< MaximumPatternLength ->
			project_i_extension(ProjectedDatabase, Item, ProjectedExtensionDatabase),
			mine_patterns(ProjectedExtensionDatabase, SupportCount, MaximumPatternLength, ExtendedPrefix, ExtendedPatterns),
			Patterns = [sequence_pattern(ExtendedPrefix, Support)| TailPatterns],
			append(ExtendedPatterns, RestPatterns, TailPatterns)
		;   Patterns = RestPatterns
		),
		mine_i_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, RestPatterns).

	mine_s_extensions([], _ProjectedDatabase, _SupportCount, _MaximumPatternLength, _Prefix, []).
	mine_s_extensions([item_support(Item, Support)| ItemSupports], ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		extend_prefix_sequence(Prefix, Item, ExtendedPrefix),
		^^pattern_length(ExtendedPrefix, PatternLength),
		(   PatternLength =< MaximumPatternLength ->
			project_s_extension(ProjectedDatabase, Item, ProjectedExtensionDatabase),
			mine_patterns(ProjectedExtensionDatabase, SupportCount, MaximumPatternLength, ExtendedPrefix, ExtendedPatterns),
			Patterns = [sequence_pattern(ExtendedPrefix, Support)| TailPatterns],
			append(ExtendedPatterns, RestPatterns, TailPatterns)
		;   Patterns = RestPatterns
		),
		mine_s_extensions(ItemSupports, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, RestPatterns).

	extend_prefix_itemset(Prefix, Item, ExtendedPrefix) :-
		Prefix \== [],
		append(LeadingItemsets, [LastItemset], Prefix),
		append(LastItemset, [Item], ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], ExtendedPrefix),
		!.

	extend_prefix_sequence(Prefix, Item, ExtendedPrefix) :-
		append(Prefix, [[Item]], ExtendedPrefix).

	project_i_extension([], _Item, []).
	project_i_extension([projected(Id, CurrentItemset, Sequence)| ProjectedDatabase], Item, ProjectedExtensionDatabase) :-
		(   itemset_suffix_after_item(Item, CurrentItemset, RemainingItemset) ->
			ProjectedExtensionDatabase = [projected(Id, RemainingItemset, Sequence)| RestProjectedExtensionDatabase]
		;   ProjectedExtensionDatabase = RestProjectedExtensionDatabase
		),
		project_i_extension(ProjectedDatabase, Item, RestProjectedExtensionDatabase).

	project_s_extension([], _Item, []).
	project_s_extension([projected(Id, _CurrentItemset, Sequence)| ProjectedDatabase], Item, ProjectedExtensionDatabase) :-
		project_sequence_occurrences(Sequence, Id, Item, ProjectedExtensionDatabase, RestProjectedExtensionDatabase),
		project_s_extension(ProjectedDatabase, Item, RestProjectedExtensionDatabase).

	project_sequence_occurrences([], _Id, _Item, ProjectedExtensionDatabase, ProjectedExtensionDatabase).
	project_sequence_occurrences([Itemset| Sequence], Id, Item, ProjectedExtensionDatabase0, ProjectedExtensionDatabase) :-
		(   itemset_suffix_after_item(Item, Itemset, RemainingItemset) ->
			ProjectedExtensionDatabase0 = [projected(Id, RemainingItemset, Sequence)| ProjectedExtensionDatabase1]
		;   ProjectedExtensionDatabase0 = ProjectedExtensionDatabase1
		),
		project_sequence_occurrences(Sequence, Id, Item, ProjectedExtensionDatabase1, ProjectedExtensionDatabase).

	itemset_suffix_after_item(Item, [Item| Itemset], Itemset) :-
		!.
	itemset_suffix_after_item(Item, [_| Itemset0], Itemset) :-
		itemset_suffix_after_item(Item, Itemset0, Itemset).

	pattern_miner_diagnostics_data(prefix_span_pattern_miner(ItemDomain, Patterns, Options), Diagnostics) :-
		^^pattern_miner_diagnostics(prefix_span_pattern_miner, ItemDomain, Patterns, Options, [
			search_strategy(depth_first_projected_growth),
			extension_modes([itemset, sequence]),
			support_layout(projected_database)
		], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(   PatternMiner = prefix_span_pattern_miner(ItemDomain, Patterns, Options),
			^^valid_sequence_patterns(ItemDomain, Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(prefix_span_pattern_miner, ItemDomain, Patterns, Options, Diagnostics),
			memberchk(search_strategy(depth_first_projected_growth), Diagnostics),
			memberchk(extension_modes([itemset, sequence]), Diagnostics),
			memberchk(support_layout(projected_database), Diagnostics) ->
			true
		;   domain_error(prefix_span_pattern_miner, PatternMiner)
		).

	pattern_miner_export_template(_Dataset, prefix_span_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(prefix_span_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('PrefixSpan Pattern Miner~n', []),
		format('=========================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

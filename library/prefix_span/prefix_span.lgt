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


:- object(prefix_span,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'PrefixSpan sequential pattern miner for sequence datasets using recursive projected databases.',
		remarks is [
			'Algorithm' - 'Builds frequent sequential patterns by recursively projecting the sequence database using both same-event itemset extensions and next-event sequence extensions.',
			'Dataset handling' - 'Requires a dataset implementing ``sequence_dataset_protocol`` with sequences represented as ordered lists of canonical sorted itemsets over a declared item domain.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``prefix_span_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores ``sequence_pattern(Pattern, SupportCount)`` terms ordered first by total item count and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, sequence_dataset_protocol]
	]).

	:- public(mine/3).
	:- mode(mine(+object_identifier, -compound, +list(compound)), one).
	:- info(mine/3, [
		comment is 'Mines frequent sequential patterns from the given sequence dataset using the specified options.',
		argnames is ['Dataset', 'PatternMiner', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2
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
		PatternMiner = prefix_span_pattern_miner(ItemDomain, Patterns, Options),
		!.

	initial_projected_database([], []).
	initial_projected_database([_Id-Sequence| Sequences], [projected([], Sequence)| ProjectedDatabase]) :-
		initial_projected_database(Sequences, ProjectedDatabase).

	mine_patterns(ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, Patterns) :-
		frequent_i_extensions(ProjectedDatabase, SupportCount, FrequentIExtensions),
		mine_i_extensions(FrequentIExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, IPatterns),
		frequent_s_extensions(ProjectedDatabase, SupportCount, FrequentSExtensions),
		mine_s_extensions(FrequentSExtensions, ProjectedDatabase, SupportCount, MaximumPatternLength, Prefix, SPatterns),
		append(IPatterns, SPatterns, Patterns).

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

	extend_prefix_itemset([], _Item, _ExtendedPrefix) :-
		!,
		fail.
	extend_prefix_itemset(Prefix, Item, ExtendedPrefix) :-
		append(LeadingItemsets, [LastItemset], Prefix),
		append(LastItemset, [Item], ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], ExtendedPrefix).

	extend_prefix_sequence(Prefix, Item, ExtendedPrefix) :-
		append(Prefix, [[Item]], ExtendedPrefix).

	project_i_extension([], _Item, []).
	project_i_extension([projected(CurrentItemset, Sequence)| ProjectedDatabase], Item, ProjectedExtensionDatabase) :-
		(   itemset_suffix_after_item(Item, CurrentItemset, RemainingItemset) ->
			ProjectedExtensionDatabase = [projected(RemainingItemset, Sequence)| RestProjectedExtensionDatabase]
		;   ProjectedExtensionDatabase = RestProjectedExtensionDatabase
		),
		project_i_extension(ProjectedDatabase, Item, RestProjectedExtensionDatabase).

	project_s_extension([], _Item, []).
	project_s_extension([projected(_CurrentItemset, Sequence)| ProjectedDatabase], Item, ProjectedExtensionDatabase) :-
		(   sequence_suffix_after_item(Item, Sequence, RemainingItemset, RemainingSequence) ->
			ProjectedExtensionDatabase = [projected(RemainingItemset, RemainingSequence)| RestProjectedExtensionDatabase]
		;   ProjectedExtensionDatabase = RestProjectedExtensionDatabase
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

	pattern_miner_export_template(_Dataset, prefix_span_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(prefix_span_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('PrefixSpan Pattern Miner~n', []),
		format('=========================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

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


:- object(spade,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'SPADE sequential pattern miner for sequence datasets using vertical occurrence lists.',
		remarks is [
			'Algorithm' - 'Builds frequent sequential patterns by extending vertical occurrence lists keyed by sequence and event position.',
			'Dataset handling' - 'Requires a dataset implementing ``sequence_dataset_protocol`` with sequences represented as ordered lists of canonical sorted itemsets over a declared item domain.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``spade_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores ``sequence_pattern(Pattern, SupportCount)`` terms ordered first by total item count and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, sequence_dataset_protocol, prefix_span, gsp, clo_span]
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
		index_sequences(Sequences, IndexedSequences),
		frequent_singletons(ItemDomain, IndexedSequences, SupportCount, FrequentSingletons),
		mine_occurrence_patterns(FrequentSingletons, IndexedSequences, ItemDomain, SupportCount, MaximumPatternLength, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = spade_pattern_miner(ItemDomain, Patterns, Options),
		!.

	index_sequences([], []).
	index_sequences([Id-Sequence| Sequences], [indexed_sequence(Id, IndexedSequence)| IndexedSequences]) :-
		index_sequence(Sequence, 1, IndexedSequence),
		index_sequences(Sequences, IndexedSequences).

	index_sequence([], _Index, []).
	index_sequence([Itemset| Sequence], Index, [Index-Itemset| IndexedSequence]) :-
		NextIndex is Index + 1,
		index_sequence(Sequence, NextIndex, IndexedSequence).

	frequent_singletons([], _IndexedSequences, _SupportCount, []).
	frequent_singletons([Item| Items], IndexedSequences, SupportCount, FrequentPatterns) :-
		collect_singleton_occurrences(Item, IndexedSequences, Occurrences0),
		sort(Occurrences0, Occurrences),
		occurrences_support(Occurrences, Support),
		(   Support >= SupportCount ->
			FrequentPatterns = [pattern_occurrence([[Item]], Occurrences, Support)| RestFrequentPatterns]
		;   FrequentPatterns = RestFrequentPatterns
		),
		frequent_singletons(Items, IndexedSequences, SupportCount, RestFrequentPatterns).

	collect_singleton_occurrences(_Item, [], []).
	collect_singleton_occurrences(Item, [indexed_sequence(Id, IndexedSequence)| IndexedSequences], Occurrences) :-
		findall(occurrence(Id, EventIndex),
			(
				member(EventIndex-Itemset, IndexedSequence),
				memberchk(Item, Itemset)
			),
			SequenceOccurrences
		),
		append(SequenceOccurrences, RestOccurrences, Occurrences),
		collect_singleton_occurrences(Item, IndexedSequences, RestOccurrences).

	mine_occurrence_patterns([], _IndexedSequences, _ItemDomain, _SupportCount, _MaximumPatternLength, []).
	mine_occurrence_patterns([pattern_occurrence(Pattern, Occurrences, Support)| PatternOccurrences], IndexedSequences, ItemDomain, SupportCount, MaximumPatternLength, Patterns) :-
		Patterns = [sequence_pattern(Pattern, Support)| RestPatterns],
		^^pattern_length(Pattern, PatternLength),
		(   PatternLength < MaximumPatternLength ->
			findall(Extension,
				(
					member(Item, ItemDomain),
					(   i_extension_occurrence(Pattern, Occurrences, IndexedSequences, Item, SupportCount, Extension)
					;   s_extension_occurrence(Pattern, Occurrences, IndexedSequences, Item, SupportCount, Extension)
					)
				),
				Extensions0
			),
			sort(Extensions0, Extensions),
			mine_occurrence_patterns(Extensions, IndexedSequences, ItemDomain, SupportCount, MaximumPatternLength, ExtensionPatterns)
		;   ExtensionPatterns = []
		),
		mine_occurrence_patterns(PatternOccurrences, IndexedSequences, ItemDomain, SupportCount, MaximumPatternLength, SiblingPatterns),
		append(ExtensionPatterns, SiblingPatterns, RestPatterns).

	i_extension_occurrence(Pattern, Occurrences, IndexedSequences, Item, SupportCount, pattern_occurrence(ExtendedPattern, ExtendedOccurrences, Support)) :-
		Pattern \== [],
		append(_LeadingItemsets, [LastItemset], Pattern),
		append(_, [LastItem], LastItemset),
		Item @> LastItem,
		\+ memberchk(Item, LastItemset),
		append(LeadingItemsets, [LastItemset], Pattern),
		append(LastItemset, [Item], ExtendedLastItemset0),
		sort(ExtendedLastItemset0, ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], ExtendedPattern),
		findall(occurrence(Id, EventIndex),
			(
				member(occurrence(Id, EventIndex), Occurrences),
				event_contains_item(IndexedSequences, Id, EventIndex, Item)
			),
			ExtendedOccurrences0
		),
		sort(ExtendedOccurrences0, ExtendedOccurrences),
		ExtendedOccurrences \== [],
		occurrences_support(ExtendedOccurrences, Support),
		Support >= SupportCount.

	s_extension_occurrence(Pattern, Occurrences, IndexedSequences, Item, SupportCount, pattern_occurrence(ExtendedPattern, ExtendedOccurrences, Support)) :-
		append(Pattern, [[Item]], ExtendedPattern),
		findall(occurrence(Id, LaterEventIndex),
			(
				member(occurrence(Id, EventIndex), Occurrences),
				later_event_contains_item(IndexedSequences, Id, EventIndex, Item, LaterEventIndex)
			),
			ExtendedOccurrences0
		),
		sort(ExtendedOccurrences0, ExtendedOccurrences),
		ExtendedOccurrences \== [],
		occurrences_support(ExtendedOccurrences, Support),
		Support >= SupportCount.

	event_contains_item([indexed_sequence(Id, IndexedSequence)| _IndexedSequences], Id, EventIndex, Item) :-
		memberchk(EventIndex-Itemset, IndexedSequence),
		memberchk(Item, Itemset),
		!.
	event_contains_item([_IndexedSequence| IndexedSequences], Id, EventIndex, Item) :-
		event_contains_item(IndexedSequences, Id, EventIndex, Item).

	later_event_contains_item([indexed_sequence(Id, IndexedSequence)| _IndexedSequences], Id, EventIndex, Item, LaterEventIndex) :-
		member(LaterEventIndex-Itemset, IndexedSequence),
		LaterEventIndex > EventIndex,
		memberchk(Item, Itemset).
	later_event_contains_item([_IndexedSequence| IndexedSequences], Id, EventIndex, Item, LaterEventIndex) :-
		later_event_contains_item(IndexedSequences, Id, EventIndex, Item, LaterEventIndex).

	occurrences_support(Occurrences, Support) :-
		findall(Id, member(occurrence(Id, _EventIndex), Occurrences), Ids0),
		sort(Ids0, Ids),
		length(Ids, Support).

	pattern_miner_export_template(_Dataset, spade_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(spade_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('SPADE Pattern Miner~n', []),
		format('===================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

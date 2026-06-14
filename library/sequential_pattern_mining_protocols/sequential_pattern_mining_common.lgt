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


:- category(sequential_pattern_mining_common,
	extends(pattern_miner_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-29,
		comment is 'Shared predicates for sequential pattern miner dataset validation, support counting, and pattern ordering helpers.'
	]).

	:- protected(check_sequences/4).
	:- mode(check_sequences(+object_identifier, +list(atom), +list(pair(integer, list(list(atom)))), -integer), one).
	:- info(check_sequences/4, [
		comment is 'Validates the collected dataset sequences and returns the maximum total sequence length.',
		argnames is ['Dataset', 'ItemDomain', 'Sequences', 'MaxSequenceLength']
	]).

	:- protected(pattern_length/2).
	:- mode(pattern_length(+list(list(atom)), -integer), one).
	:- info(pattern_length/2, [
		comment is 'Computes the total number of items in a sequential pattern.',
		argnames is ['Pattern', 'PatternLength']
	]).

	:- protected(sort_patterns/2).
	:- mode(sort_patterns(+list(compound), -list(compound)), one).
	:- info(sort_patterns/2, [
		comment is 'Sorts sequence patterns first by total item count and then lexicographically.',
		argnames is ['Patterns0', 'Patterns']
	]).

	:- protected(filter_patterns/3).
	:- mode(filter_patterns(+list(compound), +integer, -list(compound)), one).
	:- info(filter_patterns/3, [
		comment is 'Filters sequence patterns by minimum total item count.',
		argnames is ['Patterns0', 'MinimumPatternLength', 'Patterns']
	]).

	:- protected(count_items/4).
	:- mode(count_items(+list(atom), +integer, +list(pair(atom, integer)), -list(pair(atom, integer))), one).
	:- info(count_items/4, [
		comment is 'Accumulates support counts for a list of items.',
		argnames is ['Items', 'Count', 'ItemCounts0', 'ItemCounts']
	]).

	:- protected(select_frequent_item_supports/3).
	:- mode(select_frequent_item_supports(+list(pair(atom, integer)), +integer, -list(compound)), one).
	:- info(select_frequent_item_supports/3, [
		comment is 'Selects the item supports that satisfy the minimum support count.',
		argnames is ['ItemCounts', 'SupportCount', 'FrequentItemSupports']
	]).

	:- protected(sort_item_supports/2).
	:- mode(sort_item_supports(+list(compound), -list(compound)), one).
	:- info(sort_item_supports/2, [
		comment is 'Sorts item supports lexicographically by item.',
		argnames is ['ItemSupports0', 'ItemSupports']
	]).

	:- protected(valid_sequence_patterns/2).
	:- mode(valid_sequence_patterns(+list(atom), +list(compound)), zero_or_one).
	:- info(valid_sequence_patterns/2, [
		comment is 'True when the patterns are valid ``sequence_pattern(Pattern, Support)`` terms over the given item domain with positive integer supports.',
		argnames is ['ItemDomain', 'Patterns']
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(type, [
		valid/2
	]).

	check_sequences(Dataset, _ItemDomain, [], _MaxSequenceLength) :-
		!,
		domain_error(non_empty_dataset, Dataset).
	check_sequences(_Dataset, ItemDomain, Sequences, MaxSequenceLength) :-
		check_unique_sequence_ids(Sequences),
		check_sequences_list(Sequences, ItemDomain, 0, MaxSequenceLength).

	check_unique_sequence_ids(Sequences) :-
		findall(Id, member(Id-_, Sequences), Ids),
		sort(Ids, UniqueIds),
		length(Ids, IdsCount),
		length(UniqueIds, UniqueIdsCount),
		(	IdsCount =:= UniqueIdsCount ->
			true
		;	domain_error(unique_sequence_ids, Ids)
		).

	check_sequences_list([], _ItemDomain, MaxSequenceLength, MaxSequenceLength).
	check_sequences_list([_-Sequence| Sequences], ItemDomain, MaxSequenceLength0, MaxSequenceLength) :-
		check_sequence(Sequence, ItemDomain, SequenceLength),
		MaxSequenceLength1 is max(MaxSequenceLength0, SequenceLength),
		check_sequences_list(Sequences, ItemDomain, MaxSequenceLength1, MaxSequenceLength).

	check_sequence([], _ItemDomain, _SequenceLength) :-
		!,
		domain_error(non_empty_sequence, []).
	check_sequence(Sequence, ItemDomain, SequenceLength) :-
		check_sequence_itemsets(Sequence, ItemDomain, 0, SequenceLength).

	check_sequence_itemsets([], _ItemDomain, SequenceLength, SequenceLength).
	check_sequence_itemsets([Itemset| Sequence], ItemDomain, SequenceLength0, SequenceLength) :-
		check_sequence_itemset(Itemset, ItemDomain),
		length(Itemset, ItemsetLength),
		SequenceLength1 is SequenceLength0 + ItemsetLength,
		check_sequence_itemsets(Sequence, ItemDomain, SequenceLength1, SequenceLength).

	check_sequence_itemset([], _ItemDomain) :-
		!,
		domain_error(non_empty_itemset, []).
	check_sequence_itemset(Itemset, ItemDomain) :-
		sort(Itemset, SortedItemset),
		(	Itemset == SortedItemset ->
			true
		;	domain_error(canonical_itemset, Itemset)
		),
		check_itemset_items(Itemset, ItemDomain).

	check_itemset_items([], _ItemDomain).
	check_itemset_items([Item| Items], ItemDomain) :-
		(	member(Item, ItemDomain) ->
			true
		;	domain_error(item, Item)
		),
		check_itemset_items(Items, ItemDomain).

	count_items([], _Count, ItemCounts, ItemCounts).
	count_items([Item| Items], Count, ItemCounts0, ItemCounts) :-
		increment_item_count(Item, Count, ItemCounts0, ItemCounts1),
		count_items(Items, Count, ItemCounts1, ItemCounts).

	increment_item_count(Item, Count, [], [Item-Count]) :-
		!.
	increment_item_count(Item, Count, [Item-Count0| ItemCounts], [Item-Count1| ItemCounts]) :-
		!,
		Count1 is Count0 + Count.
	increment_item_count(Item, Count, [Item0-Count0| ItemCounts0], [Item0-Count0| ItemCounts]) :-
		increment_item_count(Item, Count, ItemCounts0, ItemCounts).

	select_frequent_item_supports([], _SupportCount, []).
	select_frequent_item_supports([Item-Support| ItemCounts], SupportCount, FrequentItemSupports) :-
		(	Support >= SupportCount ->
			FrequentItemSupports = [item_support(Item, Support)| RestFrequentItemSupports]
		;	FrequentItemSupports = RestFrequentItemSupports
		),
		select_frequent_item_supports(ItemCounts, SupportCount, RestFrequentItemSupports).

	sort_item_supports(ItemSupports0, ItemSupports) :-
		decorate_item_supports(ItemSupports0, DecoratedItemSupports),
		keysort(DecoratedItemSupports, SortedDecoratedItemSupports),
		undecorate_item_supports(SortedDecoratedItemSupports, ItemSupports).

	decorate_item_supports([], []).
	decorate_item_supports([item_support(Item, Support)| ItemSupports], [Item-item_support(Item, Support)| DecoratedItemSupports]) :-
		decorate_item_supports(ItemSupports, DecoratedItemSupports).

	undecorate_item_supports([], []).
	undecorate_item_supports([_Key-ItemSupport| DecoratedItemSupports], [ItemSupport| ItemSupports]) :-
		undecorate_item_supports(DecoratedItemSupports, ItemSupports).

	pattern_length(Pattern, PatternLength) :-
		pattern_length(Pattern, 0, PatternLength).

	pattern_length([], PatternLength, PatternLength).
	pattern_length([Itemset| Pattern], PatternLength0, PatternLength) :-
		length(Itemset, ItemsetLength),
		PatternLength1 is PatternLength0 + ItemsetLength,
		pattern_length(Pattern, PatternLength1, PatternLength).

	sort_patterns(Patterns0, Patterns) :-
		decorate_patterns(Patterns0, DecoratedPatterns),
		keysort(DecoratedPatterns, SortedDecoratedPatterns),
		undecorate_patterns(SortedDecoratedPatterns, Patterns).

	decorate_patterns([], []).
	decorate_patterns([sequence_pattern(Pattern, Support)| Patterns], [key(PatternLength, Pattern)-sequence_pattern(Pattern, Support)| DecoratedPatterns]) :-
		pattern_length(Pattern, PatternLength),
		decorate_patterns(Patterns, DecoratedPatterns).

	undecorate_patterns([], []).
	undecorate_patterns([_Key-Pattern| DecoratedPatterns], [Pattern| Patterns]) :-
		undecorate_patterns(DecoratedPatterns, Patterns).

	filter_patterns([], _MinimumPatternLength, []).
	filter_patterns([sequence_pattern(Pattern, Support)| Patterns], MinimumPatternLength, FilteredPatterns) :-
		pattern_length(Pattern, PatternLength),
		(	PatternLength >= MinimumPatternLength ->
			FilteredPatterns = [sequence_pattern(Pattern, Support)| RestFilteredPatterns]
		;	FilteredPatterns = RestFilteredPatterns
		),
		filter_patterns(Patterns, MinimumPatternLength, RestFilteredPatterns).

	valid_sequence_patterns(_ItemDomain, []) :-
		!.
	valid_sequence_patterns(ItemDomain, [sequence_pattern(Pattern, Support)| Patterns]) :-
		catch(check_sequence(Pattern, ItemDomain, _PatternLength), _Error, fail),
		valid(positive_integer, Support),
		valid_sequence_patterns(ItemDomain, Patterns).

:- end_category.

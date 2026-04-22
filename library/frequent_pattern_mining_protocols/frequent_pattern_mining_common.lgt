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


:- category(frequent_pattern_mining_common,
	extends(pattern_miner_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Shared predicates for frequent itemset miner dataset validation, support accumulation, and itemset ordering/filtering helpers.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- protected(check_transactions/4).
	:- mode(check_transactions(+object_identifier, +list(atom), +list(pair(integer, list(atom))), -integer), one).
	:- info(check_transactions/4, [
		comment is 'Validates the collected dataset transactions and returns the maximum transaction length.',
		argnames is ['Dataset', 'ItemDomain', 'Transactions', 'MaxTransactionLength']
	]).

	:- protected(sort_patterns/2).
	:- mode(sort_patterns(+list(compound), -list(compound)), one).
	:- info(sort_patterns/2, [
		comment is 'Sorts itemset patterns first by pattern length and then lexicographically.',
		argnames is ['Patterns0', 'Patterns']
	]).

	:- protected(filter_patterns/3).
	:- mode(filter_patterns(+list(compound), +integer, -list(compound)), one).
	:- info(filter_patterns/3, [
		comment is 'Filters itemset patterns by minimum pattern length.',
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

	check_transactions(Dataset, _ItemDomain, Transactions, _MaxTransactionLength) :-
		Transactions == [],
		!,
		domain_error(non_empty_dataset, Dataset).
	check_transactions(_Dataset, ItemDomain, Transactions, MaxTransactionLength) :-
		check_transactions_list(Transactions, ItemDomain, 0, MaxTransactionLength).

	check_transactions_list([], _ItemDomain, MaxTransactionLength, MaxTransactionLength).
	check_transactions_list([_-Transaction| Transactions], ItemDomain, MaxTransactionLength0, MaxTransactionLength) :-
		check_transaction(Transaction, ItemDomain),
		length(Transaction, TransactionLength),
		MaxTransactionLength1 is max(MaxTransactionLength0, TransactionLength),
		check_transactions_list(Transactions, ItemDomain, MaxTransactionLength1, MaxTransactionLength).

	check_transaction(Transaction, ItemDomain) :-
		sort(Transaction, SortedTransaction),
		(   Transaction == SortedTransaction ->
			true
		;   domain_error(canonical_transaction, Transaction)
		),
		check_transaction_items(Transaction, ItemDomain).

	check_transaction_items([], _ItemDomain).
	check_transaction_items([Item| Items], ItemDomain) :-
		(   memberchk(Item, ItemDomain) ->
			true
		;   domain_error(item, Item)
		),
		check_transaction_items(Items, ItemDomain).

	sort_patterns(Patterns0, Patterns) :-
		decorate_patterns(Patterns0, DecoratedPatterns),
		keysort(DecoratedPatterns, SortedDecoratedPatterns),
		undecorate_patterns(SortedDecoratedPatterns, Patterns).

	decorate_patterns([], []).
	decorate_patterns([itemset(Items, Support)| Patterns], [key(PatternLength, Items)-itemset(Items, Support)| DecoratedPatterns]) :-
		length(Items, PatternLength),
		decorate_patterns(Patterns, DecoratedPatterns).

	undecorate_patterns([], []).
	undecorate_patterns([_Key-Pattern| DecoratedPatterns], [Pattern| Patterns]) :-
		undecorate_patterns(DecoratedPatterns, Patterns).

	filter_patterns([], _MinimumPatternLength, []).
	filter_patterns([itemset(Items, Support)| Patterns], MinimumPatternLength, FilteredPatterns) :-
		length(Items, PatternLength),
		(   PatternLength >= MinimumPatternLength ->
			FilteredPatterns = [itemset(Items, Support)| RestFilteredPatterns]
		;   FilteredPatterns = RestFilteredPatterns
		),
		filter_patterns(Patterns, MinimumPatternLength, RestFilteredPatterns).

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
		(   Support >= SupportCount ->
			FrequentItemSupports = [item_support(Item, Support)| RestFrequentItemSupports]
		;   FrequentItemSupports = RestFrequentItemSupports
		),
		select_frequent_item_supports(ItemCounts, SupportCount, RestFrequentItemSupports).

:- end_category.
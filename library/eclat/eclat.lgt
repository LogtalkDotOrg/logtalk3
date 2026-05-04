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


:- object(eclat,
	imports(frequent_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Eclat frequent itemset miner for transaction datasets using portable vertical tidsets and depth-first recursive prefix extension.',
		see_also is [pattern_miner_protocol, transaction_dataset_protocol, apriori, fp_growth]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, reverse/2
	]).

	mine(Dataset, PatternMiner, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::items(ItemDomain),
		^^check_item_domain(ItemDomain),
		findall(ExternalId-Transaction, Dataset::transaction(ExternalId, Transaction), DatasetTransactions),
		^^check_transactions(Dataset, ItemDomain, DatasetTransactions, MaxTransactionLength),
		normalize_transactions(DatasetTransactions, Transactions),
		length(Transactions, TransactionCount),
		^^effective_support_count(TransactionCount, Options, SupportCount),
		^^effective_maximum_pattern_length(MaxTransactionLength, Options, MaximumPatternLength),
		build_initial_tidsets(ItemDomain, Transactions, SupportCount, InitialTidsets),
		mine_tidsets(InitialTidsets, [], SupportCount, MaximumPatternLength, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = eclat_pattern_miner(ItemDomain, Patterns, Options).

	normalize_transactions(Transactions, NormalizedTransactions) :-
		normalize_transactions(Transactions, 1, NormalizedTransactions).

	normalize_transactions([], _TransactionOrdinal, []).
	normalize_transactions([_ExternalId-Transaction| Transactions], TransactionOrdinal, [TransactionOrdinal-Transaction| NormalizedTransactions]) :-
		NextTransactionOrdinal is TransactionOrdinal + 1,
		normalize_transactions(Transactions, NextTransactionOrdinal, NormalizedTransactions).

	build_initial_tidsets(ItemDomain, Transactions, SupportCount, Tidsets) :-
		dictionary_new(TidsetDictionary0),
		build_tidset_dictionary(Transactions, TidsetDictionary0, TidsetDictionary),
		select_initial_tidsets(ItemDomain, TidsetDictionary, SupportCount, Tidsets).

	build_tidset_dictionary([], TidsetDictionary, TidsetDictionary).
	build_tidset_dictionary([TransactionOrdinal-Transaction| Transactions], TidsetDictionary0, TidsetDictionary) :-
		add_transaction_items(Transaction, TransactionOrdinal, TidsetDictionary0, TidsetDictionary1),
		build_tidset_dictionary(Transactions, TidsetDictionary1, TidsetDictionary).

	add_transaction_items([], _TransactionOrdinal, TidsetDictionary, TidsetDictionary).
	add_transaction_items([Item| Items], TransactionOrdinal, TidsetDictionary0, TidsetDictionary) :-
		increment_item_tidset(Item, TransactionOrdinal, TidsetDictionary0, TidsetDictionary1),
		add_transaction_items(Items, TransactionOrdinal, TidsetDictionary1, TidsetDictionary).

	increment_item_tidset(Item, TransactionOrdinal, TidsetDictionary0, TidsetDictionary) :-
		(   dictionary_lookup(Item, tidset_support(RevTidset0, Support0), TidsetDictionary0) ->
			RevTidset = [TransactionOrdinal| RevTidset0],
			Support is Support0 + 1
		;   RevTidset = [TransactionOrdinal],
			Support = 1
		),
		dictionary_insert(TidsetDictionary0, Item, tidset_support(RevTidset, Support), TidsetDictionary).

	select_initial_tidsets([], _TidsetDictionary, _SupportCount, []).
	select_initial_tidsets([Item| Items], TidsetDictionary, SupportCount, Tidsets) :-
		(   dictionary_lookup(Item, tidset_support(RevTidset, Support), TidsetDictionary), Support >= SupportCount ->
			reverse(RevTidset, Tidset),
			Tidsets = [item_tidset(Item, Tidset)| RestTidsets]
		;   Tidsets = RestTidsets
		),
		select_initial_tidsets(Items, TidsetDictionary, SupportCount, RestTidsets).

	mine_tidsets([], _Prefix, _SupportCount, _MaximumPatternLength, []).
	mine_tidsets([item_tidset(Item, Tidset)| Tidsets], Prefix, SupportCount, MaximumPatternLength, Patterns) :-
		extend_prefix(Prefix, Item, PatternItems),
		length(PatternItems, PatternLength),
		(   PatternLength =< MaximumPatternLength ->
			length(Tidset, Support),
			frequent_extensions(Tidsets, Tidset, SupportCount, Extensions),
			mine_tidsets(Extensions, PatternItems, SupportCount, MaximumPatternLength, ExtensionPatterns),
			Patterns = [itemset(PatternItems, Support)| TailPatterns],
			append(ExtensionPatterns, RestPatterns, TailPatterns)
		;   Patterns = RestPatterns
		),
		mine_tidsets(Tidsets, Prefix, SupportCount, MaximumPatternLength, RestPatterns).

	extend_prefix(Prefix, Item, PatternItems) :-
		append(Prefix, [Item], PatternItems).

	frequent_extensions([], _PrefixTidset, _SupportCount, []).
	frequent_extensions([item_tidset(Item, Tidset)| Tidsets], PrefixTidset, SupportCount, Extensions) :-
		intersect_tidsets(PrefixTidset, Tidset, Intersection),
		length(Intersection, Support),
		(   Support >= SupportCount ->
			Extensions = [item_tidset(Item, Intersection)| RestExtensions]
		;   Extensions = RestExtensions
		),
		frequent_extensions(Tidsets, PrefixTidset, SupportCount, RestExtensions).

	intersect_tidsets([], _Tidset, []) :-
		!.
	intersect_tidsets(_Tidset, [], []) :-
		!.
	intersect_tidsets([Tid| Tids1], [Tid| Tids2], [Tid| Intersection]) :-
		!,
		intersect_tidsets(Tids1, Tids2, Intersection).
	intersect_tidsets([Tid1| Tids1], [Tid2| Tids2], Intersection) :-
		(   Tid1 < Tid2 ->
			intersect_tidsets(Tids1, [Tid2| Tids2], Intersection)
		;   intersect_tidsets([Tid1| Tids1], Tids2, Intersection)
		).

	pattern_miner_diagnostics_data(eclat_pattern_miner(ItemDomain, Patterns, Options), Diagnostics) :-
		^^pattern_miner_diagnostics(eclat, ItemDomain, Patterns, Options, [
			search_strategy(depth_first_prefix_extension),
			extension_operator(tidset_intersection),
			support_layout(vertical_tidsets)
		], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(   PatternMiner = eclat_pattern_miner(ItemDomain, Patterns, Options),
			^^valid_itemset_patterns(ItemDomain, Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(eclat, ItemDomain, Patterns, Options, Diagnostics),
			memberchk(search_strategy(depth_first_prefix_extension), Diagnostics),
			memberchk(extension_operator(tidset_intersection), Diagnostics),
			memberchk(support_layout(vertical_tidsets), Diagnostics) ->
			true
		;   domain_error(eclat_pattern_miner, PatternMiner)
		).

	pattern_miner_export_template(_Dataset, eclat_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(eclat_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('Eclat Pattern Miner~n', []),
		format('====================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

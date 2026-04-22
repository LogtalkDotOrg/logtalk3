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
		date is 2026-04-22,
		comment is 'Eclat frequent itemset miner for transaction datasets using portable vertical tidsets and depth-first recursive prefix extension.',
		remarks is [
			'Algorithm' - 'Builds vertical tidsets for frequent singleton items and recursively extends them by lexicographic suffix joins and tidset intersections.',
			'Dataset handling' - 'Requires a dataset implementing ``transaction_dataset_protocol`` with transactions represented as canonical sorted lists of unique declared items.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``eclat_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores ``itemset(Items, SupportCount)`` terms ordered first by pattern length and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, transaction_dataset_protocol, apriori, fp_growth]
	]).

	:- public(mine/3).
	:- mode(mine(+object_identifier, -compound, +list(compound)), one).
	:- info(mine/3, [
		comment is 'Mines frequent itemsets from the given transaction dataset using the specified options.',
		argnames is ['Dataset', 'PatternMiner', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	mine(Dataset, PatternMiner, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::items(ItemDomain),
		^^check_item_domain(ItemDomain),
		findall(Id-Transaction, Dataset::transaction(Id, Transaction), Transactions),
		^^check_transactions(Dataset, ItemDomain, Transactions, MaxTransactionLength),
		length(Transactions, TransactionCount),
		^^effective_support_count(TransactionCount, Options, SupportCount),
		^^effective_maximum_pattern_length(MaxTransactionLength, Options, MaximumPatternLength),
		build_initial_tidsets(ItemDomain, Transactions, SupportCount, InitialTidsets),
		mine_tidsets(InitialTidsets, [], SupportCount, MaximumPatternLength, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = eclat_pattern_miner(ItemDomain, Patterns, Options).

	build_initial_tidsets([], _Transactions, _SupportCount, []).
	build_initial_tidsets([Item| Items], Transactions, SupportCount, Tidsets) :-
		item_tidset(Transactions, Item, Tidset),
		length(Tidset, Support),
		(   Support >= SupportCount ->
			Tidsets = [item_tidset(Item, Tidset)| RestTidsets]
		;   Tidsets = RestTidsets
		),
		build_initial_tidsets(Items, Transactions, SupportCount, RestTidsets).

	item_tidset([], _Item, []).
	item_tidset([Id-Transaction| Transactions], Item, Tidset) :-
		(   member(Item, Transaction) ->
			Tidset = [Id| RestTidset]
		;   Tidset = RestTidset
		),
		item_tidset(Transactions, Item, RestTidset).

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

	pattern_miner_export_template(_Dataset, eclat_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(eclat_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('Eclat Pattern Miner~n', []),
		format('====================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

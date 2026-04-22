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


:- object(fp_growth,
	imports(frequent_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'FP-growth frequent itemset miner for transaction datasets using recursive conditional pattern-base projection.',
		remarks is [
			'Algorithm' - 'Builds a compact FP-tree from globally ordered frequent items and mines conditional pattern bases recursively without candidate generation.',
			'Dataset handling' - 'Requires a dataset implementing ``transaction_dataset_protocol`` with transactions represented as canonical sorted lists of unique declared items.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``fp_growth_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores ``itemset(Items, SupportCount)`` terms ordered first by pattern length and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, transaction_dataset_protocol, apriori]
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
		append/3, length/2, member/2, reverse/2
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
		count_transaction_items(Transactions, [], ItemCounts0),
		^^select_frequent_item_supports(ItemCounts0, SupportCount, FrequentItemSupports0),
		sort_item_supports_by_frequency(FrequentItemSupports0, FrequentItemSupports),
		order_transactions(Transactions, FrequentItemSupports, OrderedTransactions),
		build_fp_tree(OrderedTransactions, Tree),
		mine_tree(FrequentItemSupports, Tree, SupportCount, MaximumPatternLength, [], Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = fp_growth_pattern_miner(ItemDomain, Patterns, Options),
		!.

	count_transaction_items([], ItemCounts, ItemCounts).
	count_transaction_items([_Id-Transaction| Transactions], ItemCounts0, ItemCounts) :-
		^^count_items(Transaction, 1, ItemCounts0, ItemCounts1),
		count_transaction_items(Transactions, ItemCounts1, ItemCounts).

	count_pattern_base_items([], ItemCounts, ItemCounts).
	count_pattern_base_items([Count-Items| Paths], ItemCounts0, ItemCounts) :-
		^^count_items(Items, Count, ItemCounts0, ItemCounts1),
		count_pattern_base_items(Paths, ItemCounts1, ItemCounts).

	sort_item_supports_by_frequency(ItemSupports0, ItemSupports) :-
		decorate_item_supports_by_frequency(ItemSupports0, DecoratedItemSupports),
		keysort(DecoratedItemSupports, SortedDecoratedItemSupports),
		undecorate_item_supports(SortedDecoratedItemSupports, ItemSupports).

	decorate_item_supports_by_frequency([], []).
	decorate_item_supports_by_frequency([item_support(Item, Support)| ItemSupports], [key(NegatedSupport, Item)-item_support(Item, Support)| DecoratedItemSupports]) :-
		NegatedSupport is -Support,
		decorate_item_supports_by_frequency(ItemSupports, DecoratedItemSupports).

	undecorate_item_supports([], []).
	undecorate_item_supports([_Key-ItemSupport| DecoratedItemSupports], [ItemSupport| ItemSupports]) :-
		undecorate_item_supports(DecoratedItemSupports, ItemSupports).

	order_transactions([], _FrequentItemSupports, []).
	order_transactions([_Id-Transaction| Transactions], FrequentItemSupports, OrderedTransactions) :-
		order_transaction(Transaction, FrequentItemSupports, OrderedTransaction),
		(   OrderedTransaction == [] ->
			OrderedTransactions = RestOrderedTransactions
		;   OrderedTransactions = [1-OrderedTransaction| RestOrderedTransactions]
		),
		order_transactions(Transactions, FrequentItemSupports, RestOrderedTransactions).

	order_weighted_paths([], _FrequentItemSupports, []).
	order_weighted_paths([Count-Path| Paths], FrequentItemSupports, OrderedPaths) :-
		order_transaction(Path, FrequentItemSupports, OrderedPath),
		(   OrderedPath == [] ->
			OrderedPaths = RestOrderedPaths
		;   OrderedPaths = [Count-OrderedPath| RestOrderedPaths]
		),
		order_weighted_paths(Paths, FrequentItemSupports, RestOrderedPaths).

	order_transaction(Transaction, FrequentItemSupports, OrderedTransaction) :-
		order_transaction_by_supports(FrequentItemSupports, Transaction, OrderedTransaction).

	order_transaction_by_supports([], _Transaction, []).
	order_transaction_by_supports([item_support(Item, _Support)| FrequentItemSupports], Transaction, OrderedTransaction) :-
		(   member(Item, Transaction) ->
			OrderedTransaction = [Item| RestOrderedTransaction]
		;   OrderedTransaction = RestOrderedTransaction
		),
		order_transaction_by_supports(FrequentItemSupports, Transaction, RestOrderedTransaction).

	build_fp_tree(Transactions, Tree) :-
		build_fp_tree(Transactions, tree([]), Tree).

	build_fp_tree([], Tree, Tree).
	build_fp_tree([Count-Transaction| Transactions], Tree0, Tree) :-
		insert_transaction(Transaction, Count, Tree0, Tree1),
		build_fp_tree(Transactions, Tree1, Tree).

	insert_transaction([], _Count, Tree, Tree).
	insert_transaction([Item| Items], Count, tree(Children0), tree(Children)) :-
		insert_child(Item, Items, Count, Children0, Children).

	insert_child(Item, Items, Count, [], [node(Item, Count, Children)]) :-
		insert_transaction(Items, Count, tree([]), tree(Children)).
	insert_child(Item, Items, Count, [node(Item, Count0, Children0)| Siblings], [node(Item, Count1, Children)| Siblings]) :-
		!,
		Count1 is Count0 + Count,
		insert_transaction(Items, Count, tree(Children0), tree(Children)).
	insert_child(Item, Items, Count, [Node| Siblings0], [Node| Siblings]) :-
		insert_child(Item, Items, Count, Siblings0, Siblings).

	mine_tree([], _Tree, _SupportCount, _MaximumPatternLength, _Suffix, []).
	mine_tree([item_support(Item, Support)| ItemSupports], Tree, SupportCount, MaximumPatternLength, Suffix, Patterns) :-
		canonical_pattern(Item, Suffix, Pattern),
		length(Pattern, PatternLength),
		(   PatternLength =< MaximumPatternLength ->
			conditional_pattern_base(Tree, Item, PatternBase),
			conditional_patterns(PatternBase, SupportCount, MaximumPatternLength, Pattern, ConditionalPatterns),
			Patterns = [itemset(Pattern, Support)| ConditionalPatternsAndRest],
			append(ConditionalPatterns, RestPatterns, ConditionalPatternsAndRest)
		;   Patterns = RestPatterns
		),
		mine_tree(ItemSupports, Tree, SupportCount, MaximumPatternLength, Suffix, RestPatterns).

	conditional_patterns(_PatternBase, _SupportCount, MaximumPatternLength, Pattern, []) :-
		length(Pattern, PatternLength),
		PatternLength >= MaximumPatternLength,
		!.
	conditional_patterns(PatternBase, SupportCount, MaximumPatternLength, Pattern, Patterns) :-
		count_pattern_base_items(PatternBase, [], ItemCounts0),
		^^select_frequent_item_supports(ItemCounts0, SupportCount, FrequentItemSupports0),
		(   FrequentItemSupports0 == [] ->
			Patterns = []
		;   sort_item_supports_by_frequency(FrequentItemSupports0, FrequentItemSupports),
			order_weighted_paths(PatternBase, FrequentItemSupports, OrderedPaths),
			build_fp_tree(OrderedPaths, ConditionalTree),
			reverse(FrequentItemSupports, MiningItemSupports),
			mine_tree(MiningItemSupports, ConditionalTree, SupportCount, MaximumPatternLength, Pattern, Patterns)
		).

	conditional_pattern_base(tree(Children), Item, PatternBase) :-
		conditional_pattern_base(Children, [], Item, PatternBase).

	conditional_pattern_base([], _Prefix, _Item, []).
	conditional_pattern_base([node(NodeItem, Count, Children)| Siblings], Prefix, Item, PatternBase) :-
		conditional_pattern_base(Siblings, Prefix, Item, SiblingPatternBase),
		(   NodeItem == Item ->
			(   Prefix == [] ->
				PatternBase = SiblingPatternBase
			;   PatternBase = [Count-Prefix| SiblingPatternBase]
			)
		;   append(Prefix, [NodeItem], NodePrefix),
			conditional_pattern_base(Children, NodePrefix, Item, ChildPatternBase),
			append(ChildPatternBase, SiblingPatternBase, PatternBase)
		).

	canonical_pattern(Item, Suffix, Pattern) :-
		sort([Item| Suffix], Pattern).

	pattern_miner_export_template(_Dataset, fp_growth_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(fp_growth_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('FP-Growth Pattern Miner~n', []),
		format('========================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

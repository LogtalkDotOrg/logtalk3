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


:- object(apriori,
	imports(frequent_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Apriori frequent itemset miner for transaction datasets using deterministic level-wise candidate generation and anti-monotone pruning.',
		remarks is [
			'Algorithm' - 'Builds frequent itemsets level by level by generating deterministic candidate combinations, pruning candidates whose subsets are infrequent, and rescanning transactions to compute support counts.',
			'Dataset handling' - 'Requires a dataset implementing ``transaction_dataset_protocol`` with transactions represented as canonical sorted lists of unique declared items.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``apriori_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores ``itemset(Items, SupportCount)`` terms ordered first by pattern length and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, transaction_dataset_protocol, combinations]
	]).

	:- public(mine/3).
	:- mode(mine(+object_identifier, -compound, +list(compound)), one).
	:- info(mine/3, [
		comment is 'Mines frequent itemsets from the given transaction dataset using the specified options.',
		argnames is ['Dataset', 'PatternMiner', 'Options']
	]).

	:- uses(combinations, [
		distinct_combinations/4
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
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
		mine_levels(ItemDomain, Transactions, SupportCount, MaximumPatternLength, Levels),
		flatten_levels(Levels, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = apriori_pattern_miner(ItemDomain, Patterns, Options).

	mine_levels(_ItemDomain, _Transactions, _SupportCount, MaximumPatternLength, []) :-
		MaximumPatternLength =< 0,
		!.
	mine_levels(ItemDomain, Transactions, SupportCount, MaximumPatternLength, Levels) :-
		frequent_level_one_items(ItemDomain, Transactions, SupportCount, Level1, FrequentItems),
		(   Level1 == [] ->
			Levels = []
		;   MaximumPatternLength =:= 1 ->
			Levels = [Level1]
		;   level_itemsets(Level1, PreviousItemsets),
			Levels = [Level1| RestLevels],
			mine_levels(2, FrequentItems, PreviousItemsets, Transactions, SupportCount, MaximumPatternLength, RestLevels)
		).

	mine_levels(CurrentLength, _FrequentItems, _PreviousItemsets, _Transactions, _SupportCount, MaximumPatternLength, []) :-
		CurrentLength > MaximumPatternLength,
		!.
	mine_levels(CurrentLength, FrequentItems, PreviousItemsets, Transactions, SupportCount, MaximumPatternLength, Levels) :-
		generate_candidates(CurrentLength, FrequentItems, PreviousItemsets, Candidates),
		frequent_candidates(Candidates, Transactions, SupportCount, Level),
		(   Level == [] ->
			Levels = []
		;   level_itemsets(Level, CurrentItemsets),
			Levels = [Level| RestLevels],
			NextLength is CurrentLength + 1,
			mine_levels(NextLength, FrequentItems, CurrentItemsets, Transactions, SupportCount, MaximumPatternLength, RestLevels)
		).

	frequent_level_one_items(ItemDomain, Transactions, SupportCount, Level, FrequentItems) :-
		frequent_level_one(ItemDomain, Transactions, SupportCount, [], Level),
		level_single_items(Level, FrequentItems).

	frequent_level_one([], _Transactions, _SupportCount, Level, Level).
	frequent_level_one([Item| Items], Transactions, SupportCount, Level0, Level) :-
		Candidate = [Item],
		support_count(Candidate, Transactions, Support),
		(   Support >= SupportCount ->
			append(Level0, [itemset(Candidate, Support)], Level1)
		;   Level1 = Level0
		),
		frequent_level_one(Items, Transactions, SupportCount, Level1, Level).

	level_single_items([], []).
	level_single_items([itemset([Item], _Support)| Level], [Item| Items]) :-
		level_single_items(Level, Items).

	level_itemsets([], []).
	level_itemsets([itemset(Items, _Support)| Level], [Items| Itemsets]) :-
		level_itemsets(Level, Itemsets).

	generate_candidates(CurrentLength, FrequentItems, PreviousItemsets, Candidates) :-
		distinct_combinations(CurrentLength, FrequentItems, lexicographic, CandidateCombinations),
		filter_candidates(CandidateCombinations, PreviousItemsets, [], Candidates).

	filter_candidates([], _PreviousItemsets, Candidates, Candidates).
	filter_candidates([Candidate| CandidateCombinations], PreviousItemsets, Candidates0, Candidates) :-
		(   candidate_has_frequent_subsets(Candidate, PreviousItemsets) ->
			append(Candidates0, [Candidate], Candidates1)
		;   Candidates1 = Candidates0
		),
		filter_candidates(CandidateCombinations, PreviousItemsets, Candidates1, Candidates).

	candidate_has_frequent_subsets(Candidate, PreviousItemsets) :-
		length(Candidate, CandidateLength),
		SubsetLength is CandidateLength - 1,
		distinct_combinations(SubsetLength, Candidate, lexicographic, Subsets),
		all_subsets_frequent(Subsets, PreviousItemsets).

	all_subsets_frequent([], _PreviousItemsets).
	all_subsets_frequent([Subset| Subsets], PreviousItemsets) :-
		memberchk(Subset, PreviousItemsets),
		all_subsets_frequent(Subsets, PreviousItemsets).

	frequent_candidates([], _Transactions, _SupportCount, []).
	frequent_candidates([Candidate| Candidates], Transactions, SupportCount, Level) :-
		support_count(Candidate, Transactions, Support),
		(   Support >= SupportCount ->
			Level = [itemset(Candidate, Support)| RestLevel]
		;   Level = RestLevel
		),
		frequent_candidates(Candidates, Transactions, SupportCount, RestLevel).

	support_count(Candidate, Transactions, Support) :-
		support_count(Candidate, Transactions, 0, Support).

	support_count(_Candidate, [], Support, Support) :-
		!.
	support_count(Candidate, [_Id-Transaction| Transactions], Support0, Support) :-
		(   subset_of(Candidate, Transaction) ->
			Support1 is Support0 + 1
		;   Support1 = Support0
		),
		support_count(Candidate, Transactions, Support1, Support).

	subset_of([], _Transaction).
	subset_of([Item| Items], Transaction) :-
		memberchk(Item, Transaction),
		subset_of(Items, Transaction).

	flatten_levels([], []).
	flatten_levels([Level| Levels], Patterns) :-
		append(Level, RestPatterns, Patterns),
		flatten_levels(Levels, RestPatterns).

	pattern_miner_export_template(_Dataset, apriori_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(apriori_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('Apriori Pattern Miner~n', []),
		format('======================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

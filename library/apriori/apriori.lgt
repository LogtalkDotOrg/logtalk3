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
		date is 2026-05-04,
		comment is 'Apriori frequent itemset miner for transaction datasets using deterministic level-wise candidate generation and anti-monotone pruning.',
		see_also is [pattern_miner_protocol, transaction_dataset_protocol, combinations]
	]).

	:- uses(combinations, [
		distinct_combinations/4
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_list/2 as dictionary_as_list/2, insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2, nth1/3
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
		frequent_level_one_items(ItemDomain, Transactions, SupportCount, Level1),
		(	Level1 == [] ->
			Levels = []
		;	MaximumPatternLength =:= 1 ->
			Levels = [Level1]
		;	level_itemsets(Level1, PreviousItemsets),
			Levels = [Level1| RestLevels],
			mine_levels(2, PreviousItemsets, Transactions, SupportCount, MaximumPatternLength, RestLevels)
		).

	mine_levels(CurrentLength, _PreviousItemsets, _Transactions, _SupportCount, MaximumPatternLength, []) :-
		CurrentLength > MaximumPatternLength,
		!.
	mine_levels(CurrentLength, PreviousItemsets, Transactions, SupportCount, MaximumPatternLength, Levels) :-
		generate_candidates(CurrentLength, PreviousItemsets, Candidates),
		frequent_candidates(Candidates, Transactions, SupportCount, Level),
		(	Level == [] ->
			Levels = []
		;	level_itemsets(Level, CurrentItemsets),
			Levels = [Level| RestLevels],
			NextLength is CurrentLength + 1,
			mine_levels(NextLength, CurrentItemsets, Transactions, SupportCount, MaximumPatternLength, RestLevels)
		).

	frequent_level_one_items(ItemDomain, Transactions, SupportCount, Level) :-
		dictionary_new(ItemCounts0),
		count_transactions_items(Transactions, ItemCounts0, ItemCounts),
		frequent_level_one(ItemDomain, ItemCounts, SupportCount, Level).

	count_transactions_items([], ItemCounts, ItemCounts).
	count_transactions_items([_Id-Transaction| Transactions], ItemCounts0, ItemCounts) :-
		count_items_in_transaction(Transaction, ItemCounts0, ItemCounts1),
		count_transactions_items(Transactions, ItemCounts1, ItemCounts).

	count_items_in_transaction([], ItemCounts, ItemCounts).
	count_items_in_transaction([Item| Items], ItemCounts0, ItemCounts) :-
		increment_item_count(Item, ItemCounts0, ItemCounts1),
		count_items_in_transaction(Items, ItemCounts1, ItemCounts).

	increment_item_count(Item, ItemCounts0, ItemCounts) :-
		(   dictionary_lookup(Item, Count0, ItemCounts0) ->
			Count is Count0 + 1
		;   Count = 1
		),
		dictionary_insert(ItemCounts0, Item, Count, ItemCounts).

	frequent_level_one([], _ItemCounts, _SupportCount, []).
	frequent_level_one([Item| Items], ItemCounts, SupportCount, Level) :-
		(	dictionary_lookup(Item, Support, ItemCounts) ->
			true
		;	Support = 0
		),
		(   Support >= SupportCount ->
			Level = [itemset([Item], Support)| LevelTail]
		;   Level = LevelTail
		),
		frequent_level_one(Items, ItemCounts, SupportCount, LevelTail).

	level_itemsets(Level, Itemsets) :-
		level_itemsets(Level, [], Itemsets0),
		sort(Itemsets0, Itemsets).

	level_itemsets([], Itemsets, Itemsets).
	level_itemsets([itemset(Items, _Support)| Level], Itemsets0, Itemsets) :-
		level_itemsets(Level, [Items| Itemsets0], Itemsets).

	generate_candidates(CurrentLength, PreviousItemsets, Candidates) :-
		generate_candidate_pairs(PreviousItemsets, CurrentLength, PreviousItemsets, [], Candidates0),
		sort(Candidates0, Candidates).

	generate_candidate_pairs([], _CurrentLength, _PreviousItemsets, Candidates, Candidates).
	generate_candidate_pairs([LeftItemset| LeftItemsets], CurrentLength, PreviousItemsets, Candidates0, Candidates) :-
		generate_join_candidates(LeftItemsets, LeftItemset, CurrentLength, PreviousItemsets, Candidates0, Candidates1),
		generate_candidate_pairs(LeftItemsets, CurrentLength, PreviousItemsets, Candidates1, Candidates).

	generate_join_candidates([], _LeftItemset, _CurrentLength, _PreviousItemsets, Candidates, Candidates).
	generate_join_candidates([RightItemset| RightItemsets], LeftItemset, CurrentLength, PreviousItemsets, Candidates0, Candidates) :-
		(	joinable_itemsets(CurrentLength, LeftItemset, RightItemset, Candidate),
			candidate_has_frequent_subsets(Candidate, PreviousItemsets) ->
			Candidates1 = [Candidate| Candidates0]
		;	Candidates1 = Candidates0
		),
		generate_join_candidates(RightItemsets, LeftItemset, CurrentLength, PreviousItemsets, Candidates1, Candidates).

	joinable_itemsets(CurrentLength, LeftItemset, RightItemset, Candidate) :-
		PrefixLength is CurrentLength - 2,
		append(Prefix, [LeftLastItem], LeftItemset),
		append(Prefix, [RightLastItem], RightItemset),
		length(Prefix, PrefixLength),
		LeftLastItem @< RightLastItem,
		append(LeftItemset, [RightLastItem], Candidate),
		!.

	candidate_has_frequent_subsets(Candidate, PreviousItemsets) :-
		length(Candidate, CandidateLength),
		SubsetLength is CandidateLength - 1,
		distinct_combinations(SubsetLength, Candidate, lexicographic, Subsets),
		all_subsets_frequent(Subsets, PreviousItemsets).

	all_subsets_frequent(Subsets, PreviousItemsets) :-
		all_subsets_frequent(Subsets, PreviousItemsets, _RemainingItemsets).

	all_subsets_frequent([], PreviousItemsets, PreviousItemsets).
	all_subsets_frequent([Subset| Subsets], PreviousItemsets, RemainingItemsets) :-
		advance_to_itemset(Subset, PreviousItemsets, ItemsetsAfterSubset),
		ItemsetsAfterSubset = [_FoundSubset| RestItemsets],
		all_subsets_frequent(Subsets, RestItemsets, RemainingItemsets).

	advance_to_itemset(Target, [Itemset| Itemsets], RemainingItemsets) :-
		compare(Order, Itemset, Target),
		(   Order == (=) ->
			RemainingItemsets = [Itemset| Itemsets]
		;   Order == (<) ->
			advance_to_itemset(Target, Itemsets, RemainingItemsets)
		;   fail
		).

	frequent_candidates([], _Transactions, _SupportCount, []).
	frequent_candidates([Candidate| Candidates], Transactions, SupportCount, Level) :-
		initialize_candidate_hash_tree([Candidate| Candidates], CandidateHashTree0),
		count_level_candidates(Transactions, CandidateHashTree0, CandidateHashTree),
		select_frequent_candidates(CandidateHashTree, SupportCount, Level).

	initialize_candidate_hash_tree(Candidates, CandidateHashTree) :-
		candidates_length(Candidates, CandidateLength),
		initialize_candidate_hash_tree(1, CandidateLength, Candidates, CandidateHashTree).

	initialize_candidate_hash_tree(Depth, CandidateLength, Candidates, leaf(CandidateCounts)) :-
		Depth > CandidateLength,
		!,
		initialize_candidate_counts(Candidates, CandidateCounts).
	initialize_candidate_hash_tree(Depth, CandidateLength, Candidates, hash_tree(BucketDictionary)) :-
		dictionary_new(BucketDictionary0),
		initialize_candidate_hash_tree(Candidates, Depth, CandidateLength, hash_tree(BucketDictionary0), hash_tree(BucketDictionary)).

	initialize_candidate_hash_tree([], _Depth, _CandidateLength, CandidateHashTree, CandidateHashTree).
	initialize_candidate_hash_tree([Candidate| Candidates], Depth, CandidateLength, CandidateHashTree0, CandidateHashTree) :-
		insert_candidate_hash_tree(Candidate, Depth, CandidateLength, CandidateHashTree0, CandidateHashTree1),
		initialize_candidate_hash_tree(Candidates, Depth, CandidateLength, CandidateHashTree1, CandidateHashTree).

	candidates_length([Candidate| _Candidates], CandidateLength) :-
		length(Candidate, CandidateLength).

	insert_candidate_hash_tree(Candidate, Depth, CandidateLength, hash_tree(BucketDictionary0), hash_tree(BucketDictionary)) :-
		nth1(Depth, Candidate, Item),
		hash_item(Item, Hash),
		(   dictionary_lookup(Hash, ItemDictionary0, BucketDictionary0) ->
			true
		;   dictionary_new(ItemDictionary0)
		),
		insert_candidate_item_tree(Candidate, Item, Depth, CandidateLength, ItemDictionary0, ItemDictionary),
		dictionary_insert(BucketDictionary0, Hash, ItemDictionary, BucketDictionary).

	insert_candidate_item_tree(Candidate, Item, Depth, CandidateLength, ItemDictionary0, ItemDictionary) :-
		(   dictionary_lookup(Item, CandidateHashTree0, ItemDictionary0) ->
			insert_candidate_subtree(Candidate, Depth, CandidateLength, CandidateHashTree0, CandidateHashTree)
		;   initialize_candidate_subtree(Candidate, Depth, CandidateLength, CandidateHashTree)
		),
		dictionary_insert(ItemDictionary0, Item, CandidateHashTree, ItemDictionary).

	initialize_candidate_subtree(Candidate, Depth, CandidateLength, leaf(candidate_count(Candidate, 0))) :-
		NextDepth is Depth + 1,
		NextDepth > CandidateLength,
		!.
	initialize_candidate_subtree(Candidate, Depth, CandidateLength, CandidateHashTree) :-
		NextDepth is Depth + 1,
		initialize_candidate_hash_tree(NextDepth, CandidateLength, [Candidate], CandidateHashTree).

	insert_candidate_subtree(Candidate, Depth, CandidateLength, leaf(candidate_count(Candidate, Count)), leaf(candidate_count(Candidate, Count))) :-
		NextDepth is Depth + 1,
		NextDepth > CandidateLength,
		!.
	insert_candidate_subtree(Candidate, Depth, CandidateLength, CandidateHashTree0, CandidateHashTree) :-
		NextDepth is Depth + 1,
		insert_candidate_hash_tree(Candidate, NextDepth, CandidateLength, CandidateHashTree0, CandidateHashTree).

	hash_item(Item, Hash) :-
		atom_codes(Item, Bytes),
		fnv1a_32::hash(Bytes, Hash).

	initialize_candidate_counts([], []).
	initialize_candidate_counts([Candidate| Candidates], [candidate_count(Candidate, 0)| CandidateCounts]) :-
		initialize_candidate_counts(Candidates, CandidateCounts).

	count_level_candidates([], CandidateHashTree, CandidateHashTree).
	count_level_candidates([_Id-Transaction| Transactions], CandidateHashTree0, CandidateHashTree) :-
		count_transaction_candidates(CandidateHashTree0, Transaction, CandidateHashTree1),
		count_level_candidates(Transactions, CandidateHashTree1, CandidateHashTree).

	count_transaction_candidates(leaf(candidate_count(Candidate, Count0)), _Transaction, leaf(candidate_count(Candidate, Count1))) :-
		Count1 is Count0 + 1.
	count_transaction_candidates(hash_tree(BucketDictionary), [], hash_tree(BucketDictionary)) :-
		!.
	count_transaction_candidates(hash_tree(BucketDictionary0), [Item| Transaction], hash_tree(BucketDictionary)) :-
		hash_item(Item, Hash),
		(   dictionary_lookup(Hash, ItemDictionary0, BucketDictionary0),
			dictionary_lookup(Item, CandidateHashTree0, ItemDictionary0) ->
			count_transaction_candidates(CandidateHashTree0, Transaction, CandidateHashTree),
			dictionary_insert(ItemDictionary0, Item, CandidateHashTree, ItemDictionary),
			dictionary_insert(BucketDictionary0, Hash, ItemDictionary, BucketDictionary1)
		;   BucketDictionary1 = BucketDictionary0
		),
		count_transaction_candidates(hash_tree(BucketDictionary1), Transaction, hash_tree(BucketDictionary)).

	select_frequent_candidates(leaf(candidate_count(Candidate, Support)), SupportCount, Level) :-
		(   Support >= SupportCount ->
			Level = [itemset(Candidate, Support)]
		;   Level = []
		).
	select_frequent_candidates(hash_tree(BucketDictionary), SupportCount, Level) :-
		dictionary_as_list(BucketDictionary, BucketPairs),
		select_frequent_bucket_pairs(BucketPairs, SupportCount, Level).

	select_frequent_bucket_pairs([], _SupportCount, []).
	select_frequent_bucket_pairs([_Hash-ItemDictionary| BucketPairs], SupportCount, Level) :-
		dictionary_as_list(ItemDictionary, ItemPairs),
		select_frequent_item_pairs(ItemPairs, SupportCount, BranchLevel),
		append(BranchLevel, RestLevel, Level),
		select_frequent_bucket_pairs(BucketPairs, SupportCount, RestLevel).

	select_frequent_item_pairs([], _SupportCount, []).
	select_frequent_item_pairs([_Item-CandidateHashTree| ItemPairs], SupportCount, Level) :-
		select_frequent_candidates(CandidateHashTree, SupportCount, BranchLevel),
		append(BranchLevel, RestLevel, Level),
		select_frequent_item_pairs(ItemPairs, SupportCount, RestLevel).

	flatten_levels([], []).
	flatten_levels([Level| Levels], Patterns) :-
		append(Level, RestPatterns, Patterns),
		flatten_levels(Levels, RestPatterns).

	pattern_miner_diagnostics_data(apriori_pattern_miner(ItemDomain, Patterns, Options), Diagnostics) :-
		^^pattern_miner_diagnostics(apriori, ItemDomain, Patterns, Options, [
			search_strategy(level_wise_breadth_first),
			candidate_generation(join_prune),
			candidate_counting(hash_tree),
			pruning(anti_monotone_subsets),
			support_layout(horizontal_transactions)
		], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(   PatternMiner = apriori_pattern_miner(ItemDomain, Patterns, Options),
			^^valid_itemset_patterns(ItemDomain, Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(apriori, ItemDomain, Patterns, Options, Diagnostics),
			memberchk(search_strategy(level_wise_breadth_first), Diagnostics),
			memberchk(candidate_generation(join_prune), Diagnostics),
			memberchk(candidate_counting(hash_tree), Diagnostics),
			memberchk(pruning(anti_monotone_subsets), Diagnostics),
			memberchk(support_layout(horizontal_transactions), Diagnostics) ->
			true
		;   domain_error(apriori_pattern_miner, PatternMiner)
		).

	pattern_miner_export_template(_Dataset, apriori_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(apriori_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('Apriori Pattern Miner~n', []),
		format('======================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

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


:- object(gsp_pattern_miner,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'GSP sequential pattern miner for sequence datasets using level-wise candidate generation and pruning.',
		see_also is [pattern_miner_protocol, sequence_dataset_protocol, prefix_span_pattern_miner, spade_pattern_miner, clo_span_pattern_miner]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
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
		frequent_singleton_patterns(ItemDomain, Sequences, SupportCount, FrequentSingletons),
		mine_levels(FrequentSingletons, Sequences, SupportCount, MaximumPatternLength, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = gsp_pattern_miner(ItemDomain, Patterns, Options).

	frequent_singleton_patterns(ItemDomain, Sequences, SupportCount, Patterns) :-
		dictionary_new(ItemCounts0),
		count_singleton_supports(Sequences, ItemCounts0, ItemCounts),
		select_frequent_singleton_patterns(ItemDomain, ItemCounts, SupportCount, Patterns).

	mine_levels([], _Sequences, _SupportCount, _MaximumPatternLength, []) :-
		!.
	mine_levels(CurrentPatterns, Sequences, SupportCount, MaximumPatternLength, Patterns) :-
		current_level_patterns(CurrentPatterns, CurrentLevelPatterns),
		candidate_patterns(CurrentLevelPatterns, MaximumPatternLength, Candidates),
		frequent_candidate_patterns(Candidates, CurrentLevelPatterns, Sequences, SupportCount, FrequentNextPatterns),
		(	FrequentNextPatterns == [] ->
			Patterns = CurrentPatterns
		;	append(CurrentPatterns, LaterPatterns, Patterns),
			mine_levels(FrequentNextPatterns, Sequences, SupportCount, MaximumPatternLength, LaterPatterns)
		).

	current_level_patterns([], []).
	current_level_patterns([sequence_pattern(Pattern, _Support)| Patterns], [Pattern| CurrentLevelPatterns]) :-
		current_level_patterns(Patterns, CurrentLevelPatterns).

	candidate_patterns([], _MaximumPatternLength, []) :-
		!.
	candidate_patterns(CurrentLevelPatterns, MaximumPatternLength, Candidates) :-
		CurrentLevelPatterns = [[[ _Item ]]| _],
		!,
		singleton_candidate_patterns(CurrentLevelPatterns, MaximumPatternLength, Candidates).
	candidate_patterns(CurrentLevelPatterns, MaximumPatternLength, Candidates) :-
		dictionary_new(JoinIndex0),
		index_candidate_extensions(CurrentLevelPatterns, JoinIndex0, JoinIndex),
		generate_indexed_candidate_patterns(CurrentLevelPatterns, JoinIndex, MaximumPatternLength, [], Candidates0),
		sort(Candidates0, Candidates).

	singleton_candidate_patterns(_CurrentLevelPatterns, MaximumPatternLength, []) :-
		MaximumPatternLength < 2,
		!.
	singleton_candidate_patterns(CurrentLevelPatterns, _MaximumPatternLength, Candidates) :-
		singleton_pattern_items(CurrentLevelPatterns, Items),
		singleton_level_candidates(Items, Items, Candidates).

	singleton_pattern_items([], []).
	singleton_pattern_items([[[Item]]| Patterns], [Item| Items]) :-
		singleton_pattern_items(Patterns, Items).

	singleton_level_candidates([], _AllItems, []).
	singleton_level_candidates([LeftItem| LeftItems], AllItems, Candidates) :-
		same_event_singleton_candidates(LeftItems, LeftItem, Candidates, SequenceCandidates),
		sequence_singleton_candidates(AllItems, LeftItem, SequenceCandidates, RestCandidates),
		singleton_level_candidates(LeftItems, AllItems, RestCandidates).

	same_event_singleton_candidates([], _LeftItem, Candidates, Candidates).
	same_event_singleton_candidates([RightItem| RightItems], LeftItem, [[[LeftItem, RightItem]]| Candidates], RestCandidates) :-
		same_event_singleton_candidates(RightItems, LeftItem, Candidates, RestCandidates).

	sequence_singleton_candidates([], _LeftItem, Candidates, Candidates).
	sequence_singleton_candidates([RightItem| RightItems], LeftItem, [[[LeftItem], [RightItem]]| Candidates], RestCandidates) :-
		sequence_singleton_candidates(RightItems, LeftItem, Candidates, RestCandidates).

	index_candidate_extensions([], JoinIndex, JoinIndex).
	index_candidate_extensions([Pattern| Patterns], JoinIndex0, JoinIndex) :-
		remove_last_item(Pattern, JoinPrefix, Extension),
		index_candidate_extension(JoinPrefix, Extension, JoinIndex0, JoinIndex1),
		index_candidate_extensions(Patterns, JoinIndex1, JoinIndex).

	index_candidate_extension(JoinPrefix, Extension, JoinIndex0, JoinIndex) :-
		(	dictionary_lookup(JoinPrefix, Extensions0, JoinIndex0) ->
			Extensions = [Extension| Extensions0]
		;	Extensions = [Extension]
		),
		dictionary_insert(JoinIndex0, JoinPrefix, Extensions, JoinIndex).

	generate_indexed_candidate_patterns([], _JoinIndex, _MaximumPatternLength, Candidates, Candidates).
	generate_indexed_candidate_patterns([LeftPattern| LeftPatterns], JoinIndex, MaximumPatternLength, Candidates0, Candidates) :-
		remove_first_item(LeftPattern, JoinSuffix),
		(	dictionary_lookup(JoinSuffix, Extensions, JoinIndex) ->
			generate_extension_candidates(Extensions, LeftPattern, MaximumPatternLength, Candidates0, Candidates1)
		;	Candidates1 = Candidates0
		),
		generate_indexed_candidate_patterns(LeftPatterns, JoinIndex, MaximumPatternLength, Candidates1, Candidates).

	generate_extension_candidates([], _LeftPattern, _MaximumPatternLength, Candidates, Candidates).
	generate_extension_candidates([Extension| Extensions], LeftPattern, MaximumPatternLength, Candidates0, Candidates) :-
		apply_extension(Extension, LeftPattern, Candidate),
		^^pattern_length(Candidate, PatternLength),
		(	PatternLength =< MaximumPatternLength ->
			Candidates1 = [Candidate| Candidates0]
		;	Candidates1 = Candidates0
		),
		generate_extension_candidates(Extensions, LeftPattern, MaximumPatternLength, Candidates1, Candidates).

	remove_first_item([[ _Item | RemainingItems] | Pattern], ReducedPattern) :-
		(	RemainingItems == [] ->
			ReducedPattern = Pattern
		;	ReducedPattern = [RemainingItems| Pattern]
		).

	remove_last_item(Pattern, ReducedPattern, Extension) :-
		append(Prefix, [LastItemset], Pattern),
		(	LastItemset = [Item] ->
			ReducedPattern = Prefix,
			Extension = s(Item)
		;	append(RemainingItems, [Item], LastItemset),
			append(Prefix, [RemainingItems], ReducedPattern),
			Extension = i(Item)
		),
		!.

	apply_extension(s(Item), Pattern, Candidate) :-
		append(Pattern, [[Item]], Candidate).
	apply_extension(i(Item), Pattern, Candidate) :-
		append(LeadingItemsets, [LastItemset], Pattern),
		append(LastItemset, [Item], ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], Candidate).

	frequent_candidate_patterns(Candidates, CurrentLevelPatterns, Sequences, SupportCount, FrequentPatterns) :-
		current_level_pattern_index(CurrentLevelPatterns, CurrentLevelPatternIndex),
		prune_candidates(Candidates, CurrentLevelPatternIndex, PrunedCandidates),
		dictionary_new(CandidateCounts0),
		count_candidate_supports(Sequences, PrunedCandidates, CandidateCounts0, CandidateCounts),
		select_frequent_candidate_patterns(PrunedCandidates, CandidateCounts, SupportCount, FrequentPatterns).

	current_level_pattern_index(CurrentLevelPatterns, CurrentLevelPatternIndex) :-
		dictionary_new(CurrentLevelPatternIndex0),
		index_current_level_patterns(CurrentLevelPatterns, CurrentLevelPatternIndex0, CurrentLevelPatternIndex).

	index_current_level_patterns([], CurrentLevelPatternIndex, CurrentLevelPatternIndex).
	index_current_level_patterns([Pattern| Patterns], CurrentLevelPatternIndex0, CurrentLevelPatternIndex) :-
		dictionary_insert(CurrentLevelPatternIndex0, Pattern, true, CurrentLevelPatternIndex1),
		index_current_level_patterns(Patterns, CurrentLevelPatternIndex1, CurrentLevelPatternIndex).

	prune_candidates([], _CurrentLevelPatternIndex, []).
	prune_candidates([Candidate| Candidates], CurrentLevelPatternIndex, PrunedCandidates) :-
		candidate_subpatterns(Candidate, CandidateSubpatterns),
		(	all_subpatterns_frequent(CandidateSubpatterns, CurrentLevelPatternIndex) ->
			PrunedCandidates = [Candidate| RestPrunedCandidates]
		;	PrunedCandidates = RestPrunedCandidates
		),
		prune_candidates(Candidates, CurrentLevelPatternIndex, RestPrunedCandidates).

	candidate_subpatterns(Pattern, Subpatterns) :-
		findall(Subpattern, immediate_subpattern(Pattern, Subpattern), Subpatterns0),
		sort(Subpatterns0, Subpatterns).

	immediate_subpattern(Pattern, Subpattern) :-
		append(Prefix, [Itemset| Suffix], Pattern),
		append(ItemsBefore, [_| ItemsAfter], Itemset),
		(	ItemsBefore == [], ItemsAfter == [] ->
			append(Prefix, Suffix, Subpattern)
		;	append(ItemsBefore, ItemsAfter, ReducedItemset),
			append(Prefix, [ReducedItemset| Suffix], Subpattern)
		).

	all_subpatterns_frequent([], _CurrentLevelPatternIndex).
	all_subpatterns_frequent([Subpattern| Subpatterns], CurrentLevelPatternIndex) :-
		dictionary_lookup(Subpattern, true, CurrentLevelPatternIndex),
		all_subpatterns_frequent(Subpatterns, CurrentLevelPatternIndex).

	count_singleton_supports([], ItemCounts, ItemCounts).
	count_singleton_supports([_Id-Sequence| Sequences], ItemCounts0, ItemCounts) :-
		sequence_available_items(Sequence, AvailableItems),
		increment_items_dictionary(AvailableItems, ItemCounts0, ItemCounts1),
		count_singleton_supports(Sequences, ItemCounts1, ItemCounts).

	sequence_available_items(Sequence, AvailableItems) :-
		sequence_available_items(Sequence, [], AvailableItems0),
		sort(AvailableItems0, AvailableItems).

	sequence_available_items([], AvailableItems, AvailableItems).
	sequence_available_items([Itemset| Sequence], AvailableItems0, AvailableItems) :-
		append(AvailableItems0, Itemset, AvailableItems1),
		sequence_available_items(Sequence, AvailableItems1, AvailableItems).

	increment_items_dictionary([], ItemCounts, ItemCounts).
	increment_items_dictionary([Item| Items], ItemCounts0, ItemCounts) :-
		increment_dictionary_count(Item, ItemCounts0, ItemCounts1),
		increment_items_dictionary(Items, ItemCounts1, ItemCounts).

	increment_dictionary_count(Key, Dictionary0, Dictionary) :-
		(	dictionary_lookup(Key, Count0, Dictionary0) ->
			Count is Count0 + 1
		;	Count = 1
		),
		dictionary_insert(Dictionary0, Key, Count, Dictionary).

	select_frequent_singleton_patterns([], _ItemCounts, _SupportCount, []).
	select_frequent_singleton_patterns([Item| Items], ItemCounts, SupportCount, Patterns) :-
		(	dictionary_lookup(Item, Support, ItemCounts),
			Support >= SupportCount ->
			Patterns = [sequence_pattern([[Item]], Support)| RestPatterns]
		;	Patterns = RestPatterns
		),
		select_frequent_singleton_patterns(Items, ItemCounts, SupportCount, RestPatterns).

	count_candidate_supports([], _Candidates, CandidateCounts, CandidateCounts).
	count_candidate_supports([_Id-Sequence| Sequences], Candidates, CandidateCounts0, CandidateCounts) :-
		count_sequence_candidate_supports(Candidates, Sequence, CandidateCounts0, CandidateCounts1),
		count_candidate_supports(Sequences, Candidates, CandidateCounts1, CandidateCounts).

	count_sequence_candidate_supports([], _Sequence, CandidateCounts, CandidateCounts).
	count_sequence_candidate_supports([Candidate| Candidates], Sequence, CandidateCounts0, CandidateCounts) :-
		(	pattern_in_sequence(Candidate, Sequence) ->
			increment_dictionary_count(Candidate, CandidateCounts0, CandidateCounts1)
		;	CandidateCounts1 = CandidateCounts0
		),
		count_sequence_candidate_supports(Candidates, Sequence, CandidateCounts1, CandidateCounts).

	select_frequent_candidate_patterns([], _CandidateCounts, _SupportCount, []).
	select_frequent_candidate_patterns([Candidate| Candidates], CandidateCounts, SupportCount, FrequentPatterns) :-
		(	dictionary_lookup(Candidate, Support, CandidateCounts),
			Support >= SupportCount ->
			FrequentPatterns = [sequence_pattern(Candidate, Support)| RestFrequentPatterns]
		;	FrequentPatterns = RestFrequentPatterns
		),
		select_frequent_candidate_patterns(Candidates, CandidateCounts, SupportCount, RestFrequentPatterns).

	pattern_in_sequence([], _Sequence).
	pattern_in_sequence([Itemset| Pattern], Sequence) :-
		select_matching_event(Itemset, Sequence, RestSequence),
		pattern_in_sequence(Pattern, RestSequence).

	select_matching_event(Itemset, [Event| Sequence], Sequence) :-
		itemset_subset(Itemset, Event),
		!.
	select_matching_event(Itemset, [_Event| Sequence0], Sequence) :-
		select_matching_event(Itemset, Sequence0, Sequence).

	itemset_subset([], _Event).
	itemset_subset([Item| Items], Event) :-
		memberchk(Item, Event),
		itemset_subset(Items, Event).

	pattern_miner_diagnostics_data(gsp_pattern_miner(ItemDomain, Patterns, Options), Diagnostics) :-
		^^pattern_miner_diagnostics(gsp_pattern_miner, ItemDomain, Patterns, Options, [
			search_strategy(level_wise_breadth_first),
			candidate_generation(sequential_join_prune),
			extension_modes([itemset, sequence]),
			support_layout(horizontal_sequences)
		], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(	PatternMiner = gsp_pattern_miner(ItemDomain, Patterns, Options),
			^^valid_sequence_patterns(ItemDomain, Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(gsp_pattern_miner, ItemDomain, Patterns, Options, Diagnostics),
			memberchk(search_strategy(level_wise_breadth_first), Diagnostics),
			memberchk(candidate_generation(sequential_join_prune), Diagnostics),
			memberchk(extension_modes([itemset, sequence]), Diagnostics),
			memberchk(support_layout(horizontal_sequences), Diagnostics) ->
			true
		;	domain_error(gsp_pattern_miner, PatternMiner)
		).

	pattern_miner_export_template(_Dataset, gsp_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(gsp_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('GSP Pattern Miner~n', []),
		format('=================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

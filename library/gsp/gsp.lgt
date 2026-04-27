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


:- object(gsp,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'GSP sequential pattern miner for sequence datasets using level-wise candidate generation and pruning.',
		remarks is [
			'Algorithm' - 'Builds frequent sequential patterns level by level by extending frequent patterns, pruning candidates whose immediate subpatterns are not all frequent, and rescanning the dataset for support.',
			'Dataset handling' - 'Requires a dataset implementing ``sequence_dataset_protocol`` with sequences represented as ordered lists of canonical sorted itemsets over a declared item domain.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``gsp_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores ``sequence_pattern(Pattern, SupportCount)`` terms ordered first by total item count and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, sequence_dataset_protocol, prefix_span, spade, clo_span]
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
		frequent_singleton_patterns(ItemDomain, Sequences, SupportCount, FrequentSingletons),
		mine_levels(FrequentSingletons, ItemDomain, Sequences, SupportCount, MaximumPatternLength, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = gsp_pattern_miner(ItemDomain, Patterns, Options),
		!.

	frequent_singleton_patterns([], _Sequences, _SupportCount, []).
	frequent_singleton_patterns([Item| Items], Sequences, SupportCount, Patterns) :-
		pattern_support([[Item]], Sequences, Support),
		(   Support >= SupportCount ->
			Patterns = [sequence_pattern([[Item]], Support)| RestPatterns]
		;   Patterns = RestPatterns
		),
		frequent_singleton_patterns(Items, Sequences, SupportCount, RestPatterns).

	mine_levels([], _ItemDomain, _Sequences, _SupportCount, _MaximumPatternLength, []).
	mine_levels(CurrentPatterns, ItemDomain, Sequences, SupportCount, MaximumPatternLength, Patterns) :-
		current_level_patterns(CurrentPatterns, CurrentLevelPatterns),
		candidate_patterns(CurrentLevelPatterns, ItemDomain, MaximumPatternLength, Candidates),
		frequent_candidate_patterns(Candidates, CurrentLevelPatterns, Sequences, SupportCount, FrequentNextPatterns),
		(   FrequentNextPatterns == [] ->
			Patterns = CurrentPatterns
		;   append(CurrentPatterns, LaterPatterns, Patterns),
			mine_levels(FrequentNextPatterns, ItemDomain, Sequences, SupportCount, MaximumPatternLength, LaterPatterns)
		).

	current_level_patterns([], []).
	current_level_patterns([sequence_pattern(Pattern, _Support)| Patterns], [Pattern| CurrentLevelPatterns]) :-
		current_level_patterns(Patterns, CurrentLevelPatterns).

	candidate_patterns(CurrentLevelPatterns, ItemDomain, MaximumPatternLength, Candidates) :-
		findall(Candidate,
			(
				member(Pattern, CurrentLevelPatterns),
				extension_candidate(Pattern, ItemDomain, MaximumPatternLength, Candidate)
			),
			Candidates0
		),
		sort(Candidates0, Candidates).

	extension_candidate(Pattern, ItemDomain, MaximumPatternLength, Candidate) :-
		member(Item, ItemDomain),
		(   i_extension_candidate(Pattern, Item, Candidate)
		;   s_extension_candidate(Pattern, Item, Candidate)
		),
		^^pattern_length(Candidate, PatternLength),
		PatternLength =< MaximumPatternLength.

	i_extension_candidate(Pattern, Item, Candidate) :-
		Pattern \== [],
		append(LeadingItemsets, [LastItemset], Pattern),
		\+ member(Item, LastItemset),
		append(LastItemset, [Item], ExtendedLastItemset0),
		sort(ExtendedLastItemset0, ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], Candidate).

	s_extension_candidate(Pattern, Item, Candidate) :-
		append(Pattern, [[Item]], Candidate).

	frequent_candidate_patterns([], _CurrentLevelPatterns, _Sequences, _SupportCount, []).
	frequent_candidate_patterns([Candidate| Candidates], CurrentLevelPatterns, Sequences, SupportCount, FrequentPatterns) :-
		candidate_subpatterns(Candidate, CandidateSubpatterns),
		(   all_subpatterns_frequent(CandidateSubpatterns, CurrentLevelPatterns),
			pattern_support(Candidate, Sequences, Support),
			Support >= SupportCount ->
			FrequentPatterns = [sequence_pattern(Candidate, Support)| RestFrequentPatterns]
		;   FrequentPatterns = RestFrequentPatterns
		),
		frequent_candidate_patterns(Candidates, CurrentLevelPatterns, Sequences, SupportCount, RestFrequentPatterns).

	candidate_subpatterns(Pattern, Subpatterns) :-
		findall(Subpattern, immediate_subpattern(Pattern, Subpattern), Subpatterns0),
		sort(Subpatterns0, Subpatterns).

	immediate_subpattern(Pattern, Subpattern) :-
		append(Prefix, [Itemset| Suffix], Pattern),
		append(ItemsBefore, [_| ItemsAfter], Itemset),
		(   ItemsBefore == [], ItemsAfter == [] ->
			append(Prefix, Suffix, Subpattern)
		;   append(ItemsBefore, ItemsAfter, ReducedItemset),
			append(Prefix, [ReducedItemset| Suffix], Subpattern)
		).

	all_subpatterns_frequent([], _CurrentLevelPatterns).
	all_subpatterns_frequent([Subpattern| Subpatterns], CurrentLevelPatterns) :-
		memberchk(Subpattern, CurrentLevelPatterns),
		all_subpatterns_frequent(Subpatterns, CurrentLevelPatterns).

	pattern_support(Pattern, Sequences, Support) :-
		findall(Id,
			(
				member(Id-Sequence, Sequences),
				pattern_in_sequence(Pattern, Sequence)
			),
			Ids
		),
		length(Ids, Support).

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

	pattern_miner_export_template(_Dataset, gsp_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(gsp_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('GSP Pattern Miner~n', []),
		format('=================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

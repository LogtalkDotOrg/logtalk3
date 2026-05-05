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
		date is 2026-05-05,
		comment is 'SPADE sequential pattern miner for sequence datasets using vertical occurrence lists.',
		see_also is [pattern_miner_protocol, sequence_dataset_protocol, prefix_span, gsp, clo_span]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_list/2 as dictionary_as_list/2, insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	mine(Dataset, PatternMiner, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::items(ItemDomain),
		^^check_item_domain(ItemDomain),
		findall(Id-Sequence, Dataset::sequence(Id, Sequence), Sequences0),
		sort(Sequences0, Sequences),
		^^check_sequences(Dataset, ItemDomain, Sequences, MaxSequenceLength),
		length(Sequences, SequenceCount),
		^^effective_support_count(SequenceCount, Options, SupportCount),
		^^effective_maximum_pattern_length(MaxSequenceLength, Options, MaximumPatternLength),
		occurrence_index(Sequences, OccurrenceIndex),
		frequent_singletons(ItemDomain, OccurrenceIndex, SupportCount, FrequentSingletons),
		mine_equivalence_class([], FrequentSingletons, SupportCount, MaximumPatternLength, Patterns0),
		^^sort_patterns(Patterns0, SortedPatterns),
		^^option(minimum_pattern_length(MinimumPatternLength), Options),
		^^filter_patterns(SortedPatterns, MinimumPatternLength, Patterns),
		PatternMiner = spade_pattern_miner(ItemDomain, Patterns, Options).

	occurrence_index(Sequences, OccurrenceIndex) :-
		dictionary_new(OccurrenceIndex0),
		index_sequence_occurrences(Sequences, OccurrenceIndex0, ReversedOccurrenceIndex),
		normalize_occurrence_index(ReversedOccurrenceIndex, OccurrenceIndex).

	index_sequence_occurrences([], OccurrenceIndex, OccurrenceIndex).
	index_sequence_occurrences([Id-Sequence| Sequences], OccurrenceIndex0, OccurrenceIndex) :-
		index_sequence_occurrences(Sequence, Id, 1, OccurrenceIndex0, OccurrenceIndex1),
		index_sequence_occurrences(Sequences, OccurrenceIndex1, OccurrenceIndex).

	index_sequence_occurrences([], _Id, _EventIndex, OccurrenceIndex, OccurrenceIndex).
	index_sequence_occurrences([Itemset| Sequence], Id, EventIndex, OccurrenceIndex0, OccurrenceIndex) :-
		index_itemset_occurrences(Itemset, Id, EventIndex, OccurrenceIndex0, OccurrenceIndex1),
		NextEventIndex is EventIndex + 1,
		index_sequence_occurrences(Sequence, Id, NextEventIndex, OccurrenceIndex1, OccurrenceIndex).

	index_itemset_occurrences([], _Id, _EventIndex, OccurrenceIndex, OccurrenceIndex).
	index_itemset_occurrences([Item| Items], Id, EventIndex, OccurrenceIndex0, OccurrenceIndex) :-
		index_item_occurrence(Item, occurrence(Id, EventIndex), OccurrenceIndex0, OccurrenceIndex1),
		index_itemset_occurrences(Items, Id, EventIndex, OccurrenceIndex1, OccurrenceIndex).

	index_item_occurrence(Item, Occurrence, OccurrenceIndex0, OccurrenceIndex) :-
		(   dictionary_lookup(Item, Occurrences0, OccurrenceIndex0) ->
			Occurrences = [Occurrence| Occurrences0]
		;   Occurrences = [Occurrence]
		),
		dictionary_insert(OccurrenceIndex0, Item, Occurrences, OccurrenceIndex).

	normalize_occurrence_index(OccurrenceIndex0, OccurrenceIndex) :-
		dictionary_as_list(OccurrenceIndex0, OccurrencePairs),
		dictionary_new(OccurrenceIndex1),
		normalize_occurrence_pairs(OccurrencePairs, OccurrenceIndex1, OccurrenceIndex).

	normalize_occurrence_pairs([], OccurrenceIndex, OccurrenceIndex).
	normalize_occurrence_pairs([Item-Occurrences0| OccurrencePairs], OccurrenceIndex0, OccurrenceIndex) :-
		reverse(Occurrences0, Occurrences),
		dictionary_insert(OccurrenceIndex0, Item, Occurrences, OccurrenceIndex1),
		normalize_occurrence_pairs(OccurrencePairs, OccurrenceIndex1, OccurrenceIndex).

	frequent_singletons([], _OccurrenceIndex, _SupportCount, []).
	frequent_singletons([Item| Items], OccurrenceIndex, SupportCount, FrequentPatterns) :-
		(   dictionary_lookup(Item, Occurrences, OccurrenceIndex),
			occurrences_support(Occurrences, Support),
			Support >= SupportCount ->
			FrequentPatterns = [pattern_occurrence([[Item]], Occurrences, Support)| RestFrequentPatterns]
		;   FrequentPatterns = RestFrequentPatterns
		),
		frequent_singletons(Items, OccurrenceIndex, SupportCount, RestFrequentPatterns).

	mine_equivalence_class(ClassPrefix, ClassMembers, SupportCount, MaximumPatternLength, Patterns) :-
		class_member_indexes(ClassMembers, MemberIndex, SequenceIndex),
		mine_equivalence_class_members(ClassPrefix, ClassMembers, MemberIndex, SequenceIndex, SupportCount, MaximumPatternLength, Patterns),
		!.

	mine_equivalence_class_members(_ClassPrefix, [], _MemberIndex, _SequenceIndex, _SupportCount, _MaximumPatternLength, []) :-
		!.
	mine_equivalence_class_members(ClassPrefix, [pattern_occurrence(Pattern, Occurrences, Support)| PatternOccurrences], MemberIndex, SequenceIndex, SupportCount, MaximumPatternLength, Patterns) :-
		Patterns = [sequence_pattern(Pattern, Support)| RestPatterns],
		^^pattern_length(Pattern, PatternLength),
		(   PatternLength < MaximumPatternLength ->
			class_child_members(ClassPrefix, pattern_occurrence(Pattern, Occurrences, Support), MemberIndex, SequenceIndex, SupportCount, ChildMembers),
			mine_equivalence_class(Pattern, ChildMembers, SupportCount, MaximumPatternLength, ExtensionPatterns)
		;   ExtensionPatterns = []
		),
		append(ExtensionPatterns, SiblingPatterns, RestPatterns),
		mine_equivalence_class_members(ClassPrefix, PatternOccurrences, MemberIndex, SequenceIndex, SupportCount, MaximumPatternLength, SiblingPatterns).

	class_member_indexes(ClassMembers, MemberIndex, SequenceIndex) :-
		dictionary_new(MemberIndex0),
		dictionary_new(SequenceIndex0),
		index_class_members(ClassMembers, 1, MemberIndex0, MemberIndex, SequenceIndex0, SequenceIndex).

	index_class_members([], _MemberId, MemberIndex, MemberIndex, SequenceIndex, SequenceIndex).
	index_class_members([Member| ClassMembers], MemberId, MemberIndex0, MemberIndex, SequenceIndex0, SequenceIndex) :-
		dictionary_insert(MemberIndex0, MemberId, Member, MemberIndex1),
		arg(2, Member, Occurrences),
		occurrence_sequence_ids(Occurrences, SequenceIds),
		index_member_sequence_ids(SequenceIds, MemberId, SequenceIndex0, SequenceIndex1),
		NextMemberId is MemberId + 1,
		index_class_members(ClassMembers, NextMemberId, MemberIndex1, MemberIndex, SequenceIndex1, SequenceIndex).

	index_member_sequence_ids([], _MemberId, SequenceIndex, SequenceIndex).
	index_member_sequence_ids([SequenceId| SequenceIds], MemberId, SequenceIndex0, SequenceIndex) :-
		(   dictionary_lookup(SequenceId, Members0, SequenceIndex0) ->
			Members = [MemberId| Members0]
		;   Members = [MemberId]
		),
		dictionary_insert(SequenceIndex0, SequenceId, Members, SequenceIndex1),
		index_member_sequence_ids(SequenceIds, MemberId, SequenceIndex1, SequenceIndex).

	occurrence_sequence_ids([], []).
	occurrence_sequence_ids([occurrence(SequenceId, _EventIndex)| Occurrences], [SequenceId| SequenceIds]) :-
		skip_occurrence_sequence(Occurrences, SequenceId, RemainingOccurrences),
		occurrence_sequence_ids(RemainingOccurrences, SequenceIds).

	skip_occurrence_sequence([], _SequenceId, []).
	skip_occurrence_sequence([occurrence(SequenceId, _EventIndex)| Occurrences], SequenceId, RemainingOccurrences) :-
		!,
		skip_occurrence_sequence(Occurrences, SequenceId, RemainingOccurrences).
	skip_occurrence_sequence(Occurrences, _SequenceId, Occurrences).

	class_child_members(ClassPrefix, LeftMember, MemberIndex, SequenceIndex, SupportCount, ChildMembers) :-
		arg(2, LeftMember, LeftOccurrences),
		active_class_member_ids(LeftOccurrences, SequenceIndex, CandidateMemberIds),
		findall(ChildMember,
			(
				member(CandidateMemberId, CandidateMemberIds),
				dictionary_lookup(CandidateMemberId, RightMember, MemberIndex),
				class_child_member(ClassPrefix, LeftMember, RightMember, SupportCount, ChildMember)
			),
			ChildMembers0
		),
		sort(ChildMembers0, ChildMembers).

	active_class_member_ids(LeftOccurrences, SequenceIndex, CandidateMemberIds) :-
		occurrence_sequence_ids(LeftOccurrences, SequenceIds),
		dictionary_new(CandidateMemberIds0),
		active_class_member_ids(SequenceIds, SequenceIndex, CandidateMemberIds0, CandidateMemberIds1),
		dictionary_as_list(CandidateMemberIds1, CandidateMemberPairs),
		candidate_member_ids(CandidateMemberPairs, CandidateMemberIds).

	active_class_member_ids([], _SequenceIndex, CandidateMemberIds, CandidateMemberIds).
	active_class_member_ids([SequenceId| SequenceIds], SequenceIndex, CandidateMemberIds0, CandidateMemberIds) :-
		(   dictionary_lookup(SequenceId, SequenceMembers, SequenceIndex) ->
			mark_candidate_member_ids(SequenceMembers, CandidateMemberIds0, CandidateMemberIds1)
		;   CandidateMemberIds1 = CandidateMemberIds0
		),
		active_class_member_ids(SequenceIds, SequenceIndex, CandidateMemberIds1, CandidateMemberIds).

	mark_candidate_member_ids([], CandidateMemberIds, CandidateMemberIds).
	mark_candidate_member_ids([MemberId| MemberIds], CandidateMemberIds0, CandidateMemberIds) :-
		(   dictionary_lookup(MemberId, _Seen, CandidateMemberIds0) ->
			CandidateMemberIds1 = CandidateMemberIds0
		;   dictionary_insert(CandidateMemberIds0, MemberId, true, CandidateMemberIds1)
		),
		mark_candidate_member_ids(MemberIds, CandidateMemberIds1, CandidateMemberIds).

	candidate_member_ids([], []).
	candidate_member_ids([CandidateMemberId-_Seen| CandidateMemberPairs], [CandidateMemberId| CandidateMemberIds]) :-
		candidate_member_ids(CandidateMemberPairs, CandidateMemberIds).

	class_child_member(ClassPrefix, pattern_occurrence(LeftPattern, LeftOccurrences, _LeftSupport), pattern_occurrence(RightPattern, RightOccurrences, _RightSupport), SupportCount, pattern_occurrence(ChildPattern, ChildOccurrences, Support)) :-
		class_member_extension_item(ClassPrefix, RightPattern, RightItem),
		(   same_event_child_pattern(LeftPattern, RightItem, ChildPattern),
			same_event_join_occurrences(LeftOccurrences, RightOccurrences, ChildOccurrences)
		;   sequence_child_pattern(LeftPattern, RightItem, ChildPattern),
			sequence_join_occurrences(LeftOccurrences, RightOccurrences, ChildOccurrences)
		),
		ChildOccurrences \== [],
		occurrences_support(ChildOccurrences, Support),
		Support >= SupportCount.

	class_member_extension_item([], [[Item]], Item) :-
		!.
	class_member_extension_item(ClassPrefix, Pattern, Item) :-
		once((
			append(LeadingItemsets, [PrefixLastItemset], ClassPrefix),
			append(LeadingItemsets, [PatternLastItemset], Pattern),
			append(PrefixLastItemset, [Item], PatternLastItemset)
		;   append(ClassPrefix, [[Item]], Pattern)
		)).

	same_event_child_pattern(LeftPattern, RightItem, ChildPattern) :-
		append(LeadingItemsets, [LastItemset], LeftPattern),
		append(_, [LastItem], LastItemset),
		RightItem @> LastItem,
		append(LastItemset, [RightItem], ExtendedLastItemset),
		append(LeadingItemsets, [ExtendedLastItemset], ChildPattern).

	sequence_child_pattern(LeftPattern, RightItem, ChildPattern) :-
		append(LeftPattern, [[RightItem]], ChildPattern).

	same_event_join_occurrences([], _RightOccurrences, []).
	same_event_join_occurrences(_LeftOccurrences, [], []).
	same_event_join_occurrences([LeftOccurrence| LeftOccurrences], [RightOccurrence| RightOccurrences], JoinedOccurrences) :-
		compare(Order, LeftOccurrence, RightOccurrence),
		same_event_join_occurrences(Order, LeftOccurrence, LeftOccurrences, RightOccurrence, RightOccurrences, JoinedOccurrences).

	same_event_join_occurrences(=, LeftOccurrence, LeftOccurrences, _RightOccurrence, RightOccurrences, [LeftOccurrence| JoinedOccurrences]) :-
		same_event_join_occurrences(LeftOccurrences, RightOccurrences, JoinedOccurrences).
	same_event_join_occurrences(<, _LeftOccurrence, LeftOccurrences, RightOccurrence, RightOccurrences, JoinedOccurrences) :-
		same_event_join_occurrences(LeftOccurrences, [RightOccurrence| RightOccurrences], JoinedOccurrences).
	same_event_join_occurrences(>, LeftOccurrence, LeftOccurrences, _RightOccurrence, RightOccurrences, JoinedOccurrences) :-
		same_event_join_occurrences([LeftOccurrence| LeftOccurrences], RightOccurrences, JoinedOccurrences).

	sequence_join_occurrences([], _RightOccurrences, []).
	sequence_join_occurrences(_LeftOccurrences, [], []).
	sequence_join_occurrences([LeftOccurrence| LeftOccurrences], [RightOccurrence| RightOccurrences], JoinedOccurrences) :-
		LeftOccurrence = occurrence(LeftSequenceId, _LeftEventIndex),
		RightOccurrence = occurrence(RightSequenceId, _RightEventIndex),
		compare(Order, LeftSequenceId, RightSequenceId),
		sequence_join_occurrences(Order, LeftSequenceId, [LeftOccurrence| LeftOccurrences], RightSequenceId, [RightOccurrence| RightOccurrences], JoinedOccurrences).

	sequence_join_occurrences(<, LeftSequenceId, LeftOccurrences, _RightSequenceId, RightOccurrences, JoinedOccurrences) :-
		skip_occurrence_sequence(LeftOccurrences, LeftSequenceId, RemainingLeftOccurrences),
		sequence_join_occurrences(RemainingLeftOccurrences, RightOccurrences, JoinedOccurrences).
	sequence_join_occurrences(>, _LeftSequenceId, LeftOccurrences, RightSequenceId, RightOccurrences, JoinedOccurrences) :-
		skip_occurrence_sequence(RightOccurrences, RightSequenceId, RemainingRightOccurrences),
		sequence_join_occurrences(LeftOccurrences, RemainingRightOccurrences, JoinedOccurrences).
	sequence_join_occurrences(=, SequenceId, LeftOccurrences, _RightSequenceId, RightOccurrences, JoinedOccurrences) :-
		occurrence_sequence_events(LeftOccurrences, SequenceId, LeftEventIndices, RemainingLeftOccurrences),
		occurrence_sequence_events(RightOccurrences, SequenceId, RightEventIndices, RemainingRightOccurrences),
		sequence_group_join_occurrences(SequenceId, LeftEventIndices, RightEventIndices, GroupJoinedOccurrences),
		sequence_join_occurrences(RemainingLeftOccurrences, RemainingRightOccurrences, RemainingJoinedOccurrences),
		append(GroupJoinedOccurrences, RemainingJoinedOccurrences, JoinedOccurrences).

	occurrence_sequence_events([], _SequenceId, [], []).
	occurrence_sequence_events([occurrence(SequenceId, EventIndex)| Occurrences], SequenceId, [EventIndex| EventIndices], RemainingOccurrences) :-
		!,
		occurrence_sequence_events(Occurrences, SequenceId, EventIndices, RemainingOccurrences).
	occurrence_sequence_events(Occurrences, _SequenceId, [], Occurrences).

	sequence_group_join_occurrences(_SequenceId, [], _RightEventIndices, []) :-
		!.
	sequence_group_join_occurrences(_SequenceId, _LeftEventIndices, [], []) :-
		!.
	sequence_group_join_occurrences(SequenceId, [MinimumLeftEventIndex| _LeftEventIndices], RightEventIndices, JoinedOccurrences) :-
		skip_non_later_event_indices(RightEventIndices, MinimumLeftEventIndex, LaterRightEventIndices),
		event_indices_occurrences(LaterRightEventIndices, SequenceId, JoinedOccurrences).

	skip_non_later_event_indices([], _MinimumLeftEventIndex, []).
	skip_non_later_event_indices([RightEventIndex| RightEventIndices], MinimumLeftEventIndex, LaterRightEventIndices) :-
		(   RightEventIndex > MinimumLeftEventIndex ->
			LaterRightEventIndices = [RightEventIndex| RightEventIndices]
		;   skip_non_later_event_indices(RightEventIndices, MinimumLeftEventIndex, LaterRightEventIndices)
		).

	event_indices_occurrences([], _SequenceId, []).
	event_indices_occurrences([EventIndex| EventIndices], SequenceId, [occurrence(SequenceId, EventIndex)| Occurrences]) :-
		event_indices_occurrences(EventIndices, SequenceId, Occurrences).

	occurrences_support(Occurrences, Support) :-
		Occurrences = [occurrence(Id, _EventIndex)| RemainingOccurrences],
		occurrences_support(RemainingOccurrences, Id, 1, Support).

	occurrences_support([], _Id, Support, Support).
	occurrences_support([occurrence(Id, _EventIndex)| Occurrences], PreviousId, Support0, Support) :-
		(   Id == PreviousId ->
			Support1 = Support0
		;   Support1 is Support0 + 1
		),
		occurrences_support(Occurrences, Id, Support1, Support).

	pattern_miner_diagnostics_data(spade_pattern_miner(ItemDomain, Patterns, Options), Diagnostics) :-
		^^pattern_miner_diagnostics(spade, ItemDomain, Patterns, Options, [
			search_strategy(lattice_depth_first_growth),
			candidate_generation(equivalence_class_temporal_joins),
			extension_modes([itemset, sequence]),
			support_layout(vertical_occurrence_lists)
		], Diagnostics).

	check_pattern_miner(PatternMiner) :-
		(   PatternMiner = spade_pattern_miner(ItemDomain, Patterns, Options),
			^^valid_sequence_patterns(ItemDomain, Patterns),
			::pattern_miner_diagnostics_data(PatternMiner, Diagnostics),
			^^valid_pattern_miner_metadata(spade, ItemDomain, Patterns, Options, Diagnostics),
			memberchk(search_strategy(lattice_depth_first_growth), Diagnostics),
			memberchk(candidate_generation(equivalence_class_temporal_joins), Diagnostics),
			memberchk(extension_modes([itemset, sequence]), Diagnostics),
			memberchk(support_layout(vertical_occurrence_lists), Diagnostics) ->
			true
		;   domain_error(spade_pattern_miner, PatternMiner)
		).

	pattern_miner_export_template(_Dataset, spade_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(spade_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('SPADE Pattern Miner~n', []),
		format('===================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.

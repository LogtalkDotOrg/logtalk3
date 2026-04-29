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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-29,
		comment is 'Smoke tests for the "sequential_pattern_mining_protocols" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	test(clickstream_sequences_items, deterministic(memberchk(search, Items))) :-
		clickstream_sequences::items(Items).

	test(clickstream_sequences_shape, deterministic((length(Sequence, 8), memberchk([cart], Sequence), sequence_is_declared(clickstream_sequences, Sequence)))) :-
		clickstream_sequences::sequence(1, Sequence).

	test(clickstream_sequences_multi_item_event, deterministic(memberchk([browse, search], Sequence))) :-
		clickstream_sequences::sequence(5, Sequence).

	test(prefix_ladder_sequences_valid, deterministic) :-
		forall(prefix_ladder_sequences::sequence(_Id, Sequence), sequence_is_declared(prefix_ladder_sequences, Sequence)).

	test(prefix_ladder_sequences_shape, deterministic((Count == 5, Last == [[a]]))) :-
		findall(Id, prefix_ladder_sequences::sequence(Id, _Sequence), Ids),
		length(Ids, Count),
		prefix_ladder_sequences::sequence(4, Last).

	test(same_event_vs_next_event_sequences_distinction, deterministic((memberchk([a, b], EventSequence), SeparateSequence == [[a], [b], [c]]))) :-
		same_event_vs_next_event_sequences::sequence(1, EventSequence),
		same_event_vs_next_event_sequences::sequence(2, SeparateSequence).

	test(repeated_embedding_sequences_repeated_prefix, deterministic(Sequence == [[a], [b], [a], [b], [c]])) :-
		repeated_embedding_sequences::sequence(1, Sequence).

	test(border_threshold_sequences_valid, deterministic) :-
		forall(border_threshold_sequences::sequence(_Id, Sequence), sequence_is_declared(border_threshold_sequences, Sequence)).

	test(border_threshold_sequences_border_cases, deterministic((Count == 5, Last == [[b], [c]]))) :-
		findall(Id, border_threshold_sequences::sequence(Id, _Sequence), Ids),
		length(Ids, Count),
		border_threshold_sequences::sequence(5, Last).

	test(closure_sequences_valid, deterministic) :-
		forall(closure_sequences::sequence(_Id, Sequence), sequence_is_declared(closure_sequences, Sequence)).

	test(closure_sequences_common_prefix, deterministic((Prefix == [[a], [b]], Extended == [[a], [b], [c]]))) :-
		closure_sequences::sequence(1, Prefix),
		closure_sequences::sequence(4, Extended).

	test(dense_overlap_sequences_overlap_forms, deterministic((memberchk([b, c], Sequence2), memberchk([a, b], Sequence3), memberchk([c, d], Sequence4)))) :-
		dense_overlap_sequences::sequence(2, Sequence2),
		dense_overlap_sequences::sequence(3, Sequence3),
		dense_overlap_sequences::sequence(4, Sequence4).

	test(branching_sequences_branch_coverage, deterministic((Count == 6, Branches == [a, b, c]))) :-
		findall(Id, branching_sequences::sequence(Id, _Sequence), Ids),
		length(Ids, Count),
		findall(Branch, (member(Id, [1, 3, 5]), branching_sequences::sequence(Id, [[start], [Branch], [end]])), Branches).

	test(invalid_undeclared_item_sequences_has_undeclared_item, deterministic(\+ sequence_uses_only_declared_items(invalid_undeclared_item_sequences, Sequence))) :-
		invalid_undeclared_item_sequences::sequence(1, Sequence).

	test(invalid_unsorted_itemset_sequences_has_uncanonical_event, deterministic(\+ sequence_has_canonical_itemsets(Sequence))) :-
		invalid_unsorted_itemset_sequences::sequence(1, Sequence).

	test(invalid_duplicate_item_in_event_sequences_has_duplicate_item, deterministic(\+ sequence_has_canonical_itemsets(Sequence))) :-
		invalid_duplicate_item_in_event_sequences::sequence(1, Sequence).

	test(invalid_duplicate_id_sequences_has_non_unique_ids, true(\+ unique_sequence_ids(Ids))) :-
		findall(Id, invalid_duplicate_id_sequences::sequence(Id, _Sequence), Ids).

	test(invalid_empty_event_sequences_has_empty_event, deterministic(\+ sequence_has_canonical_itemsets(Sequence))) :-
		invalid_empty_event_sequences::sequence(1, Sequence).

	sequence_is_declared(Dataset, Sequence) :-
		sequence_uses_only_declared_items(Dataset, Sequence),
		sequence_has_canonical_itemsets(Sequence).

	sequence_uses_only_declared_items(Dataset, Sequence) :-
		Dataset::items(Items),
		forall(
			member(Itemset, Sequence),
			forall(member(Item, Itemset), memberchk(Item, Items))
		).

	sequence_has_canonical_itemsets(Sequence) :-
		forall(
			member(Itemset, Sequence),
			(
				Itemset \== [],
				sort(Itemset, Sorted),
				Itemset == Sorted
			)
		).

	unique_sequence_ids(Ids) :-
		sort(Ids, UniqueIds),
		length(Ids, IdsCount),
		length(UniqueIds, UniqueIdsCount),
		IdsCount =:= UniqueIdsCount.

:- end_object.

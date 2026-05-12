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


:- object(partitions,
	implements(partitions_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Implementation of set partition operations over lists.'
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(natural, [
		bell/2, stirling_second/3
	]).

	:- uses(list, [
		length/2, member/2, msort/2, nth0/3, remove_duplicates/2, reverse/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	partitions(List, Partitions) :-
		all_partitions(List, default, Partitions).

	partition(List, Partition) :-
		partitions(List, Partitions),
		member(Partition, Partitions).

	partitions(K, List, Partitions) :-
		integer(K),
		!,
		exact_partitions(K, List, default, Partitions).
	partitions(List, Order, Partitions) :-
		all_partitions(List, Order, Partitions).

	partition(K, List, Partition) :-
		integer(K),
		!,
		partitions(K, List, Partitions),
		member(Partition, Partitions).
	partition(List, Order, Partition) :-
		partitions(List, Order, Partitions),
		member(Partition, Partitions).

	partitions(K, List, Order, Partitions) :-
		exact_partitions(K, List, Order, Partitions).

	partition(K, List, Order, Partition) :-
		partitions(K, List, Order, Partitions),
		member(Partition, Partitions).

	distinct_partitions(List, Partitions) :-
		distinct_all_partitions(List, default, Partitions).

	distinct_partition(List, Partition) :-
		distinct_partitions(List, Partitions),
		member(Partition, Partitions).

	distinct_partitions(K, List, Partitions) :-
		integer(K),
		!,
		distinct_exact_partitions(K, List, default, Partitions).
	distinct_partitions(List, Order, Partitions) :-
		distinct_all_partitions(List, Order, Partitions).

	distinct_partition(K, List, Partition) :-
		integer(K),
		!,
		distinct_partitions(K, List, Partitions),
		member(Partition, Partitions).
	distinct_partition(List, Order, Partition) :-
		distinct_partitions(List, Order, Partitions),
		member(Partition, Partitions).

	distinct_partitions(K, List, Order, Partitions) :-
		distinct_exact_partitions(K, List, Order, Partitions).

	distinct_partition(K, List, Order, Partition) :-
		distinct_partitions(K, List, Order, Partitions),
		member(Partition, Partitions).

	count_partitions(List, Count) :-
		length(List, N),
		bell(N, Count).

	nth_partition(List, Index, Partition) :-
		Index >= 0,
		partitions(List, Partitions),
		nth0(Index, Partitions, Partition).

	nth_partition(K, List, Index, Partition) :-
		integer(K),
		!,
		Index >= 0,
		partitions(K, List, Partitions),
		nth0(Index, Partitions, Partition).
	nth_partition(List, Order, Index, Partition) :-
		Index >= 0,
		partitions(List, Order, Partitions),
		nth0(Index, Partitions, Partition).

	nth_partition(K, List, Order, Index, Partition) :-
		Index >= 0,
		partitions(K, List, Order, Partitions),
		nth0(Index, Partitions, Partition).

	partition_index(List, Partition, Index) :-
		partitions(List, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	partition_index(K, List, Partition, Index) :-
		integer(K),
		!,
		partitions(K, List, Partitions),
		nth0(Index, Partitions, Partition),
		!.
	partition_index(List, Order, Partition, Index) :-
		partitions(List, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	partition_index(K, List, Order, Partition, Index) :-
		partitions(K, List, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	count_partitions(K, List, Count) :-
		length(List, N),
		stirling_second(N, K, Count).

	count_distinct_partitions(List, Count) :-
		(	all_distinct(List) ->
			count_partitions(List, Count)
		;	distinct_partitions(List, Partitions),
			length(Partitions, Count)
		).

	count_distinct_partitions(K, List, Count) :-
		(	all_distinct(List) ->
			count_partitions(K, List, Count)
		;	distinct_partitions(K, List, Partitions),
			length(Partitions, Count)
		).

	nth_distinct_partition(List, Index, Partition) :-
		Index >= 0,
		distinct_partitions(List, Partitions),
		nth0(Index, Partitions, Partition).

	nth_distinct_partition(K, List, Index, Partition) :-
		integer(K),
		!,
		Index >= 0,
		distinct_partitions(K, List, Partitions),
		nth0(Index, Partitions, Partition).
	nth_distinct_partition(List, Order, Index, Partition) :-
		Index >= 0,
		distinct_partitions(List, Order, Partitions),
		nth0(Index, Partitions, Partition).

	nth_distinct_partition(K, List, Order, Index, Partition) :-
		Index >= 0,
		distinct_partitions(K, List, Order, Partitions),
		nth0(Index, Partitions, Partition).

	distinct_partition_index(List, Partition, Index) :-
		distinct_partitions(List, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	distinct_partition_index(K, List, Partition, Index) :-
		integer(K),
		!,
		distinct_partitions(K, List, Partitions),
		nth0(Index, Partitions, Partition),
		!.
	distinct_partition_index(List, Order, Partition, Index) :-
		distinct_partitions(List, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	distinct_partition_index(K, List, Order, Partition, Index) :-
		distinct_partitions(K, List, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	random_partition(List, Partition) :-
		count_partitions(List, Count),
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_partition(List, Index, Partition).

	random_partition(K, List, Partition) :-
		count_partitions(K, List, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_partition(K, List, Index, Partition).

	sample_partitions(List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, all, List, Samples).

	sample_partitions(K, List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, exact(K), List, Samples).

	sample_partitions_loop(0, _Mode, _List, []) :-
		!.
	sample_partitions_loop(SampleCount, Mode, List, [Partition| Samples]) :-
		SampleCount > 0,
		random_partition_for_mode(Mode, List, Partition),
		SampleCount1 is SampleCount - 1,
		sample_partitions_loop(SampleCount1, Mode, List, Samples).

	random_distinct_partition(List, Partition) :-
		count_distinct_partitions(List, Count),
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_distinct_partition(List, Index, Partition).

	random_distinct_partition(K, List, Partition) :-
		count_distinct_partitions(K, List, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_distinct_partition(K, List, Index, Partition).

	sample_distinct_partitions(List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, distinct_all, List, Samples).

	sample_distinct_partitions(K, List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, distinct_exact(K), List, Samples).

	random_partition_for_mode(all, List, Partition) :-
		random_partition(List, Partition).
	random_partition_for_mode(exact(K), List, Partition) :-
		random_partition(K, List, Partition).
	random_partition_for_mode(distinct_all, List, Partition) :-
		random_distinct_partition(List, Partition).
	random_partition_for_mode(distinct_exact(K), List, Partition) :-
		random_distinct_partition(K, List, Partition).

	next_partition(List, Partition, Next) :-
		canonical_lexicographic_partitions(List, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Partitions, Next).

	next_partition(K, List, Partition, Next) :-
		canonical_lexicographic_partitions(K, List, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Partitions, Next).

	previous_partition(List, Partition, Previous) :-
		canonical_lexicographic_partitions(List, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Partitions, Previous).

	previous_partition(K, List, Partition, Previous) :-
		canonical_lexicographic_partitions(K, List, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Partitions, Previous).

	all_partitions(List, Order, Partitions) :-
		findall(Partition, generate_partition(List, Partition), Partitions0),
		apply_order(Order, Partitions0, Partitions).

	exact_partitions(K, List, Order, Partitions) :-
		findall(Partition, generate_partition(K, List, Partition), Partitions0),
		apply_order(Order, Partitions0, Partitions).

	distinct_all_partitions(List, Order, Partitions) :-
		findall(Partition, generate_partition(List, Partition), Partitions0),
		canonicalize_partitions(Partitions0, CanonicalPartitions),
		remove_duplicates(CanonicalPartitions, DistinctPartitions),
		apply_order(Order, DistinctPartitions, Partitions).

	distinct_exact_partitions(K, List, Order, Partitions) :-
		findall(Partition, generate_partition(K, List, Partition), Partitions0),
		canonicalize_partitions(Partitions0, CanonicalPartitions),
		remove_duplicates(CanonicalPartitions, DistinctPartitions),
		apply_order(Order, DistinctPartitions, Partitions).

	generate_partition([], []) :-
		!.
	generate_partition(List, Partition) :-
		length(List, N),
		N > 0,
		partition_assignment(N, K, Assignment),
		assignment_partition(List, K, Assignment, Partition).

	generate_partition(0, [], []) :-
		!.
	generate_partition(K, List, Partition) :-
		length(List, N),
		N > 0,
		partition_assignment(N, K, Assignment),
		assignment_partition(List, K, Assignment, Partition).

	partition_assignment(N, K, Assignment) :-
		( 	N =:= 0 ->
			K = 0,
			Assignment = []
		; 	Assignment = [0| Labels],
			partition_assignment_loop(N, 1, 0, 1, K, Labels)
		).

	partition_assignment_loop(N, N, _CurrentMax, Used, Used, []) :-
		!.
	partition_assignment_loop(N, Position, CurrentMax, Used0, K, [Label| Labels]) :-
		Position < N,
		Upper is CurrentMax + 1,
		between(0, Upper, Label),
		update_partition_assignment_state(Label, CurrentMax, Used0, CurrentMax1, Used1),
		Position1 is Position + 1,
		partition_assignment_loop(N, Position1, CurrentMax1, Used1, K, Labels).

	update_partition_assignment_state(Label, CurrentMax, Used0, CurrentMax1, Used1) :-
		Upper is CurrentMax + 1,
		(	Label =:= Upper ->
			CurrentMax1 = Upper,
			Used1 is Used0 + 1
		;	CurrentMax1 = CurrentMax,
			Used1 = Used0
		).

	assignment_partition(List, K, Assignment, Partition) :-
		empty_blocks(K, EmptyBlocks),
		reverse(List, ReverseList),
		reverse(Assignment, ReverseAssignment),
		fill_blocks(ReverseList, ReverseAssignment, EmptyBlocks, Partition).

	empty_blocks(0, []) :-
		!.
	empty_blocks(K, [[]| Blocks]) :-
		K > 0,
		K1 is K - 1,
		empty_blocks(K1, Blocks).

	fill_blocks([], [], Blocks, Blocks).
	fill_blocks([Element| Elements], [Label| Labels], Blocks0, Blocks) :-
		prepend_to_block(Label, Element, Blocks0, Blocks1),
		fill_blocks(Elements, Labels, Blocks1, Blocks).

	prepend_to_block(0, Element, [Block| Blocks], [[Element| Block]| Blocks]) :-
		!.
	prepend_to_block(Index, Element, [Block| Blocks], [Block| UpdatedBlocks]) :-
		Index > 0,
		Index1 is Index - 1,
		prepend_to_block(Index1, Element, Blocks, UpdatedBlocks).

	canonicalize_partitions([], []).
	canonicalize_partitions([Partition| Partitions], [Canonical| Canonicals]) :-
		canonical_partition(Partition, Canonical),
		canonicalize_partitions(Partitions, Canonicals).

	canonical_partition(Partition, Canonical) :-
		msort(Partition, Canonical).

	all_distinct(List) :-
		sort(List, Sorted),
		List == Sorted.

	canonical_lexicographic_partitions(List, Partitions) :-
		distinct_partitions(List, lexicographic, Partitions).

	canonical_lexicographic_partitions(K, List, Partitions) :-
		distinct_partitions(K, List, lexicographic, Partitions).

	apply_order(default, Partitions, Partitions).
	apply_order(lexicographic, Partitions, SortedPartitions) :-
		msort(Partitions, SortedPartitions).

:- end_object.

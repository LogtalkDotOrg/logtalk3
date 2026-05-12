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


:- object(multisets,
	implements(multisets_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Implementation of multiset operations with repetition over lists.'
	]).

	:- uses(natural, [
		binomial/3
	]).

	:- uses(list, [
		append/3, drop/3, length/2, member/2, msort/2, nth0/3, remove_duplicates/2, reverse/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	multisets(K, List, Multisets) :-
		findall(Multiset, multiset(K, List, Multiset), Multisets).

	multiset(0, _, []) :-
		!.
	multiset(K, List, [Head| Multiset]) :-
		K > 0,
		K1 is K - 1,
		append(_, [Head| Tail], List),
		multiset(K1, [Head| Tail], Multiset).

	multisets(K, List, Order, Multisets) :-
		findall(Multiset, multiset(K, List, Multiset), Multisets0),
		apply_order(Order, Multisets0, Multisets).

	multiset(K, List, Order, Multiset) :-
		multiset(K, List, Multiset0),
		apply_order(Order, [Multiset0], [Multiset]).

	distinct_multisets(K, List, Multisets) :-
		distinct_multisets(K, List, default, Multisets).

	distinct_multiset(K, List, Multiset) :-
		distinct_multiset(K, List, default, Multiset).

	distinct_multisets(K, List, Order, Multisets) :-
		findall(Multiset, multiset(K, List, Multiset), Multisets0),
		remove_duplicates(Multisets0, DistinctMultisets),
		apply_order(Order, DistinctMultisets, Multisets).

	distinct_multiset(K, List, Order, Multiset) :-
		distinct_multisets(K, List, Order, Multisets),
		member(Multiset, Multisets).

	nth_multiset(K, List, Index, Multiset) :-
		length(List, N),
		Index >= 0,
		multiset_count_total(N, K, Total),
		Index < Total,
		nth_multiset_default(K, List, Index, Multiset).

	nth_multiset(K, List, default, Index, Multiset) :-
		!,
		nth_multiset(K, List, Index, Multiset).
	nth_multiset(K, List, lexicographic, Index, Multiset) :-
		Index >= 0,
		multisets(K, List, lexicographic, Multisets),
		nth0(Index, Multisets, Multiset),
		!.

	nth_multiset_default(0, _, _, []) :-
		!.
	nth_multiset_default(K, List, Index, [Head| Multiset]) :-
		K > 0,
		find_multiset_position(K, List, Index, Pos, IndexRemainder),
		nth0(Pos, List, Head),
		drop(Pos, List, Suffix),
		K1 is K - 1,
		nth_multiset_default(K1, Suffix, IndexRemainder, Multiset).

	find_multiset_position(K, List, Index, Pos, IndexRemainder) :-
		find_multiset_position(0, K, List, Index, Pos, IndexRemainder).

	find_multiset_position(Pos, K, List, Index, Pos, Index) :-
		K > 0,
		K1 is K - 1,
		drop(Pos, List, Suffix),
		length(Suffix, N),
		N > 0,
		multiset_count_total(N, K1, Count),
		Index < Count,
		!.
	find_multiset_position(Pos0, K, List, Index0, Pos, Index) :-
		K > 0,
		K1 is K - 1,
		drop(Pos0, List, Suffix),
		length(Suffix, N),
		N > 0,
		multiset_count_total(N, K1, Count),
		Index0 >= Count,
		Index1 is Index0 - Count,
		Pos1 is Pos0 + 1,
		find_multiset_position(Pos1, K, List, Index1, Pos, Index).

	multiset_index(K, List, Multiset, Index) :-
		length(Multiset, K),
		multiset_index_default(K, List, Multiset, 0, Index).

	multiset_index(K, List, default, Multiset, Index) :-
		!,
		multiset_index(K, List, Multiset, Index).
	multiset_index(K, List, lexicographic, Multiset, Index) :-
		length(Multiset, K),
		multisets(K, List, lexicographic, Multisets),
		nth0(Index, Multisets, Multiset),
		!.

	multiset_index_default(0, _, [], Index, Index) :-
		!.
	multiset_index_default(K, List, [Head| Multiset], Index0, Index) :-
		K > 0,
		nth0(Pos, List, Head),
		!,
		K1 is K - 1,
		multiset_skip_count(Pos, K1, List, Skip),
		drop(Pos, List, Suffix),
		Index1 is Index0 + Skip,
		multiset_index_default(K1, Suffix, Multiset, Index1, Index).

	multiset_skip_count(Pos, K, List, Skip) :-
		multiset_skip_count(0, Pos, K, List, 0, Skip).

	multiset_skip_count(Pos, Pos, _, _, Skip, Skip) :-
		!.
	multiset_skip_count(Current, Pos, K, List, Skip0, Skip) :-
		drop(Current, List, Suffix),
		length(Suffix, N),
		multiset_count_total(N, K, Count),
		Current1 is Current + 1,
		Skip1 is Skip0 + Count,
		multiset_skip_count(Current1, Pos, K, List, Skip1, Skip).

	count_multisets(K, List, Count) :-
		length(List, N),
		multiset_count_total(N, K, Count),
		!.

	count_distinct_multisets(K, List, Count) :-
		distinct_source(List, DistinctSource),
		count_multisets(K, DistinctSource, Count).

	nth_distinct_multiset(K, List, Index, Multiset) :-
		distinct_source(List, DistinctSource),
		nth_multiset(K, DistinctSource, Index, Multiset).

	distinct_multiset_index(K, List, Multiset, Index) :-
		distinct_source(List, DistinctSource),
		multiset_index(K, DistinctSource, Multiset, Index).

	random_multiset(K, List, Multiset) :-
		length(List, N),
		multiset_count_total(N, K, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_multiset(K, List, Index, Multiset).

	sample_multisets(K, List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_multisets_loop(SampleCount, K, List, [], Samples0),
		reverse(Samples0, Samples).

	sample_multisets_loop(0, _, _, Samples, Samples) :-
		!.
	sample_multisets_loop(SampleCount, K, List, Samples0, Samples) :-
		SampleCount > 0,
		random_multiset(K, List, Multiset),
		SampleCount1 is SampleCount - 1,
		sample_multisets_loop(SampleCount1, K, List, [Multiset| Samples0], Samples).

	random_distinct_multiset(K, List, Multiset) :-
		distinct_source(List, DistinctSource),
		random_multiset(K, DistinctSource, Multiset).

	sample_distinct_multisets(K, List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_distinct_multisets_loop(SampleCount, K, List, [], Samples0),
		reverse(Samples0, Samples).

	sample_distinct_multisets_loop(0, _, _, Samples, Samples) :-
		!.
	sample_distinct_multisets_loop(SampleCount, K, List, Samples0, Samples) :-
		SampleCount > 0,
		random_distinct_multiset(K, List, Multiset),
		SampleCount1 is SampleCount - 1,
		sample_distinct_multisets_loop(SampleCount1, K, List, [Multiset| Samples0], Samples).

	next_multiset(List, Multiset, Next) :-
		length(Multiset, K),
		canonical_lexicographic_multisets(K, List, Multisets),
		nth0(Index, Multisets, Multiset),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Multisets, Next).

	previous_multiset(List, Multiset, Previous) :-
		length(Multiset, K),
		canonical_lexicographic_multisets(K, List, Multisets),
		nth0(Index, Multisets, Multiset),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Multisets, Previous).

	canonical_lexicographic_multisets(K, List, Multisets) :-
		distinct_multisets(K, List, lexicographic, Multisets).

	multiset_count_total(N, K, Count) :-
		( 	K < 0 ->
			Count = 0
		; 	N =:= 0 ->
			( 	K =:= 0 ->
				Count = 1
			; 	Count = 0
			)
		; 	NK1 is N + K - 1,
			binomial(NK1, K, Count)
		).

	apply_order(default, List, List).
	apply_order(lexicographic, List, Sorted) :-
		msort(List, Sorted).

	distinct_source(List, DistinctSource) :-
		remove_duplicates(List, DistinctSource).

:- end_object.

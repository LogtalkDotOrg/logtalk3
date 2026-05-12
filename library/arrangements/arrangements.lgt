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


:- object(arrangements,
	implements(arrangements_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Implementation of arrangements operations with repetition over lists.'
	]).

	:- uses(list, [
		length/2, member/2, msort/2, nth0/3, remove_duplicates/2, reverse/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	arrangements(K, List, Arrangements) :-
		findall(Arrangement, arrangement(K, List, Arrangement), Arrangements).

	arrangement(0, _, []) :-
		!.
	arrangement(K, List, [Head| Arrangement]) :-
		K > 0,
		K1 is K - 1,
		member(Head, List),
		arrangement(K1, List, Arrangement).

	arrangements(K, List, Order, Arrangements) :-
		generator_order_source(Order, List, OrderedList),
		arrangements(K, OrderedList, Arrangements).

	arrangement(K, List, Order, Arrangement) :-
		generator_order_source(Order, List, OrderedList),
		arrangement(K, OrderedList, Arrangement).

	distinct_arrangements(K, List, Arrangements) :-
		distinct_arrangements(K, List, default, Arrangements).

	distinct_arrangement(K, List, Arrangement) :-
		distinct_arrangement(K, List, default, Arrangement).

	distinct_arrangements(K, List, Order, Arrangements) :-
		findall(Arrangement, arrangement(K, List, Arrangement), Arrangements0),
		remove_duplicates(Arrangements0, DistinctArrangements),
		apply_order(Order, DistinctArrangements, Arrangements).

	distinct_arrangement(K, List, Order, Arrangement) :-
		distinct_arrangements(K, List, Order, Arrangements),
		member(Arrangement, Arrangements).

	cartesian_product(Lists, Product) :-
		findall(Tuple, cartesian_tuple(Lists, Tuple), Product).

	cartesian_tuple([], []).
	cartesian_tuple([List| Lists], [Element| Tuple]) :-
		member(Element, List),
		cartesian_tuple(Lists, Tuple).

	cartesian_product(Lists, Order, Product) :-
		findall(Tuple, cartesian_tuple(Lists, Tuple), Product0),
		apply_order(Order, Product0, Product).

	cartesian_tuple(Lists, Order, Tuple) :-
		cartesian_product(Lists, Order, Product),
		member(Tuple, Product).

	nth_arrangement(K, List, Index, Arrangement) :-
		length(List, N),
		Index >= 0,
		count_arrangements_total(N, K, Total),
		Index < Total,
		nth_arrangement_default(K, List, Index, Arrangement).

	nth_arrangement(K, List, default, Index, Arrangement) :-
		!,
		nth_arrangement(K, List, Index, Arrangement).
	nth_arrangement(K, List, lexicographic, Index, Arrangement) :-
		Index >= 0,
		arrangements(K, List, lexicographic, Arrangements),
		nth0(Index, Arrangements, Arrangement),
		!.

	nth_arrangement_default(0, _, _, []) :-
		!.
	nth_arrangement_default(K, List, Index, [Head| Arrangement]) :-
		K > 0,
		length(List, N),
		K1 is K - 1,
		count_arrangements_total(N, K1, BlockSize),
		Pos is Index // BlockSize,
		nth0(Pos, List, Head),
		Index1 is Index mod BlockSize,
		nth_arrangement_default(K1, List, Index1, Arrangement).

	arrangement_index(K, List, Arrangement, Index) :-
		length(Arrangement, K),
		arrangement_index_default(K, List, Arrangement, 0, Index).

	arrangement_index(K, List, default, Arrangement, Index) :-
		!,
		arrangement_index(K, List, Arrangement, Index).
	arrangement_index(K, List, lexicographic, Arrangement, Index) :-
		length(Arrangement, K),
		arrangements(K, List, lexicographic, Arrangements),
		nth0(Index, Arrangements, Arrangement),
		!.

	arrangement_index_default(0, _, [], Index, Index) :-
		!.
	arrangement_index_default(K, List, [Head| Arrangement], Index0, Index) :-
		K > 0,
		nth0(Pos, List, Head),
		!,
		length(List, N),
		K1 is K - 1,
		count_arrangements_total(N, K1, BlockSize),
		Index1 is Index0 + Pos * BlockSize,
		arrangement_index_default(K1, List, Arrangement, Index1, Index).

	count_arrangements(K, List, Count) :-
		length(List, N),
		count_arrangements_total(N, K, Count),
		!.

	count_distinct_arrangements(K, List, Count) :-
		distinct_source(List, DistinctSource),
		count_arrangements(K, DistinctSource, Count).

	nth_distinct_arrangement(K, List, Index, Arrangement) :-
		distinct_source(List, DistinctSource),
		nth_arrangement(K, DistinctSource, Index, Arrangement).

	distinct_arrangement_index(K, List, Arrangement, Index) :-
		distinct_source(List, DistinctSource),
		arrangement_index(K, DistinctSource, Arrangement, Index).

	random_arrangement(0, _, []) :-
		!.
	random_arrangement(K, List, [Element| Arrangement]) :-
		K > 0,
		length(List, N),
		N > 0,
		N1 is N - 1,
		random_between(0, N1, Index),
		nth0(Index, List, Element),
		K1 is K - 1,
		random_arrangement(K1, List, Arrangement).

	sample_arrangements(K, List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_arrangements_loop(SampleCount, K, List, [], Samples0),
		reverse(Samples0, Samples).

	sample_arrangements_loop(0, _, _, Samples, Samples) :-
		!.
	sample_arrangements_loop(SampleCount, K, List, Samples0, Samples) :-
		SampleCount > 0,
		random_arrangement(K, List, Arrangement),
		SampleCount1 is SampleCount - 1,
		sample_arrangements_loop(SampleCount1, K, List, [Arrangement| Samples0], Samples).

	random_distinct_arrangement(K, List, Arrangement) :-
		distinct_source(List, DistinctSource),
		random_arrangement(K, DistinctSource, Arrangement).

	sample_distinct_arrangements(K, List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_distinct_arrangements_loop(SampleCount, K, List, [], Samples0),
		reverse(Samples0, Samples).

	sample_distinct_arrangements_loop(0, _, _, Samples, Samples) :-
		!.
	sample_distinct_arrangements_loop(SampleCount, K, List, Samples0, Samples) :-
		SampleCount > 0,
		random_distinct_arrangement(K, List, Arrangement),
		SampleCount1 is SampleCount - 1,
		sample_distinct_arrangements_loop(SampleCount1, K, List, [Arrangement| Samples0], Samples).

	next_arrangement(List, Arrangement, Next) :-
		lexicographic_domain(List, Domain),
		length(Arrangement, K),
		arrangement_index(K, Domain, Arrangement, Index),
		NextIndex is Index + 1,
		nth_arrangement(K, Domain, NextIndex, Next).

	previous_arrangement(List, Arrangement, Previous) :-
		lexicographic_domain(List, Domain),
		length(Arrangement, K),
		arrangement_index(K, Domain, Arrangement, Index),
		Index > 0,
		PreviousIndex is Index - 1,
		nth_arrangement(K, Domain, PreviousIndex, Previous).

	count_arrangements_total(N, K, Count) :-
		( 	K < 0 ->
			Count = 0
		; 	count_power(K, N, 1, Count)
		).

	count_power(0, _, Count, Count) :-
		!.
	count_power(K, N, Count0, Count) :-
		K > 0,
		K1 is K - 1,
		Count1 is Count0 * N,
		count_power(K1, N, Count1, Count).

	lexicographic_domain(List, Domain) :-
		msort(List, Sorted),
		remove_duplicates(Sorted, Domain).

	generator_order_source(default, List, List).
	generator_order_source(lexicographic, List, Sorted) :-
		msort(List, Sorted).

	apply_order(default, List, List).
	apply_order(lexicographic, List, Sorted) :-
		msort(List, Sorted).

	distinct_source(List, DistinctSource) :-
		remove_duplicates(List, DistinctSource).

:- end_object.

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


:- object(permutations,
	implements(permutations_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Implementation of permutations operations over lists.'
	]).

	:- uses(natural, [
		factorial/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, msort/2, nth0/4, remove_duplicates/2, reverse/2, select/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	permutations(List, Permutations) :-
		findall(Permutation, permutation(List, Permutation), Permutations).

	permutation([], []).
	permutation(List, [Head| Permutation]) :-
		select(Head, List, Rest),
		permutation(Rest, Permutation).

	permutations(List, Order, Permutations) :-
		findall(Permutation, permutation(List, Permutation), Permutations0),
		apply_order(Order, Permutations0, Permutations).

	permutation(List, Order, Permutation) :-
		permutation(List, Permutation0),
		apply_order(Order, [Permutation0], [Permutation]).

	distinct_permutations(List, Permutations) :-
		distinct_permutations(List, default, Permutations).

	distinct_permutations(List, Order, Permutations) :-
		findall(Permutation, permutation(List, Permutation), Permutations0),
		remove_duplicates(Permutations0, DistinctPermutations),
		apply_order(Order, DistinctPermutations, Permutations).

	distinct_permutation(List, Permutation) :-
		distinct_permutation(List, default, Permutation).

	distinct_permutation(List, Order, Permutation) :-
		distinct_permutations(List, Order, Permutations),
		member(Permutation, Permutations).

	k_permutations(K, List, Permutations) :-
		findall(Permutation, k_permutation(K, List, Permutation), Permutations).

	k_permutation(0, _, []) :-
		!.
	k_permutation(K, List, [Head| Permutation]) :-
		K > 0,
		K1 is K - 1,
		select(Head, List, Rest),
		k_permutation(K1, Rest, Permutation).

	k_permutations(K, List, Order, Permutations) :-
		findall(Permutation, k_permutation(K, List, Permutation), Permutations0),
		apply_order(Order, Permutations0, Permutations).

	k_permutation(K, List, Order, Permutation) :-
		k_permutation(K, List, Permutation0),
		apply_order(Order, [Permutation0], [Permutation]).

	cartesian_product(K, List, Tuples) :-
		findall(Tuple, cartesian_tuple(K, List, Tuple), Tuples).

	cartesian_tuple(0, _, []).
	cartesian_tuple(K, List, [Head| Tuple]) :-
		K > 0,
		K1 is K - 1,
		member(Head, List),
		cartesian_tuple(K1, List, Tuple).

	derangements(List, Derangements) :-
		findall(Derangement, derangement(List, Derangement), Derangements).

	derangement(List, Derangement) :-
		permutation(List, Derangement),
		is_derangement(List, Derangement).

	is_derangement([], []).
	is_derangement([Head1| Tail1], [Head2| Tail2]) :-
		Head1 \== Head2,
		is_derangement(Tail1, Tail2).

	next_permutation(Permutation, Next) :-
		append(Prefix, [Pivot| Suffix], Permutation),
		Suffix \= [],
		has_greater(Pivot, Suffix),
		\+ has_greater_in_prefix(Prefix, [Pivot| Suffix]),
		!,
		find_rightmost_greater(Pivot, Suffix, Swap, SuffixWithoutSwap),
		append(SuffixWithoutSwap, [Pivot], NewSuffix),
		reverse(NewSuffix, RevSuffix),
		append(Prefix, [Swap| RevSuffix], Next).

	has_greater(Pivot, [Head| _]) :-
		Head @> Pivot,
		!.
	has_greater(Pivot, [_| Tail]) :-
		has_greater(Pivot, Tail).

	has_greater_in_prefix(Prefix, Suffix) :-
		append(Prefix, Suffix, Full),
		append(NewPrefix, [Pivot| Rest], Full),
		length(NewPrefix, PrefixLength),
		length(Prefix, CurrentPrefixLength),
		PrefixLength > CurrentPrefixLength,
		Rest \= [],
		has_greater(Pivot, Rest).

	find_rightmost_greater(Pivot, Suffix, Swap, SuffixWithoutSwap) :-
		reverse(Suffix, RevSuffix),
		RevSuffix = [Head| Tail],
		( 	Head @> Pivot ->
			Swap = Head,
			reverse(Tail, SuffixWithoutSwap)
		;	find_rightmost_greater_helper(Pivot, RevSuffix, Swap, RevNewSuffix),
			reverse(RevNewSuffix, SuffixWithoutSwap)
		).

	find_rightmost_greater_helper(Pivot, [Head| Tail], Swap, [Head| Rest]) :-
		Head @=< Pivot,
		!,
		find_rightmost_greater_helper(Pivot, Tail, Swap, Rest).
	find_rightmost_greater_helper(Pivot, [Head| Tail], Head, Tail) :-
		Head @> Pivot.

	previous_permutation(Permutation, Previous) :-
		append(Prefix, [Pivot| Suffix], Permutation),
		Suffix \= [],
		has_smaller(Pivot, Suffix),
		\+ has_smaller_in_prefix(Prefix, [Pivot| Suffix]),
		!,
		replace_rightmost_smaller(Pivot, Suffix, Swap, SwappedSuffix),
		reverse(SwappedSuffix, RevSuffix),
		append(Prefix, [Swap| RevSuffix], Previous).

	has_smaller(Pivot, [Head| _]) :-
		Head @< Pivot,
		!.
	has_smaller(Pivot, [_| Tail]) :-
		has_smaller(Pivot, Tail).

	has_smaller_in_prefix(Prefix, Suffix) :-
		append(Prefix, Suffix, Full),
		append(NewPrefix, [Pivot| Rest], Full),
		length(NewPrefix, PrefixLength),
		length(Prefix, CurrentPrefixLength),
		PrefixLength > CurrentPrefixLength,
		Rest \= [],
		has_smaller(Pivot, Rest).

	replace_rightmost_smaller(Pivot, Suffix, Swap, SwappedSuffix) :-
		reverse(Suffix, RevSuffix),
		replace_first_smaller(Pivot, RevSuffix, Swap, RevSwapped),
		reverse(RevSwapped, SwappedSuffix).

	replace_first_smaller(Pivot, [Head| Tail], Head, [Pivot| Tail]) :-
		Head @< Pivot,
		!.
	replace_first_smaller(Pivot, [Head| Tail], Swap, [Head| Rest]) :-
		Head @>= Pivot,
		replace_first_smaller(Pivot, Tail, Swap, Rest).

	nth_permutation(List, Index, Permutation) :-
		length(List, N),
		factorial(N, Total),
		Index >= 0,
		Index < Total,
		nth_permutation_helper(List, Index, Permutation).

	nth_permutation_helper([], _, []) :-
		!.
	nth_permutation_helper(List, Index, [Head| Permutation]) :-
		length(List, N),
		N1 is N - 1,
		factorial(N1, F),
		Pos is Index // F,
		nth0(Pos, List, Head, Rest),
		Index1 is Index mod F,
		nth_permutation_helper(Rest, Index1, Permutation).

	permutation_index(List, Permutation, Index) :-
		length(List, N),
		length(Permutation, N),
		permutation_index_helper(List, Permutation, 0, Index).

	permutation_index_helper([], [], Index, Index) :-
		!.
	permutation_index_helper(List, [Head| Permutation], Index0, Index) :-
		List \= [],
		length(List, N),
		once(nth0(Pos, List, Head, Rest)),
		N1 is N - 1,
		factorial(N1, F),
		Index1 is Index0 + Pos * F,
		permutation_index_helper(Rest, Permutation, Index1, Index).

	count_permutations(List, Count) :-
		length(List, N),
		factorial(N, Count).

	random_permutation(List, Permutation) :-
		random_permutation_helper(List, [], Permutation).

	random_permutation_helper([], Permutation, Permutation) :-
		!.
	random_permutation_helper(List, Permutation0, Permutation) :-
		length(List, N),
		N1 is N - 1,
		random_between(0, N1, Index),
		nth0(Index, List, Element, Rest),
		random_permutation_helper(Rest, [Element| Permutation0], Permutation).

	apply_order(default, List, List).
	apply_order(lexicographic, List, Sorted) :-
		msort(List, Sorted).
	apply_order(shortlex, List, Sorted) :-
		map_with_length(List, Mapped),
		msort(Mapped, MappedSorted),
		unmap_length(MappedSorted, Sorted).

	map_with_length([], []).
	map_with_length([Head| Tail], [Length-Head| Mapped]) :-
		length(Head, Length),
		map_with_length(Tail, Mapped).

	unmap_length([], []).
	unmap_length([_-Head| Mapped], [Head| Tail]) :-
		unmap_length(Mapped, Tail).

:- end_object.

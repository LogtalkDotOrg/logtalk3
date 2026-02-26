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


:- object(combinations,
	implements(combinations_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Implementation of combinations operations over lists.'
	]).

	:- uses(natural, [
		binomial/3
	]).

	:- uses(list, [
		append/3, drop/3, length/2, member/2, msort/2, nth0/4, nth1/3, remove_duplicates/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		set/4 as random_set/4
	]).

	combinations(K, List, Combinations) :-
		combinations(K, List, default, Combinations).

	combination(0, _, []).
	combination(K, [Head| Tail], [Head| Combination]) :-
		K > 0,
		K1 is K - 1,
		combination(K1, Tail, Combination).
	combination(K, [_| Tail], Combination) :-
		K > 0,
		combination(K, Tail, Combination).

	combinations(K, List, Order, Combinations) :-
		findall(Combination, combination(K, List, Combination), Combinations0),
		apply_order(Order, Combinations0, Combinations).

	combination(K, List, Order, Combination) :-
		combination(K, List, Combination0),
		apply_order(Order, [Combination0], [Combination]).

	combinations_with_replacement(K, List, Combinations) :-
		findall(Combination, combination_with_replacement(K, List, Combination), Combinations).

	combinations_with_replacement(K, List, Order, Combinations) :-
		findall(Combination, combination_with_replacement(K, List, Combination), Combinations0),
		apply_order(Order, Combinations0, Combinations).

	combination_with_replacement(0, _, []).
	combination_with_replacement(K, List, [Head| Combination]) :-
		K > 0,
		K1 is K - 1,
		append(_, [Head| Tail], List),
		combination_with_replacement(K1, [Head| Tail], Combination).

	combination_with_replacement(K, List, Order, Combination) :-
		combination_with_replacement(K, List, Combination0),
		apply_order(Order, [Combination0], [Combination]).

	distinct_combinations(K, List, Combinations) :-
		distinct_combinations(K, List, default, Combinations).

	distinct_combinations(K, List, Order, Combinations) :-
		findall(Combination, combination(K, List, Combination), Combinations0),
		remove_duplicates(Combinations0, DistinctCombinations),
		apply_order(Order, DistinctCombinations, Combinations).

	distinct_combination(K, List, Combination) :-
		distinct_combination(K, List, default, Combination).

	distinct_combination(K, List, Order, Combination) :-
		distinct_combinations(K, List, Order, Combinations),
		member(Combination, Combinations).

	nth_combination(K, List, Index, Combination) :-
		length(List, N),
		Index >= 0,
		binomial(N, K, Total),
		Index < Total,
		nth_combination_helper(K, List, Index, Combination).

	nth_combination_helper(0, _, _, []) :-
		!.
	nth_combination_helper(K, List, Index, [Head| Comb]) :-
		K > 0,
		length(List, N),
		find_comb_position(N, K, Index, C, IndexRemainder),
		nth0(C, List, Head, _),
		C1 is C + 1,
		drop(C1, List, Rest),
		K1 is K - 1,
		nth_combination_helper(K1, Rest, IndexRemainder, Comb).

	find_comb_position(N, K, Index, C, IndexRemainder) :-
		find_comb_position_loop(0, N, K, Index, 0, C, IndexRemainder).

	find_comb_position_loop(Pos, N, _K, Index, _Count, C, IndexRemainder) :-
		Pos >= N,
		!,
		C is N - 1,
		IndexRemainder = Index.
	find_comb_position_loop(Pos, N, K, Index, Count, C, IndexRemainder) :-
		Pos < N,
		K1 is K - 1,
		Remaining is N - Pos - 1,
		binomial(Remaining, K1, CombsAtPos),
		NewCount is Count + CombsAtPos,
		( 	NewCount > Index ->
			C = Pos,
			IndexRemainder is Index - Count
		;	Pos1 is Pos + 1,
			find_comb_position_loop(Pos1, N, K, Index, NewCount, C, IndexRemainder)
		).

	combination_index(K, List, Combination, Index) :-
		length(Combination, K),
		combination_index_helper(K, List, Combination, 0, Index).

	combination_index_helper(0, _, [], Index, Index) :-
		!.
	combination_index_helper(K, [Head| Tail], [Head| Comb], Index0, Index) :-
		K > 0,
		!,
		K1 is K - 1,
		combination_index_helper(K1, Tail, Comb, Index0, Index).
	combination_index_helper(K, [_| Tail], Combination, Index0, Index) :-
		K > 0,
		length(Tail, N),
		K1 is K - 1,
		binomial(N, K1, Skip),
		Index1 is Index0 + Skip,
		combination_index_helper(K, Tail, Combination, Index1, Index).

	count_combinations(K, List, Count) :-
		length(List, N),
		( 	N >= K, K >= 0 ->
			binomial(N, K, Count)
		;	Count = 0
		).

	count_combinations_with_replacement(K, List, Count) :-
		length(List, N),
		( 	K < 0 ->
			Count = 0
		; 	N =:= 0 ->
			( 	K =:= 0 -> Count = 1 ; Count = 0 )
		; 	NK1 is N + K - 1,
			binomial(NK1, K, Count)
		).

	random_combination(K, List, Combination) :-
		length(List, N),
		K =< N,
		random_set(K, 1, N, Indices),
		select_by_indices(Indices, List, Combination).

	select_by_indices([], _, []).
	select_by_indices([Indice| Indices], List, [Element| Elements]) :-
		nth1(Indice, List, Element),
		select_by_indices(Indices, List, Elements).

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

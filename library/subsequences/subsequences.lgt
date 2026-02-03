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


:- object(subsequences,
	implements(subsequences_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-03,
		comment is 'Implementation of subsequence operations over lists.'
	]).

	:- uses(natural, [
		binomial/3, factorial/2
	]).

	:- uses(list, [
		append/3, drop/3, length/2, member/2, msort/2, nth0/3, nth0/4, nth1/3, reverse/2, select/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3, maybe/0, set/4 as random_set/4
	]).

	% generation operations - Creating all subsequences

	subsequences(List, Subsequences) :-
		findall(Subsequence, subsequence(List, Subsequence), Subsequences).

	subsequence([], []).
	subsequence([Head| Tail], [Head| Subsequence]) :-
		subsequence(Tail, Subsequence).
	subsequence([_| Tail], Subsequence) :-
		subsequence(Tail, Subsequence).

	subsequences(List, Order, Subsequences) :-
		findall(Subsequence, subsequence(List, Subsequence), Subsequences0),
		apply_order(Order, Subsequences0, Subsequences).

	subsequence(List, Order, Subsequence) :-
		subsequence(List, Subsequence0),
		apply_order(Order, [Subsequence0], [Subsequence]).

	% Non-empty subsequences
	nonempty_subsequences(List, Subsequences) :-
		findall(Subsequence, nonempty_subsequence(List, Subsequence), Subsequences).

	nonempty_subsequence(List, Subsequence) :-
		subsequence(List, Subsequence),
		Subsequence \= [].

	% Power set - alias for subsequences
	power_set(List, PowerSet) :-
		findall(Sub, subsequence(List, Sub), PowerSet).

	inits(List, Inits) :-
		findall(Init, init(List, Init), Inits).

	init(List, Init) :-
		append(Init, _, List).

	tails(List, Tails) :-
		findall(Tail, tail(List, Tail), Tails).

	tail(List, Tail) :-
		append(_, Tail, List).

	inits1(List, Inits) :-
		findall(Init, (init(List, Init), Init \= []), Inits).

	init1(List, Init) :-
		init(List, Init),
		Init \= [].

	tails1(List, Tails) :-
		findall(Tail, (tail(List, Tail), Tail \= []), Tails).

	tail1(List, Tail) :-
		tail(List, Tail),
		Tail \= [].

	init_tails(List, Pairs) :-
		findall(Init-Tail, append(Init, Tail, List), Pairs).

	init_tail(List, Init-Tail) :-
		append(Init, Tail, List).

	% filtered subsequence generation

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

	combination_with_replacement(0, _, []).
	combination_with_replacement(K, List, [Head| Combination]) :-
		K > 0,
		K1 is K - 1,
		append(_, [Head| Tail], List),  % Select element H and everything after
		combination_with_replacement(K1, [Head| Tail], Combination).  % Can reuse H

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

	% Cartesian product - K-tuples with replacement where order matters
	cartesian_product(K, List, Tuples) :-
		findall(Tuple, cartesian_tuple(K, List, Tuple), Tuples).

	cartesian_tuple(0, _, []).
	cartesian_tuple(K, List, [H| Tuple]) :-
		K > 0,
		K1 is K - 1,
		member(H, List),
		cartesian_tuple(K1, List, Tuple).

	derangements(List, Derangements) :-
		findall(Derang, derangement(List, Derang), Derangements).

	derangement(List, Derang) :-
		permutation(List, Derang),
		is_derangement(List, Derang).

	is_derangement([], []).
	is_derangement([Head1| Tail1], [Head2| Tail2]) :-
		Head1 \== Head2,
		is_derangement(Tail1, Tail2).

	% Next permutation in lexicographic order (C++ STL algorithm)
	% Algorithm:
	% 1. Find largest index i such that Perm[i] < Perm[i+1] (the pivot)
	% 2. Find largest index j > i such that Perm[i] < Perm[j]
	% 3. Swap Perm[i] and Perm[j]
	% 4. Reverse the suffix starting at Perm[i+1]
	next_permutation(Perm, Next) :-
		append(Prefix, [Pivot|Suffix], Perm),
		Suffix \= [],
		has_greater(Pivot, Suffix),
		\+ has_greater_in_prefix(Prefix, [Pivot|Suffix]),
		!,
		find_rightmost_greater(Pivot, Suffix, Swap, SuffixWithoutSwap),
		% Insert Pivot where Swap was (at the end, since Swap is rightmost greater)
		append(SuffixWithoutSwap, [Pivot], NewSuffix),
		reverse(NewSuffix, RevSuffix),
		append(Prefix, [Swap|RevSuffix], Next).

	% Check if there's an element greater than Pivot in the suffix
	has_greater(Pivot, [Head| _]) :-
		Head @> Pivot,
		!.
	has_greater(Pivot, [_| Tail]) :-
		has_greater(Pivot, Tail).

	% Check if there's a valid pivot point later in the list
	has_greater_in_prefix(Prefix, Suffix) :-
		append(Prefix, Suffix, Full),
		append(NewPrefix, [P|S], Full),
		length(NewPrefix, L1),
		length(Prefix, L2),
		L1 > L2,
		S \= [],
		has_greater(P, S).

	% Find rightmost element greater than Pivot and remove it from suffix
	find_rightmost_greater(Pivot, Suffix, Swap, SuffixWithoutSwap) :-
		reverse(Suffix, RevSuffix),
		RevSuffix = [H|T],
		(	H @> Pivot ->
			Swap = H,
			reverse(T, SuffixWithoutSwap)
		;	find_rightmost_greater_helper(Pivot, RevSuffix, Swap, RevNewSuffix),
			reverse(RevNewSuffix, SuffixWithoutSwap)
		).

	find_rightmost_greater_helper(Pivot, [H| T], Swap, [H| Rest]) :-
		H @=< Pivot,
		!,
		find_rightmost_greater_helper(Pivot, T, Swap, Rest).
	find_rightmost_greater_helper(Pivot, [H| T], H, T) :-
		H @> Pivot.

	% Previous permutation in lexicographic order
	% Algorithm: similar to next but with reversed comparisons
	% 1. Find largest index i such that Perm[i] > Perm[i+1]
	% 2. Find largest index j > i such that Perm[i] > Perm[j] (largest element smaller than pivot)
	% 3. Swap Perm[i] and Perm[j]
	% 4. Reverse the suffix starting at Perm[i+1]
	previous_permutation(Perm, Prev) :-
		append(Prefix, [Pivot| Suffix], Perm),
		Suffix \= [],
		has_smaller(Pivot, Suffix),
		\+ has_smaller_in_prefix(Prefix, [Pivot| Suffix]),
		!,
		% Find rightmost element smaller than pivot and swap with pivot in suffix
		replace_rightmost_smaller(Pivot, Suffix, Swap, SwappedSuffix),
		reverse(SwappedSuffix, RevSuffix),
		append(Prefix, [Swap| RevSuffix], Prev).

	has_smaller(Pivot, [Head| _]) :-
		Head @< Pivot,
		!.
	has_smaller(Pivot, [_| Tail]) :-
		has_smaller(Pivot, Tail).

	has_smaller_in_prefix(Prefix, Suffix) :-
		append(Prefix, Suffix, Full),
		append(NewPrefix, [P| S], Full),
		length(NewPrefix, Length1),
		length(Prefix, Length2),
		Length1 > Length2,
		S \= [],
		has_smaller(P, S).

	% Replace the rightmost element smaller than Pivot with Pivot
	% Returns the element that was replaced (Swap) and the new suffix
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

	% indexed access to subsequences

	nth_permutation(List, Index, Permutation) :-
		length(List, N),
		factorial(N, Total),
		Index >= 0,
		Index < Total,
		nth_permutation_helper(List, Index, Permutation).

	% Helper using factorial number system (Lehmer code)
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

	nth_combination(K, List, Index, Combination) :-
		length(List, N),
		Index >= 0,
		binomial(N, K, Total),
		Index < Total,
		nth_combination_helper(K, List, Index, Combination).

	% Helper using combinatorial number system
	% Algorithm: For each element in the combination, find which element from the list to pick
	% such that the remaining elements can form valid combinations
	nth_combination_helper(0, _, _, []) :-
		!.
	nth_combination_helper(K, List, Index, [Head| Comb]) :-
		K > 0,
		length(List, N),
		% We need to find position C in the current list
		% Count how many combinations start with each position
		find_comb_position(N, K, Index, C, IndexRemainder),
		nth0(C, List, Head),
		C1 is C + 1,
		drop(C1, List, Rest),
		K1 is K - 1,
		nth_combination_helper(K1, Rest, IndexRemainder, Comb).

	% Find the position C where the first element comes from
	% Also return the adjusted index for the remaining elements
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
		(	NewCount > Index ->
			C = Pos,
			IndexRemainder is Index - Count
		;	Pos1 is Pos + 1,
			find_comb_position_loop(Pos1, N, K, Index, NewCount, C, IndexRemainder)
		).

	combination_index(K, List, Combination, Index) :-
		length(Combination, K),
		combination_index_helper(K, List, Combination, 0, Index).

	% Base case: empty combination
	combination_index_helper(0, _, [], Index, Index) :-
		!.
	% Recursive case: find position of first element and count skipped combinations
	combination_index_helper(K, [Head| Tail], [Head| Comb], Index0, Index) :-
		% First element matches - continue with rest
		K > 0,
		!,
		K1 is K - 1,
		combination_index_helper(K1, Tail, Comb, Index0, Index).
	combination_index_helper(K, [_| Tail], Combination, Index0, Index) :-
		% First element doesn't match - count combinations we're skipping
		K > 0,
		length(Tail, N),
		K1 is K - 1,
		binomial(N, K1, Skip),
		Index1 is Index0 + Skip,
		combination_index_helper(K, Tail, Combination, Index1, Index).

	permutation_index(List, Permutation, Index) :-
		length(List, N),
		length(Permutation, N),
		permutation_index_helper(List, Permutation, 0, Index).

	% Helper using factorial number system (Lehmer code) - inverse of nth_permutation_helper
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

	% searching and matching subsequences

	longest_common_subsequence(List1, List2, LCS) :-
		lcs_dp(List1, List2, LCS).

	% Dynamic programming LCS
	lcs_dp([], _, []) :-
		!.
	lcs_dp(_, [], []) :-
		!.
	lcs_dp([Head| Tail1], [Head| Tail2], [Head| LCS]) :-
		!,
		lcs_dp(Tail1, Tail2, LCS).
	lcs_dp([Head1| Tail1], [Head2| Tail2], LCS) :-
		lcs_dp([Head1| Tail1], Tail2, LCS1),
		lcs_dp(Tail1, [Head2| Tail2], LCS2),
		(	length(LCS1, L1),
			length(LCS2, L2),
			L1 >= L2 ->
			LCS = LCS1
		;	LCS = LCS2
		).

	longest_increasing_subsequence(List, LIS) :-
		lis_helper(List, [], LIS).

	% Helper for LIS using patience sorting approach
	lis_helper([], LIS0, LIS) :-
		longest_list(LIS0, LIS).
	lis_helper([Head| Tail], LIS0, LIS) :-
		insert_or_extend(Head, LIS0, LIS1),
		lis_helper(Tail, LIS1, LIS).

	insert_or_extend(X, [], [[X]]).
	insert_or_extend(X, [Seq| Rest], [Seq| NewRest]) :-
		Seq = [Last| _],
		X =< Last,
		!,
		insert_or_extend(X, Rest, NewRest).
	insert_or_extend(X, [Seq| Rest], [[X| Seq]| Rest]).

	longest_list([], []).
	longest_list([Head| Tail], Longest) :-
		longest_list(Tail, TailLongest),
		(	length(Head, HeadLength),
			length(TailLongest, TailLongestLength),
			HeadLength > TailLongestLength ->
			Longest = Head
		;	Longest = TailLongest
		).

	% Longest decreasing subsequence - symmetric to LIS
	longest_decreasing_subsequence(List, LDS) :-
		lds_helper(List, [], LDS).

	lds_helper([], LDS0, LDS) :-
		longest_list(LDS0, LDS).
	lds_helper([Head| Tail], LDS0, LDS) :-
		insert_or_extend_decreasing(Head, LDS0, LDS1),
		lds_helper(Tail, LDS1, LDS).

	insert_or_extend_decreasing(X, [], [[X]]).
	insert_or_extend_decreasing(X, [Seq| Rest], [Seq| NewRest]) :-
		Seq = [Last|_],
		X >= Last,
		!,
		insert_or_extend_decreasing(X, Rest, NewRest).
	insert_or_extend_decreasing(X, [Seq| Rest], [[X| Seq]| Rest]).

	longest_common_increasing_subsequence(List1, List2, LCIS) :-
		findall(Seq, (subsequence(List1, Seq), subsequence(List2, Seq), is_increasing(Seq)), Seqs),
		longest_list(Seqs, LCIS).

	is_increasing([]).
	is_increasing([_]).
	is_increasing([A,B| Tail]) :-
		A @< B,
		is_increasing([B| Tail]).

	longest_repeating_subsequence(List, LRS) :-
		% LRS is like LCS but with constraint that indices must differ
		lrs_helper(List, List, 1, 1, LRS).

	lrs_helper([], _, _, _, []).
	lrs_helper(_, [], _, _, []).
	lrs_helper([Head| Tail1], [Head| Tail2], I, J, [Head| LRS]) :-
		I \= J,
		!,
		I1 is I + 1,
		J1 is J + 1,
		lrs_helper(Tail1, Tail2, I1, J1, LRS).
	lrs_helper([_| Tail1], [_| Tail2], I, J, LRS) :-
		I1 is I + 1,
		J1 is J + 1,
		lrs_helper([_| Tail1], Tail2, I, J1, LRS1),
		lrs_helper(Tail1, [_| Tail2], I1, J, LRS2),
		(	length(LRS1, Length1),
			length(LRS2, Length2),
			Length1 >= Length2 ->
			LRS = LRS1
		;	LRS = LRS2
		).

	is_subsequence_of([], _) :-
		!.
	is_subsequence_of([Head| Tail1], [Head| Tail2]) :-
		!,
		is_subsequence_of(Tail1, Tail2).
	is_subsequence_of(Subsequence, [_| Tail]) :-
		is_subsequence_of(Subsequence, Tail).

	common_subsequences(List1, List2, CommonSubsequences) :-
		findall(Subsequence, (subsequence(List1, Subsequence), subsequence(List2, Subsequence)), CommonSubsequences).

	common_subsequence(List1, List2, CommonSubsequence) :-
		subsequence(List1, CommonSubsequence),
		subsequence(List2, CommonSubsequence).

	% Count distinct subsequences - using dynamic programming approach
	count_distinct_subsequences(Pattern, List, Count) :-
		length(Pattern, M),
		length(List, N),
		count_distinct_subseq_dp(Pattern, List, M, N, Count).

	count_distinct_subseq_dp([], _, _, _, 1) :- !.
	count_distinct_subseq_dp(_, [], _, _, 0) :- !.
	count_distinct_subseq_dp([P| Ps], [L| Ls], M, N, Count) :-
		M1 is M - 1,
		N1 is N - 1,
		(	P == L ->
			% Both match: count subsequences with and without using L
			count_distinct_subseq_dp(Ps, Ls, M1, N1, Count1),
			count_distinct_subseq_dp([P| Ps], Ls, M, N1, Count2),
			Count is Count1 + Count2
		;	% No match: skip L
			count_distinct_subseq_dp([P| Ps], Ls, M, N1, Count)
		).

	% prefix and suffix operations

	is_prefix_of([], _).
	is_prefix_of([Head| Tail1], [Head| Tail2]) :-
		is_prefix_of(Tail1, Tail2).

	is_suffix_of(Suffix, List) :-
		append(_, Suffix, List),
		!.

	% contiguous subsequences

	% Generate all contiguous non-empty subslices
	subslices(List, Subslices) :-
		findall(Subslice, subslice(List, Subslice), Subslices).

	subslice(List, Subslice) :-
		append(_, Suffix, List),
		Suffix \= [],
		append(Subslice, _, Suffix),
		Subslice \= [].

	% Sliding window - fixed-size windows
	sliding_window(N, List, Windows) :-
		findall(Window, sliding_window_gen(N, List, Window), Windows).

	sliding_window_gen(N, List, Window) :-
		length(Window, N),
		append(Window, _, Suffix),
		append(_, Suffix, List).

	% random selection

	random_combination(K, List, Combination) :-
		length(List, N),
		K =< N,
		random_set(K, 1, N, Indices),
		select_by_indices(Indices, List, Combination).

	select_by_indices([], _, []).
	select_by_indices([Indice| Indices], List, [Element| Elements]) :-
		nth1(Indice, List, Element),
		select_by_indices(Indices, List, Elements).

	random_permutation(List, Permutation) :-
		random_permutation_helper(List, [], Permutation).

	random_permutation_helper([], Permutation, Permutation) :-
		!.
	random_permutation_helper(List, Permutation0, Permutation) :-
		length(List, N),
		N1 is N - 1,
		random_between(0, N1, Indice),
		nth0(Indice, List, Element, Rest),
		random_permutation_helper(Rest, [Element| Permutation0], Permutation).

	random_subsequence(List, Subsequence) :-
		random_subsequence_helper(List, Subsequence).

	random_subsequence_helper([], []).
	random_subsequence_helper([Head| Tail], Subsequence) :-
		(	maybe ->
			random_subsequence_helper(Tail, Subsequence)
		;	random_subsequence_helper(Tail, Rest),
			Subsequence = [Head| Rest]
		).

	% constrained subsequence operations

	subsequences_with_min_span(MinSpan, List, Subsequences) :-
		findall(Subsequence, subsequence_with_min_span(MinSpan, List, 0, Subsequence), Subsequences).

	subsequence_with_min_span(_, [], _, []).
	subsequence_with_min_span(MinSpan, [Head| Tail], Pos, [Head| Subsequence]) :-
		NextPos is Pos + MinSpan,
		drop_until(NextPos, Tail, 1, Rest),
		subsequence_with_min_span(MinSpan, Rest, NextPos, Subsequence).
	subsequence_with_min_span(MinSpan, [_| Tail], Pos, Subsequence) :-
		Pos1 is Pos + 1,
		subsequence_with_min_span(MinSpan, Tail, Pos1, Subsequence).

	drop_until(_, List, Pos, List) :-
		Pos >= 0,
		!.
	drop_until(Target, [_| Tail], Pos, Rest) :-
		Pos1 is Pos + 1,
		drop_until(Target, Tail, Pos1, Rest).

	alternating_subsequences(List, Subsequences) :-
		findall(Sub, alternating_subsequence(List, Sub), Subsequences).

	alternating_subsequence(List, Subsequence) :-
		subsequence(List, Subsequence),
		% At least 2 elements
		Subsequence = [_, _| _],
		is_alternating(Subsequence).

	is_alternating([A, B, C| Tail]) :-
		!,
		(	A @< B, B @> C ->
			true
		; 	A @> B, B @< C
		),
		is_alternating([B, C| Tail]).
	is_alternating(_).

	k_distinct_subsequences(K, List, Subsequences) :-
		findall(Sub, k_distinct_subsequence(K, List, Sub), Subsequences).

	k_distinct_subsequence(K, List, Subsequence) :-
		combination(K, List, Subsequence),
		all_distinct(Subsequence).

	all_distinct([]).
	all_distinct([Head| Tail]) :-
		\+ member(Head, Tail),
		all_distinct(Tail).

	% utility predicates

	count_subsequences(List, Count) :-
		length(List, N),
		Count is 2 ^ N.

	count_combinations(K, List, Count) :-
		length(List, N),
		(	N >= K, K >= 0 ->
			binomial(N, K, Count)
		;	Count = 0
		).

	count_permutations(List, Count) :-
		length(List, N),
		factorial(N, Count).

	subsequence_length(Subsequence, Length) :-
		length(Subsequence, Length).

	% auxiliary predicates

	% Apply ordering to a list of results
	% default: keep as-is
	% lexicographic: sort lexicographically
	% shortlex: sort by length first, then lexicographically
	apply_order(default, List, List).
	apply_order(lexicographic, List, Sorted) :-
		msort(List, Sorted).
	apply_order(shortlex, List, Sorted) :-
		map_with_length(List, Mapped),
		msort(Mapped, MappedSorted),
		unmap_length(MappedSorted, Sorted).

	map_with_length([], []).
	map_with_length([Head| Tail], [Lenght-Head| Mapped]) :-
		length(Head, Lenght),
		map_with_length(Tail, Mapped).

	unmap_length([], []).
	unmap_length([_-Head| Mapped], [Head| Tail]) :-
		unmap_length(Mapped, Tail).

:- end_object.

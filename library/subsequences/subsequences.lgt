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
		date is 2026-02-26,
		comment is 'Implementation of subsequence operations over lists.'
	]).

	:- uses(list, [
		append/3, length/2, member/2, msort/2, nth1/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		maybe/0
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

	% Auxiliary predicate for LIS using patience sorting approach
	lis_helper([], LIS0, LIS) :-
		longest_list(LIS0, LIS).
	lis_helper([Head| Tail], LIS0, LIS) :-
		insert_or_extend(Head, LIS0, LIS1),
		lis_helper(Tail, LIS1, LIS).

	insert_or_extend(X, [], [[X]]) :-
		!.
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

	proper_subsequence(Subsequence, List) :-
		is_subsequence_of(Subsequence, List),
		Subsequence \== List.

	subsequence_at_indices(List, Indices, Subsequence) :-
		subsequence_at_indices(Indices, 0, List, Subsequence).

	subsequence_at_indices([], _, _, []).
	subsequence_at_indices([Index| Indices], Previous, List, [Element| Subsequence]) :-
		integer(Index),
		Index > Previous,
		nth1(Index, List, Element),
		subsequence_at_indices(Indices, Index, List, Subsequence).

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
		subsequence(List, Subsequence),
		length(Subsequence, K),
		all_distinct(Subsequence).

	all_distinct([]).
	all_distinct([Head| Tail]) :-
		\+ member(Head, Tail),
		all_distinct(Tail).

	% utility predicates

	count_subsequences(List, Count) :-
		length(List, N),
		Count is 2 ^ N.

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

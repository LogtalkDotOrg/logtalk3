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


:- object(derangements,
	implements(derangements_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Implementation of derangement operations over lists.'
	]).

	:- uses(natural, [
		binomial/3, subfactorial/2
	]).

	:- uses(list, [
		length/2, member/2, nth0/3, remove_duplicates/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	derangements(List, Derangements) :-
		derangements(List, default, Derangements).

	derangements(List, Order, Derangements) :-
		permutations::permutations(List, Order, Permutations),
		filter_derangements(Permutations, List, Derangements).

	derangement(List, Derangement) :-
		derangements(List, Derangements),
		member(Derangement, Derangements).

	derangement(List, Order, Derangement) :-
		derangements(List, Order, Derangements),
		member(Derangement, Derangements).

	distinct_derangements(List, Derangements) :-
		distinct_derangements(List, default, Derangements).

	distinct_derangements(List, Order, Derangements) :-
		permutations::distinct_permutations(List, Order, Permutations),
		filter_derangements(Permutations, List, Derangements).

	distinct_derangement(List, Derangement) :-
		distinct_derangement(List, default, Derangement).

	distinct_derangement(List, Order, Derangement) :-
		distinct_derangements(List, Order, Derangements),
		member(Derangement, Derangements).

	nth_derangement(List, Index, Derangement) :-
		Index >= 0,
		derangements(List, Derangements),
		nth0(Index, Derangements, Derangement),
		!.

	nth_derangement(List, default, Index, Derangement) :-
		!,
		nth_derangement(List, Index, Derangement).
	nth_derangement(List, Order, Index, Derangement) :-
		Index >= 0,
		derangements(List, Order, Derangements),
		nth0(Index, Derangements, Derangement),
		!.

	derangement_index(List, Derangement, Index) :-
		length(List, N),
		length(Derangement, N),
		derangements(List, Derangements),
		nth0(Index, Derangements, Derangement),
		!.

	derangement_index(List, default, Derangement, Index) :-
		!,
		derangement_index(List, Derangement, Index).
	derangement_index(List, Order, Derangement, Index) :-
		length(List, N),
		length(Derangement, N),
		derangements(List, Order, Derangements),
		nth0(Index, Derangements, Derangement),
		!.

	count_derangements(List, Count) :-
		all_distinct(List),
		!,
		length(List, N),
		subfactorial(N, Count).
	count_derangements(List, Count) :-
		derangements(List, Derangements),
		length(Derangements, Count).

	count_partial_derangements(FixedPoints, List, Count) :-
		length(List, N),
		(	FixedPoints >= 0,
			FixedPoints =< N ->
			count_partial_derangements_checked(FixedPoints, List, N, Count)
		;	Count = 0
		).

	count_distinct_derangements(List, Count) :-
		distinct_derangements(List, Derangements),
		length(Derangements, Count).

	nth_distinct_derangement(List, Index, Derangement) :-
		Index >= 0,
		distinct_derangements(List, Derangements),
		nth0(Index, Derangements, Derangement),
		!.

	distinct_derangement_index(List, Derangement, Index) :-
		length(List, N),
		length(Derangement, N),
		distinct_derangements(List, Derangements),
		nth0(Index, Derangements, Derangement),
		!.

	random_derangement(List, Derangement) :-
		all_distinct(List),
		!,
		distinct_input_derangement_count(List, Count),
		Count > 0,
		random_derangement_distinct_input(List, Derangement).
	random_derangement(List, Derangement) :-
		derangements(List, Derangements),
		length(Derangements, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth0(Index, Derangements, Derangement),
		!.

	sample_derangements(List, SampleCount, Samples) :-
		SampleCount >= 0,
		all_distinct(List),
		!,
		distinct_input_derangement_count(List, Count),
		once((
			SampleCount =:= 0
		;	Count > 0
		)),
		sample_derangements_distinct_input_loop(SampleCount, List, Samples).
	sample_derangements(List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_derangements_loop(SampleCount, List, Samples).

	sample_derangements_loop(0, _, []) :-
		!.
	sample_derangements_loop(SampleCount, List, [Derangement| Samples]) :-
		SampleCount > 0,
		random_derangement(List, Derangement),
		SampleCount1 is SampleCount - 1,
		sample_derangements_loop(SampleCount1, List, Samples).

	random_distinct_derangement(List, Derangement) :-
		distinct_derangements(List, Derangements),
		length(Derangements, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth0(Index, Derangements, Derangement),
		!.

	sample_distinct_derangements(List, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_distinct_derangements_loop(SampleCount, List, Samples).

	sample_distinct_derangements_loop(0, _, []) :-
		!.
	sample_distinct_derangements_loop(SampleCount, List, [Derangement| Samples]) :-
		SampleCount > 0,
		random_distinct_derangement(List, Derangement),
		SampleCount1 is SampleCount - 1,
		sample_distinct_derangements_loop(SampleCount1, List, Samples).

	next_derangement(List, Derangement, Next) :-
		canonical_lexicographic_derangements(List, Derangements),
		once(nth0(Index, Derangements, Derangement)),
		NextIndex is Index + 1,
		nth0(NextIndex, Derangements, Next).

	previous_derangement(List, Derangement, Previous) :-
		canonical_lexicographic_derangements(List, Derangements),
		once(nth0(Index, Derangements, Derangement)),
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Derangements, Previous).

	canonical_lexicographic_derangements(List, Derangements) :-
		distinct_derangements(List, lexicographic, Derangements).

	distinct_input_derangement_count(List, Count) :-
		length(List, N),
		subfactorial(N, Count).

	random_derangement_distinct_input(List, Derangement) :-
		permutations::random_permutation(List, Derangement0),
		(	is_derangement(List, Derangement0) ->
			Derangement = Derangement0
		;	random_derangement_distinct_input(List, Derangement)
		).

	sample_derangements_distinct_input_loop(0, _, []) :-
		!.
	sample_derangements_distinct_input_loop(SampleCount, List, [Derangement| Samples]) :-
		SampleCount > 0,
		random_derangement_distinct_input(List, Derangement),
		SampleCount1 is SampleCount - 1,
		sample_derangements_distinct_input_loop(SampleCount1, List, Samples).

	all_distinct(List) :-
		remove_duplicates(List, Distinct),
		length(List, Length),
		length(Distinct, Length).

	count_partial_derangements_checked(FixedPoints, List, N, Count) :-
		all_distinct(List),
		!,
		binomial(N, FixedPoints, Binomial),
		Remaining is N - FixedPoints,
		subfactorial(Remaining, Subfactorial),
		Count is Binomial * Subfactorial.
	count_partial_derangements_checked(FixedPoints, List, _, Count) :-
		permutations::permutations(List, Permutations),
		count_permutations_with_fixed_points(Permutations, List, FixedPoints, 0, Count).

	count_permutations_with_fixed_points([], _, _, Count, Count).
	count_permutations_with_fixed_points([Permutation| Permutations], List, FixedPoints, Count0, Count) :-
		fixed_points_count(List, Permutation, 0, PermutationFixedPoints),
		(	PermutationFixedPoints =:= FixedPoints ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		count_permutations_with_fixed_points(Permutations, List, FixedPoints, Count1, Count).

	fixed_points_count([], [], Count, Count).
	fixed_points_count([Head1| Tail1], [Head2| Tail2], Count0, Count) :-
		(	Head1 == Head2 ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		fixed_points_count(Tail1, Tail2, Count1, Count).

	filter_derangements([], _, []).
	filter_derangements([Permutation| Permutations], List, [Permutation| Derangements]) :-
		is_derangement(List, Permutation),
		!,
		filter_derangements(Permutations, List, Derangements).
	filter_derangements([_| Permutations], List, Derangements) :-
		filter_derangements(Permutations, List, Derangements).

	is_derangement([], []).
	is_derangement([Head1| Tail1], [Head2| Tail2]) :-
		Head1 \== Head2,
		is_derangement(Tail1, Tail2).

:- end_object.

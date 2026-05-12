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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Unit tests for the "combinations" library.'
	]).

	cover(combinations).

	test(combinations_3_two_of_three, deterministic(Combinations == [[a,b],[a,c],[b,c]])) :-
		combinations::combinations(2, [a,b,c], Combinations).

	test(combinations_3_zero_of_any, deterministic(Combinations == [[]])) :-
		combinations::combinations(0, [a,b,c], Combinations).

	test(combinations_3_none, deterministic(Combinations == [])) :-
		combinations::combinations(4, [a,b,c], Combinations).

	test(combination_3_all, true(Combinations == [[a,b],[a,c],[b,c]])) :-
		findall(Combination, combinations::combination(2, [a,b,c], Combination), Combinations).

	test(combinations_4_lexicographic, deterministic(Combinations == [[a,c],[b,a],[b,c],[b,d],[d,a],[d,c]])) :-
		combinations::combinations(2, [b,d,a,c], lexicographic, Combinations).

	test(combinations_4_shortlex, deterministic(Combinations == [[a,c],[b,a],[b,c],[b,d],[d,a],[d,c]])) :-
		combinations::combinations(2, [b,d,a,c], shortlex, Combinations).

	test(combination_4_lexicographic_single, true(Combination == [b,d])) :-
		combinations::combination(2, [b,d,a,c], lexicographic, Combination).

	test(distinct_combinations_3_with_duplicates, deterministic(Combinations == [[a,a],[a,b]])) :-
		combinations::distinct_combinations(2, [a,a,b], Combinations).

	test(distinct_combination_3_with_duplicates, true(Combinations == [[a,a],[a,b]])) :-
		findall(Combination, combinations::distinct_combination(2, [a,a,b], Combination), Combinations).

	test(distinct_combinations_4_lexicographic, deterministic(Combinations == [[a,a],[b,a]])) :-
		combinations::distinct_combinations(2, [b,a,a], lexicographic, Combinations).

	test(distinct_combination_4_exists, exists(Combination == [a,b])) :-
		combinations::distinct_combination(2, [a,a,b], default, Combination).

	test(count_combinations_3_two_of_five, deterministic(Count == 10)) :-
		combinations::count_combinations(2, [a,b,c,d,e], Count).

	test(count_distinct_combinations_3_with_duplicates, deterministic(Count == 4)) :-
		combinations::count_distinct_combinations(2, [a,a,b,c], Count).

	test(nth_distinct_combination_4_third, deterministic(Combination == [a,c])) :-
		combinations::nth_distinct_combination(2, [a,a,b,c], 2, Combination).

	test(distinct_combination_index_4_third, deterministic(Index == 2)) :-
		combinations::distinct_combination_index(2, [a,a,b,c], [a,c], Index).

	test(random_combination_3_length, true(list::length(Combination, 2))) :-
		combinations::random_combination(2, [a,b,c,d], Combination).

	test(sample_combinations_4_zero, deterministic(Samples == [])) :-
		combinations::sample_combinations(2, [a,b,c,d], 0, Samples).

	test(sample_combinations_4_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), combinations::combination(2, [a,b,c,d], Sample))))) :-
		combinations::sample_combinations(2, [a,b,c,d], 3, Samples).

	test(random_distinct_combination_3_exists, true(combinations::distinct_combination(2, [a,a,b,c], Combination))) :-
		combinations::random_distinct_combination(2, [a,a,b,c], Combination).

	test(sample_distinct_combinations_4_zero, deterministic(Samples == [])) :-
		combinations::sample_distinct_combinations(2, [a,a,b,c], 0, Samples).

	test(sample_distinct_combinations_4_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), combinations::distinct_combination(2, [a,a,b,c], Sample))))) :-
		combinations::sample_distinct_combinations(2, [a,a,b,c], 3, Samples).

	test(next_combination_3, deterministic(Next == [b,c])) :-
		combinations::next_combination([b,d,a,c], [b,a], Next).

	test(next_combination_3_duplicates, deterministic(Next == [a,b])) :-
		combinations::next_combination([a,a,b,c], [a,a], Next).

	test(next_combination_3_last, fail) :-
		combinations::next_combination([b,d,a,c], [d,c], _).

	test(previous_combination_3, deterministic(Previous == [b,a])) :-
		combinations::previous_combination([b,d,a,c], [b,c], Previous).

	test(previous_combination_3_duplicates, deterministic(Previous == [a,b])) :-
		combinations::previous_combination([a,a,b,c], [a,c], Previous).

	test(previous_combination_3_first, fail) :-
		combinations::previous_combination([b,d,a,c], [a,c], _).

	test(nth_combination_4_second, deterministic(Combination == [a,c])) :-
		combinations::nth_combination(2, [a,b,c], 1, Combination).

	test(nth_combination_5_default_second, deterministic(Combination == [a,c])) :-
		combinations::nth_combination(2, [a,b,c], default, 1, Combination).

	test(nth_combination_5_lexicographic_second, deterministic(Combination == [b,a])) :-
		combinations::nth_combination(2, [b,d,a,c], lexicographic, 1, Combination).

	test(nth_combination_5_shortlex_second, deterministic(Combination == [b,a])) :-
		combinations::nth_combination(2, [b,d,a,c], shortlex, 1, Combination).

	test(combination_index_4_second, deterministic(Index == 1)) :-
		combinations::combination_index(2, [a,b,c], [a,c], Index).

	test(combination_index_5_default_second, deterministic(Index == 1)) :-
		combinations::combination_index(2, [a,b,c], default, [a,c], Index).

	test(combination_index_5_lexicographic_second, deterministic(Index == 1)) :-
		combinations::combination_index(2, [b,d,a,c], lexicographic, [b,a], Index).

	test(combination_index_5_shortlex_second, deterministic(Index == 1)) :-
		combinations::combination_index(2, [b,d,a,c], shortlex, [b,a], Index).

	test(nth_combination_combination_index_roundtrip, deterministic(Index == 2)) :-
		combinations::nth_combination(2, [a,b,c,d], 2, Combination),
		combinations::combination_index(2, [a,b,c,d], Combination, Index).

:- end_object.

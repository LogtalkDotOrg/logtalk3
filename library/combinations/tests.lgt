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
		date is 2026-02-26,
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
		once(combinations::combination(2, [b,d,a,c], lexicographic, Combination)).

	test(combinations_with_replacement_3_two_of_two, deterministic(Combinations == [[a,a],[a,b],[b,b]])) :-
		combinations::combinations_with_replacement(2, [a,b], Combinations).

	test(combinations_with_replacement_4_lexicographic, deterministic(Combinations == [[a,a],[b,a],[b,b]])) :-
		combinations::combinations_with_replacement(2, [b,a], lexicographic, Combinations).

	test(combination_with_replacement_3_all, true(Combinations == [[a,a],[a,b],[b,b]])) :-
		findall(Combination, combinations::combination_with_replacement(2, [a,b], Combination), Combinations).

	test(combination_with_replacement_4_exists, exists(Combination == [a,b])) :-
		combinations::combination_with_replacement(2, [a,b], default, Combination).

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

	test(count_combinations_with_replacement_3_two_of_two, deterministic(Count == 3)) :-
		combinations::count_combinations_with_replacement(2, [a,b], Count).

	test(random_combination_3_length, true(list::length(Combination, 2))) :-
		combinations::random_combination(2, [a,b,c,d], Combination).

	test(nth_combination_4_second, deterministic(Combination == [a,c])) :-
		combinations::nth_combination(2, [a,b,c], 1, Combination).

	test(combination_index_4_second, deterministic(Index == 1)) :-
		combinations::combination_index(2, [a,b,c], [a,c], Index).

	test(nth_combination_combination_index_roundtrip, deterministic(Index == 2)) :-
		combinations::nth_combination(2, [a,b,c,d], 2, Combination),
		combinations::combination_index(2, [a,b,c,d], Combination, Index).

:- end_object.

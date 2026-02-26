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
		comment is 'Unit tests for the "permutations" library.'
	]).

	:- uses(list, [
		length/2
	]).

	cover(permutations).

	test(permutations_2_all_of_three, deterministic(length(Permutations, 6))) :-
		permutations::permutations([a,b,c], Permutations).

	test(permutation_2_all_of_two, true(Permutations == [[a,b],[b,a]])) :-
		findall(Permutation, permutations::permutation([a,b], Permutation), Permutations).

	test(permutations_3_lexicographic, deterministic(Permutations == [[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,a,b],[c,b,a]])) :-
		permutations::permutations([c,b,a], lexicographic, Permutations).

	test(permutations_3_shortlex, deterministic(Permutations == [[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,a,b],[c,b,a]])) :-
		permutations::permutations([c,b,a], shortlex, Permutations).

	test(permutation_3_lexicographic_single, true(Permutation == [c,b,a])) :-
		once(permutations::permutation([c,b,a], lexicographic, Permutation)).

	test(distinct_permutations_2_with_duplicates, deterministic(Permutations == [[a,a,b],[a,b,a],[b,a,a]])) :-
		permutations::distinct_permutations([a,a,b], Permutations).

	test(distinct_permutation_2_with_duplicates, true(Permutations == [[a,a,b],[a,b,a],[b,a,a]])) :-
		findall(Permutation, permutations::distinct_permutation([a,a,b], Permutation), Permutations).

	test(distinct_permutations_3_lexicographic, deterministic(Permutations == [[a,a,b],[a,b,a],[b,a,a]])) :-
		permutations::distinct_permutations([b,a,a], lexicographic, Permutations).

	test(distinct_permutation_3_default_exists, exists(Permutation == [b,a,a])) :-
		permutations::distinct_permutation([a,a,b], default, Permutation).

	test(k_permutations_3_two_of_three, deterministic(length(Permutations, 6))) :-
		permutations::k_permutations(2, [a,b,c], Permutations).

	test(k_permutation_3_one_of_three, true(Permutations == [[a],[b],[c]])) :-
		findall(Permutation, permutations::k_permutation(1, [a,b,c], Permutation), Permutations).

	test(k_permutations_4_lexicographic, deterministic(Permutations == [[a,b],[a,c],[b,a],[b,c],[c,a],[c,b]])) :-
		permutations::k_permutations(2, [c,b,a], lexicographic, Permutations).

	test(k_permutation_4_lexicographic_single, true(Permutation == [c,b])) :-
		once(permutations::k_permutation(2, [c,b,a], lexicographic, Permutation)).

	test(cartesian_product_3_two_of_two, deterministic(Tuples == [[a,a],[a,b],[b,a],[b,b]])) :-
		permutations::cartesian_product(2, [a,b], Tuples).

	test(cartesian_product_3_zero_of_any, deterministic(Tuples == [[]])) :-
		permutations::cartesian_product(0, [a,b,c], Tuples).

	test(cartesian_product_3_one_of_three, deterministic(Tuples == [[a],[b],[c]])) :-
		permutations::cartesian_product(1, [a,b,c], Tuples).

	test(cartesian_product_3_three_of_two, true(length(Tuples, 8))) :-
		permutations::cartesian_product(3, [a,b], Tuples).

	test(derangements_2_of_three, deterministic(Derangements == [[b,c,a],[c,a,b]])) :-
		permutations::derangements([a,b,c], Derangements).

	test(derangement_2_exists, exists(Derangement == [c,a,b])) :-
		permutations::derangement([a,b,c], Derangement).

	test(next_permutation_2, deterministic(Next == [1,3,2])) :-
		permutations::next_permutation([1,2,3], Next).

	test(next_permutation_2_helper_branch, deterministic(Next == [1,3,2,1])) :-
		permutations::next_permutation([1,2,3,1], Next).

	test(previous_permutation_2, deterministic(Previous == [3,1,2])) :-
		permutations::previous_permutation([3,2,1], Previous).

	test(previous_permutation_2_helper_branch, deterministic(Previous == [1,1,3,2])) :-
		permutations::previous_permutation([1,2,1,3], Previous).

	test(nth_permutation_3_third, deterministic(Permutation == [2,1,3])) :-
		permutations::nth_permutation([1,2,3], 2, Permutation).

	test(permutation_index_3_third, deterministic(Index == 2)) :-
		permutations::permutation_index([1,2,3], [2,1,3], Index).

	test(count_permutations_2, deterministic(Count == 24)) :-
		permutations::count_permutations([a,b,c,d], Count).

	test(random_permutation_2_length, true(length(Permutation, 4))) :-
		permutations::random_permutation([a,b,c,d], Permutation).

:- end_object.

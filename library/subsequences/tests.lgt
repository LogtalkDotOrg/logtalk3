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
		date is 2026-01-30,
		comment is 'Unit tests for the "subsequences" library.'
	]).

	:- uses(list, [
		length/2, member/2
	]).

	cover(subsequences).

	% subsequences/2 tests

	test(subsequences_subsequences_2_all, true(length(Subsequences, 8))) :-
		subsequences::subsequences([a,b,c], Subsequences).

	test(subsequences_subsequences_2_empty_list, true(Subsequences == [[]])) :-
		subsequences::subsequences([], Subsequences).

	test(subsequences_subsequences_2_single_element, true(length(Subsequences, 2))) :-
		subsequences::subsequences([a], Subsequences).

	% subsequence/2 tests

	test(subsequences_subsequence_2_all, true(Subsequences == [[1, 2], [1], [2], []])) :-
		findall(Sub, subsequences::subsequence([1,2], Sub), Subsequences).

	% inits/2 tests

	test(subsequences_inits_2_all, true(Inits == [[],[a],[a,b],[a,b,c]])) :-
		subsequences::inits([a,b,c], Inits).

	test(subsequences_inits_2_empty_list, true(Inits == [[]])) :-
		subsequences::inits([], Inits).

	test(subsequences_inits_2_single_element, true(Inits == [[],[a]])) :-
		subsequences::inits([a], Inits).

	% init/2 tests

	test(subsequences_init_2_membership_check, true) :-
		subsequences::init([1,2], [1]).

	test(subsequences_init_2_membership_check_fail, false) :-
		subsequences::init([1,2], [2]).

	% tails/2 tests

	test(subsequences_tails_2_all, true(Tails == [[a,b,c],[b,c],[c],[]])) :-
		subsequences::tails([a,b,c], Tails).

	test(subsequences_tails_2_empty_list, true(Tails == [[]])) :-
		subsequences::tails([], Tails).

	test(subsequences_tails_2_single_element, true(Tails == [[a],[]])) :-
		subsequences::tails([a], Tails).

	% tail/2 tests

	test(subsequences_tail_2_membership_check, true) :-
		subsequences::tail([1,2], [2]).

	test(subsequences_tail_2_membership_check_fail, false) :-
		subsequences::tail([1,2], [1]).

	% inits1/2 tests

	test(subsequences_inits1_2_all, true(Inits == [[a],[a,b],[a,b,c]])) :-
		subsequences::inits1([a,b,c], Inits).

	test(subsequences_inits1_2_empty_list, true(Inits == [])) :-
		subsequences::inits1([], Inits).

	test(subsequences_inits1_2_single_element, true(Inits == [[a]])) :-
		subsequences::inits1([a], Inits).

	% init1/2 tests

	test(subsequences_init1_2_all, true(Inits == [[a],[a,b],[a,b,c]])) :-
		findall(Init, subsequences::init1([a,b,c], Init), Inits).

	% tails1/2 tests

	test(subsequences_tails1_2_all, true(Tails == [[a,b,c],[b,c],[c]])) :-
		subsequences::tails1([a,b,c], Tails).

	test(subsequences_tails1_2_empty_list, true(Tails == [])) :-
		subsequences::tails1([], Tails).

	test(subsequences_tails1_2_single_element, true(Tails == [[a]])) :-
		subsequences::tails1([a], Tails).

	% tail1/2 tests

	test(subsequences_tail1_2_all, true(Tails == [[a,b,c],[b,c],[c]])) :-
		findall(Tail, subsequences::tail1([a,b,c], Tail), Tails).

	% init_tails/2 tests

	test(subsequences_init_tails_2_all, true(Pairs == [[]-[a,b],[a]-[b],[a,b]-[]])) :-
		subsequences::init_tails([a,b], Pairs).

	test(subsequences_init_tails_2_empty_list, true(Pairs == [[]-[]])) :-
		subsequences::init_tails([], Pairs).

	test(subsequences_init_tails_2_single_element, true(Pairs == [[]-[a],[a]-[]])) :-
		subsequences::init_tails([a], Pairs).

	% init_tail/2 tests

	test(subsequences_init_tail_2_backtracking, true(Pairs == [[]-[1,2],[1]-[2],[1,2]-[]])) :-
		findall(Pair, subsequences::init_tail([1,2], Pair), Pairs).

	% combinations/3 tests

	test(subsequences_combinations_3_two_of_three, true(Combinations == [[a,b],[a,c],[b,c]])) :-
		subsequences::combinations(2, [a,b,c], Combinations).

	test(subsequences_combinations_3_zero_of_any, true(Combinations == [[]])) :-
		subsequences::combinations(0, [a,b,c], Combinations).

	test(subsequences_combinations_3_all_elements, true(Combinations == [[a,b,c]])) :-
		subsequences::combinations(3, [a,b,c], Combinations).

	test(subsequences_combinations_3_none, true(Combinations == [])) :-
		subsequences::combinations(4, [a,b,c], Combinations).

	test(subsequences_combinations_3_single_element, true(length(Combinations, 3))) :-
		subsequences::combinations(1, [a,b,c], Combinations).

	test(subsequences_combinations_3_1_of_4, true(length(Combinations, 4))) :-
		subsequences::combinations(1, [a,b,c,d], Combinations).

	% combination/3 tests

	test(subsequences_combination_3_all, true(Combinations == [[a,b],[a,c],[b,c]])) :-
		findall(Comb, subsequences::combination(2, [a,b,c], Comb), Combinations).

	% combinations_with_replacement/3 tests

	test(subsequences_combinations_with_replacement_3_two_of_two, true(Combinations == [[a,a],[a,b],[b,b]])) :-
		subsequences::combinations_with_replacement(2, [a,b], Combinations).

	test(subsequences_combinations_with_replacement_3_zero_of_any, true(Combinations == [[]])) :-
		subsequences::combinations_with_replacement(0, [a,b], Combinations).

	test(subsequences_combinations_with_replacement_3_three_of_two, true(Combinations == [[a,a,a],[a,a,b],[a,b,b],[b,b,b]])) :-
		subsequences::combinations_with_replacement(3, [a,b], Combinations).

	test(subsequences_combinations_with_replacement_3_single_element, true(Combinations == [[a,a],[a,b],[b,b]])) :-
		subsequences::combinations_with_replacement(2, [a,b], Combinations).

	% permutations/2 tests

	test(subsequences_permutations_2_all_of_two, true(sort(Permutations, [[1,2],[2,1]]))) :-
		subsequences::permutations([1,2], Permutations).

	test(subsequences_permutations_2_all_of_three, true(length(Permutations, 6))) :-
		subsequences::permutations([a,b,c], Permutations).

	test(subsequences_permutations_2_empty, true(Permutations == [[]])) :-
		subsequences::permutations([], Permutations).

	test(subsequences_permutations_2_single, true(Permutations == [[a]])) :-
		subsequences::permutations([a], Permutations).

	% permutation/2 tests

	test(subsequences_permutation_2_all, true(Permutations == [[a,b],[b,a]])) :-
		findall(Permutation, subsequences::permutation([a,b], Permutation), Permutations).

	% k_permutations/3 tests

	test(subsequences_k_permutations_3_two_of_three, true(length(Permutations, 6))) :-
		subsequences::k_permutations(2, [a,b,c], Permutations).

	test(subsequences_k_permutations_3_zero_of_any, true(Permutations == [[]])) :-
		subsequences::k_permutations(0, [a,b,c], Permutations).

	test(subsequences_k_permutations_3_all_elements, true(length(Permutations, 6))) :-
		subsequences::k_permutations(3, [a,b,c], Permutations).

	test(subsequences_k_permutations_3_one_of_any, true(Permutations == [[a],[b],[c]])) :-
		subsequences::k_permutations(1, [a,b,c], Permutations).

	% k_permutation/3 tests

	test(subsequences_k_permutation_3_one_of_any, true(Permutations == [[a],[b],[c]])) :-
		findall(Permutation, subsequences::k_permutation(1, [a,b,c], Permutation), Permutations).

	% derangements/2 tests

	test(subsequences_derangements_of_3, true(Derangements == [[b,c,a],[c,a,b]])) :-
		subsequences::derangements([a,b,c], Derangements).

	test(subsequences_derangements_empty, true(Derangements == [[]])) :-
		subsequences::derangements([], Derangements).

	test(subsequences_derangements_single, true(Derangements == [])) :-
		subsequences::derangements([a], Derangements).

	test(subsequences_derangements_of_two, true(Derangements == [[b,a]])) :-
		subsequences::derangements([a,b], Derangements).

	% derangement/2 tests

	test(subsequences_derangement_2_of_3, exists(Derangement == [c,a,b])) :-
		subsequences::derangement([a,b,c], Derangement).

	test(subsequences_derangement_2_fails, false) :-
		subsequences::derangement([a,b,c], [c,b,a]).

	% is_subsequence_of/2 tests

	test(subsequences_is_subsequence_of_2_valid) :-
		subsequences::is_subsequence_of([a,c], [a,b,c]).

	test(subsequences_is_subsequence_of_2_invalid_order, false) :-
		subsequences::is_subsequence_of([c,a], [a,b,c]).

	test(subsequences_is_subsequence_of_2_empty) :-
		subsequences::is_subsequence_of([], [a,b,c]).

	test(subsequences_is_subsequence_of_2_full_list) :-
		subsequences::is_subsequence_of([a,b,c], [a,b,c]).

	test(subsequences_is_subsequence_of_2_invalid_element, false) :-
		subsequences::is_subsequence_of([a,d], [a,b,c]).

	test(subsequences_is_subsequence_of_2_longer_than_list, false) :-
		subsequences::is_subsequence_of([a,b,c,d], [a,b,c]).

	% longest_common_subsequence/3 tests

	test(subsequences_longest_common_subsequence_3_example, true(LCS == [a,c,e])) :-
		subsequences::longest_common_subsequence([a,b,c,d,e], [a,c,e,f], LCS).

	test(subsequences_longest_common_subsequence_3_identical, true(LCS == [a,b,c])) :-
		subsequences::longest_common_subsequence([a,b,c], [a,b,c], LCS).

	test(subsequences_longest_common_subsequence_3_empty, true(LCS == [])) :-
		subsequences::longest_common_subsequence([a,b], [c,d], LCS).

	test(subsequences_longest_common_subsequence_3_empty_list, true(LCS == [])) :-
		subsequences::longest_common_subsequence([], [a,b,c], LCS).

	% longest_increasing_subsequence/2 tests

	test(subsequences_longest_increasing_subsequence_2_example, true(length(LIS, 4))) :-
		subsequences::longest_increasing_subsequence([3,1,4,1,5,9,2,6], LIS).

	test(subsequences_longest_increasing_subsequence_2_sorted, true(length(LIS, 5))) :-
		subsequences::longest_increasing_subsequence([1,2,3,4,5], LIS).

	test(subsequences_longest_increasing_subsequence_2_reverse, true(length(LIS, 1))) :-
		subsequences::longest_increasing_subsequence([5,4,3,2,1], LIS).

	test(subsequences_longest_increasing_subsequence_2_single, true(LIS == [a])) :-
		subsequences::longest_increasing_subsequence([a], LIS).

	% longest_common_increasing_subsequence/3 tests

	test(subsequences_longest_common_increasing_subsequence_3_example, true(length(LCIS, 2))) :-
		subsequences::longest_common_increasing_subsequence([1,4,2,5], [4,1,3,5], LCIS).

	test(subsequences_longest_common_increasing_subsequence_3_identical, true(LCIS == [1,2,3])) :-
		subsequences::longest_common_increasing_subsequence([1,2,3], [1,2,3], LCIS).

	% longest_repeating_subsequence/2 tests

	test(subsequences_longest_repeating_subsequence_2_example, true((length(LRS, L), L >= 0))) :-
		subsequences::longest_repeating_subsequence([a,a,b,a,b], LRS).

	test(subsequences_longest_repeating_subsequence_2_none, true((length(LRS, L), L >= 0))) :-
		subsequences::longest_repeating_subsequence([a,b,c], LRS).

	% common_subsequences/3 tests

	test(subsequences_common_subsequences_3_example, true(length(CSS, 4))) :-
		subsequences::common_subsequences([a,b,c], [a,c,d], CSS).

	test(subsequences_common_subsequences_3_identical, true(length(CSS, 8))) :-
		subsequences::common_subsequences([a,b,c], [a,b,c], CSS).

	test(subsequences_common_subsequences_3_no_common, true(length(CSS, 1))) :-
		subsequences::common_subsequences([a,b], [c,d], CSS),
		CSS == [[]].

	test(subsequences_common_subsequences_3_empty, true(length(CSS, 1))) :-
		subsequences::common_subsequences([], [a,b], CSS).

	% common_subsequence/3 tests

	test(subsequences_common_subsequence_3_succeeds, true(Subsequence == [a])) :-
		subsequences::common_subsequence([a,b,c], [a,e,f], Subsequence).

	% count_subsequences/2 tests

	test(subsequences_count_subsequences_2_of_three, true(Count == 8)) :-
		subsequences::count_subsequences([a,b,c], Count).

	test(subsequences_count_subsequences_2_empty, true(Count == 1)) :-
		subsequences::count_subsequences([], Count).

	test(subsequences_count_subsequences_2_single, true(Count == 2)) :-
		subsequences::count_subsequences([a], Count).

	% count_combinations/3 tests

	test(subsequences_count_combinations_2_of_4, true(Count == 6)) :-
		subsequences::count_combinations(2, [a,b,c,d], Count).

	test(subsequences_count_combinations_0_of_any, true(Count == 1)) :-
		subsequences::count_combinations(0, [a,b,c], Count).

	test(subsequences_count_combinations_all, true(Count == 1)) :-
		subsequences::count_combinations(3, [a,b,c], Count).

	test(subsequences_count_combinations_impossible, true(Count == 0)) :-
		subsequences::count_combinations(5, [a,b,c], Count).

	% count_permutations/2 tests

	test(subsequences_count_permutations_2_of_three, true(Count == 6)) :-
		subsequences::count_permutations([a,b,c], Count).

	test(subsequences_count_permutations_2_empty, true(Count == 1)) :-
		subsequences::count_permutations([], Count).

	test(subsequences_count_permutations_2_of_four, true(Count == 24)) :-
		subsequences::count_permutations([a,b,c,d], Count).

	% subsequence_length/2 tests

	test(subsequences_subsequence_length_2_of_three, true(Length == 3)) :-
		subsequences::subsequence_length([a,b,c], Length).

	test(subsequences_subsequence_length_2_empty, true(Length == 0)) :-
		subsequences::subsequence_length([], Length).

	test(subsequences_subsequence_length_2_single, true(Length == 1)) :-
		subsequences::subsequence_length([x], Length).

	% nth_permutation/3 tests

	test(subsequences_nth_permutation_3_first, true(Permutation == [a,b,c])) :-
		subsequences::nth_permutation([a,b,c], 0, Permutation).

	test(subsequences_nth_permutation_3_second, true(Permutation == [a,c,b])) :-
		subsequences::nth_permutation([a,b,c], 1, Permutation).

	test(subsequences_nth_permutation_3_last, true(Permutation == [c,b,a])) :-
		subsequences::nth_permutation([a,b,c], 5, Permutation).

	test(subsequences_nth_permutation_3_out_of_range, false) :-
		subsequences::nth_permutation([a,b,c], 10, _).

	% permutation_index/3 tests

	test(subsequences_permutation_index_3_first, true(Index == 0)) :-
		subsequences::permutation_index([a,b,c], [a,b,c], Index).

	test(subsequences_permutation_index_3_second, true(Index == 1)) :-
		subsequences::permutation_index([a,b,c], [a,c,b], Index).

	test(subsequences_permutation_index_3_last, true(Index == 5)) :-
		subsequences::permutation_index([a,b,c], [c,b,a], Index).

	test(subsequences_permutation_index_3_inverse_of_nth, true(Permutation == [a,b,c])) :-
		subsequences::nth_permutation([a,b,c], 0, Permutation),
		subsequences::permutation_index([a,b,c], Permutation, 0).

	test(subsequences_permutation_index_3_roundtrip_one, true(Index == 1)) :-
		subsequences::nth_permutation([a,b,c], 1, Permutation),
		subsequences::permutation_index([a,b,c], Permutation, Index).

	test(subsequences_permutation_index_3_roundtrip_last, true(Index == 5)) :-
		subsequences::nth_permutation([a,b,c], 5, Permutation),
		subsequences::permutation_index([a,b,c], Permutation, Index).

	% nth_combination/4 tests

	test(subsequences_nth_combination_4_zero, true(Combination == [a,b])) :-
		subsequences::nth_combination(2, [a,b,c,d], 0, Combination).

	test(subsequences_nth_combination_4_one, true(Combination == [a,c])) :-
		subsequences::nth_combination(2, [a,b,c,d], 1, Combination).

	test(subsequences_nth_combination_4_two, true(Combination == [a,d])) :-
		subsequences::nth_combination(2, [a,b,c,d], 2, Combination).

	test(subsequences_nth_combination_4_three, true(Combination == [b,c])) :-
		subsequences::nth_combination(2, [a,b,c,d], 3, Combination).

	test(subsequences_nth_combination_4_five, true(Combination == [c,d])) :-
		subsequences::nth_combination(2, [a,b,c,d], 5, Combination).

	test(subsequences_nth_combination_4_out_of_range, false) :-
		subsequences::nth_combination(2, [a,b,c], 10, _).

	% combination_index/4 tests

	test(subsequences_combination_4_index_first, true(Index == 0)) :-
		subsequences::combination_index(2, [a,b,c,d], [a,b], Index).

	test(subsequences_combination_4_index_second, true(Index == 1)) :-
		subsequences::combination_index(2, [a,b,c,d], [a,c], Index).

	test(subsequences_combination_4_index_third, true(Index == 2)) :-
		subsequences::combination_index(2, [a,b,c,d], [a,d], Index).

	test(subsequences_combination_4_index_fourth, true(Index == 3)) :-
		subsequences::combination_index(2, [a,b,c,d], [b,c], Index).

	test(subsequences_combination_4_index_last, true(Index == 5)) :-
		subsequences::combination_index(2, [a,b,c,d], [c,d], Index).

	% subsequences_with_min_span/3 tests

	test(subsequences_subsequences_with_min_span_3_all, true(length(Subsequences, 16))) :-
		subsequences::subsequences_with_min_span(2, [a,b,c,d], Subsequences).

	test(subsequences_subsequences_with_min_span_3_span_1, true(length(Subsequences, 8))) :-
		subsequences::subsequences_with_min_span(1, [a,b,c], Subsequences).

	% alternating_subsequences/2 tests

	test(subsequences_alternating_subsequences_2, true(Subsequences == [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]])) :-
		subsequences::alternating_subsequences([1,2,3,4], Subsequences).

	% alternating_subsequence/2 tests

	test(subsequences_alternating_subsequence_2, exists(Subsequence == [2,3])) :-
		subsequences::alternating_subsequence([1,2,3,4], Subsequence).

	% k_distinct_subsequences/3 tests

	test(subsequences_k_distinct_subsequences_3_two_of_three_distinct, true(Subsequences == [[a,b],[a,c],[b,c]])) :-
		subsequences::k_distinct_subsequences(2, [a,b,c], Subsequences).

	test(subsequences_k_distinct_subsequence_3, exists(Subsequence == [a,b])) :-
		subsequences::k_distinct_subsequence(2, [a,b], Subsequence).

	% subsequences/3 (ordering variants) tests

	test(subsequences_subsequences_3_default_order, true(length(Subsequences, 8))) :-
		subsequences::subsequences([a,b,c], default, Subsequences).

	test(subsequences_subsequences_3_lexicographic_order, true(length(Subsequences, 8))) :-
		subsequences::subsequences([a,b,c], lexicographic, Subsequences).

	test(subsequences_subsequences_3_shortlex_order, true((Subsequences = [[]|Rest], Rest = [[_]|_]))) :-
		subsequences::subsequences([a,b,c], shortlex, Subsequences).

	% subsequence/3 (ordering variants) tests

	test(subsequences_subsequence_3_default_order, exists(Subsequence == [b,c])) :-
		subsequences::subsequence([a,b,c], default, Subsequence).

	test(subsequences_subsequence_3_lexicographic_order, exists(Subsequence == [a,c])) :-
		subsequences::subsequence([a,b,c], lexicographic, Subsequence).

	test(subsequences_subsequence_3_shortlex_order, exists(Subsequence == [])) :-
		subsequences::subsequence([a,b,c], shortlex, Subsequence).

	% nonempty_subsequences/2 tests

	test(subsequences_nonempty_subsequences_2_of_three, true(length(Subsequences, 7))) :-
		subsequences::nonempty_subsequences([a,b,c], Subsequences).

	test(subsequences_nonempty_subsequences_2_empty_list, true(Subsequences == [])) :-
		subsequences::nonempty_subsequences([], Subsequences).

	test(subsequences_nonempty_subsequences_2_single, true(Subsequences == [[a]])) :-
		subsequences::nonempty_subsequences([a], Subsequences).

	% power_set/2 tests

	test(subsequences_power_set_of_2, true(length(PS, 4))) :-
		subsequences::power_set([a,b], PS).

	test(subsequences_power_set_empty, true(PS == [[]])) :-
		subsequences::power_set([], PS).

	% combinations/4 (ordering variants) tests

	test(subsequences_combinations_4_default, true(Combinations == [[a,b],[a,c],[b,c]])) :-
		subsequences::combinations(2, [a,b,c], default, Combinations).

	test(subsequences_combinations_4_lexicographic, true(length(Combinations, 3))) :-
		subsequences::combinations(2, [a,b,c], lexicographic, Combinations).

	% combination/4 (ordering variants) tests

	test(subsequences_combination_4_default, true(Combination == [a,b])) :-
		subsequences::combination(2, [a,b,c], default, Combination).

	test(subsequences_combination_4_lexicographic, true(length(Combination, 2))) :-
		subsequences::combination(2, [a,b,c], lexicographic, Combination).

	% permutations/3 (ordering variants) tests

	test(subsequences_permutations_3_default, true(length(Permutations, 6))) :-
		subsequences::permutations([a,b,c], default, Permutations).

	test(subsequences_permutations_3_lexicographic, true(length(Permutations, 6))) :-
		subsequences::permutations([a,b,c], lexicographic, Permutations).

	% permutation/3 (ordering variants) tests

	test(subsequences_permutation_3_default, exists(Permutation == [c,b,a])) :-
		subsequences::permutation([a,b,c], default, Permutation).

	test(subsequences_permutation_3_lexicographic, exists(Permutation == [c,a,b])) :-
		subsequences::permutation([a,b,c], lexicographic, Permutation).

	% k_permutations/4 (ordering variants) tests

	test(subsequences_k_permutations_4_default, true(length(Permutations, 6))) :-
		subsequences::k_permutations(2, [a,b,c], default, Permutations).

	test(subsequences_k_permutations_4_lexicographic, true(length(Permutations, 6))) :-
		subsequences::k_permutations(2, [a,b,c], lexicographic, Permutations).

	% k_permutation/4 (ordering variants) tests

	test(subsequences_k_permutation_4_default, exists(Permutation == [c,b])) :-
		subsequences::k_permutation(2, [a,b,c], default, Permutation).

	test(subsequences_k_permutation_4_lexicographic, exists(Permutation == [c,b])) :-
		subsequences::k_permutation(2, [a,b,c], lexicographic, Permutation).

	% cartesian_product/3 tests

	test(subsequences_cartesian_product_3_two_of_two, true(Tuples == [[a,a],[a,b],[b,a],[b,b]])) :-
		subsequences::cartesian_product(2, [a,b], Tuples).

	test(subsequences_cartesian_product_3_zero_of_any, true(Tuples == [[]])) :-
		subsequences::cartesian_product(0, [a,b,c], Tuples).

	test(subsequences_cartesian_product_3_one_of_three, true(Tuples == [[a],[b],[c]])) :-
		subsequences::cartesian_product(1, [a,b,c], Tuples).

	test(subsequences_cartesian_product_3_three_of_two, true(length(Tuples, 8))) :-
		subsequences::cartesian_product(3, [a,b], Tuples).

	% next_permutation/2 tests

	test(subsequences_next_permutation_2_abc, true(Next == [a,c,b])) :-
		subsequences::next_permutation([a,b,c], Next).

	test(subsequences_next_permutation_2_acb, true(Next == [b,a,c])) :-
		subsequences::next_permutation([a,c,b], Next).

	test(subsequences_next_permutation_2_last, false) :-
		subsequences::next_permutation([c,b,a], _).

	% previous_permutation/2 tests

	test(subsequences_previous_permutation_2_acb, true(Previous == [a,b,c])) :-
		subsequences::previous_permutation([a,c,b], Previous).

	test(subsequences_previous_permutation_2_bac, true(Previous == [a,c,b])) :-
		subsequences::previous_permutation([b,a,c], Previous).

	test(subsequences_previous_permutation_2_first, false) :-
		subsequences::previous_permutation([a,b,c], _).

	% longest_decreasing_subsequence/2 tests

	test(subsequences_longest_decreasing_subsequence_2_example, true(length(LDS, 4))) :-
		subsequences::longest_decreasing_subsequence([9,5,2,8,3,1], LDS).

	test(subsequences_longest_decreasing_subsequence_2_sorted_desc, true(length(LDS, 5))) :-
		subsequences::longest_decreasing_subsequence([5,4,3,2,1], LDS).

	test(subsequences_longest_decreasing_subsequence_2_sorted_asc, true(length(LDS, 1))) :-
		subsequences::longest_decreasing_subsequence([1,2,3,4,5], LDS).

	% count_distinct_subsequences/3 tests

	test(subsequences_count_distinct_subsequences_3_basic, true(Count == 4)) :-
		subsequences::count_distinct_subsequences([a,b], [a,a,b,b], Count).

	test(subsequences_count_distinct_subsequences_3_single, true(Count == 3)) :-
		subsequences::count_distinct_subsequences([a], [a,a,a], Count).

	test(subsequences_count_distinct_subsequences_3_no_match, true(Count == 0)) :-
		subsequences::count_distinct_subsequences([x], [a,b,c], Count).

	test(subsequences_count_distinct_subsequences_3_empty_pattern, true(Count == 1)) :-
		subsequences::count_distinct_subsequences([], [a,b,c], Count).

	% is_prefix_of/2 tests

	test(subsequences_is_prefix_of_2_valid) :-
		subsequences::is_prefix_of([a,b], [a,b,c]).

	test(subsequences_is_prefix_of_2_empty) :-
		subsequences::is_prefix_of([], [a,b,c]).

	test(subsequences_is_prefix_of_2_full) :-
		subsequences::is_prefix_of([a,b,c], [a,b,c]).

	test(subsequences_is_prefix_of_2_invalid, false) :-
		subsequences::is_prefix_of([b,c], [a,b,c]).

	% is_suffix_of/2 tests

	test(subsequences_is_suffix_of_2_valid) :-
		subsequences::is_suffix_of([b,c], [a,b,c]).

	test(subsequences_is_suffix_of_2_empty) :-
		subsequences::is_suffix_of([], [a,b,c]).

	test(subsequences_is_suffix_of_2_full) :-
		subsequences::is_suffix_of([a,b,c], [a,b,c]).

	test(subsequences_is_suffix_of_2_invalid, false) :-
		subsequences::is_suffix_of([a,b], [a,b,c]).

	% subslices/2 tests

	test(subsequences_subslices_2_two_of_three, true(length(Subsequenceslices, 6))) :-
		subsequences::subslices([a,b,c], Subsequenceslices).

	test(subsequences_subslices_2_single, true(Subsequenceslices == [[a]])) :-
		subsequences::subslices([a], Subsequenceslices).

	test(subsequences_subslices_2_two_of_two, true(Subsequenceslices == [[a],[a,b],[b]])) :-
		subsequences::subslices([a,b], Subsequenceslices).

	% sliding_window/3 tests

	test(subsequences_sliding_window_3_two_of_four, true(Windows == [[a,b],[b,c],[c,d]])) :-
		subsequences::sliding_window(2, [a,b,c,d], Windows).

	test(subsequences_sliding_window_3_three_of_four, true(Windows == [[a,b,c],[b,c,d]])) :-
		subsequences::sliding_window(3, [a,b,c,d], Windows).

	test(subsequences_sliding_window_3_full, true(Windows == [[a,b,c]])) :-
		subsequences::sliding_window(3, [a,b,c], Windows).

	test(subsequences_sliding_window_3_one_of_three, true(Windows == [[a],[b],[c]])) :-
		subsequences::sliding_window(1, [a,b,c], Windows).

	% random_combination/3 tests

	test(subsequences_random_combination_3_two_of_three, true(length(Combination, 2))) :-
		subsequences::random_combination(2, [a,b,c], Combination).

	% random_permutation/2 tests

	test(subsequences_random_permutation_2_three, true(length(Permutation, 3))) :-
		subsequences::random_permutation([a,b,c], Permutation).

	% random_subsequence/2 tests

	test(subsequences_random_subsequence_2_three, true(Length =< 3)) :-
		subsequences::random_subsequence([a,b,c], Subsequence),
		length(Subsequence, Length).

:- end_object.

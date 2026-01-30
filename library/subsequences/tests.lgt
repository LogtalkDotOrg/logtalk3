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

	% =========================================================================
	% Tests for subsequences/2
	% =========================================================================

	test(subsequences_all_deterministic, true(length(Subs, 8))) :-
		subsequences::subsequences([a,b,c], Subs).

	test(subsequences_empty_list, true(Subs == [[]])) :-
		subsequences::subsequences([], Subs).

	test(subsequences_single_element, true(length(Subs, 2))) :-
		subsequences::subsequences([a], Subs).

	test(subsequences_backtracking, true(Subs == [[1, 2], [1], [2], []])) :-
		subsequences::subsequences([1,2], Subs).

	% =========================================================================
	% Tests for inits/2
	% =========================================================================

	test(inits_all_deterministic, true(Inits == [[],[a],[a,b],[a,b,c]])) :-
		subsequences::inits([a,b,c], Inits).

	test(inits_empty_list, true(Inits == [[]])) :-
		subsequences::inits([], Inits).

	test(inits_single_element, true(Inits == [[],[a]])) :-
		subsequences::inits([a], Inits).

	test(inits_backtracking) :-
		subsequences::inits([1,2], I),
		(I = [] ; I = [1] ; I = [1,2]).

	% =========================================================================
	% Tests for tails/2
	% =========================================================================

	test(tails_all_deterministic, true(Tails == [[a,b,c],[b,c],[c],[]])) :-
		subsequences::tails([a,b,c], Tails).

	test(tails_empty_list, true(Tails == [[]])) :-
		subsequences::tails([], Tails).

	test(tails_single_element, true(Tails == [[a],[]])) :-
		subsequences::tails([a], Tails).

	test(tails_backtracking) :-
		subsequences::tails([1,2], T),
		(T = [1,2] ; T = [2] ; T = []).

	% =========================================================================
	% Tests for inits1/2
	% =========================================================================

	test(inits1_all_deterministic, true(Inits == [[a],[a,b],[a,b,c]])) :-
		subsequences::inits1([a,b,c], Inits).

	test(inits1_empty_list, true(Inits == [])) :-
		subsequences::inits1([], Inits).

	test(inits1_single_element, true(Inits == [[a]])) :-
		subsequences::inits1([a], Inits).

	% =========================================================================
	% Tests for tails1/2
	% =========================================================================

	test(tails1_all_deterministic, true(Tails == [[a,b,c],[b,c],[c]])) :-
		subsequences::tails1([a,b,c], Tails).

	test(tails1_empty_list, true(Tails == [])) :-
		subsequences::tails1([], Tails).

	test(tails1_single_element, true(Tails == [[a]])) :-
		subsequences::tails1([a], Tails).

	% =========================================================================
	% Tests for init_tails/2
	% =========================================================================

	test(init_tails_all_deterministic, true(Pairs == [([],[a,b]),([a],[b]),([a,b],[])])) :-
		subsequences::init_tails([a,b], Pairs).

	test(init_tails_empty_list, true(Pairs == [([],[])])) :-
		subsequences::init_tails([], Pairs).

	test(init_tails_single_element, true(Pairs == [([],[a]),([a],[])])) :-
		subsequences::init_tails([a], Pairs).

	test(init_tails_backtracking, true(Pairs == [([],[1,2]),([1],[2]),([1,2],[])])) :-
		subsequences::init_tails([1,2], Pairs).

	% =========================================================================
	% Tests for combinations/3
	% =========================================================================

	test(combinations_2_of_3, true(Combs == [[a,b],[a,c],[b,c]])) :-
		subsequences::combinations(2, [a,b,c], Combs).

	test(combinations_0_of_any, true(Combs == [[]])) :-
		subsequences::combinations(0, [a,b,c], Combs).

	test(combinations_all_elements, true(Combs == [[a,b,c]])) :-
		subsequences::combinations(3, [a,b,c], Combs).

	test(combinations_impossible, fail) :-
		subsequences::combinations(4, [a,b,c], Combs),
		Combs \= [].

	test(combinations_single_element, true(length(Combs, 3))) :-
		subsequences::combinations(1, [a,b,c], Combs).

	test(combinations_1_of_4, true(length(Combs, 4))) :-
		subsequences::combinations(1, [a,b,c,d], Combs).

	test(combinations_backtracking, true(Combs == [[a,b],[a,c],[b,c]])) :-
		subsequences::combinations(2, [a,b,c], Combs).

	% =========================================================================
	% Tests for combinations_with_replacement/3
	% =========================================================================

	test(combinations_with_replacement_2_of_2, true(Combs == [[a,a],[a,b],[b,b]])) :-
		subsequences::combinations_with_replacement(2, [a,b], Combs).

	test(combinations_with_replacement_0_of_any, true(Combs == [[]])) :-
		subsequences::combinations_with_replacement(0, [a,b], Combs).

	test(combinations_with_replacement_3_of_2, true(Combs == [[a,a,a],[a,a,b],[a,b,b],[b,b,b]])) :-
		subsequences::combinations_with_replacement(3, [a,b], Combs).

	test(combinations_with_replacement_single_element, true(Combs == [[a,a],[a,b],[b,b]])) :-
		subsequences::combinations_with_replacement(2, [a,b], Combs).

	% =========================================================================
	% Tests for permutations/2
	% =========================================================================

	test(permutations_all_of_2, true(sort(Perms, [[1,2],[2,1]]))) :-
		subsequences::permutations([1,2], Perms).

	test(permutations_all_of_3, true(length(Perms, 6))) :-
		subsequences::permutations([a,b,c], Perms).

	test(permutations_empty, true(Perms == [[]])) :-
		subsequences::permutations([], Perms).

	test(permutations_single, true(Perms == [[a]])) :-
		subsequences::permutations([a], Perms).

	test(permutations_backtracking, true(Perms == [[a,b],[b,a]])) :-
		subsequences::permutations([a,b], Perms).

	% =========================================================================
	% Tests for k_permutations/3
	% =========================================================================

	test(k_permutations_2_of_3, true(length(Perms, 6))) :-
		subsequences::k_permutations(2, [a,b,c], Perms).

	test(k_permutations_0_of_any, true(Perms == [[]])) :-
		subsequences::k_permutations(0, [a,b,c], Perms).

	test(k_permutations_all_elements, true(length(Perms, 6))) :-
		subsequences::k_permutations(3, [a,b,c], Perms).

	test(k_permutations_1_of_any, true(Perms == [[a],[b],[c]])) :-
		subsequences::k_permutations(1, [a,b,c], Perms).

	% =========================================================================
	% Tests for derangements/2
	% =========================================================================

	test(derangements_of_3, true(length(Derangs, 2))) :-
		subsequences::derangements([a,b,c], Derangs).

	test(derangements_empty, true(Derangs == [[]])) :-
		subsequences::derangements([], Derangs).

	test(derangements_single, true(Derangs == [])) :-
		subsequences::derangements([a], Derangs).

	test(derangements_of_2, true(Derangs == [[b,a]])) :-
		subsequences::derangements([a,b], Derangs).

	% =========================================================================
	% Tests for is_subsequence_of/2
	% =========================================================================

	test(is_subsequence_valid) :-
		subsequences::is_subsequence_of([a,c], [a,b,c]).

	test(is_subsequence_invalid_order, fail) :-
		subsequences::is_subsequence_of([c,a], [a,b,c]).

	test(is_subsequence_empty) :-
		subsequences::is_subsequence_of([], [a,b,c]).

	test(is_subsequence_full_list) :-
		subsequences::is_subsequence_of([a,b,c], [a,b,c]).

	test(is_subsequence_invalid_element, fail) :-
		subsequences::is_subsequence_of([a,d], [a,b,c]).

	test(is_subsequence_longer_than_list, fail) :-
		subsequences::is_subsequence_of([a,b,c,d], [a,b,c]).

	% =========================================================================
	% Tests for longest_common_subsequence/3
	% =========================================================================

	test(longest_common_subsequence_example, true(LCS == [a,c,e])) :-
		subsequences::longest_common_subsequence([a,b,c,d,e], [a,c,e,f], LCS).

	test(longest_common_subsequence_identical, true(LCS == [a,b,c])) :-
		subsequences::longest_common_subsequence([a,b,c], [a,b,c], LCS).

	test(longest_common_subsequence_empty, true(LCS == [])) :-
		subsequences::longest_common_subsequence([a,b], [c,d], LCS).

	test(longest_common_subsequence_empty_list, true(LCS == [])) :-
		subsequences::longest_common_subsequence([], [a,b,c], LCS).

	% =========================================================================
	% Tests for longest_increasing_subsequence/2
	% =========================================================================

	test(longest_increasing_subsequence_example, true(length(LIS, 4))) :-
		subsequences::longest_increasing_subsequence([3,1,4,1,5,9,2,6], LIS).

	test(longest_increasing_subsequence_sorted, true(length(LIS, 5))) :-
		subsequences::longest_increasing_subsequence([1,2,3,4,5], LIS).

	test(longest_increasing_subsequence_reverse, true(length(LIS, 1))) :-
		subsequences::longest_increasing_subsequence([5,4,3,2,1], LIS).

	test(longest_increasing_subsequence_single, true(LIS == [a])) :-
		subsequences::longest_increasing_subsequence([a], LIS).

	% =========================================================================
	% Tests for longest_common_increasing_subsequence/3
	% =========================================================================

	test(longest_common_increasing_subsequence_example, true(length(LCIS, 2))) :-
		subsequences::longest_common_increasing_subsequence([1,4,2,5], [4,1,3,5], LCIS).

	test(longest_common_increasing_subsequence_identical, true(LCIS == [1,2,3])) :-
		subsequences::longest_common_increasing_subsequence([1,2,3], [1,2,3], LCIS).

	% =========================================================================
	% Tests for longest_repeating_subsequence/2
	% =========================================================================

	test(longest_repeating_subsequence_example, true((length(LRS, L), L >= 0))) :-
		subsequences::longest_repeating_subsequence([a,a,b,a,b], LRS).

	test(longest_repeating_subsequence_none, true((length(LRS, L), L >= 0))) :-
		subsequences::longest_repeating_subsequence([a,b,c], LRS).

	% =========================================================================
	% Tests for all_common_subsequences/3
	% =========================================================================

	test(all_common_subsequences_example, true(length(CSS, 4))) :-
		subsequences::all_common_subsequences([a,b,c], [a,c,d], CSS).

	test(all_common_subsequences_identical, true(length(CSS, 8))) :-
		subsequences::all_common_subsequences([a,b,c], [a,b,c], CSS).

	test(all_common_subsequences_no_common, true(length(CSS, 1))) :-
		subsequences::all_common_subsequences([a,b], [c,d], CSS),
		CSS == [[]].

	test(all_common_subsequences_empty, true(length(CSS, 1))) :-
		subsequences::all_common_subsequences([], [a,b], CSS).

	% =========================================================================
	% Tests for count_subsequences/2
	% =========================================================================

	test(count_subsequences_of_3, true(Count == 8)) :-
		subsequences::count_subsequences([a,b,c], Count).

	test(count_subsequences_empty, true(Count == 1)) :-
		subsequences::count_subsequences([], Count).

	test(count_subsequences_single, true(Count == 2)) :-
		subsequences::count_subsequences([a], Count).

	% =========================================================================
	% Tests for count_combinations/3
	% =========================================================================

	test(count_combinations_2_of_4, true(Count == 6)) :-
		subsequences::count_combinations(2, [a,b,c,d], Count).

	test(count_combinations_0_of_any, true(Count == 1)) :-
		subsequences::count_combinations(0, [a,b,c], Count).

	test(count_combinations_all, true(Count == 1)) :-
		subsequences::count_combinations(3, [a,b,c], Count).

	test(count_combinations_impossible, true(Count == 0)) :-
		subsequences::count_combinations(5, [a,b,c], Count).

	% =========================================================================
	% Tests for count_permutations/2
	% =========================================================================

	test(count_permutations_of_3, true(Count == 6)) :-
		subsequences::count_permutations([a,b,c], Count).

	test(count_permutations_empty, true(Count == 1)) :-
		subsequences::count_permutations([], Count).

	test(count_permutations_of_4, true(Count == 24)) :-
		subsequences::count_permutations([a,b,c,d], Count).

	% =========================================================================
	% Tests for subsequence_length/2
	% =========================================================================

	test(subsequence_length_of_3, true(Length == 3)) :-
		subsequences::subsequence_length([a,b,c], Length).

	test(subsequence_length_empty, true(Length == 0)) :-
		subsequences::subsequence_length([], Length).

	test(subsequence_length_single, true(Length == 1)) :-
		subsequences::subsequence_length([x], Length).

	% =========================================================================
	% Tests for nth_combination/4
	% =========================================================================

	test(nth_combination_first, true(length(Comb, 2))) :-
		subsequences::nth_combination(2, [a,b,c,d], 0, Comb).

	test(nth_combination_second, true(length(Comb, 2))) :-
		subsequences::nth_combination(2, [a,b,c,d], 1, Comb).

	test(nth_combination_out_of_range, fail) :-
		subsequences::nth_combination(2, [a,b,c], 10, _).

	% =========================================================================
	% Tests for nth_permutation/3
	% =========================================================================

	test(nth_permutation_first, true(Perm == [a,b,c])) :-
		subsequences::nth_permutation([a,b,c], 0, Perm).

	test(nth_permutation_second, true(Perm == [a,c,b])) :-
		subsequences::nth_permutation([a,b,c], 1, Perm).

	test(nth_permutation_last, true(Perm == [c,b,a])) :-
		subsequences::nth_permutation([a,b,c], 5, Perm).

	test(nth_permutation_out_of_range, false) :-
		subsequences::nth_permutation([a,b,c], 10, _).

	% =========================================================================
	% Tests for subsequences_with_min_span/3
	% =========================================================================

	test(subsequences_with_min_span_all_deterministic) :-
		subsequences::subsequences_with_min_span(2, [a,b,c,d], Subs),
		length(Subs, 1).

	test(subsequences_with_min_span_span_1, true(length(Subs, 8))) :-
		subsequences::subsequences_with_min_span(1, [a,b,c], Subs).

	% =========================================================================
	% Tests for k_distinct_subsequences/3
	% =========================================================================

	test(k_distinct_subsequences_2_of_3_distinct, true(Subs == [[a,b],[a,c],[b,c]])) :-
		subsequences::k_distinct_subsequences(2, [a,b,c], Subs).

	test(k_distinct_subsequences_backtracking) :-
		subsequences::k_distinct_subsequences(2, [a,b], S),
		S = [a,b].

	% =========================================================================
	% Tests for subsequences/3 (ordering variants)
	% =========================================================================

	test(subsequences_default_order, true(length(Subs, 8))) :-
		subsequences::subsequences([a,b,c], default, Subs).

	test(subsequences_lexicographic_order, true(length(Subs, 8))) :-
		subsequences::subsequences([a,b,c], lexicographic, Subs).

	test(subsequences_shortlex_order, true((Subs = [[]|Rest], Rest = [[_]|_]))) :-
		subsequences::subsequences([a,b,c], shortlex, Subs).

	% =========================================================================
	% Tests for nonempty_subsequences/2
	% =========================================================================

	test(nonempty_subsequences_of_3, true(length(Subs, 7))) :-
		subsequences::nonempty_subsequences([a,b,c], Subs).

	test(nonempty_subsequences_empty_list, true(Subs == [])) :-
		subsequences::nonempty_subsequences([], Subs).

	test(nonempty_subsequences_single, true(Subs == [[a]])) :-
		subsequences::nonempty_subsequences([a], Subs).

	% =========================================================================
	% Tests for power_set/2
	% =========================================================================

	test(power_set_of_2, true(length(PS, 4))) :-
		subsequences::power_set([a,b], PS).

	test(power_set_empty, true(PS == [[]])) :-
		subsequences::power_set([], PS).

	% =========================================================================
	% Tests for combinations/4 (ordering variants)
	% =========================================================================

	test(combinations_4_default, true(Combs == [[a,b],[a,c],[b,c]])) :-
		subsequences::combinations(2, [a,b,c], default, Combs).

	test(combinations_4_lexicographic, true(length(Combs, 3))) :-
		subsequences::combinations(2, [a,b,c], lexicographic, Combs).

	% =========================================================================
	% Tests for permutations/3 (ordering variants)
	% =========================================================================

	test(permutations_3_default, true(length(Perms, 6))) :-
		subsequences::permutations([a,b,c], default, Perms).

	test(permutations_3_lexicographic, true(length(Perms, 6))) :-
		subsequences::permutations([a,b,c], lexicographic, Perms).

	% =========================================================================
	% Tests for k_permutations/4 (ordering variants)
	% =========================================================================

	test(k_permutations_4_default, true(length(Perms, 6))) :-
		subsequences::k_permutations(2, [a,b,c], default, Perms).

	test(k_permutations_4_lexicographic, true(length(Perms, 6))) :-
		subsequences::k_permutations(2, [a,b,c], lexicographic, Perms).

	% =========================================================================
	% Tests for cartesian_product/3
	% =========================================================================

	test(cartesian_product_2_of_2, true(Tuples == [[a,a],[a,b],[b,a],[b,b]])) :-
		subsequences::cartesian_product(2, [a,b], Tuples).

	test(cartesian_product_0_of_any, true(Tuples == [[]])) :-
		subsequences::cartesian_product(0, [a,b,c], Tuples).

	test(cartesian_product_1_of_3, true(Tuples == [[a],[b],[c]])) :-
		subsequences::cartesian_product(1, [a,b,c], Tuples).

	test(cartesian_product_3_of_2, true(length(Tuples, 8))) :-
		subsequences::cartesian_product(3, [a,b], Tuples).

	% =========================================================================
	% Tests for next_permutation/2
	% =========================================================================

	test(next_permutation_abc, true(Next == [a,c,b])) :-
		subsequences::next_permutation([a,b,c], Next).

	test(next_permutation_acb, true(Next == [b,a,c])) :-
		subsequences::next_permutation([a,c,b], Next).

	test(next_permutation_last, fail) :-
		subsequences::next_permutation([c,b,a], _).

	% =========================================================================
	% Tests for prev_permutation/2
	% =========================================================================

	test(prev_permutation_acb, true(Prev == [a,b,c])) :-
		subsequences::prev_permutation([a,c,b], Prev).

	test(prev_permutation_bac, true(Prev == [a,c,b])) :-
		subsequences::prev_permutation([b,a,c], Prev).

	test(prev_permutation_first, fail) :-
		subsequences::prev_permutation([a,b,c], _).

	% =========================================================================
	% Tests for longest_decreasing_subsequence/2
	% =========================================================================

	test(longest_decreasing_subsequence_example, true(length(LDS, 4))) :-
		subsequences::longest_decreasing_subsequence([9,5,2,8,3,1], LDS).

	test(longest_decreasing_subsequence_sorted_desc, true(length(LDS, 5))) :-
		subsequences::longest_decreasing_subsequence([5,4,3,2,1], LDS).

	test(longest_decreasing_subsequence_sorted_asc, true(length(LDS, 1))) :-
		subsequences::longest_decreasing_subsequence([1,2,3,4,5], LDS).

	% =========================================================================
	% Tests for count_distinct_subsequences/3
	% =========================================================================

	test(count_distinct_subsequences_basic, true(Count == 4)) :-
		subsequences::count_distinct_subsequences([a,b], [a,a,b,b], Count).

	test(count_distinct_subsequences_single, true(Count == 3)) :-
		subsequences::count_distinct_subsequences([a], [a,a,a], Count).

	test(count_distinct_subsequences_no_match, true(Count == 0)) :-
		subsequences::count_distinct_subsequences([x], [a,b,c], Count).

	test(count_distinct_subsequences_empty_pattern, true(Count == 1)) :-
		subsequences::count_distinct_subsequences([], [a,b,c], Count).

	% =========================================================================
	% Tests for is_prefix_of/2
	% =========================================================================

	test(is_prefix_of_valid) :-
		subsequences::is_prefix_of([a,b], [a,b,c]).

	test(is_prefix_of_empty) :-
		subsequences::is_prefix_of([], [a,b,c]).

	test(is_prefix_of_full) :-
		subsequences::is_prefix_of([a,b,c], [a,b,c]).

	test(is_prefix_of_invalid, fail) :-
		subsequences::is_prefix_of([b,c], [a,b,c]).

	% =========================================================================
	% Tests for is_suffix_of/2
	% =========================================================================

	test(is_suffix_of_valid) :-
		subsequences::is_suffix_of([b,c], [a,b,c]).

	test(is_suffix_of_empty) :-
		subsequences::is_suffix_of([], [a,b,c]).

	test(is_suffix_of_full) :-
		subsequences::is_suffix_of([a,b,c], [a,b,c]).

	test(is_suffix_of_invalid, fail) :-
		subsequences::is_suffix_of([a,b], [a,b,c]).

	% =========================================================================
	% Tests for subslices/2
	% =========================================================================

	test(subslices_of_3, true(length(Subs, 6))) :-
		subsequences::subslices([a,b,c], Subs).

	test(subslices_single, true(Subs == [[a]])) :-
		subsequences::subslices([a], Subs).

	test(subslices_of_2, true(Subs == [[a],[a,b],[b]])) :-
		subsequences::subslices([a,b], Subs).

	% =========================================================================
	% Tests for sliding_window/3
	% =========================================================================

	test(sliding_window_2_of_4, true(Windows == [[a,b],[b,c],[c,d]])) :-
		subsequences::sliding_window(2, [a,b,c,d], Windows).

	test(sliding_window_3_of_4, true(Windows == [[a,b,c],[b,c,d]])) :-
		subsequences::sliding_window(3, [a,b,c,d], Windows).

	test(sliding_window_full, true(Windows == [[a,b,c]])) :-
		subsequences::sliding_window(3, [a,b,c], Windows).

	test(sliding_window_1_of_3, true(Windows == [[a],[b],[c]])) :-
		subsequences::sliding_window(1, [a,b,c], Windows).

:- end_object.

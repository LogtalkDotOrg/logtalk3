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
		comment is 'Unit tests for the "subsequences" library.'
	]).

	:- uses(list, [
		length/2
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

	test(subsequences_inits_2_all, deterministic(Inits == [[],[a],[a,b],[a,b,c]])) :-
		subsequences::inits([a,b,c], Inits).

	test(subsequences_inits_2_empty_list, deterministic(Inits == [[]])) :-
		subsequences::inits([], Inits).

	test(subsequences_inits_2_single_element, deterministic(Inits == [[],[a]])) :-
		subsequences::inits([a], Inits).

	% init/2 tests

	test(subsequences_init_2_membership_check, true) :-
		subsequences::init([1,2], [1]).

	test(subsequences_init_2_membership_check_fail, false) :-
		subsequences::init([1,2], [2]).

	% tails/2 tests

	test(subsequences_tails_2_all, deterministic(Tails == [[a,b,c],[b,c],[c],[]])) :-
		subsequences::tails([a,b,c], Tails).

	test(subsequences_tails_2_empty_list, deterministic(Tails == [[]])) :-
		subsequences::tails([], Tails).

	test(subsequences_tails_2_single_element, deterministic(Tails == [[a],[]])) :-
		subsequences::tails([a], Tails).

	% tail/2 tests

	test(subsequences_tail_2_membership_check, true) :-
		subsequences::tail([1,2], [2]).

	test(subsequences_tail_2_membership_check_fail, false) :-
		subsequences::tail([1,2], [1]).

	% inits1/2 tests

	test(subsequences_inits1_2_all, deterministic(Inits == [[a],[a,b],[a,b,c]])) :-
		subsequences::inits1([a,b,c], Inits).

	test(subsequences_inits1_2_empty_list, deterministic(Inits == [])) :-
		subsequences::inits1([], Inits).

	test(subsequences_inits1_2_single_element, deterministic(Inits == [[a]])) :-
		subsequences::inits1([a], Inits).

	% init1/2 tests

	test(subsequences_init1_2_all, true(Inits == [[a],[a,b],[a,b,c]])) :-
		findall(Init, subsequences::init1([a,b,c], Init), Inits).

	% tails1/2 tests

	test(subsequences_tails1_2_all, deterministic(Tails == [[a,b,c],[b,c],[c]])) :-
		subsequences::tails1([a,b,c], Tails).

	test(subsequences_tails1_2_empty_list, deterministic(Tails == [])) :-
		subsequences::tails1([], Tails).

	test(subsequences_tails1_2_single_element, deterministic(Tails == [[a]])) :-
		subsequences::tails1([a], Tails).

	% tail1/2 tests

	test(subsequences_tail1_2_all, true(Tails == [[a,b,c],[b,c],[c]])) :-
		findall(Tail, subsequences::tail1([a,b,c], Tail), Tails).

	% init_tails/2 tests

	test(subsequences_init_tails_2_all, deterministic(Pairs == [[]-[a,b],[a]-[b],[a,b]-[]])) :-
		subsequences::init_tails([a,b], Pairs).

	test(subsequences_init_tails_2_empty_list, deterministic(Pairs == [[]-[]])) :-
		subsequences::init_tails([], Pairs).

	test(subsequences_init_tails_2_single_element, deterministic(Pairs == [[]-[a],[a]-[]])) :-
		subsequences::init_tails([a], Pairs).

	% init_tail/2 tests

	test(subsequences_init_tail_2_backtracking, true(Pairs == [[]-[1,2],[1]-[2],[1,2]-[]])) :-
		findall(Pair, subsequences::init_tail([1,2], Pair), Pairs).

	% is_subsequence_of/2 tests

	test(subsequences_is_subsequence_of_2_valid, deterministic) :-
		subsequences::is_subsequence_of([a,c], [a,b,c]).

	test(subsequences_is_subsequence_of_2_invalid_order, false) :-
		subsequences::is_subsequence_of([c,a], [a,b,c]).

	test(subsequences_is_subsequence_of_2_empty, deterministic) :-
		subsequences::is_subsequence_of([], [a,b,c]).

	test(subsequences_is_subsequence_of_2_full_list, deterministic) :-
		subsequences::is_subsequence_of([a,b,c], [a,b,c]).

	test(subsequences_is_subsequence_of_2_invalid_element, false) :-
		subsequences::is_subsequence_of([a,d], [a,b,c]).

	test(subsequences_is_subsequence_of_2_longer_than_list, false) :-
		subsequences::is_subsequence_of([a,b,c,d], [a,b,c]).

	% proper_subsequence/2 tests

	test(subsequences_proper_subsequence_2_valid, deterministic) :-
		subsequences::proper_subsequence([a,c], [a,b,c]).

	test(subsequences_proper_subsequence_2_equal_fails, false) :-
		subsequences::proper_subsequence([a,b,c], [a,b,c]).

	test(subsequences_proper_subsequence_2_invalid, false) :-
		subsequences::proper_subsequence([c,a], [a,b,c]).

	% subsequence_at_indices/3 tests

	test(subsequences_subsequence_at_indices_3_basic, deterministic(Subsequence == [a,c])) :-
		subsequences::subsequence_at_indices([a,b,c,d], [1,3], Subsequence).

	test(subsequences_subsequence_at_indices_3_empty_indices, deterministic(Subsequence == [])) :-
		subsequences::subsequence_at_indices([a,b,c], [], Subsequence).

	test(subsequences_subsequence_at_indices_3_nonincreasing_fails, false) :-
		subsequences::subsequence_at_indices([a,b,c], [2,2], _).

	% longest_common_subsequence/3 tests

	test(subsequences_longest_common_subsequence_3_example, deterministic(LCS == [a,c,e])) :-
		subsequences::longest_common_subsequence([a,b,c,d,e], [a,c,e,f], LCS).

	test(subsequences_longest_common_subsequence_3_identical, deterministic(LCS == [a,b,c])) :-
		subsequences::longest_common_subsequence([a,b,c], [a,b,c], LCS).

	test(subsequences_longest_common_subsequence_3_empty, deterministic(LCS == [])) :-
		subsequences::longest_common_subsequence([a,b], [c,d], LCS).

	test(subsequences_longest_common_subsequence_3_empty_list, deterministic(LCS == [])) :-
		subsequences::longest_common_subsequence([], [a,b,c], LCS).

	% longest_increasing_subsequence/2 tests

	test(subsequences_longest_increasing_subsequence_2_example, deterministic(length(LIS, 4))) :-
		subsequences::longest_increasing_subsequence([3,1,4,1,5,9,2,6], LIS).

	test(subsequences_longest_increasing_subsequence_2_sorted, deterministic(length(LIS, 5))) :-
		subsequences::longest_increasing_subsequence([1,2,3,4,5], LIS).

	test(subsequences_longest_increasing_subsequence_2_reverse, deterministic(length(LIS, 1))) :-
		subsequences::longest_increasing_subsequence([5,4,3,2,1], LIS).

	test(subsequences_longest_increasing_subsequence_2_single, deterministic(LIS == [a])) :-
		subsequences::longest_increasing_subsequence([a], LIS).

	% longest_common_increasing_subsequence/3 tests

	test(subsequences_longest_common_increasing_subsequence_3_example, deterministic(length(LCIS, 2))) :-
		subsequences::longest_common_increasing_subsequence([1,4,2,5], [4,1,3,5], LCIS).

	test(subsequences_longest_common_increasing_subsequence_3_identical, deterministic(LCIS == [1,2,3])) :-
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

	test(subsequences_common_subsequences_3_no_common, true(CSS == [[]])) :-
		subsequences::common_subsequences([a,b], [c,d], CSS).

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

	% subsequence_length/2 tests

	test(subsequences_subsequence_length_2_of_three, deterministic(Length == 3)) :-
		subsequences::subsequence_length([a,b,c], Length).

	test(subsequences_subsequence_length_2_empty, deterministic(Length == 0)) :-
		subsequences::subsequence_length([], Length).

	test(subsequences_subsequence_length_2_single, deterministic(Length == 1)) :-
		subsequences::subsequence_length([x], Length).

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

	test(subsequences_k_distinct_subsequences_3_two_of_three_distinct, deterministic(Subsequences == [[a,b],[a,c],[b,c]])) :-
		subsequences::k_distinct_subsequences(2, [a,b,c], Subsequences).

	test(subsequences_k_distinct_subsequence_3, exists(Subsequence == [a,b])) :-
		subsequences::k_distinct_subsequence(2, [a,b], Subsequence).

	% subsequences/3 (ordering variants) tests

	test(subsequences_subsequences_3_default_order, true(length(Subsequences, 8))) :-
		subsequences::subsequences([a,b,c], default, Subsequences).

	test(subsequences_subsequences_3_lexicographic_order, deterministic(Subsequences == [[],[a],[a,b],[b],[d],[d,a],[d,a,b],[d,b]])) :-
		subsequences::subsequences([d,a,b], lexicographic, Subsequences).

	test(subsequences_subsequences_3_shortlex_order, deterministic(Subsequences == [[],[a],[b],[d],[a,b],[d,a],[d,b],[d,a,b]])) :-
		subsequences::subsequences([d,a,b], shortlex, Subsequences).

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

	test(subsequences_nonempty_subsequences_2_empty_list, deterministic(Subsequences == [])) :-
		subsequences::nonempty_subsequences([], Subsequences).

	test(subsequences_nonempty_subsequences_2_single, deterministic(Subsequences == [[a]])) :-
		subsequences::nonempty_subsequences([a], Subsequences).

	% power_set/2 tests

	test(subsequences_power_set_of_2, true(length(PS, 4))) :-
		subsequences::power_set([a,b], PS).

	test(subsequences_power_set_empty, true(PS == [[]])) :-
		subsequences::power_set([], PS).

	% longest_decreasing_subsequence/2 tests

	test(subsequences_longest_decreasing_subsequence_2_example, true(length(LDS, 4))) :-
		subsequences::longest_decreasing_subsequence([9,5,2,8,3,1], LDS).

	test(subsequences_longest_decreasing_subsequence_2_sorted_desc, true(length(LDS, 5))) :-
		subsequences::longest_decreasing_subsequence([5,4,3,2,1], LDS).

	test(subsequences_longest_decreasing_subsequence_2_sorted_asc, true(length(LDS, 1))) :-
		subsequences::longest_decreasing_subsequence([1,2,3,4,5], LDS).

	% count_distinct_subsequences/3 tests

	test(subsequences_count_distinct_subsequences_3_basic, deterministic(Count == 4)) :-
		subsequences::count_distinct_subsequences([a,b], [a,a,b,b], Count).

	test(subsequences_count_distinct_subsequences_3_single, deterministic(Count == 3)) :-
		subsequences::count_distinct_subsequences([a], [a,a,a], Count).

	test(subsequences_count_distinct_subsequences_3_no_match, deterministic(Count == 0)) :-
		subsequences::count_distinct_subsequences([x], [a,b,c], Count).

	test(subsequences_count_distinct_subsequences_3_empty_pattern, deterministic(Count == 1)) :-
		subsequences::count_distinct_subsequences([], [a,b,c], Count).

	% is_prefix_of/2 tests

	test(subsequences_is_prefix_of_2_valid, deterministic) :-
		subsequences::is_prefix_of([a,b], [a,b,c]).

	test(subsequences_is_prefix_of_2_empty, deterministic) :-
		subsequences::is_prefix_of([], [a,b,c]).

	test(subsequences_is_prefix_of_2_full, deterministic) :-
		subsequences::is_prefix_of([a,b,c], [a,b,c]).

	test(subsequences_is_prefix_of_2_invalid, false) :-
		subsequences::is_prefix_of([b,c], [a,b,c]).

	% is_suffix_of/2 tests

	test(subsequences_is_suffix_of_2_valid, deterministic) :-
		subsequences::is_suffix_of([b,c], [a,b,c]).

	test(subsequences_is_suffix_of_2_empty, deterministic) :-
		subsequences::is_suffix_of([], [a,b,c]).

	test(subsequences_is_suffix_of_2_full, deterministic) :-
		subsequences::is_suffix_of([a,b,c], [a,b,c]).

	test(subsequences_is_suffix_of_2_invalid, false) :-
		subsequences::is_suffix_of([a,b], [a,b,c]).

	% subslices/2 tests

	test(subsequences_subslices_2_two_of_three, true(length(Subsequenceslices, 6))) :-
		subsequences::subslices([a,b,c], Subsequenceslices).

	test(subsequences_subslices_2_single, deterministic(Subsequenceslices == [[a]])) :-
		subsequences::subslices([a], Subsequenceslices).

	test(subsequences_subslices_2_two_of_two, deterministic(Subsequenceslices == [[a],[a,b],[b]])) :-
		subsequences::subslices([a,b], Subsequenceslices).

	% sliding_window/3 tests

	test(subsequences_sliding_window_3_two_of_four, deterministic(Windows == [[a,b],[b,c],[c,d]])) :-
		subsequences::sliding_window(2, [a,b,c,d], Windows).

	test(subsequences_sliding_window_3_three_of_four, deterministic(Windows == [[a,b,c],[b,c,d]])) :-
		subsequences::sliding_window(3, [a,b,c,d], Windows).

	test(subsequences_sliding_window_3_full, deterministic(Windows == [[a,b,c]])) :-
		subsequences::sliding_window(3, [a,b,c], Windows).

	test(subsequences_sliding_window_3_one_of_three, deterministic(Windows == [[a],[b],[c]])) :-
		subsequences::sliding_window(1, [a,b,c], Windows).

	% random_subsequence/2 tests

	test(subsequences_random_subsequence_2_three, true(Length =< 3)) :-
		subsequences::random_subsequence([a,b,c], Subsequence),
		length(Subsequence, Length).

:- end_object.

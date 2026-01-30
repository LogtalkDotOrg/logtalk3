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


:- protocol(subsequences_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-30,
		comment is 'Protocol for subsequence operations over lists.',
		remarks is [
			'Generation operations' - 'Predicates for generating all subsequences or variants thereof.',
			'Ordering variants' - 'Predicates that support an additional Order argument (default, lexicographic, or shortlex) for controlling output order.',
			'Filtered generation' - 'Predicates for generating specific types of subsequences (combinations, permutations).',
			'Indexed access' - 'Predicates for direct access to subsequences at specific positions.',
			'Searching and matching' - 'Predicates for finding specific subsequences with desired properties.',
			'Prefix and suffix operations' - 'Predicates for checking and finding prefixes and suffixes.',
			'Contiguous subsequences' - 'Predicates for working with contiguous subsequences (subslices, sliding windows).',
			'Random selection' - 'Predicates for randomly selecting subsequences.',
			'Constrained operations' - 'Predicates for generating subsequences with specific constraints.',
			'Utility predicates' - 'Helper predicates for subsequence operations.'
		]
	]).

	% =========================================================================
	% Generation operations - Creating all subsequences
	% =========================================================================

	:- public(subsequences/2).
	:- mode(subsequences(+list, -list), one).
	:- mode(subsequences(+list, ?list), zero_or_more).
	:- info(subsequences/2, [
		comment is 'Generates all subsequences of a list using default order. A subsequence maintains the relative order of elements but need not be contiguous. The empty list is included.',
		argnames is ['List', 'Subsequence'],
		examples is [
			'Simple subsequences' - subsequences([a,b,c], Subs) - {Subs = [[],[a],[b],[a,b],[c],[a,c],[b,c],[a,b,c]]},
			'Backtracking through subsequences' - subsequences([1,2], Sub) - {Sub = [], Sub = [1], Sub = [2], Sub = [1,2]}
		]
	]).

	:- public(subsequences/3).
	:- mode(subsequences(+list, +atom, -list), one).
	:- mode(subsequences(+list, +atom, ?list), zero_or_more).
	:- info(subsequences/3, [
		comment is 'Generates all subsequences of a list with specified ordering: ``default`` (as naturally produced), ``lexicographic``, or ``shortlex`` (by length first, then lexicographically).',
		argnames is ['List', 'Order', 'Subsequence'],
		examples is [
			'Shortlex order' - subsequences([a,b], shortlex, Subs) - {Subs = [[],[a],[b],[a,b]]}
		]
	]).

	:- public(nonempty_subsequences/2).
	:- mode(nonempty_subsequences(+list, -list), one).
	:- mode(nonempty_subsequences(+list, ?list), zero_or_more).
	:- info(nonempty_subsequences/2, [
		comment is 'Generates all non-empty subsequences of a list.',
		argnames is ['List', 'Subsequence'],
		examples is [
			'Non-empty subsequences' - nonempty_subsequences([a,b], Subs) - {Subs = [[a],[b],[a,b]]}
		]
	]).

	:- public(power_set/2).
	:- mode(power_set(+list, -list), one).
	:- info(power_set/2, [
		comment is 'Generates the power set of a list (all possible subsequences). Alias for subsequences/2 when first argument is ground.',
		argnames is ['List', 'PowerSet'],
		examples is [
			'Power set' - power_set([a,b], PS) - {PS = [[],[a],[b],[a,b]]}
		]
	]).

	:- public(inits/2).
	:- mode(inits(+list, -list), one).
	:- mode(inits(+list, ?list), zero_or_more).
	:- info(inits/2, [
		comment is 'Generates all initial segments (prefixes) of a list, shortest first. Includes the empty list.',
		argnames is ['List', 'Init'],
		examples is [
			'All prefixes' - inits([a,b,c], Inits) - {Inits = [[],[a],[a,b],[a,b,c]]},
			'Backtracking' - inits([1,2], I) - {I = [], I = [1], I = [1,2]}
		]
	]).

	:- public(tails/2).
	:- mode(tails(+list, -list), one).
	:- mode(tails(+list, ?list), zero_or_more).
	:- info(tails/2, [
		comment is 'Generates all final segments (suffixes) of a list, longest first. Includes the empty list.',
		argnames is ['List', 'Tail'],
		examples is [
			'All suffixes' - tails([a,b,c], Tails) - {Tails = [[a,b,c],[b,c],[c],[]]},
			'Backtracking' - tails([1,2], T) - {T = [1,2], T = [2], T = []}
		]
	]).

	:- public(inits1/2).
	:- mode(inits1(+list, -list), one).
	:- mode(inits1(+list, ?list), zero_or_more).
	:- info(inits1/2, [
		comment is 'Generates all non-empty initial segments (prefixes) of a list, shortest first. Excludes the empty list.',
		argnames is ['List', 'Init'],
		examples is [
			'Non-empty prefixes' - inits1([a,b,c], Inits) - {Inits = [[a],[a,b],[a,b,c]]}
		]
	]).

	:- public(tails1/2).
	:- mode(tails1(+list, -list), one).
	:- mode(tails1(+list, ?list), zero_or_more).
	:- info(tails1/2, [
		comment is 'Generates all non-empty final segments (suffixes) of a list, longest first. Excludes the empty list.',
		argnames is ['List', 'Tail'],
		examples is [
			'Non-empty suffixes' - tails1([a,b,c], Tails) - {Tails = [[a,b,c],[b,c],[c]]}
		]
	]).

	:- public(init_tails/2).
	:- mode(init_tails(+list, -list), one).
	:- mode(init_tails(+list, ?pair), zero_or_more).
	:- info(init_tails/2, [
		comment is 'Generates all pairs of initial and final segments. Each pair (Init,Tail) represents a split of the list where Init+Tail equals the original list.',
		argnames is ['List', 'InitTailPairs'],
		examples is [
			'All splits' - init_tails([a,b], Pairs) - {Pairs = [([],[a,b]),([a],[b]),([a,b],[])]},
			'Backtracking through splits' - init_tails([1,2], (I,T)) - {I = [], T = [1,2]} %, ([1],[2]), ([1,2],[])}
		]
	]).

	% =========================================================================
	% Filtered subsequence generation
	% =========================================================================

	:- public(combinations/3).
	:- mode(combinations(+integer, +list, -list), one).
	:- mode(combinations(+integer, +list, ?list), zero_or_more).
	:- info(combinations/3, [
		comment is 'Generates all K-element subsequences (combinations) of a list. Order of elements is preserved from the original list, but position selection varies. No repetitions allowed.',
		argnames is ['K', 'List', 'Combination'],
		examples is [
			'2-combinations' - combinations(2, [a,b,c], Combs) - {Combs = [[a,b],[a,c],[b,c]]},
			'Empty combination' - combinations(0, [a,b], C) - {C = [[]]},
			'Impossible combination' - combinations(3, [a,b], C) - {false}
		]
	]).

	:- public(combinations/4).
	:- mode(combinations(+integer, +list, +atom, -list), one).
	:- mode(combinations(+integer, +list, +atom, ?list), zero_or_more).
	:- info(combinations/4, [
		comment is 'Generates all K-element combinations with specified ordering: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combination'],
		examples is [
			'Lexicographic 2-combinations' - combinations(2, [a,b,c], lexicographic, Combs) - {Combs = [[a,b],[a,c],[b,c]]}
		]
	]).

	:- public(combinations_with_replacement/3).
	:- mode(combinations_with_replacement(+integer, +list, -list), one).
	:- mode(combinations_with_replacement(+integer, +list, ?list), zero_or_more).
	:- info(combinations_with_replacement/3, [
		comment is 'Generates all K-element subsequences with replacement allowed. Elements can be repeated.',
		argnames is ['K', 'List', 'Combination'],
		examples is [
			'2-combinations with replacement' - combinations_with_replacement(2, [a,b], Combs) - {Combs = [[a,a],[a,b],[b,b]]}
		]
	]).

	:- public(permutations/2).
	:- mode(permutations(+list, -list), one).
	:- mode(permutations(+list, ?list), zero_or_more).
	:- info(permutations/2, [
		comment is 'Generates all permutations of a list. Unlike subsequences, permutations rearrange all elements without preserving relative order.',
		argnames is ['List', 'Permutation'],
		examples is [
			'All permutations' - permutations([a,b,c], Perms) - {Perms = [[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,a,b],[c,b,a]]}
%			'Backtracking' - permutations([1,2], P) generates all 2! = 2 permutations on backtracking
		]
	]).

	:- public(permutations/3).
	:- mode(permutations(+list, +atom, -list), one).
	:- mode(permutations(+list, +atom, ?list), zero_or_more).
	:- info(permutations/3, [
		comment is 'Generates all permutations of a list with specified ordering: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Permutation'],
		examples is [
			'Lexicographic permutations' - permutations([a,b,c], lexicographic, Perms) - {Perms = [[a,b,c],[a,c,b],[b,a,c],[b,c,a],[c,a,b],[c,b,a]]}
		]
	]).

	:- public(k_permutations/3).
	:- mode(k_permutations(+integer, +list, -list), one).
	:- mode(k_permutations(+integer, +list, ?list), zero_or_more).
	:- info(k_permutations/3, [
		comment is 'Generates all K-element permutations of a list. These are ordered selections where order matters.',
		argnames is ['K', 'List', 'Permutation'],
		examples is [
			'2-permutations' - k_permutations(2, [a,b,c], Perms) - {Perms = [[a,b],[a,c],[b,a],[b,c],[c,a],[c,b]]}
		]
	]).

	:- public(k_permutations/4).
	:- mode(k_permutations(+integer, +list, +atom, -list), one).
	:- mode(k_permutations(+integer, +list, +atom, ?list), zero_or_more).
	:- info(k_permutations/4, [
		comment is 'Generates all K-element permutations with specified ordering: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Permutation'],
		examples is [
			'Lexicographic 2-permutations' - k_permutations(2, [a,b,c], lexicographic, Perms) - {Perms = [[a,b],[a,c],[b,a],[b,c],[c,a],[c,b]]}
		]
	]).

	:- public(cartesian_product/3).
	:- mode(cartesian_product(+integer, +list, -list), one).
	:- mode(cartesian_product(+integer, +list, ?list), zero_or_more).
	:- info(cartesian_product/3, [
		comment is 'Generates all K-element tuples from the list with replacement and where order matters (Cartesian product of the list with itself K times).',
		argnames is ['K', 'List', 'Tuple'],
		examples is [
			'2-tuples' - cartesian_product(2, [a,b], Tuples) - {Tuples = [[a,a],[a,b],[b,a],[b,b]]}
		]
	]).

	:- public(derangements/2).
	:- mode(derangements(+list, -list), one).
	:- mode(derangements(+list, ?list), zero_or_more).
	:- info(derangements/2, [
		comment is 'Generates all derangements of a list. A derangement is a permutation where no element appears in its original position.',
		argnames is ['List', 'Derangement'],
		examples is [
			'Simple derangement' - derangements([a,b,c], Derangs) - {Derangs = [[b,c,a],[c,a,b]]} % but not [a,b,c],[a,c,b],[b,a,c],[c,b,a]
		]
	]).

	:- public(next_permutation/2).
	:- mode(next_permutation(+list, -list), zero_or_one).
	:- info(next_permutation/2, [
		comment is 'Computes the next permutation in lexicographic order. Fails if the input is the last permutation.',
		argnames is ['Permutation', 'NextPermutation'],
		examples is [
			'Next permutation' - next_permutation([a,b,c], Next) - {Next = [a,c,b]}
		]
	]).

	:- public(prev_permutation/2).
	:- mode(prev_permutation(+list, -list), zero_or_one).
	:- info(prev_permutation/2, [
		comment is 'Computes the previous permutation in lexicographic order. Fails if the input is the first permutation.',
		argnames is ['Permutation', 'PrevPermutation'],
		examples is [
			'Previous permutation' - prev_permutation([a,c,b], Prev) - {Prev = [a,b,c]}
		]
	]).

	% =========================================================================
	% Indexed access to subsequences
	% =========================================================================

	:- public(nth_combination/4).
	:- mode(nth_combination(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_combination/4, [
		comment is 'Directly computes the Nth K-element combination in lexicographic order without generating all previous ones. Index starts at 0.',
		argnames is ['K', 'List', 'Index', 'Combination'],
		examples is [
			'Second 2-combination' - nth_combination(2, [a,b,c,d], 1, C) - {C = [a,c]} % (0:[a,b], 1:[a,c], 2:[a,d], ...)
		]
	]).

	:- public(nth_permutation/3).
	:- mode(nth_permutation(+list, +integer, -list), zero_or_one).
	:- info(nth_permutation/3, [
		comment is 'Directly computes the Nth permutation in lexicographic order. Index starts at 0.',
		argnames is ['List', 'Index', 'Permutation'],
		examples is [
			'Third permutation' - nth_permutation([a,b,c], 2, P) - {P = [b,a,c]}
		]
	]).

	:- public(combination_index/3).
	:- mode(combination_index(+integer, +list, -integer), zero_or_one).
	:- info(combination_index/3, [
		comment is 'Finds the lexicographic index of a given K-element combination. Inverse of nth_combination/4.',
		argnames is ['K', 'Combination', 'Index'],
		examples is [
			'Index of combination' - combination_index(2, [a,c], Idx) - {Idx = 1} % (given original list [a,b,c,d])
		]
	]).

	:- public(permutation_index/2).
	:- mode(permutation_index(+list, -integer), zero_or_one).
	:- info(permutation_index/2, [
		comment is 'Finds the lexicographic index of a given permutation. Inverse of nth_permutation/3.',
		argnames is ['Permutation', 'Index'],
		examples is [
			'Index of permutation' - permutation_index([b,a,c], Idx) - {Idx = 2}
		]
	]).

	% =========================================================================
	% Searching and matching subsequences
	% =========================================================================

	:- public(longest_common_subsequence/3).
	:- mode(longest_common_subsequence(+list, +list, -list), one).
	:- info(longest_common_subsequence/3, [
		comment is 'Finds the longest common subsequence (LCS) between two lists. Uses dynamic programming.',
		argnames is ['List1', 'List2', 'LCS'],
		examples is [
			'LCS example' - longest_common_subsequence([a,b,c,d,e], [a,c,e,f], LCS) - {LCS = [a,c,e]}
		]
	]).

	:- public(longest_increasing_subsequence/2).
	:- mode(longest_increasing_subsequence(+list, -list), one).
	:- info(longest_increasing_subsequence/2, [
		comment is 'Finds the longest strictly increasing subsequence in a list. Elements must be comparable.',
		argnames is ['List', 'LIS'],
		examples is [
			'LIS example' - longest_increasing_subsequence([3,1,4,1,5,9,2,6], LIS) - {LIS = [1,4,5,9]}
		]
	]).

	:- public(longest_decreasing_subsequence/2).
	:- mode(longest_decreasing_subsequence(+list, -list), one).
	:- info(longest_decreasing_subsequence/2, [
		comment is 'Finds the longest strictly decreasing subsequence in a list. Elements must be comparable.',
		argnames is ['List', 'LDS'],
		examples is [
			'LDS example' - longest_decreasing_subsequence([9,5,2,8,3,1], LDS) - {LDS = [9,5,2,1]}
		]
	]).

	:- public(longest_common_increasing_subsequence/3).
	:- mode(longest_common_increasing_subsequence(+list, +list, -list), one).
	:- info(longest_common_increasing_subsequence/3, [
		comment is 'Finds the longest subsequence that is both common to two lists and strictly increasing.',
		argnames is ['List1', 'List2', 'LCIS'],
		examples is [
			'LCIS example' - longest_common_increasing_subsequence([1,4,2,5], [4,1,3,5], LCIS) - {LCIS = [1,5]}
		]
	]).

	:- public(longest_repeating_subsequence/2).
	:- mode(longest_repeating_subsequence(+list, -list), one).
	:- info(longest_repeating_subsequence/2, [
		comment is 'Finds the longest subsequence that appears at least twice in the list (at different positions).',
		argnames is ['List', 'LRS'],
		examples is [
			'LRS example' - longest_repeating_subsequence([a,a,b,a,b], LRS) - {LRS = [a,b]}
		]
	]).

	:- public(is_subsequence_of/2).
	:- mode(is_subsequence_of(+list, +list), zero_or_one).
	:- info(is_subsequence_of/2, [
		comment is 'Checks if the first list is a subsequence of the second list. All elements must occur in order.',
		argnames is ['Subsequence', 'List'],
		examples is [
			'Valid subsequence' - is_subsequence_of([a,c], [a,b,c]) - {true},
			'Invalid subsequence' - is_subsequence_of([c,a], [a,b,c]) - {false}
		]
	]).

	:- public(all_common_subsequences/3).
	:- mode(all_common_subsequences(+list, +list, -list), one).
	:- mode(all_common_subsequences(+list, +list, ?list), zero_or_more).
	:- info(all_common_subsequences/3, [
		comment is 'Generates all subsequences that are common to both lists.',
		argnames is ['List1', 'List2', 'CommonSubsequence'],
		examples is [
			'Common subsequences' - all_common_subsequences([a,b,c], [a,c,d], CS) - {CS = [], CS = [a], CS = [c], CS = [a,c]}
		]
	]).

	:- public(count_distinct_subsequences/3).
	:- mode(count_distinct_subsequences(+list, +list, -integer), one).
	:- info(count_distinct_subsequences/3, [
		comment is 'Counts the number of distinct occurrences of a pattern as a subsequence in a list.',
		argnames is ['Pattern', 'List', 'Count'],
		examples is [
			'Count occurrences' - count_distinct_subsequences([a,b], [a,a,b,b], Count) - {Count = 4}
		]
	]).

	% =========================================================================
	% Prefix and suffix operations
	% =========================================================================

	:- public(is_prefix_of/2).
	:- mode(is_prefix_of(+list, +list), zero_or_one).
	:- info(is_prefix_of/2, [
		comment is 'Checks if the first list is a prefix of the second list.',
		argnames is ['Prefix', 'List'],
		examples is [
			'Valid prefix' - is_prefix_of([a,b], [a,b,c]) - {true},
			'Invalid prefix' - is_prefix_of([b,c], [a,b,c]) - {false}
		]
	]).

	:- public(is_suffix_of/2).
	:- mode(is_suffix_of(+list, +list), zero_or_one).
	:- info(is_suffix_of/2, [
		comment is 'Checks if the first list is a suffix of the second list.',
		argnames is ['Suffix', 'List'],
		examples is [
			'Valid suffix' - is_suffix_of([b,c], [a,b,c]) - {true},
			'Invalid suffix' - is_suffix_of([a,b], [a,b,c]) - {false}
		]
	]).

	% =========================================================================
	% Contiguous subsequences
	% =========================================================================

	:- public(subslices/2).
	:- mode(subslices(+list, -list), one).
	:- mode(subslices(+list, ?list), zero_or_more).
	:- info(subslices/2, [
		comment is 'Generates all contiguous non-empty subslices (sublists) of a list.',
		argnames is ['List', 'Subslice'],
		examples is [
			'All subslices' - subslices([a,b,c], Subs) - {Subs = [[a],[a,b],[a,b,c],[b],[b,c],[c]]}
		]
	]).

	:- public(sliding_window/3).
	:- mode(sliding_window(+integer, +list, -list), one).
	:- mode(sliding_window(+integer, +list, ?list), zero_or_more).
	:- info(sliding_window/3, [
		comment is 'Generates all contiguous windows of size N from a list. Fails if N is larger than the list length.',
		argnames is ['N', 'List', 'Window'],
		examples is [
			'Windows of size 2' - sliding_window(2, [a,b,c,d], Windows) - {Windows = [[a,b],[b,c],[c,d]]}
		]
	]).

	% =========================================================================
	% Random selection
	% =========================================================================

	:- public(random_combination/3).
	:- mode(random_combination(+integer, +list, -list), one).
	:- info(random_combination/3, [
		comment is 'Randomly selects one K-element combination uniformly from all possible combinations.',
		argnames is ['K', 'List', 'Combination'],
		examples is [
			'Random 2-combination' - random_combination(2, [a,b,c,d], C) - {C = [b,d]}
		]
	]).

	:- public(random_permutation/2).
	:- mode(random_permutation(+list, -list), one).
	:- info(random_permutation/2, [
		comment is 'Randomly selects one permutation uniformly from all possible permutations. Also known as shuffling.',
		argnames is ['List', 'Permutation'],
		examples is [
			'Random permutation' - random_permutation([a,b,c], P) - {P = [c,a,b]}
		]
	]).

	:- public(random_subsequence/2).
	:- mode(random_subsequence(+list, -list), one).
	:- info(random_subsequence/2, [
		comment is 'Randomly selects one subsequence uniformly from all 2^N possible subsequences.',
		argnames is ['List', 'Subsequence'],
		examples is [
			'Random subsequence' - random_subsequence([a,b,c], S) - {S = [a,c]}
		]
	]).

	% =========================================================================
	% Constrained subsequence operations
	% =========================================================================

	:- public(subsequences_with_min_span/3).
	:- mode(subsequences_with_min_span(+integer, +list, -list), one).
	:- mode(subsequences_with_min_span(+integer, +list, ?list), zero_or_more).
	:- info(subsequences_with_min_span/3, [
		comment is 'Generates subsequences where consecutive elements are at least MinSpan positions apart in the original list.',
		argnames is ['MinSpan', 'List', 'Subsequence'],
		examples is [
			'Min span of 2' - subsequences_with_min_span(2, [a,b,c,d], S) - {S = [a,c]}
		]
	]).

	:- public(alternating_subsequences/2).
	:- mode(alternating_subsequences(+list, -list), one).
	:- mode(alternating_subsequences(+list, ?list), zero_or_more).
	:- info(alternating_subsequences/2, [
		comment is 'Generates subsequences that alternate between increasing and decreasing (or vice versa). Elements must be comparable.',
		argnames is ['List', 'AlternatingSubsequence'],
		examples is [
			'Alternating' - alternating_subsequences([1,3,2,4,3,5], S) - {S = [1,3,2,4,3,5]}
		]
	]).

	:- public(k_distinct_subsequences/3).
	:- mode(k_distinct_subsequences(+integer, +list, -list), one).
	:- mode(k_distinct_subsequences(+integer, +list, ?list), zero_or_more).
	:- info(k_distinct_subsequences/3, [
		comment is 'Generates K-element subsequences where all elements are distinct (no duplicates in the subsequence itself).',
		argnames is ['K', 'List', 'DistinctSubsequence'],
		examples is [
			'Distinct only' - k_distinct_subsequences(2, [a,a,b], S) - {S = [a,b]}
		]
	]).

	% =========================================================================
	% Utility predicates
	% =========================================================================

	:- public(count_subsequences/2).
	:- mode(count_subsequences(+list, -integer), one).
	:- info(count_subsequences/2, [
		comment is 'Counts the total number of subsequences (always 2^N for a list of length N).',
		argnames is ['List', 'Count'],
		examples is [
			'Count' - count_subsequences([a,b,c], N) - {N = 8}
		]
	]).

	:- public(count_combinations/3).
	:- mode(count_combinations(+integer, +list, -integer), one).
	:- info(count_combinations/3, [
		comment is 'Counts the number of K-element combinations: C(N,K) = N!/(K!(N-K)!).',
		argnames is ['K', 'List', 'Count'],
		examples is [
			'C(4,2)' - count_combinations(2, [a,b,c,d], N) - {N = 6}
		]
	]).

	:- public(count_permutations/2).
	:- mode(count_permutations(+list, -integer), one).
	:- info(count_permutations/2, [
		comment is 'Counts the number of permutations (always N! for a list of length N).',
		argnames is ['List', 'Count'],
		examples is [
			'Count' - count_permutations([a,b,c], N) - {N = 6}
		]
	]).

	:- public(subsequence_length/2).
	:- mode(subsequence_length(+list, -integer), one).
	:- info(subsequence_length/2, [
		comment is 'Returns the length of a subsequence (same as list length, provided for consistency).',
		argnames is ['Subsequence', 'Length'],
		examples is [
			'Length' - subsequence_length([a,b,c], N) - {N = 3}
		]
	]).

:- end_protocol.

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
		date is 2026-02-26,
		comment is 'Protocol for subsequence operations over lists.',
		remarks is [
			'Generation operations' - 'Predicates for generating all subsequences or variants thereof.',
			'Ordering variants' - 'Predicates that support an additional Order argument (default, lexicographic, or shortlex) for controlling output order.',
			'Searching and matching' - 'Predicates for finding specific subsequences with desired properties.',
			'Prefix and suffix operations' - 'Predicates for checking and finding prefixes and suffixes.',
			'Contiguous subsequences' - 'Predicates for working with contiguous subsequences (subslices, sliding windows).',
			'Random selection' - 'Predicates for randomly selecting subsequences.',
			'Constrained operations' - 'Predicates for generating subsequences with specific constraints.',
			'Utility predicates' - 'Helper predicates for subsequence operations.'
		]
	]).

	% generation operations - Creating all subsequences

	:- public(subsequences/2).
	:- mode(subsequences(+list, -list), one).
	:- info(subsequences/2, [
		comment is 'Generates all subsequences of a list using default order. A subsequence maintains the relative order of elements but need not be contiguous. The empty list is included.',
		argnames is ['List', 'Subsequences'],
		examples is [
			'All subsequences' - subsequences([a,b,c], Subsequences) - {Subsequences = [[],[a],[b],[a,b],[c],[a,c],[b,c],[a,b,c]]}
		]
	]).

	:- public(subsequence/2).
	:- mode(subsequence(+list, -list), one_or_more).
	:- info(subsequence/2, [
		comment is 'True iff the second argument is a subsequence of the first argument. Subsequences of a list using default order. A subsequence maintains the relative order of elements but need not be contiguous. The empty list is included.',
		argnames is ['List', 'Subsequence'],
		examples is [
			'A subsequence' - subsequence([1,2], Subsequence) - {Subsequence = []}
		]
	]).

	:- public(subsequences/3).
	:- mode(subsequences(+list, +atom, -list), one).
	:- info(subsequences/3, [
		comment is 'Generates all subsequences of a list with specified ordering: ``default`` (as naturally produced), ``lexicographic``, or ``shortlex`` (by length first, then lexicographically).',
		argnames is ['List', 'Order', 'Subsequences'],
		examples is [
			'Shortlex order' - subsequences([a,b], shortlex, Subsequences) - {Subsequences = [[],[a],[b],[a,b]]}
		]
	]).

	:- public(subsequence/3).
	:- mode(subsequence(+list, +atom, -list), one_or_more).
	:- info(subsequence/3, [
		comment is 'True iff the third argument is a subsequence of the first argument with specified ordering: ``default`` (as naturally produced), ``lexicographic``, or ``shortlex`` (by length first, then lexicographically).',
		argnames is ['List', 'Order', 'Subsequence'],
		examples is [
			'Shortlex order' - subsequence([a,b], shortlex, Subsequence) - {Subsequence = []}
		]
	]).

	:- public(nonempty_subsequences/2).
	:- mode(nonempty_subsequences(+list, -list), one).
	:- info(nonempty_subsequences/2, [
		comment is 'Generates all non-empty subsequences of a list.',
		argnames is ['List', 'Subsequences'],
		examples is [
			'Non-empty subsequences' - nonempty_subsequences([a,b], Subsequences) - {Subsequences = [[a],[b],[a,b]]}
		]
	]).

	:- public(nonempty_subsequence/2).
	:- mode(nonempty_subsequence(+list, -list), one_or_more).
	:- info(nonempty_subsequence/2, [
		comment is 'True iff the second argument is a non-empty subsequence of the first argument.',
		argnames is ['List', 'Subsequence'],
		examples is [
			'Non-empty subsequence' - nonempty_subsequence([a,b], Subsequence) - {Subsequence = [a]}
		]
	]).

	:- public(power_set/2).
	:- mode(power_set(+list, -list), one).
	:- info(power_set/2, [
		comment is 'Generates the power set of a list (all possible subsequences). Alias for subsequences/2 when first argument is ground.',
		argnames is ['List', 'PowerSet'],
		examples is [
			'Power set' - power_set([a,b], PowerSet) - {PowerSet = [[],[a],[b],[a,b]]}
		]
	]).

	:- public(inits/2).
	:- mode(inits(+list, -list), one).
	:- info(inits/2, [
		comment is 'Generates all initial segments (prefixes) of a list, shortest first. Includes the empty list.',
		argnames is ['List', 'Inits'],
		examples is [
			'All prefixes' - inits([a,b,c], Inits) - {Inits = [[],[a],[a,b],[a,b,c]]}
		]
	]).

	:- public(init/2).
	:- mode(init(+list, -term), zero_or_more).
	:- mode(init(+list, +term), zero_or_one).
	:- info(init/2, [
		comment is 'True iff the second argument is one of the initial segments (prefixes) of a list.',
		argnames is ['List', 'Inits'],
		examples is [
			'Check prefix' - init([a,b,c], [a,b]) - {true}
		]
	]).

	:- public(tails/2).
	:- mode(tails(+list, -list), one).
	:- info(tails/2, [
		comment is 'Generates all final segments (suffixes) of a list, longest first. Includes the empty list.',
		argnames is ['List', 'Tails'],
		examples is [
			'All suffixes' - tails([a,b,c], Tails) - {Tails = [[a,b,c],[b,c],[c],[]]}
		]
	]).

	:- public(tail/2).
	:- mode(tail(+list, -term), zero_or_more).
	:- mode(tail(+list, +term), zero_or_one).
	:- info(tail/2, [
		comment is 'True iff the second argument is one of the final segments (suffixes) of a list.',
		argnames is ['List', 'Tails'],
		examples is [
			'Check suffix' - tail([a,b,c], [b,c]) - {true}
		]
	]).

	:- public(inits1/2).
	:- mode(inits1(+list, -list), one).
	:- info(inits1/2, [
		comment is 'Generates all non-empty initial segments (prefixes) of a list, shortest first.',
		argnames is ['List', 'Inits'],
		examples is [
			'Non-empty prefixes' - inits1([a,b,c], Inits) - {Inits = [[a],[a,b],[a,b,c]]}
		]
	]).

	:- public(init1/2).
	:- mode(init1(+list, -term), one_or_more).
	:- info(init1/2, [
		comment is 'True iff the second argument is a non-empty initial segment (prefix) of a list, shortest first.',
		argnames is ['List', 'Init'],
		examples is [
			'Non-empty prefix' - init1([a,b,c], Init) - {Init = [a]}
		]
	]).

	:- public(tails1/2).
	:- mode(tails1(+list, -list), one).
	:- info(tails1/2, [
		comment is 'Generates all non-empty final segments (suffixes) of a list, longest first.',
		argnames is ['List', 'Tails'],
		examples is [
			'Non-empty suffix' - tails1([a,b,c], Tails) - {Tails = [[a,b,c],[b,c],[c]]}
		]
	]).

	:- public(tail1/2).
	:- mode(tail1(+list, -list), one).
	:- info(tail1/2, [
		comment is 'True iff the second argument is a non-empty final segment (suffix) of a list, longest first.',
		argnames is ['List', 'Tail'],
		examples is [
			'Non-empty suffix' - tail1([a,b,c], Tail) - {Tail = [a,b,c]}
		]
	]).

	:- public(init_tails/2).
	:- mode(init_tails(+list, -list), one).
	:- info(init_tails/2, [
		comment is 'Generates all pairs of initial and final segments. Each pair Init-Tail represents a split of the list where Init+Tail equals the original list. When the second argument is bound, checks if it is a valid split.',
		argnames is ['List', 'InitTailPairs'],
		examples is [
			'All splits' - init_tails([a,b], Pairs) - {Pairs = [[]-[a,b],[a]-[b],[a,b]-[]]}
		]
	]).

	:- public(init_tail/2).
	:- mode(init_tail(+list, -term), one_or_more).
	:- info(init_tail/2, [
		comment is 'True iff (Init,Tail) represents a split of the list where Init+Tail equals the original list.',
		argnames is ['List', 'InitTailPairs'],
		examples is [
			'Check split' - init_tail([a,b,c], [a]-[b,c]) - {true}
		]
	]).

	% searching and matching subsequences

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

	:- public(proper_subsequence/2).
	:- mode(proper_subsequence(+list, +list), zero_or_one).
	:- info(proper_subsequence/2, [
		comment is 'Checks if the first list is a proper subsequence of the second list (i.e., subsequence and not equal).',
		argnames is ['Subsequence', 'List'],
		examples is [
			'Proper subsequence' - proper_subsequence([a,c], [a,b,c]) - {true},
			'Not proper (equal lists)' - proper_subsequence([a,b,c], [a,b,c]) - {false}
		]
	]).

	:- public(subsequence_at_indices/3).
	:- mode(subsequence_at_indices(+list, +list, -list), zero_or_one).
	:- info(subsequence_at_indices/3, [
		comment is 'Extracts a subsequence using a strictly increasing list of 1-based indices.',
		argnames is ['List', 'Indices', 'Subsequence'],
		examples is [
			'Indices selection' - subsequence_at_indices([a,b,c,d], [1,3], Subsequence) - {Subsequence = [a,c]}
		]
	]).

	:- public(common_subsequences/3).
	:- mode(common_subsequences(+list, +list, -list), one).
	:- info(common_subsequences/3, [
		comment is 'Generates all subsequences that are common to both lists.',
		argnames is ['List1', 'List2', 'CommonSubsequences'],
		examples is [
			'All common subsequences' - common_subsequences([a,b,c], [a,c,d], CommonSubsequences) - {CommonSubsequences = [[a,c],[a],[c],[]]}
		]
	]).

	:- public(common_subsequence/3).
	:- mode(common_subsequence(+list, +list, -list), one_or_more).
	:- info(common_subsequence/3, [
		comment is 'True iff the third argument is a common subsequence of both lists.',
		argnames is ['List1', 'List2', 'CommonSubsequence'],
		examples is [
			'Check common subsequence' - common_subsequence([a,b,c], [a,c,d], [a,c]) - {true}
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

	% prefix and suffix operations

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

	% contiguous subsequences

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

	% random selection

	:- public(random_subsequence/2).
	:- mode(random_subsequence(+list, -list), one).
	:- info(random_subsequence/2, [
		comment is 'Randomly selects one subsequence uniformly from all 2^N possible subsequences.',
		argnames is ['List', 'Subsequence'],
		examples is [
			'Random subsequence' - random_subsequence([a,b,c], Subsequence) - {Subsequence = [a,c]}
		]
	]).

	% constrained subsequence operations

	:- public(subsequences_with_min_span/3).
	:- mode(subsequences_with_min_span(+integer, +list, -list), one).
	:- mode(subsequences_with_min_span(+integer, +list, ?list), zero_or_more).
	:- info(subsequences_with_min_span/3, [
		comment is 'Generates subsequences where consecutive elements are at least MinSpan positions apart in the original list.',
		argnames is ['MinSpan', 'List', 'Subsequence'],
		examples is [
			'Min span of 2' - subsequences_with_min_span(2, [a,b,c,d], Sequences) - {Sequences = [a,c]}
		]
	]).

	:- public(alternating_subsequences/2).
	:- mode(alternating_subsequences(+list, -list), one).
	:- info(alternating_subsequences/2, [
		comment is 'Generates all subsequences that alternate between increasing and decreasing (or vice versa). Elements must be comparable.',
		argnames is ['List', 'AlternatingSubsequences'],
		examples is [
			'All alternating subsequences' - alternating_subsequences([1,3,2,4], Sequences) - {Sequences = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]}
		]
	]).

	:- public(alternating_subsequence/2).
	:- mode(alternating_subsequence(+list, -list), one_or_more).
	:- info(alternating_subsequence/2, [
		comment is 'True iff the second argument is a subsequence that alternates between increasing and decreasing (or vice versa). Elements must be comparable.',
		argnames is ['List', 'AlternatingSubsequence'],
		examples is [
			'Alternating' - alternating_subsequence([1,3,2,4], Sequences) - {Sequences = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]}
		]
	]).

	:- public(k_distinct_subsequences/3).
	:- mode(k_distinct_subsequences(+integer, +list, -list), one).
	:- info(k_distinct_subsequences/3, [
		comment is 'Generates all K-element subsequences where all elements are distinct (no duplicates in the subsequence itself).',
		argnames is ['K', 'List', 'DistinctSubsequences'],
		examples is [
			'All distinct only' - k_distinct_subsequences(2, [a,a,b], Subsequences) - {Subsequences = [[a,b],[a,c],[b,c]]}
		]
	]).

	:- public(k_distinct_subsequence/3).
	:- mode(k_distinct_subsequence(+integer, +list, -list), one).
	:- info(k_distinct_subsequence/3, [
		comment is 'True iff the third argument is a subsequence of the first argument that is a K-element subsequence where all elements are distinct (no duplicates in the subsequence itself).',
		argnames is ['K', 'List', 'DistinctSubsequence'],
		examples is [
			'A distinct only' - k_distinct_subsequence(2, [a,a,b], Subsequences) - {Subsequences = [a,b]}
		]
	]).

	% utility predicates

	:- public(count_subsequences/2).
	:- mode(count_subsequences(+list, -integer), one).
	:- info(count_subsequences/2, [
		comment is 'Counts the total number of subsequences (always 2^N for a list of length N).',
		argnames is ['List', 'Count'],
		examples is [
			'Count' - count_subsequences([a,b,c], N) - {N = 8}
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

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
		date is 2026-05-12,
		comment is 'Unit tests for the "multisets" library.'
	]).

	cover(multisets).

	test(multisets_3_two_of_three, deterministic(Multisets == [[a,a],[a,b],[a,c],[b,b],[b,c],[c,c]])) :-
		multisets::multisets(2, [a,b,c], Multisets).

	test(multisets_3_zero_of_any, deterministic(Multisets == [[]])) :-
		multisets::multisets(0, [a,b,c], Multisets).

	test(multisets_3_empty_source, deterministic(Multisets == [])) :-
		multisets::multisets(2, [], Multisets).

	test(multiset_3_all, true(Multisets == [[a,a],[a,b],[b,b]])) :-
		findall(Multiset, multisets::multiset(2, [a,b], Multiset), Multisets).

	test(multisets_4_lexicographic, deterministic(Multisets == [[a,a],[b,a],[b,b]])) :-
		multisets::multisets(2, [b,a], lexicographic, Multisets).

	test(multiset_4_lexicographic_single, true(Multiset == [b,b])) :-
		multisets::multiset(2, [b,a], lexicographic, Multiset).

	test(multisets_3_three_of_two, deterministic(Multisets == [[a,a,a],[a,a,b],[a,b,b],[b,b,b]])) :-
		multisets::multisets(3, [a,b], Multisets).

	test(distinct_multisets_3_with_duplicates, deterministic(Multisets == [[a,a],[a,b],[b,b]])) :-
		multisets::distinct_multisets(2, [a,a,b], Multisets).

	test(distinct_multiset_3_with_duplicates, true(Multisets == [[a,a],[a,b],[b,b]])) :-
		findall(Multiset, multisets::distinct_multiset(2, [a,a,b], Multiset), Multisets).

	test(distinct_multisets_4_lexicographic, deterministic(Multisets == [[a,a],[b,a],[b,b]])) :-
		multisets::distinct_multisets(2, [b,a,a], lexicographic, Multisets).

	test(distinct_multiset_4_default_exists, exists(Multiset == [a,b])) :-
		multisets::distinct_multiset(2, [a,a,b], default, Multiset).

	test(count_multisets_3_two_of_four, deterministic(Count == 10)) :-
		multisets::count_multisets(2, [a,b,c,d], Count).

	test(count_distinct_multisets_3_two_of_duplicates, deterministic(Count == 3)) :-
		multisets::count_distinct_multisets(2, [a,a,b], Count).

	test(nth_multiset_4_second, deterministic(Multiset == [a,b])) :-
		multisets::nth_multiset(2, [a,b,c], 1, Multiset).

	test(nth_multiset_5_default_second, deterministic(Multiset == [a,b])) :-
		multisets::nth_multiset(2, [a,b,c], default, 1, Multiset).

	test(nth_multiset_5_lexicographic_first, deterministic(Multiset == [a,a])) :-
		multisets::nth_multiset(2, [b,a], lexicographic, 0, Multiset).

	test(nth_multiset_5_lexicographic_last, deterministic(Multiset == [b,b])) :-
		multisets::nth_multiset(2, [b,a], lexicographic, 2, Multiset).

	test(multiset_index_4_fifth, deterministic(Index == 4)) :-
		multisets::multiset_index(2, [a,b,c], [b,c], Index).

	test(multiset_index_5_default_fifth, deterministic(Index == 4)) :-
		multisets::multiset_index(2, [a,b,c], default, [b,c], Index).

	test(multiset_index_5_lexicographic_last, deterministic(Index == 2)) :-
		multisets::multiset_index(2, [b,a], lexicographic, [b,b], Index).

	test(multiset_index_5_lexicographic_first, deterministic(Index == 0)) :-
		multisets::multiset_index(2, [b,a], lexicographic, [a,a], Index).

	test(nth_multiset_multiset_index_roundtrip, deterministic(Index == 2)) :-
		multisets::nth_multiset(2, [a,b,c,d], 2, Multiset),
		multisets::multiset_index(2, [a,b,c,d], Multiset, Index).

	test(nth_distinct_multiset_4_second, deterministic(Multiset == [a,b])) :-
		multisets::nth_distinct_multiset(2, [a,a,b], 1, Multiset).

	test(distinct_multiset_index_4_last, deterministic(Index == 2)) :-
		multisets::distinct_multiset_index(2, [a,a,b], [b,b], Index).

	test(random_multiset_3_length, true(list::length(Multiset, 2))) :-
		multisets::random_multiset(2, [a,b,c,d], Multiset).

	test(sample_multisets_4_zero, deterministic(Samples == [])) :-
		multisets::sample_multisets(2, [a,b,c], 0, Samples).

	test(sample_multisets_4_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), multisets::multiset(2, [a,b,c], Sample))))) :-
		multisets::sample_multisets(2, [a,b,c], 3, Samples).

	test(random_distinct_multiset_3_length, true(list::length(Multiset, 2))) :-
		multisets::random_distinct_multiset(2, [a,a,b,c], Multiset).

	test(sample_distinct_multisets_4_zero, deterministic(Samples == [])) :-
		multisets::sample_distinct_multisets(2, [a,a,b,c], 0, Samples).

	test(sample_distinct_multisets_4_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), multisets::distinct_multiset(2, [a,a,b,c], Sample))))) :-
		multisets::sample_distinct_multisets(2, [a,a,b,c], 3, Samples).

	test(next_multiset_3_middle, deterministic(Next == [b,a])) :-
		multisets::next_multiset([b,a], [a,a], Next).

	test(next_multiset_3_duplicates, deterministic(Next == [a,b])) :-
		multisets::next_multiset([a,a,b], [a,a], Next).

	test(previous_multiset_3_middle, deterministic(Previous == [b,a])) :-
		multisets::previous_multiset([b,a], [b,b], Previous).

	test(previous_multiset_3_duplicates, deterministic(Previous == [a,b])) :-
		multisets::previous_multiset([a,a,b], [b,b], Previous).

:- end_object.

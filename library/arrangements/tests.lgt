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
		comment is 'Unit tests for the "arrangements" library.'
	]).

	cover(arrangements).

	test(arrangements_3_two_of_three, deterministic(Arrangements == [[a,a],[a,b],[a,c],[b,a],[b,b],[b,c],[c,a],[c,b],[c,c]])) :-
		arrangements::arrangements(2, [a,b,c], Arrangements).

	test(arrangement_3_all, true(Arrangements == [[a,a],[a,b],[b,a],[b,b]])) :-
		findall(Arrangement, arrangements::arrangement(2, [a,b], Arrangement), Arrangements).

	test(arrangements_4_lexicographic, deterministic(Arrangements == [[a,a],[a,b],[a,c],[b,a],[b,b],[b,c],[c,a],[c,b],[c,c]])) :-
		arrangements::arrangements(2, [c,b,a], lexicographic, Arrangements).

	test(arrangement_4_lexicographic_single, true(Arrangement == [a,a])) :-
		arrangements::arrangement(2, [c,b,a], lexicographic, Arrangement).

	test(arrangement_4_lexicographic_all, true(Arrangements == [[a,a],[a,b],[b,a],[b,b]])) :-
		findall(Arrangement, arrangements::arrangement(2, [b,a], lexicographic, Arrangement), Arrangements).

	test(arrangement_4_default_exists, exists(Arrangement == [b,a])) :-
		arrangements::arrangement(2, [a,b], default, Arrangement).

	test(arrangements_3_three_of_two, deterministic(Arrangements == [[a,a,a],[a,a,b],[a,b,a],[a,b,b],[b,a,a],[b,a,b],[b,b,a],[b,b,b]])) :-
		arrangements::arrangements(3, [a,b], Arrangements).

	test(distinct_arrangements_3_with_duplicates, deterministic(Arrangements == [[a,a],[a,b],[b,a],[b,b]])) :-
		arrangements::distinct_arrangements(2, [a,a,b], Arrangements).

	test(distinct_arrangement_3_with_duplicates, true(Arrangements == [[a,a],[a,b],[b,a],[b,b]])) :-
		findall(Arrangement, arrangements::distinct_arrangement(2, [a,a,b], Arrangement), Arrangements).

	test(distinct_arrangements_4_lexicographic, deterministic(Arrangements == [[a,a],[a,b],[b,a],[b,b]])) :-
		arrangements::distinct_arrangements(2, [b,a,a], lexicographic, Arrangements).

	test(distinct_arrangement_4_default_exists, exists(Arrangement == [b,a])) :-
		arrangements::distinct_arrangement(2, [a,a,b], default, Arrangement).

	test(cartesian_product_2_two_lists, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		arrangements::cartesian_product([[a,b],[1,2]], Product).

	test(cartesian_product_3_zero_lists, deterministic(Product == [[]])) :-
		arrangements::cartesian_product([], Product).

	test(cartesian_product_3_lexicographic, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		arrangements::cartesian_product([[b,a],[2,1]], lexicographic, Product).

	test(cartesian_tuple_3_all, true(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		findall(Tuple, arrangements::cartesian_tuple([[a,b],[1,2]], Tuple), Product).

	test(cartesian_tuple_3_lexicographic_all, true(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		findall(Tuple, arrangements::cartesian_tuple([[b,a],[2,1]], lexicographic, Tuple), Product).

	test(count_arrangements_3_two_of_four, deterministic(Count == 16)) :-
		arrangements::count_arrangements(2, [a,b,c,d], Count).

	test(count_distinct_arrangements_3_with_duplicates, deterministic(Count == 4)) :-
		arrangements::count_distinct_arrangements(2, [a,a,b], Count).

	test(nth_arrangement_4_fourth, deterministic(Arrangement == [b,a])) :-
		arrangements::nth_arrangement(2, [a,b,c], 3, Arrangement).

	test(nth_distinct_arrangement_4_third, deterministic(Arrangement == [b,a])) :-
		arrangements::nth_distinct_arrangement(2, [a,a,b], 2, Arrangement).

	test(nth_arrangement_5_default_fourth, deterministic(Arrangement == [b,a])) :-
		arrangements::nth_arrangement(2, [a,b,c], default, 3, Arrangement).

	test(nth_arrangement_5_lexicographic_fourth, deterministic(Arrangement == [b,a])) :-
		arrangements::nth_arrangement(2, [c,b,a], lexicographic, 3, Arrangement).

	test(arrangement_index_4_sixth, deterministic(Index == 5)) :-
		arrangements::arrangement_index(2, [a,b,c], [b,c], Index).

	test(arrangement_index_5_default_sixth, deterministic(Index == 5)) :-
		arrangements::arrangement_index(2, [a,b,c], default, [b,c], Index).

	test(arrangement_index_5_lexicographic_fourth, deterministic(Index == 3)) :-
		arrangements::arrangement_index(2, [c,b,a], lexicographic, [b,a], Index).

	test(distinct_arrangement_index_4_third, deterministic(Index == 2)) :-
		arrangements::distinct_arrangement_index(2, [a,a,b], [b,a], Index).

	test(nth_arrangement_arrangement_index_three_of_two_roundtrip, deterministic(Index == 5)) :-
		arrangements::nth_arrangement(3, [a,b], 5, Arrangement),
		arrangements::arrangement_index(3, [a,b], Arrangement, Index).

	test(nth_arrangement_arrangement_index_roundtrip, deterministic(Index == 4)) :-
		arrangements::nth_arrangement(2, [a,b,c], 4, Arrangement),
		arrangements::arrangement_index(2, [a,b,c], Arrangement, Index).

	test(next_arrangement_3, deterministic(Next == [b,a])) :-
		arrangements::next_arrangement([b,a], [a,b], Next).

	test(next_arrangement_3_last, fail) :-
		arrangements::next_arrangement([a,b], [b,b], _).

	test(previous_arrangement_3, deterministic(Previous == [a,b])) :-
		arrangements::previous_arrangement([b,a], [b,a], Previous).

	test(previous_arrangement_3_first, fail) :-
		arrangements::previous_arrangement([a,b], [a,a], _).

	test(random_arrangement_3_length, true(list::length(Arrangement, 2))) :-
		arrangements::random_arrangement(2, [a,b,c,d], Arrangement).

	test(sample_arrangements_4_zero, deterministic(Samples == [])) :-
		arrangements::sample_arrangements(2, [a,b], 0, Samples).

	test(sample_arrangements_4_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), arrangements::arrangement(2, [a,b], Sample))))) :-
		arrangements::sample_arrangements(2, [a,b], 3, Samples).

	test(random_distinct_arrangement_3_exists, true(arrangements::distinct_arrangement(2, [a,a,b], Arrangement))) :-
		arrangements::random_distinct_arrangement(2, [a,a,b], Arrangement).

	test(sample_distinct_arrangements_4_zero, deterministic(Samples == [])) :-
		arrangements::sample_distinct_arrangements(2, [a,a,b], 0, Samples).

	test(sample_distinct_arrangements_4_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), arrangements::distinct_arrangement(2, [a,a,b], Sample))))) :-
		arrangements::sample_distinct_arrangements(2, [a,a,b], 3, Samples).

:- end_object.

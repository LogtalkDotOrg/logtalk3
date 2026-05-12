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
		comment is 'Unit tests for the "cartesian_products" library.'
	]).

	cover(cartesian_products).

	test(cartesian_product_2_two_lists, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		cartesian_products::cartesian_product([[a,b],[1,2]], Product).

	test(cartesian_product_2_zero_lists, deterministic(Product == [[]])) :-
		cartesian_products::cartesian_product([], Product).

	test(cartesian_product_2_empty_factor, deterministic(Product == [])) :-
		cartesian_products::cartesian_product([[a,b],[]], Product).

	test(cartesian_product_3_lexicographic, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		cartesian_products::cartesian_product([[b,a],[2,1]], lexicographic, Product).

	test(cartesian_product_3_default, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		cartesian_products::cartesian_product([[a,b],[1,2]], default, Product).

	test(cartesian_tuple_2_all, true(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		findall(Tuple, cartesian_products::cartesian_tuple([[a,b],[1,2]], Tuple), Product).

	test(cartesian_tuple_3_lexicographic_all, true(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		findall(Tuple, cartesian_products::cartesian_tuple([[b,a],[2,1]], lexicographic, Tuple), Product).

	test(distinct_cartesian_product_2_with_duplicates, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		cartesian_products::distinct_cartesian_product([[a,a,b],[1,1,2]], Product).

	test(distinct_cartesian_tuple_2_all, true(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		findall(Tuple, cartesian_products::distinct_cartesian_tuple([[a,a,b],[1,1,2]], Tuple), Product).

	test(distinct_cartesian_product_3_lexicographic, deterministic(Product == [[a,1],[a,2],[b,1],[b,2]])) :-
		cartesian_products::distinct_cartesian_product([[b,a,a],[2,1,1]], lexicographic, Product).

	test(count_cartesian_product_2_two_by_three, deterministic(Count == 6)) :-
		cartesian_products::count_cartesian_product([[a,b],[1,2,3]], Count).

	test(count_cartesian_product_2_empty_factor, deterministic(Count == 0)) :-
		cartesian_products::count_cartesian_product([[a,b],[]], Count).

	test(count_distinct_cartesian_product_2_with_duplicates, deterministic(Count == 4)) :-
		cartesian_products::count_distinct_cartesian_product([[a,a,b],[1,1,2]], Count).

	test(nth_cartesian_tuple_3_fourth, deterministic(Tuple == [b,1])) :-
		cartesian_products::nth_cartesian_tuple([[a,b],[1,2,3]], 3, Tuple).

	test(nth_cartesian_tuple_4_lexicographic_fourth, deterministic(Tuple == [b,1])) :-
		cartesian_products::nth_cartesian_tuple([[b,a],[3,1,2]], lexicographic, 3, Tuple).

	test(nth_cartesian_tuple_4_default_fourth, deterministic(Tuple == [b,1])) :-
		cartesian_products::nth_cartesian_tuple([[a,b],[1,2,3]], default, 3, Tuple).

	test(cartesian_tuple_index_3_fifth, deterministic(Index == 4)) :-
		cartesian_products::cartesian_tuple_index([[a,b],[1,2,3]], [b,2], Index).

	test(cartesian_tuple_index_4_lexicographic_fifth, deterministic(Index == 4)) :-
		cartesian_products::cartesian_tuple_index([[b,a],[3,1,2]], lexicographic, [b,2], Index).

	test(cartesian_tuple_index_4_default_fifth, deterministic(Index == 4)) :-
		cartesian_products::cartesian_tuple_index([[a,b],[1,2,3]], default, [b,2], Index).

	test(nth_distinct_cartesian_tuple_3_third, deterministic(Tuple == [b,1])) :-
		cartesian_products::nth_distinct_cartesian_tuple([[a,a,b],[1,1,2]], 2, Tuple).

	test(distinct_cartesian_tuple_index_3_fourth, deterministic(Index == 3)) :-
		cartesian_products::distinct_cartesian_tuple_index([[a,a,b],[1,1,2]], [b,2], Index).

	test(nth_cartesian_tuple_cartesian_tuple_index_roundtrip, deterministic(Index == 4)) :-
		cartesian_products::nth_cartesian_tuple([[a,b],[1,2,3]], 4, Tuple),
		cartesian_products::cartesian_tuple_index([[a,b],[1,2,3]], Tuple, Index).

	test(nth_distinct_cartesian_tuple_distinct_cartesian_tuple_index_roundtrip, deterministic(Index == 1)) :-
		cartesian_products::nth_distinct_cartesian_tuple([[a,a,b],[1,1,2]], 1, Tuple),
		cartesian_products::distinct_cartesian_tuple_index([[a,a,b],[1,1,2]], Tuple, Index).

	test(next_cartesian_tuple_3, deterministic(Next == [b,1])) :-
		cartesian_products::next_cartesian_tuple([[b,a],[2,1]], [a,2], Next).

	test(next_cartesian_tuple_3_last, fail) :-
		cartesian_products::next_cartesian_tuple([[a,b],[1,2]], [b,2], _).

	test(previous_cartesian_tuple_3, deterministic(Previous == [a,2])) :-
		cartesian_products::previous_cartesian_tuple([[b,a],[2,1]], [b,1], Previous).

	test(previous_cartesian_tuple_3_first, fail) :-
		cartesian_products::previous_cartesian_tuple([[a,b],[1,2]], [a,1], _).

	test(random_cartesian_tuple_2_length, true(list::length(Tuple, 2))) :-
		cartesian_products::random_cartesian_tuple([[a,b,c],[1,2]], Tuple).

	test(sample_cartesian_tuples_3_zero, deterministic(Samples == [])) :-
		cartesian_products::sample_cartesian_tuples([[a,b],[1,2]], 0, Samples).

	test(sample_cartesian_tuples_3_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), cartesian_products::cartesian_tuple([[a,b],[1,2]], Sample))))) :-
		cartesian_products::sample_cartesian_tuples([[a,b],[1,2]], 3, Samples).

	test(random_distinct_cartesian_tuple_2_exists, true(cartesian_products::distinct_cartesian_tuple([[a,a,b],[1,1,2]], Tuple))) :-
		cartesian_products::random_distinct_cartesian_tuple([[a,a,b],[1,1,2]], Tuple).

	test(sample_distinct_cartesian_tuples_3_zero, deterministic(Samples == [])) :-
		cartesian_products::sample_distinct_cartesian_tuples([[a,a,b],[1,1,2]], 0, Samples).

	test(sample_distinct_cartesian_tuples_3_count, true((list::length(Samples, 3), forall(list::member(Sample, Samples), cartesian_products::distinct_cartesian_tuple([[a,a,b],[1,1,2]], Sample))))) :-
		cartesian_products::sample_distinct_cartesian_tuples([[a,a,b],[1,1,2]], 3, Samples).

:- end_object.

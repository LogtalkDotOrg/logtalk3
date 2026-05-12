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


:- object(cartesian_products,
	implements(cartesian_products_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Implementation of Cartesian-product operations over lists.'
	]).

	:- uses(list, [
		length/2, member/2, msort/2, nth0/3, remove_duplicates/2, reverse/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	cartesian_product(Lists, Product) :-
		findall(Tuple, cartesian_tuple(Lists, Tuple), Product).

	cartesian_tuple([], []).
	cartesian_tuple([List| Lists], [Element| Tuple]) :-
		member(Element, List),
		cartesian_tuple(Lists, Tuple).

	cartesian_product(Lists, Order, Product) :-
		generator_order_source(Order, Lists, OrderedLists),
		cartesian_product(OrderedLists, Product).

	cartesian_tuple(Lists, Order, Tuple) :-
		generator_order_source(Order, Lists, OrderedLists),
		cartesian_tuple(OrderedLists, Tuple).

	distinct_cartesian_product(Lists, Product) :-
		distinct_cartesian_product(Lists, default, Product).

	distinct_cartesian_tuple(Lists, Tuple) :-
		distinct_cartesian_tuple(Lists, default, Tuple).

	distinct_cartesian_product(Lists, Order, Product) :-
		distinct_generator_order_source(Order, Lists, DistinctLists),
		cartesian_product(DistinctLists, Product).

	distinct_cartesian_tuple(Lists, Order, Tuple) :-
		distinct_generator_order_source(Order, Lists, DistinctLists),
		cartesian_tuple(DistinctLists, Tuple).

	count_cartesian_product(Lists, Count) :-
		count_cartesian_product(Lists, 1, Count).

	count_cartesian_product([], Count, Count) :-
		!.
	count_cartesian_product([List| Lists], Count0, Count) :-
		length(List, Length),
		Count1 is Count0 * Length,
		count_cartesian_product(Lists, Count1, Count).

	count_distinct_cartesian_product(Lists, Count) :-
		distinct_generator_order_source(default, Lists, DistinctLists),
		count_cartesian_product(DistinctLists, Count).

	nth_cartesian_tuple(Lists, Index, Tuple) :-
		Index >= 0,
		count_cartesian_product(Lists, Count),
		Index < Count,
		nth_cartesian_tuple_(Lists, Index, Tuple).

	nth_cartesian_tuple(Lists, default, Index, Tuple) :-
		!,
		nth_cartesian_tuple(Lists, Index, Tuple).
	nth_cartesian_tuple(Lists, lexicographic, Index, Tuple) :-
		generator_order_source(lexicographic, Lists, OrderedLists),
		nth_cartesian_tuple(OrderedLists, Index, Tuple).

	nth_cartesian_tuple_([], _, []) :-
		!.
	nth_cartesian_tuple_([List| Lists], Index, [Element| Tuple]) :-
		count_cartesian_product(Lists, BlockSize),
		Pos is Index // BlockSize,
		nth0(Pos, List, Element),
		Index1 is Index mod BlockSize,
		nth_cartesian_tuple_(Lists, Index1, Tuple).

	cartesian_tuple_index(Lists, Tuple, Index) :-
		cartesian_tuple_index_(Lists, Tuple, 0, Index).

	cartesian_tuple_index(Lists, default, Tuple, Index) :-
		!,
		cartesian_tuple_index(Lists, Tuple, Index).
	cartesian_tuple_index(Lists, lexicographic, Tuple, Index) :-
		generator_order_source(lexicographic, Lists, OrderedLists),
		cartesian_tuple_index(OrderedLists, Tuple, Index).

	nth_distinct_cartesian_tuple(Lists, Index, Tuple) :-
		distinct_generator_order_source(default, Lists, DistinctLists),
		nth_cartesian_tuple(DistinctLists, Index, Tuple).

	distinct_cartesian_tuple_index(Lists, Tuple, Index) :-
		distinct_generator_order_source(default, Lists, DistinctLists),
		cartesian_tuple_index(DistinctLists, Tuple, Index).

	cartesian_tuple_index_([], [], Index, Index) :-
		!.
	cartesian_tuple_index_([List| Lists], [Element| Tuple], Index0, Index) :-
		nth0(Pos, List, Element),
		!,
		count_cartesian_product(Lists, BlockSize),
		Index1 is Index0 + Pos * BlockSize,
		cartesian_tuple_index_(Lists, Tuple, Index1, Index).

	random_cartesian_tuple([], []) :-
		!.
	random_cartesian_tuple([List| Lists], [Element| Tuple]) :-
		length(List, Length),
		Length > 0,
		Last is Length - 1,
		random_between(0, Last, Index),
		nth0(Index, List, Element),
		random_cartesian_tuple(Lists, Tuple).

	sample_cartesian_tuples(Lists, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_cartesian_tuples(SampleCount, Lists, [], Samples0),
		reverse(Samples0, Samples).

	sample_cartesian_tuples(0, _, Samples, Samples) :-
		!.
	sample_cartesian_tuples(SampleCount, Lists, Samples0, Samples) :-
		SampleCount > 0,
		random_cartesian_tuple(Lists, Tuple),
		SampleCount1 is SampleCount - 1,
		sample_cartesian_tuples(SampleCount1, Lists, [Tuple| Samples0], Samples).

	random_distinct_cartesian_tuple(Lists, Tuple) :-
		distinct_generator_order_source(default, Lists, DistinctLists),
		random_cartesian_tuple(DistinctLists, Tuple).

	sample_distinct_cartesian_tuples(Lists, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_distinct_cartesian_tuples(SampleCount, Lists, [], Samples0),
		reverse(Samples0, Samples).

	sample_distinct_cartesian_tuples(0, _, Samples, Samples) :-
		!.
	sample_distinct_cartesian_tuples(SampleCount, Lists, Samples0, Samples) :-
		SampleCount > 0,
		random_distinct_cartesian_tuple(Lists, Tuple),
		SampleCount1 is SampleCount - 1,
		sample_distinct_cartesian_tuples(SampleCount1, Lists, [Tuple| Samples0], Samples).

	next_cartesian_tuple(Lists, Tuple, Next) :-
		canonical_lexicographic_factors(Lists, CanonicalLists),
		cartesian_tuple_index(CanonicalLists, Tuple, Index),
		NextIndex is Index + 1,
		nth_cartesian_tuple(CanonicalLists, NextIndex, Next).

	previous_cartesian_tuple(Lists, Tuple, Previous) :-
		canonical_lexicographic_factors(Lists, CanonicalLists),
		cartesian_tuple_index(CanonicalLists, Tuple, Index),
		Index > 0,
		PreviousIndex is Index - 1,
		nth_cartesian_tuple(CanonicalLists, PreviousIndex, Previous).

	canonical_lexicographic_factors(Lists, CanonicalLists) :-
		distinct_generator_order_source(lexicographic, Lists, CanonicalLists).

	generator_order_source(default, Lists, Lists).
	generator_order_source(lexicographic, Lists, OrderedLists) :-
		sort_factor_lists(Lists, OrderedLists).

	distinct_generator_order_source(default, Lists, DistinctLists) :-
		distinct_factor_lists(Lists, DistinctLists).
	distinct_generator_order_source(lexicographic, Lists, DistinctLists) :-
		sort_factor_lists(Lists, SortedLists),
		distinct_factor_lists(SortedLists, DistinctLists).

	sort_factor_lists([], []).
	sort_factor_lists([List| Lists], [Sorted| OrderedLists]) :-
		msort(List, Sorted),
		sort_factor_lists(Lists, OrderedLists).

	distinct_factor_lists([], []).
	distinct_factor_lists([List| Lists], [Distinct| DistinctLists]) :-
		remove_duplicates(List, Distinct),
		distinct_factor_lists(Lists, DistinctLists).

:- end_object.

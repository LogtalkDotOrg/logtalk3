%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests(_SetObject_),
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/05/21,
		comment is 'Unit tests for the "sets" library.',
		parnames is ['SetObject']
	]).

	:- uses(list, [msort/2]).

	cover(_SetObject_).

	% as_set/2 and as_list/2 tests

	test(set_as_set_2_01, deterministic(List == [])) :-
		_SetObject_::as_set([], Set),
		_SetObject_::as_list(Set, List).

	test(set_as_set_2_02, deterministic(Sorted == [1,2,3])) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::as_list(Set, List),
		msort(List, Sorted).

	% delete/3 tests

	test(set_delete_3_01, deterministic(Set1 == Set2)) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::delete(Set1, 1, Set2).

	test(set_delete_3_02, deterministic(Set1 == Set2)) :-
		_SetObject_::as_set([3,2,5], Set1),
		_SetObject_::delete(Set1, 4, Set2).

	test(set_delete_3_03, deterministic(Sorted == [3,4])) :-
		_SetObject_::as_set([3,2,4], Set1),
		_SetObject_::delete(Set1, 2, Set2),
		_SetObject_::as_list(Set2, List),
		msort(List, Sorted).

	% disjoint/2 tests

	test(set_disjoint_2_01, true) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::as_set([1,3,4], Set2),
		_SetObject_::disjoint(Set1, Set2).

	test(set_disjoint_2_02, true) :-
		_SetObject_::as_set([1,3,4], Set1),
		_SetObject_::as_set([], Set2),
		_SetObject_::disjoint(Set1, Set2).

	test(set_disjoint_2_03, fail) :-
		_SetObject_::as_set([3,6,2], Set1),
		_SetObject_::as_set([1,3,4], Set2),
		_SetObject_::disjoint(Set1, Set2).

	test(set_disjoint_2_04, true) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([4,6,5], Set2),
		_SetObject_::disjoint(Set1, Set2).

	% equal/2 tests

	test(set_equal_2_01, true) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::as_set([], Set2),
		_SetObject_::equal(Set1, Set2).

	test(set_equal_2_02, fail) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([1,1,1], Set2),
		_SetObject_::equal(Set1, Set2).

	test(set_equal_2_03, true) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([2,3,1], Set2),
		_SetObject_::equal(Set1, Set2).

	% empty/1 tests

	test(set_empty_1_01, true) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::empty(EmptySet).

	test(set_empty_1_02, fail) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::empty(Set).

	% insert/3 tests

	test(set_insert_3_01, deterministic(List == [1])) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::insert(Set1, 1, Set2),
		_SetObject_::as_list(Set2, List).

	test(set_insert_3_02, deterministic(Set1 == Set2)) :-
		_SetObject_::as_set([1], Set1),
		_SetObject_::insert(Set1, 1, Set2).

	test(set_insert_3_03, deterministic(Sorted == [1,2])) :-
		_SetObject_::as_set([1], Set1),
		_SetObject_::insert(Set1, 2, Set2),
		_SetObject_::as_list(Set2, List),
		msort(List, Sorted).

	% insert_all/3 tests

	test(set_insert_all_3_01, deterministic(Set1 == Set2)) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::insert_all([], Set1, Set2).

	test(set_insert_all_3_02, deterministic(List == [1])) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::insert_all([1,1,1], Set1, Set2),
		_SetObject_::as_list(Set2, List).

	test(set_insert_all_3_03, deterministic(Sorted == [1,2,3])) :-
		_SetObject_::as_set([1], Set1),
		_SetObject_::insert_all([3,1,2], Set1, Set2),
		_SetObject_::as_list(Set2, List),
		msort(List, Sorted).

	% intersect/2 tests

	test(set_intersect_2_01, fail) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::as_set([], Set2),
		_SetObject_::intersect(Set1, Set2).

	test(set_intersect_2_02, deterministic) :-
		_SetObject_::as_set([3,6,2], Set1),
		_SetObject_::as_set([1,6,4], Set2),
		_SetObject_::intersect(Set1, Set2).

	test(set_intersect_2_03, fail) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([4,6,5], Set2),
		_SetObject_::intersect(Set1, Set2).

	% intersection/3 tests

	test(set_intersection_3_01, deterministic) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::intersection(EmptySet, Set, Intersection),
		_SetObject_::empty(Intersection).

	test(set_intersection_3_02, deterministic) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::intersection(Set, EmptySet, Intersection),
		_SetObject_::empty(Intersection).

	test(set_intersection_3_03, deterministic(List == [3])) :-
		_SetObject_::as_set([3,5,2], Set1),
		_SetObject_::as_set([1,6,3], Set2),
		_SetObject_::intersection(Set1, Set2, Intersection),
		_SetObject_::as_list(Intersection, List).

	test(set_intersection_3_04, deterministic) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([4,6,5], Set2),
		_SetObject_::intersection(Set1, Set2, Intersection),
		_SetObject_::empty(Intersection).

	% intersection/4 tests

	test(set_intersection_4_01, deterministic) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::intersection(EmptySet, Set, Intersection, Difference),
		^^assertion(intersection, _SetObject_::empty(Intersection)),
		_SetObject_::as_list(Difference, List),
		msort(List, Sorted),
		^^assertion(difference, Sorted == [1,2,3]).

	test(set_intersection_4_02, deterministic) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::intersection(Set, EmptySet, Intersection, Difference),
		^^assertion(intersection, _SetObject_::empty(Intersection)),
		^^assertion(difference, _SetObject_::empty(Difference)).

	test(set_intersection_4_03, deterministic) :-
		_SetObject_::as_set([3,5,2], Set1),
		_SetObject_::as_set([1,6,3], Set2),
		_SetObject_::intersection(Set1, Set2, Intersection, Difference),
		_SetObject_::as_list(Intersection, List1),
		^^assertion(intersection, List1 == [3]),
		_SetObject_::as_list(Difference, List2),
		msort(List2, Sorted2),
		^^assertion(difference, Sorted2 == [1,6]).

	test(set_intersection_4_04, deterministic) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([4,6,5], Set2),
		_SetObject_::intersection(Set1, Set2, Intersection, Difference),
		_SetObject_::as_list(Intersection, List1),
		^^assertion(intersection, List1 == []),
		_SetObject_::as_list(Difference, List2),
		msort(List2, Sorted2),
		^^assertion(difference, Sorted2 == [4,5,6]).

	% length/2 tests

	test(set_length_2_01, deterministic(Length == 0)) :-
		_SetObject_::as_set([], Set),
		_SetObject_::length(Set, Length).

	test(set_length_2_02, deterministic(Length == 3)) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::length(Set, Length).

	% member/2 tests

	test(set_member_2_01, fail) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::member(_, EmptySet).

	test(set_member_2_02, deterministic(Elements == [1,2,3])) :-
		_SetObject_::as_set([3,1,2], Set),
		setof(Element, _SetObject_::member(Element, Set), Elements).

	% memberchk/2 tests

	test(set_memberchk_2_01, fail) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::memberchk(_, EmptySet).

	test(set_memberchk_2_02, deterministic) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::memberchk(2, Set).

	test(set_memberchk_2_03, fail) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::memberchk(4, Set).

	% powerset/2 tests

	test(set_powerset_2_01, deterministic(PowerSet == [[]])) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::powerset(EmptySet, PowerSet).

	test(set_powerset_2_02, deterministic(Sorted == [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]])) :-
		_SetObject_::as_set([3,1,2], EmptySet),
		_SetObject_::powerset(EmptySet, PowerSet),
		msort(PowerSet, Sorted).

	% product/3 tests

	test(set_product_3_01, deterministic(Product == EmptySet)) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::product(EmptySet, Set, Product).

	test(set_product_3_02, deterministic(Product == EmptySet)) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::product(Set, EmptySet, Product).

	test(set_product_3_03, deterministic(Sorted == [1-4,1-6,3-4,3-6])) :-
		_SetObject_::as_set([3,1], Set1),
		_SetObject_::as_set([4,6], Set2),
		_SetObject_::product(Set1, Set2, Product),
		_SetObject_::as_list(Product, List),
		msort(List, Sorted).

	% select/3 tests

	test(set_select_3_01, fail) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::select(_, EmptySet, _).

	test(set_select_3_02, fail) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::select(4, Set, _).

	test(set_select_3_03, true(Sorted == [1,3])) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::select(2, Set1, Set2),
		_SetObject_::as_list(Set2, List),
		msort(List, Sorted).

	test(set_select_3_04, deterministic(Elements == [1,2,3])) :-
		_SetObject_::as_set([3,1,2], Set1),
		setof(Element, Set2^(_SetObject_::select(Element, Set1, Set2)), Elements).

	% selectchk/3 tests

	test(set_selectchk_3_01, fail) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::selectchk(_, EmptySet, _).

	test(set_selectchk_3_02, fail) :-
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::selectchk(4, Set, _).

	test(set_selectchk_3_03, deterministic(Sorted == [1,3])) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::selectchk(2, Set1, Set2),
		_SetObject_::as_list(Set2, List),
		msort(List, Sorted).

	test(set_selectchk_3_04, deterministic) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::selectchk(_, Set1, _).

	% subset/2 tests

	test(set_subset_2_01, deterministic) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::subset(EmptySet, Set).

	test(set_subset_2_02, deterministic) :-
		_SetObject_::as_set([1], Set1),
		_SetObject_::as_set([3,1,2], Set2),
		_SetObject_::subset(Set1, Set2).

	test(set_subset_2_03, fail) :-
		_SetObject_::as_set([4,1], Set1),
		_SetObject_::as_set([3,1,2], Set2),
		_SetObject_::subset(Set1, Set2).

	% subtract/3 tests

	test(set_subtract_3_01, deterministic(Difference == EmptySet)) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::subtract(EmptySet, Set, Difference).

	test(set_subtract_3_02, deterministic(Difference == Set)) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::subtract(Set, EmptySet, Difference).

	test(set_subtract_3_03, deterministic(List == [4])) :-
		_SetObject_::as_set([1,4,1], Set1),
		_SetObject_::as_set([6,1,3], Set2),
		_SetObject_::subtract(Set1, Set2, Difference),
		_SetObject_::as_list(Difference, List).

	% symdiff/3 tests

	test(set_symdiff_3_01, deterministic(Difference == Set)) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::symdiff(EmptySet, Set, Difference).

	test(set_symdiff_3_02, deterministic(Difference == Set)) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::symdiff(Set, EmptySet, Difference).

	test(set_symdiff_3_03, deterministic(Sorted == [1,2,3,4,5,6])) :-
		_SetObject_::as_set([3,6,2], Set1),
		_SetObject_::as_set([4,1,5], Set2),
		_SetObject_::symdiff(Set1, Set2, Difference),
		_SetObject_::as_list(Difference, List),
		msort(List, Sorted).

	test(set_symdiff_3_04, deterministic(Sorted == [1,4])) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([4,2,3], Set2),
		_SetObject_::symdiff(Set1, Set2, Difference),
		_SetObject_::as_list(Difference, List),
		msort(List, Sorted).

	% union/3 tests

	test(set_union_3_01, deterministic(Sorted == [1,2,3])) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::union(EmptySet, Set, Union),
		_SetObject_::as_list(Union, List),
		msort(List, Sorted).

	test(set_union_3_02, deterministic(Sorted == [1,2,3])) :-
		_SetObject_::as_set([], EmptySet),
		_SetObject_::as_set([3,1,2], Set),
		_SetObject_::union(Set, EmptySet, Union),
		_SetObject_::as_list(Union, List),
		msort(List, Sorted).

	test(set_union_3_03, deterministic(Sorted == [1,2,3,4,5,6])) :-
		_SetObject_::as_set([3,6,2], Set1),
		_SetObject_::as_set([4,1,5,3], Set2),
		_SetObject_::union(Set1, Set2, Union),
		_SetObject_::as_list(Union, List),
		msort(List, Sorted).

	% union/4 tests

	test(set_union_4_01, deterministic) :-
		_SetObject_::as_set([], Set1),
		_SetObject_::as_set([1], Set2),
		_SetObject_::union(Set1, Set2, Union, Difference),
		^^assertion(union, Union == Set2),
		^^assertion(difference, Difference == Set2).

	test(set_union_4_02, deterministic) :-
		_SetObject_::as_set([1], Set1),
		_SetObject_::as_set([], Set2),
		_SetObject_::union(Set1, Set2, Union, Difference),
		^^assertion(union, Union == Set1),
		^^assertion(difference, _SetObject_::empty(Difference)).

	test(set_union_4_03, deterministic) :-
		_SetObject_::as_set([3,1,2], Set1),
		_SetObject_::as_set([1,6,3], Set2),
		_SetObject_::union(Set1, Set2, Union, Difference),
		_SetObject_::as_list(Union, List1),
		msort(List1, Sorted1),
		^^assertion(union, Sorted1 == [1,2,3,6]),
		_SetObject_::as_list(Difference, List2),
		^^assertion(difference, List2 == [6]).

	test(set_union_4_04, deterministic) :-
		_SetObject_::as_set([3,6,2], Set1),
		_SetObject_::as_set([4,1,5,3], Set2),
		_SetObject_::union(Set1, Set2, Union, Difference),
		_SetObject_::as_list(Union, List1),
		msort(List1, Sorted1),
		^^assertion(union, Sorted1 == [1,2,3,4,5,6]),
		_SetObject_::as_list(Difference, List2),
		msort(List2, Sorted),
		^^assertion(difference, Sorted == [1,4,5]).

	% valid/1 tests

	test(set_valid_1_01, deterministic) :-
		_SetObject_::as_set([], Set),
		_SetObject_::valid(Set).

	test(set_valid_1_02, deterministic) :-
		_SetObject_::as_set([1,2,1,2,1], Set),
		_SetObject_::valid(Set).

	% new/1 tests

	test(set_new_1_01, deterministic) :-
		_SetObject_::new(Set),
		_SetObject_::valid(Set).

:- end_object.

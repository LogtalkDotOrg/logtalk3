%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2019-05-27,
		comment is 'Unit tests for the "sets" library.',
		parnames is ['SetObject']
	]).

	:- uses(_SetObject_, [
		as_set/2, as_list/2,
		delete/3, insert/3, insert_all/3,
		disjoint/2, equal/2, empty/1, intersect/2, intersection/3, intersection/4,
		size/2, member/2, memberchk/2, powerset/2, product/3, select/3, selectchk/3,
		subset/2, subtract/3, symdiff/3, union/3, union/4, valid/1, new/1
	]).

	:- uses(lgtunit, [assertion/2]).
	:- uses(list, [msort/2]).

	cover(_SetObject_).

	% as_set/2 and as_list/2 tests

	test(set_as_set_2_01, deterministic(List == [])) :-
		as_set([], Set),
		as_list(Set, List).

	test(set_as_set_2_02, deterministic(Sorted == [1,2,3])) :-
		as_set([3,1,2], Set),
		as_list(Set, List),
		msort(List, Sorted).

	% delete/3 tests

	test(set_delete_3_01, deterministic(Set1 == Set2)) :-
		as_set([], Set1),
		delete(Set1, 1, Set2).

	test(set_delete_3_02, deterministic(Set1 == Set2)) :-
		as_set([3,2,5], Set1),
		delete(Set1, 4, Set2).

	test(set_delete_3_03, deterministic(Sorted == [3,4])) :-
		as_set([3,2,4], Set1),
		delete(Set1, 2, Set2),
		as_list(Set2, List),
		msort(List, Sorted).

	% disjoint/2 tests

	test(set_disjoint_2_01, true) :-
		as_set([], Set1),
		as_set([1,3,4], Set2),
		disjoint(Set1, Set2).

	test(set_disjoint_2_02, true) :-
		as_set([1,3,4], Set1),
		as_set([], Set2),
		disjoint(Set1, Set2).

	test(set_disjoint_2_03, fail) :-
		as_set([3,6,2], Set1),
		as_set([1,3,4], Set2),
		disjoint(Set1, Set2).

	test(set_disjoint_2_04, true) :-
		as_set([3,1,2], Set1),
		as_set([4,6,5], Set2),
		disjoint(Set1, Set2).

	% equal/2 tests

	test(set_equal_2_01, true) :-
		as_set([], Set1),
		as_set([], Set2),
		equal(Set1, Set2).

	test(set_equal_2_02, fail) :-
		as_set([3,1,2], Set1),
		as_set([1,1,1], Set2),
		equal(Set1, Set2).

	test(set_equal_2_03, true) :-
		as_set([3,1,2], Set1),
		as_set([2,3,1], Set2),
		equal(Set1, Set2).

	% empty/1 tests

	test(set_empty_1_01, true) :-
		as_set([], EmptySet),
		empty(EmptySet).

	test(set_empty_1_02, fail) :-
		as_set([3,1,2], Set),
		empty(Set).

	% insert/3 tests

	test(set_insert_3_01, deterministic(List == [1])) :-
		as_set([], Set1),
		insert(Set1, 1, Set2),
		as_list(Set2, List).

	test(set_insert_3_02, deterministic(Set1 == Set2)) :-
		as_set([1], Set1),
		insert(Set1, 1, Set2).

	test(set_insert_3_03, deterministic(Sorted == [1,2])) :-
		as_set([1], Set1),
		insert(Set1, 2, Set2),
		as_list(Set2, List),
		msort(List, Sorted).

	% insert_all/3 tests

	test(set_insert_all_3_01, deterministic(Set1 == Set2)) :-
		as_set([], Set1),
		insert_all([], Set1, Set2).

	test(set_insert_all_3_02, deterministic(List == [1])) :-
		as_set([], Set1),
		insert_all([1,1,1], Set1, Set2),
		as_list(Set2, List).

	test(set_insert_all_3_03, deterministic(Sorted == [1,2,3])) :-
		as_set([1], Set1),
		insert_all([3,1,2], Set1, Set2),
		as_list(Set2, List),
		msort(List, Sorted).

	% intersect/2 tests

	test(set_intersect_2_01, fail) :-
		as_set([], Set1),
		as_set([], Set2),
		intersect(Set1, Set2).

	test(set_intersect_2_02, deterministic) :-
		as_set([3,6,2], Set1),
		as_set([1,6,4], Set2),
		intersect(Set1, Set2).

	test(set_intersect_2_03, fail) :-
		as_set([3,1,2], Set1),
		as_set([4,6,5], Set2),
		intersect(Set1, Set2).

	% intersection/3 tests

	test(set_intersection_3_01, deterministic) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		intersection(EmptySet, Set, Intersection),
		empty(Intersection).

	test(set_intersection_3_02, deterministic) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		intersection(Set, EmptySet, Intersection),
		empty(Intersection).

	test(set_intersection_3_03, deterministic(List == [3])) :-
		as_set([3,5,2], Set1),
		as_set([1,6,3], Set2),
		intersection(Set1, Set2, Intersection),
		as_list(Intersection, List).

	test(set_intersection_3_04, deterministic) :-
		as_set([3,1,2], Set1),
		as_set([4,6,5], Set2),
		intersection(Set1, Set2, Intersection),
		empty(Intersection).

	% intersection/4 tests

	test(set_intersection_4_01, deterministic) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		intersection(EmptySet, Set, Intersection, Difference),
		assertion(intersection, empty(Intersection)),
		as_list(Difference, List),
		msort(List, Sorted),
		assertion(difference, Sorted == [1,2,3]).

	test(set_intersection_4_02, deterministic) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		intersection(Set, EmptySet, Intersection, Difference),
		assertion(intersection, empty(Intersection)),
		assertion(difference,   empty(Difference)).

	test(set_intersection_4_03, deterministic) :-
		as_set([3,5,2], Set1),
		as_set([1,6,3], Set2),
		intersection(Set1, Set2, Intersection, Difference),
		as_list(Intersection, List1),
		assertion(intersection, List1 == [3]),
		as_list(Difference, List2),
		msort(List2, Sorted2),
		assertion(difference, Sorted2 == [1,6]).

	test(set_intersection_4_04, deterministic) :-
		as_set([3,1,2], Set1),
		as_set([4,6,5], Set2),
		intersection(Set1, Set2, Intersection, Difference),
		as_list(Intersection, List1),
		assertion(intersection, List1 == []),
		as_list(Difference, List2),
		msort(List2, Sorted2),
		assertion(difference, Sorted2 == [4,5,6]).

	% size/2 tests

	test(set_size_2_01, deterministic(Size == 0)) :-
		as_set([], Set),
		size(Set, Size).

	test(set_size_2_02, deterministic(Size == 3)) :-
		as_set([3,1,2], Set),
		size(Set, Size).

	% member/2 tests

	test(set_member_2_01, fail) :-
		as_set([], EmptySet),
		member(_, EmptySet).

	test(set_member_2_02, deterministic(Elements == [1,2,3])) :-
		as_set([3,1,2], Set),
		setof(Element, member(Element, Set), Elements).

	% memberchk/2 tests

	test(set_memberchk_2_01, fail) :-
		as_set([], EmptySet),
		memberchk(_, EmptySet).

	test(set_memberchk_2_02, deterministic) :-
		as_set([3,1,2], Set),
		memberchk(2, Set).

	test(set_memberchk_2_03, fail) :-
		as_set([3,1,2], Set),
		memberchk(4, Set).

	% powerset/2 tests

	test(set_powerset_2_01, deterministic(PowerSet == [[]])) :-
		as_set([], EmptySet),
		powerset(EmptySet, PowerSet).

	test(set_powerset_2_02, deterministic(Sorted == [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]])) :-
		as_set([3,1,2], EmptySet),
		powerset(EmptySet, PowerSet),
		msort(PowerSet, Sorted).

	% product/3 tests

	test(set_product_3_01, deterministic(Product == EmptySet)) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		product(EmptySet, Set, Product).

	test(set_product_3_02, deterministic(Product == EmptySet)) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		product(Set, EmptySet, Product).

	test(set_product_3_03, deterministic(Sorted == [1-4,1-6,3-4,3-6])) :-
		as_set([3,1], Set1),
		as_set([4,6], Set2),
		product(Set1, Set2, Product),
		as_list(Product, List),
		msort(List, Sorted).

	% select/3 tests

	test(set_select_3_01, fail) :-
		as_set([], EmptySet),
		select(_, EmptySet, _).

	test(set_select_3_02, fail) :-
		as_set([3,1,2], Set),
		select(4, Set, _).

	test(set_select_3_03, true(Sorted == [1,3])) :-
		as_set([3,1,2], Set1),
		select(2, Set1, Set2),
		as_list(Set2, List),
		msort(List, Sorted).

	test(set_select_3_04, deterministic(Elements == [1,2,3])) :-
		as_set([3,1,2], Set1),
		setof(Element, Set2^(select(Element, Set1, Set2)), Elements).

	% selectchk/3 tests

	test(set_selectchk_3_01, fail) :-
		as_set([], EmptySet),
		selectchk(_, EmptySet, _).

	test(set_selectchk_3_02, fail) :-
		as_set([3,1,2], Set),
		selectchk(4, Set, _).

	test(set_selectchk_3_03, deterministic(Sorted == [1,3])) :-
		as_set([3,1,2], Set1),
		selectchk(2, Set1, Set2),
		as_list(Set2, List),
		msort(List, Sorted).

	test(set_selectchk_3_04, deterministic) :-
		as_set([3,1,2], Set1),
		selectchk(_, Set1, _).

	% subset/2 tests

	test(set_subset_2_01, deterministic) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		subset(EmptySet, Set).

	test(set_subset_2_02, deterministic) :-
		as_set([1], Set1),
		as_set([3,1,2], Set2),
		subset(Set1, Set2).

	test(set_subset_2_03, fail) :-
		as_set([4,1], Set1),
		as_set([3,1,2], Set2),
		subset(Set1, Set2).

	% subtract/3 tests

	test(set_subtract_3_01, deterministic(Difference == EmptySet)) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		subtract(EmptySet, Set, Difference).

	test(set_subtract_3_02, deterministic(Difference == Set)) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		subtract(Set, EmptySet, Difference).

	test(set_subtract_3_03, deterministic(List == [4])) :-
		as_set([1,4,1], Set1),
		as_set([6,1,3], Set2),
		subtract(Set1, Set2, Difference),
		as_list(Difference, List).

	% symdiff/3 tests

	test(set_symdiff_3_01, deterministic(Difference == Set)) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		symdiff(EmptySet, Set, Difference).

	test(set_symdiff_3_02, deterministic(Difference == Set)) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		symdiff(Set, EmptySet, Difference).

	test(set_symdiff_3_03, deterministic(Sorted == [1,2,3,4,5,6])) :-
		as_set([3,6,2], Set1),
		as_set([4,1,5], Set2),
		symdiff(Set1, Set2, Difference),
		as_list(Difference, List),
		msort(List, Sorted).

	test(set_symdiff_3_04, deterministic(Sorted == [1,4])) :-
		as_set([3,1,2], Set1),
		as_set([4,2,3], Set2),
		symdiff(Set1, Set2, Difference),
		as_list(Difference, List),
		msort(List, Sorted).

	% union/3 tests

	test(set_union_3_01, deterministic(Sorted == [1,2,3])) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		union(EmptySet, Set, Union),
		as_list(Union, List),
		msort(List, Sorted).

	test(set_union_3_02, deterministic(Sorted == [1,2,3])) :-
		as_set([], EmptySet),
		as_set([3,1,2], Set),
		union(Set, EmptySet, Union),
		as_list(Union, List),
		msort(List, Sorted).

	test(set_union_3_03, deterministic(Sorted == [1,2,3,4,5,6])) :-
		as_set([3,6,2], Set1),
		as_set([4,1,5,3], Set2),
		union(Set1, Set2, Union),
		as_list(Union, List),
		msort(List, Sorted).

	% union/4 tests

	test(set_union_4_01, deterministic) :-
		as_set([], Set1),
		as_set([1], Set2),
		union(Set1, Set2, Union, Difference),
		assertion(union, Union == Set2),
		assertion(difference, Difference == Set2).

	test(set_union_4_02, deterministic) :-
		as_set([1], Set1),
		as_set([], Set2),
		union(Set1, Set2, Union, Difference),
		assertion(union, Union == Set1),
		assertion(difference, empty(Difference)).

	test(set_union_4_03, deterministic) :-
		as_set([3,1,2], Set1),
		as_set([1,6,3], Set2),
		union(Set1, Set2, Union, Difference),
		as_list(Union, List1),
		msort(List1, Sorted1),
		assertion(union, Sorted1 == [1,2,3,6]),
		as_list(Difference, List2),
		assertion(difference, List2 == [6]).

	test(set_union_4_04, deterministic) :-
		as_set([3,6,2], Set1),
		as_set([4,1,5,3], Set2),
		union(Set1, Set2, Union, Difference),
		as_list(Union, List1),
		msort(List1, Sorted1),
		assertion(union, Sorted1 == [1,2,3,4,5,6]),
		as_list(Difference, List2),
		msort(List2, Sorted),
		assertion(difference, Sorted == [1,4,5]).

	% valid/1 tests

	test(set_valid_1_01, deterministic) :-
		as_set([], Set),
		valid(Set).

	test(set_valid_1_02, deterministic) :-
		as_set([1,2,1,2,1], Set),
		valid(Set).

	% new/1 tests

	test(set_new_1_01, deterministic) :-
		new(Set),
		valid(Set).

:- end_object.

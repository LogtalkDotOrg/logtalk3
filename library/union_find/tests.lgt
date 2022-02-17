%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2022 José Antonio Riaza Valverde <riazavalverde@gmail.com>
%  Copyright 2022 Paulo Moura <pmoura@logtalk.org>
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
		author is 'José Antonio Riaza Valverde and Paulo Moura',
		date is 2022-02-17,
		comment is 'Unit tests for the "union_find" library.'
	]).

	:- uses(union_find, [
		new/2,
		disjoint_sets/2,
		union/4, union_all/3,
		find/4, find/5,
		make_set/3
	]).

	cover(union_find).

	% new/2 tests

	test(union_find_new_2_empty, true(nonvar(UnionFind))) :-
		new([], UnionFind).

	test(union_find_new_2_set_distinct, true(nonvar(UnionFind))) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind).

	test(union_find_new_2_set_repeated, true(nonvar(UnionFind))) :-
		new([1,2,3,1,2,3,1,2,3], UnionFind).

	% disjoint_sets/2 tests

	test(union_find_disjoint_sets_2_set_empty, true(Sets == [])) :-
		new([], UnionFind),
		disjoint_sets(UnionFind, Sets).

	test(union_find_disjoint_sets_2_set_distinct, true(Sets == [[1],[2],[3]])) :-
		new([1,2,3], UnionFind),
		disjoint_sets(UnionFind, Sets).

	test(union_find_disjoint_sets_2_set_repeated, true(Sets == [[1],[2],[3]])) :-
		new([1,2,3,1,2,3,1,2,3], UnionFind),
		disjoint_sets(UnionFind, Sets).

	% union/4 tests

	test(union_find_union_4_existing, true(Sets == [[1],[2],[3,5],[4],[6],[7]])) :-
		new([1,2,3,4,5,6,7], UnionFind),
		union(UnionFind, 3, 5, NewUnionFind),
		disjoint_sets(NewUnionFind, Sets).

	test(union_find_union_4_non_existing, false) :-
		new([1,2,3,4,5,6,7], UnionFind),
		union(UnionFind, 7, 8, _).

	% union_all/3 tests

	test(union_find_union_all_3_empty, true(UnionFind == NewUnionFind)) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [], NewUnionFind).

	test(union_find_union_all_3_non_empty, true(Sets == [[1],[2],[3,4,5,6],[7],[8],[9]])) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		disjoint_sets(NewUnionFind, Sets).

	% find/4 tests

	test(union_find_find_4_existing_01, true(Root == 3)) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		find(NewUnionFind, 3, Root, _).

	test(union_find_find_4_existing_02, true(Root == 3)) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		find(NewUnionFind, 5, Root, _).

	test(union_find_find_4_non_existing, false) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		find(NewUnionFind, 0, _, _).

	% find/5 tests

	test(union_find_find_5_existing_01, true(Root-Rank == 3-1)) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		find(NewUnionFind, 3, Root, Rank, _).

	test(union_find_find_5_existing_02, true(Root-Rank == 3-1)) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		find(NewUnionFind, 5, Root, Rank, _).

	test(union_find_find_5_non_existing, false) :-
		new([1,2,3,4,5,6,7,8,9], UnionFind),
		union_all(UnionFind, [3,4,5,6], NewUnionFind),
		find(NewUnionFind, 0, _, _, _).

	% make_set/3 tests

	test(union_find_make_set_3_empty, true(Root-Rank == 1-0)) :-
		new([], UnionFind),
		make_set(UnionFind, 1, NewUnionFind),
		find(NewUnionFind, 1, Root, Rank, _).

	test(union_find_make_set_3_non_empty, true(Root-Rank == 3-0)) :-
		new([1,2,4], UnionFind),
		make_set(UnionFind, 3, NewUnionFind),
		find(NewUnionFind, 3, Root, Rank, _).

:- end_object.

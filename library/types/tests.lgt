%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.7,
		author is 'Paulo Moura',
		date is 2020-02-01,
		comment is 'Unit tests for the "types" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	test(atom_replace_sub_atom_4_01, true(Atom == axc)) :-
		atom::replace_sub_atom(b, x, abc, Atom).

	test(atom_replace_sub_atom_4_02, true(Atom == abc)) :-
		atom::replace_sub_atom(d, x, abc, Atom).

	test(atom_replace_sub_atom_4_03, true(Atom == axcxd)) :-
		atom::replace_sub_atom(b, x, abcbd, Atom).

	test(atom_replace_sub_atom_4_04, true(Atom == ac)) :-
		atom::replace_sub_atom(b, '', abc, Atom).

	test(atom_replace_sub_atom_4_05, true(Atom == abc)) :-
		atom::replace_sub_atom('', 'x', abc, Atom).

	test(atom_split_3_01, true(Atoms == [a,b,c])) :-
		atom::split(abc, '', Atoms).

	test(atom_split_3_02, true(Atoms == [a,c])) :-
		atom::split(abc, b, Atoms).

	test(atom_split_3_03, true(Atoms == [abc])) :-
		atom::split(abc, d, Atoms).

	test(atom_split_3_04, true(Atoms == [''])) :-
		atom::split('', d, Atoms).

	test(list_sort_4_01, true(Sorted == [1,2,3])) :-
		list::sort(0, @<, [3,2,1], Sorted).

	test(list_sort_4_02, true(Sorted == [3,2,1])) :-
		list::sort(0, @>, [1,2,3], Sorted).

	test(list_sort_4_03, true(Sorted == [1,2,2,3,3])) :-
		list::sort(0, @=<, [3,2,1,2,3], Sorted).

	test(list_sort_4_04, true(Sorted == [3,2,2,1,1])) :-
		list::sort(0, @>=, [1,2,3,2,1], Sorted).

	test(list_sort_4_05, true(Sorted == [1-c,2-b,3-a])) :-
		list::sort(1, @<, [3-a,2-b,1-c], Sorted).

	test(list_sort_4_06, true(Sorted == [3-a,2-b,1-c])) :-
		list::sort(1, @>, [1-c,2-b,3-a], Sorted).

	test(list_sort_4_07, true(Sorted == [1-c,2-b,2-b,3-a,3-a])) :-
		list::sort(1, @=<, [3-a,2-b,1-c,2-b,3-a], Sorted).

	test(list_sort_4_08, true(Sorted == [3-a,2-b,2-b,1-c,1-c])) :-
		list::sort(1, @>=, [1-c,2-b,3-a,2-b,1-c], Sorted).

	test(list_sort_4_09, true(Sorted == [3-a,2-b,1-c])) :-
		list::sort(2, @<, [3-a,2-b,1-c], Sorted).

	test(list_sort_4_10, true(Sorted == [1-c,2-b,3-a])) :-
		list::sort(2, @>, [1-c,2-b,3-a], Sorted).

	test(list_sort_4_11, true(Sorted == [3-a,3-a,2-b,2-b,1-c])) :-
		list::sort(2, @=<, [3-a,2-b,1-c,2-b,3-a], Sorted).

	test(list_sort_4_12, true(Sorted == [1-c,1-c,2-b,2-b,3-a])) :-
		list::sort(2, @>=, [1-c,2-b,3-a,2-b,1-c], Sorted).

	test(numberlist_euclidean_norm_2_01, true(Norm =~= 119.0)) :-
		numberlist::euclidean_norm([35,36,46,68,70], Norm).

	test(numberlist_sum_2_01, true(Sum == 15)) :-
		numberlist::sum([1,2,3,4,5], Sum).

	test(numberlist_normalize_range_2_01, true(Normalized =~= [1.0,1.0,0.5,0.0])) :-
		numberlist::normalize_range([2,2,1,0], Normalized).

	test(numberlist_normalize_range_4_01, true(Normalized =~= [100.0,100.0,50.0,0.0])) :-
		numberlist::normalize_range([2,2,1,0], 0, 100, Normalized).

	test(numberlist_normalize_unit_2_01, true(Norm =~= 1.0)) :-
		numberlist::normalize_unit([2,2,1,0], Normalized),
		numberlist::euclidean_norm(Normalized, Norm).

	test(numberlist_normalize_scalar_2_01, true(Sum =~= 1.0)) :-
		numberlist::normalize_scalar([2,2,1,0], Normalized),
		numberlist::sum(Normalized, Sum).

	test(numberlist_rescale_3_01, true(Rescaled == [2,4,6,8])) :-
		numberlist::rescale([1,2,3,4], 2, Rescaled).

:- end_object.

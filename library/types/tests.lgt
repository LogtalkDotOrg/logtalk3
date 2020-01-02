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
		version is 0.6,
		author is 'Paulo Moura',
		date is 2019/11/28,
		comment is 'Unit tests for the "types" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	test(atom_replace_sub_atom_4_01) :-
		atom::replace_sub_atom(b, x, abc, Atom),
		Atom == axc.

	test(atom_replace_sub_atom_4_02) :-
		atom::replace_sub_atom(d, x, abc, Atom),
		Atom == abc.

	test(atom_replace_sub_atom_4_03) :-
		atom::replace_sub_atom(b, x, abcbd, Atom),
		Atom == axcxd.

	test(atom_replace_sub_atom_4_04) :-
		atom::replace_sub_atom(b, '', abc, Atom),
		Atom == ac.

	test(atom_replace_sub_atom_4_05) :-
		atom::replace_sub_atom('', 'x', abc, Atom),
		Atom == abc.

	test(atom_split_3_01) :-
		atom::split(abc, '', Atoms),
		Atoms == [a,b,c].

	test(atom_split_3_02) :-
		atom::split(abc, b, Atoms),
		Atoms == [a,c].

	test(atom_split_3_03) :-
		atom::split(abc, d, Atoms),
		Atoms == [abc].

	test(atom_split_3_04) :-
		atom::split('', d, Atoms),
		Atoms == [''].

	test(list_sort_4_01) :-
		list::sort(0, @<, [3,2,1], Sorted),
		Sorted == [1,2,3].

	test(list_sort_4_02) :-
		list::sort(0, @>, [1,2,3], Sorted),
		Sorted == [3,2,1].

	test(list_sort_4_03) :-
		list::sort(0, @=<, [3,2,1,2,3], Sorted),
		Sorted == [1,2,2,3,3].

	test(list_sort_4_04) :-
		list::sort(0, @>=, [1,2,3,2,1], Sorted),
		Sorted == [3,2,2,1,1].

	test(list_sort_4_05) :-
		list::sort(1, @<, [3-a,2-b,1-c], Sorted),
		Sorted == [1-c,2-b,3-a].

	test(list_sort_4_06) :-
		list::sort(1, @>, [1-c,2-b,3-a], Sorted),
		Sorted == [3-a,2-b,1-c].

	test(list_sort_4_07) :-
		list::sort(1, @=<, [3-a,2-b,1-c,2-b,3-a], Sorted),
		Sorted == [1-c,2-b,2-b,3-a,3-a].

	test(list_sort_4_08) :-
		list::sort(1, @>=, [1-c,2-b,3-a,2-b,1-c], Sorted),
		Sorted == [3-a,2-b,2-b,1-c,1-c].

	test(list_sort_4_09) :-
		list::sort(2, @<, [3-a,2-b,1-c], Sorted),
		Sorted == [3-a,2-b,1-c].

	test(list_sort_4_10) :-
		list::sort(2, @>, [1-c,2-b,3-a], Sorted),
		Sorted == [1-c,2-b,3-a].

	test(list_sort_4_11) :-
		list::sort(2, @=<, [3-a,2-b,1-c,2-b,3-a], Sorted),
		Sorted == [3-a,3-a,2-b,2-b,1-c].

	test(list_sort_4_12) :-
		list::sort(2, @>=, [1-c,2-b,3-a,2-b,1-c], Sorted),
		Sorted == [1-c,1-c,2-b,2-b,3-a].

	test(numberlist_euclidean_norm_2_01) :-
		numberlist::euclidean_norm([35,36,46,68,70], Norm),
		Norm =~= 119.0.

	test(numberlist_sum_2_01) :-
		numberlist::sum([1,2,3,4,5], Sum),
		Sum == 15.

	test(numberlist_normalize_range_2_01) :-
		numberlist::normalize_range([2,2,1,0], Normalized),
		Normalized =~= [1.0,1.0,0.5,0.0].

	test(numberlist_normalize_range_4_01) :-
		numberlist::normalize_range([2,2,1,0], 0, 100, Normalized),
		Normalized =~= [100.0,100.0,50.0,0.0].

	test(numberlist_normalize_unit_2_01) :-
		numberlist::normalize_unit([2,2,1,0], Normalized),
		numberlist::euclidean_norm(Normalized, Norm), Norm =~= 1.0.

	test(numberlist_normalize_scalar_2_01) :-
		numberlist::normalize_scalar([2,2,1,0], Normalized),
		numberlist::sum(Normalized, Sum), Sum =~= 1.0.

	test(numberlist_rescale_3_01) :-
		numberlist::rescale([1,2,3,4], 2, Rescaled),
		Rescaled == [2,4,6,8].

:- end_object.

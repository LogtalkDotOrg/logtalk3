%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:13:0,
		author is 'Paulo Moura',
		date is 2022-12-13,
		comment is 'Unit tests for the "types" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	test(type_checking_deterministic, true) :-
		forall(
			(	type::type(Type),
				ground(Type),
				type::arbitrary(Type, Value)
			),
			^^assertion(Type, lgtunit::deterministic(type::valid(Type,Value)))
		).

	test(term_numbervars_3_01, true(ground(Term))) :-
		Term = a(_,_,_),
		term::numbervars(Term, 0, _).

	test(term_numbervars_3_02, true(N == 3)) :-
		Term = a(_,_,_),
		term::numbervars(Term, 0, N).

	test(term_numbervars_3_03, true(N == 2)) :-
		Term = a(A,_,A),
		term::numbervars(Term, 0, N).

	test(term_numbervars_1_01, true(ground(Term))) :-
		Term = a(_,_,_),
		term::numbervars(Term).

	test(term_varnumbers_3_01, variant(Term, a(A,_,A))) :-
		term::varnumbers(a('$VAR'(0),'$VAR'(1),'$VAR'(0)), 0, Term).

	test(term_varnumbers_3_02, variant(Term, a('$VAR'(0),_,'$VAR'(0)))) :-
		term::varnumbers(a('$VAR'(0),'$VAR'(1),'$VAR'(0)), 1, Term).

	test(term_varnumbers_2_01, variant(Term, a(A,_,A))) :-
		term::varnumbers(a('$VAR'(0),'$VAR'(1),'$VAR'(0)), Term).

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

	test(integer_sequence_3_01, false) :-
		integer::sequence(5, 3, _).

	test(integer_sequence_3_02, true(Sequence == [1,2,3])) :-
		integer::sequence(1, 3, Sequence).

	test(integer_sequence_3_03, true(Sequence == [-3,-2,-1])) :-
		integer::sequence(-3, -1, Sequence).

	test(integer_sequence_4_01, false) :-
		integer::sequence(5, 3, 2, _).

	test(integer_sequence_4_02, false) :-
		integer::sequence(1, 3, 0, _).

	test(integer_sequence_4_03, false) :-
		integer::sequence(1, 3, -2, _).

	test(integer_sequence_4_04, true(Sequence == [1,5])) :-
		integer::sequence(1, 7, 4, Sequence).

	test(integer_sequence_4_05, true(Sequence == [10,20,30,40,50,60,70,80,90,100])) :-
		integer::sequence(10, 100, 10, Sequence).

	test(integer_sequence_4_06, true(Sequence == [-100,-90,-80,-70,-60,-50,-40,-30,-20,-10])) :-
		integer::sequence(-100, -10, 10, Sequence).

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

	test(numberlist_median_2_01, fail) :-
		numberlist::median([], _).

	test(numberlist_median_2_02, deterministic(Median =~= 41.0)) :-
		numberlist::median([35,36,46,68], Median).

	test(numberlist_median_2_03, deterministic(Median =~= 46.0)) :-
		numberlist::median([35,36,46,68,70], Median).

	test(numberlist_modes_2_01, fail) :-
		numberlist::modes([], _).

	test(numberlist_modes_2_02, deterministic(Modes == [1])) :-
		numberlist::modes([1,0,1,2,2,1], Modes).

	test(numberlist_modes_2_03, deterministic(Modes == [2,3])) :-
		numberlist::modes([1,2,1,2,2,3,3,4,3], Modes).

	test(numberlist_modes_2_04, deterministic(Modes == [1,2,3])) :-
		numberlist::modes([1,2,3], Modes).

	test(numberlist_euclidean_norm_2_01, true(Norm =~= 119.0)) :-
		numberlist::euclidean_norm([35,36,46,68,70], Norm).

	test(numberlist_sum_2_01, true(Sum == 15)) :-
		numberlist::sum([1,2,3,4,5], Sum).

	test(numberlist_product_2_01, true(Product == 120)) :-
		numberlist::product([1,2,3,4,5], Product).

	test(numberlist_min_2_01, true(Min == 1)) :-
		numberlist::min([1,2,3,4,5], Min).

	test(numberlist_max_2_01, true(Max == 5)) :-
		numberlist::max([1,2,3,4,5], Max).

	test(numberlist_min_max_2_01, true(Min-Max == 1-5)) :-
		numberlist::min_max([1,2,3,4,5], Min, Max).

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

	test(pairs_keys_values_3_01, true(Keys-Values == [a,b,c]-[1,2,3])) :-
		pairs::keys_values([a-1,b-2,c-3], Keys, Values).

	test(pairs_keys_2_01, true(Keys == [a,b,c])) :-
		pairs::keys([a-1,b-2,c-3], Keys).

	test(pairs_key_2_01, true(Keys == [a,b,c])) :-
		findall(Key, pairs::key([a-1,b-2,c-3], Key), Keys).

	test(pairs_key_2_02, false) :-
		pairs::key([a-1,b-2,c-3], d).

	test(pairs_values_2_01, true(Values == [1,2,3])) :-
		pairs::values([a-1,b-2,c-3], Values).

	test(pairs_value_3_01, true(Value == 2)) :-
		pairs::value([a-1,b-2,c-3], b, Value).

	test(pairs_value_3_02, true(Value == 4)) :-
		pairs::value([a-1,b-2,c-[d-4,e-5]], [c,d], Value).

	test(pairs_value_3_03, false) :-
		pairs::value([a-1,b-2,c-3], [c,d], _).

	test(pairs_transpose_2_01, true(Pairs == [1-a,2-b,3-c])) :-
		pairs::transpose([a-1,b-2,c-3], Pairs).

	test(pairs_group_sorted_by_key_2_01, true(Pairs == [a-[1,11],b-[2,22],c-[3,33]])) :-
		pairs::group_sorted_by_key([b-2,a-1,c-3,b-22,c-33,a-11], Pairs).

	test(pairs_group_consecutive_by_key_2_01, true(Pairs == [b-[2],a-[1,11],b-[22],c-[3,33]])) :-
		pairs::group_consecutive_by_key([b-2,a-1,a-11,b-22,c-3,c-33], Pairs).

	test(pairs_map_3_01, true(Pairs == [a-97,b-98,c-99])) :-
		pairs::map([Code,Char]>>char_code(Char,Code), [97,98,99], Pairs).

:- end_object.

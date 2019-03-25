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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2019/03/25,
		comment is 'Unit tests for the "types" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

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

	% all arbitrary types must also be supported type-checked types
	test(type_arbitrary_2_01) :-
		forall(
			type::arbitrary(Type),
			^^assertion(type(Type), type::type(Type))
		).

	% arbitrary types must generate valid values
	test(type_arbitrary_2_02) :-
		forall(
			(	type::type(Type),
				ground(Type),
				type::arbitrary(Type)
			),
			lgtunit::quick_check(type::arbitrary({Type}, -Type))
		).

	% all shrinker types must also be arbitrary types
	test(type_arbitrary_2_03) :-
		forall(
			type::shrinker(Type),
			^^assertion(type(Type), type::arbitrary(Type))
		).

	% all shrinkers must generate valid values
	test(type_arbitrary_2_04) :-
		forall(
			(	type::shrinker(Type),
				ground(Type)
			),
			lgtunit::quick_check(shrink_value({Type}, -Type))
		).

	% test support predicates

	shrink_value(Type, Small) :-
		type::arbitrary(Type, Arbitrary),
		(	type::shrink(Type, Arbitrary, Small) ->
			true
		;	% shrinking is not always possible
			Small = Arbitrary
		).

:- end_object.

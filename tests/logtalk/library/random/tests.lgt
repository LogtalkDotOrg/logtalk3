%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests(_RandomObject_),
	extends(lgtunit)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2018/02/25,
		comment is 'Unit tests for the "random" library.',
		parnames is ['RandomObject']
	]).

	cover(random).
	cover(backend_random).
	cover(fast_random).

	:- uses(list, [
		length/2, member/2, msort/2,
		min/2, max/2
	]).

	% random/1 tests

	quick_check(random_random_1_01, _RandomObject_::random(-between(float, 0.0, 1.0))).

	% between/3 tests

	quick_check(random_between_3_01, random_between_3_positive(-between(integer, 1, 4))).

	quick_check(random_between_3_02, random_between_3_negative(-between(integer, -4, -1))).

	% member/2 tests

	quick_check(random_member_2_01, random_member_2(-one_of(atom, [a,b,c,d,e,f,g,h,i,j]))).

	% select/3 tests

	quick_check(
		random_select_3_01,
		random_select_3(
			-property(atom, [Random]>>member(Random,[a,b,c,d,e,f,g,h,i,j])),
			-property(list(atom), [All]>>msort(All,[a,b,c,d,e,f,g,h,i,j]))
		)
	).

	% permutation/2 tests

	quick_check(random_permutation_2_01, random_permutation_2(-property(list(integer), [Permutation]>>msort(Permutation,[0,1,2,3,4,5,6,7,8,9])))).

	% sequence/4 tests

	quick_check(random_sequence_4_01, random_sequence_4_positive(-property(list(integer), [Sequence]>>length(Sequence,10)))).

	quick_check(random_sequence_4_02, random_sequence_4_positive(-list(integer,1,100))).

	quick_check(random_sequence_4_03, random_sequence_4_negative(-property(list(integer), [Sequence]>>length(Sequence,10)))).

	quick_check(random_sequence_4_04, random_sequence_4_negative(-list(integer,-100,-1))).

	% set/4 tests

	quick_check(random_set_4_01, random_set_4_positive(-property(list(integer), [Set]>>msort(Set,Set)))).

	quick_check(random_set_4_02, random_set_4_positive(-property(list(integer), [Set]>>length(Set,10)))).

	quick_check(random_set_4_03, random_set_4_positive(-list(integer,1,100))).

	quick_check(random_set_4_04, random_set_4_negative(-property(list(integer), [Set]>>msort(Set,Set)))).

	quick_check(random_set_4_05, random_set_4_negative(-property(list(integer), [Set]>>length(Set,10)))).

	quick_check(random_set_4_06, random_set_4_negative(-list(integer,-100,-1))).

	% get_seed/1 tests

	quick_check(random_get_seed_1_01, _RandomObject_::get_seed(-ground)).

	% set_seed/1 tests

	quick_check(random_set_seed_1_01, random_set_seed_1(-ground)).

	% auxiliary predicates

	random_between_3_positive(Random) :-
		_RandomObject_::between(1, 4, Random).

	random_between_3_negative(Random) :-
		_RandomObject_::between(-4, -1, Random).

	random_member_2(N) :-
		_RandomObject_::member(N, [a,b,c,d,e,f,g,h,i,j]).

	random_select_3(Random, [Random| Rest]) :-
		_RandomObject_::select(Random, [a,b,c,d,e,f,g,h,i,j], Rest).

	random_permutation_2(Permutation) :-
		_RandomObject_::permutation([0,1,2,3,4,5,6,7,8,9], Permutation).

	random_sequence_4_positive(Sequence) :-
		_RandomObject_::sequence(10, 1, 100, Sequence).

	random_sequence_4_negative(Sequence) :-
		_RandomObject_::sequence(10, -100, -1, Sequence).

	random_set_4_positive(Set) :-
		_RandomObject_::set(10, 1, 100, Set).

	random_set_4_negative(Set) :-
		_RandomObject_::set(10, -100, -1, Set).

	random_set_seed_1(Seed) :-
		_RandomObject_::get_seed(Seed),
		_RandomObject_::set_seed(Seed).

:- end_object.

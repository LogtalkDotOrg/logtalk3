%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	:- import(from(/(between,3), basics)).
	:- import(from(/(length,2), basics)).
:- endif.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2021-12-09,
		comment is 'Unit tests for the de facto Prolog standard length/2 built-in predicate.'
	]).

	test(commons_length_2_01, deterministic(N == 0)) :-
		{length([], N)}.

	test(commons_length_2_02, deterministic) :-
		{length([], 0)}.

	test(commons_length_2_03, deterministic(List == [])) :-
		{length(List, 0)}.

	test(commons_length_2_04, deterministic(N == 3)) :-
		{length([1, 2, 3], N)}.

	test(commons_length_2_05, deterministic) :-
		{length([1, 2, 3], 3)}.

	test(commons_length_2_06, deterministic) :-
		{length(List, 3)},
		^^variant(List, [_, _, _]).

	test(commons_length_2_07, false) :-
		{length([1, 2, 3], 0)}.

	test(commons_length_2_08, false) :-
		{length([], 3)}.

	test(commons_length_2_09, variant(Lists, [[],[_],[_,_],[_,_,_]])) :-
		findall(List, {between(0,3,N), length(List,N)}, Lists).

	test(commons_length_2_10, variant(Lists, [[]-0,[_]-1,[_,_]-2,[_,_,_]-3])) :-
		findall(List-N, {(length(List, N), (N < 3 -> true; !))}, Lists).

	test(commons_length_2_11, variant(Lists, [[]-3,[_]-4,[_,_]-5,[_,_,_]-6])) :-
		findall(Tail-N, {(length([1, 2, 3| Tail], N), (N < 6 -> true; !))}, Lists).

	test(commons_length_2_12, error(type_error(integer,a))) :-
		{length(_, a)}.

	test(commons_length_2_13, error(type_error(integer,a))) :-
		{length([], a)}.

	test(commons_length_2_14, error(type_error(list,a))) :-
		{length(a, _)}.

	test(commons_length_2_15, error(type_error(list,a))) :-
		{length(a, 1)}.

	test(commons_length_2_16, error(type_error(list,[_,_|a]))) :-
		{length([_, _| a], _)}.

	test(commons_length_2_17, error(type_error(list,[_,_|a]))) :-
		{length([_, _| a], 0)}.

	test(commons_length_2_18, error(type_error(list,[_,_|a]))) :-
		{length([_, _| a], 3)}.

	test(commons_length_2_19, error(domain_error(not_less_than_zero,-1))) :-
		{length(_, -1)}.

	test(commons_length_2_20, error(domain_error(not_less_than_zero,-1))) :-
		{length([], -1)}.

	test(commons_length_2_21, true(Tail == [])) :-
		{length([1, 2| Tail], 2)}.

	test(commons_length_2_22, false) :-
		{length([1, 2, 3, 4, 5| _], 2)}.

	% tests from the Logtalk portability work

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(commons_length_2_23, error(_)) :-
			L = [_| L],
			{length(L, _)}.

	:- else.

		- test(commons_length_2_23, error(_)) :-
			% STO; Undefined.
			L = [_| L],
			{length(L, _)}.

	:- endif.

:- end_object.

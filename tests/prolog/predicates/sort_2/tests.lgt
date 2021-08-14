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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2021-08-14,
		comment is 'Unit tests for the ISO Prolog standard sort/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.3.4

	test(iso_sort_2_01, true(Sorted == [1])) :-
		{sort([1, 1], Sorted)}.

	test(iso_sort_2_02, true(Sorted == [V, 7.0, 8.0, 1, 2, a, z, -X, -a, 1+Y, 1+2])) :-
		{sort([1+Y, z, a, V, 1, 2, V, 1, 7.0, 8.0, 1+Y, 1+2, 8.0, -a, -X, a], Sorted)}.

	test(iso_sort_2_03, false) :-
		{sort([1, 1], [1, 1])}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_sort_2_04, true) :-
			{sort([V], V)}.

	:- else.

		- test(iso_sort_2_04, true) :-
			% STO; Undefined.
			{sort([V], V)}.

	:- endif.

	test(iso_sort_2_05, true((L == [U,V,f(U),f(V)]; L == [V,U,f(V),f(U)]))) :-
		{sort([f(U),U,U,f(V),f(U),V], L)}.

	% tests from the ECLiPSe test suite

	test(eclipse_sort_2_06, error(instantiation_error)) :-
		{sort(_, _)}.

	test(eclipse_sort_2_07, error(instantiation_error)) :-
		{sort([a|_],_)}.

	test(eclipse_sort_2_08, error(type_error(list,3))) :-
		{sort(3, _)}.

	test(eclipse_sort_2_09, error(type_error(list,[a|b]))) :-
		{sort([a|b],_)}.

	test(eclipse_sort_2_10, error(type_error(list,3))) :-
		{sort([], 3)}.

	test(eclipse_sort_2_11, error(type_error(list,[a|b]))) :-
		{sort([], [a|b])}.

	% tests from the Logtalk portability work

	test(lgt_sort_2_12, true(Sorted == [[a,b],[c,d],[e]])) :-
		{sort([[e],[c,d],[a,b]], Sorted)}.

	test(lgt_sort_2_13, true(Second == 2)) :-
		{sort([4,1,3,2,7], [_, Second| _])}.

	test(lgt_sort_2_14, true(Others == [2,3,4,7])) :-
		{sort([4,1,3,2,7], [_| Others])}.

:- end_object.

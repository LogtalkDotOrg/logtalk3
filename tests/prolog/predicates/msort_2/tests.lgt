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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-09-15,
		comment is 'Unit tests for the de facto Prolog standard msort/2 built-in predicate.'
	]).

	% tests from the Logtalk portability work

	test(lgt_msort_2_01, true(Sorted == [1, 1])) :-
		{msort([1, 1], Sorted)}.

	test(lgt_msort_2_02, true(Sorted == [1, 1, 2, 2, 3, 3])) :-
		{msort([3, 2, 1, 1, 2, 3], Sorted)}.

	test(lgt_msort_2_03, true(Sorted == [V, V, 7.0, 8.0, 8.0, 1, 1, 2, a, a, z, -X, -a, 1+Y, 1+Y, 1+2])) :-
		{msort([1+Y, z, a, V, 1, 2, V, 1, 7.0, 8.0, 1+Y, 1+2, 8.0, -a, -X, a], Sorted)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		test(lgt_msort_2_04, true) :-
			{msort([V], V)}.
	:- else.
		- test(lgt_msort_2_04, true) :-
			% STO; Undefined.
			{msort([V], V)}.
	:- endif.

	test(lgt_msort_2_05, true) :-
		{msort([f(U),U,U,f(V),f(U),V],L)},
		(	L == [U,U,V,f(U),f(U),f(V)] ->
			true
		;	L == [V,U,U,f(V),f(U),f(U)]
		).

	test(lgt_msort_2_06, true(Sorted == [[a,b],[c,d],[e]])) :-
		{msort([[e],[c,d],[a,b]], Sorted)}.

	test(lgt_msort_2_07, error(instantiation_error)) :-
		{msort(_, _)}.

	test(lgt_msort_2_08, error(instantiation_error)) :-
		{msort([a|_],_)}.

	test(lgt_msort_2_09, error(type_error(list,3))) :-
		{msort(3, _)}.

	test(lgt_msort_2_10, error(type_error(list,[a|b]))) :-
		{msort([a|b],_)}.

	test(lgt_msort_2_11, error(type_error(list,3))) :-
		{msort([], 3)}.

:- end_object.

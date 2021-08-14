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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-08-14,
		comment is 'Unit tests for the ISO Prolog standard keysort/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.4.4

	test(iso_keysort_2_01, true(Sorted == [1-1, 1-1])) :-
		{keysort([1-1, 1-1], Sorted)}.

	test(iso_keysort_2_02, true(Sorted == [1-a, 1-z, 1-a, 2-99, 2-44, 3-f(X)])) :-
		{keysort([2-99, 1-a, 3-f(X), 1-z, 1-a, 2-44], Sorted)}.

	test(iso_keysort_2_03, true(X == 2)) :-
		{keysort([X-1,1-1],[2-1,1-1])}.

	- test(iso_keysort_2_04, true) :-
		% STO; Undefined.
		{Pairs = [1-2|Pairs], keysort(Pairs, _Sorted)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		test(iso_keysort_2_05, true) :-
			{keysort([V-V], V)}.
	:- else.
		- test(iso_keysort_2_05, true) :-
			% STO; Undefined.
			{keysort([V-V], V)}.
	:- endif.

	% tests from the ECLiPSe test suite

	test(eclipse_keysort_2_06, error(instantiation_error)) :-
		{keysort(_, _)}.

	test(eclipse_keysort_2_07, error(instantiation_error)) :-
		{keysort([1-a|_], _)}.

	test(eclipse_keysort_2_08, error(type_error(list,3))) :-
		{keysort(3, _)}.

	test(eclipse_keysort_2_09, error(type_error(list,[1-a|b]))) :-
		{keysort([1-a|b], _)}.

	test(eclipse_keysort_2_10, error(type_error(list,3))) :-
		{keysort([], 3)}.

	test(eclipse_keysort_2_11, error(type_error(list,[1-a|b]))) :-
		{keysort([], [1-a|b])}.

	test(eclipse_keysort_2_12, error(instantiation_error)) :-
		{keysort([_], _)}.

	test(eclipse_keysort_2_13, error(type_error(pair,1/a))) :-
		{keysort([1/a], _)}.

	test(eclipse_keysort_2_14, error(type_error(pair,1/a))) :-
		{keysort([], [1/a])}.

	% tests from the Logtalk portability work

	test(lgt_keysort_2_15, true(Key == a)) :-
		{keysort([d-4,a-1,c-3,b-2,g-7], [Key-_| _])}.

	test(lgt_keysort_2_16, true(Value == 1)) :-
		{keysort([d-4,a-1,c-3,b-2,g-7], [_-Value| _])}.

	test(lgt_keysort_2_17, true(Second == b-2)) :-
		{keysort([d-4,a-1,c-3,b-2,g-7], [_, Second| _])}.

	test(lgt_keysort_2_18, true(Others == [b-2,c-3,d-4,g-7])) :-
		{keysort([d-4,a-1,c-3,b-2,g-7], [_| Others])}.

:- end_object.

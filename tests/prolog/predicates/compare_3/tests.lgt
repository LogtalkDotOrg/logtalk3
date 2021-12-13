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
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2021-12-13,
		comment is 'Unit tests for the ISO Prolog standard compare/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.2.4

	test(iso_compare_3_01, true(Order == (<))) :-
		{compare(Order, 3, 5)}.

	test(iso_compare_3_02, true(Order == (=))) :-
		{compare(Order, d, d)}.

	test(iso_compare_3_03, true(Order == (<))) :-
		{compare(Order, Order, <)}.

	test(iso_compare_3_04, false) :-
		{compare(<, <, <)}.

	test(iso_compare_3_05, error(type_error(atom,1+2))) :-
		{compare(1+2, 3, 3.0)}.

	test(iso_compare_3_06, error(domain_error(order,>=))) :-
		{compare(>=, 3, 3.0)}.

	% standard order tests from the Logtalk portability work

	test(lgt_compare_3_07, true) :-
		{compare(<, _X, 1.1)}.

	test(lgt_compare_3_08, true) :-
		{compare(<, 1.1, 1)}.

	test(lgt_compare_3_09, true) :-
		{compare(>, 1, 1.1)}.

	test(lgt_compare_3_10, true(Order == (<))) :-
		{compare(Order, 1.1, 1)}.

	test(lgt_compare_3_11, true(Order == (>))) :-
		{compare(Order, 1, 1.1)}.

	test(lgt_compare_3_12, true(Order == (<))) :-
		{compare(Order, 1.0, 1)}.

	test(lgt_compare_3_13, true(Order == (>))) :-
		{compare(Order, 1, 1.0)}.

	test(lgt_compare_3_14, true) :-
		{compare(<, 1, a)}.

	test(lgt_compare_3_15, true) :-
		{compare(<, a, a(_))}.

	test(lgt_compare_3_16, true) :-
		{compare(<, a(_), a(_,_))}.

	test(lgt_compare_3_17, true) :-
		{compare(<, b(_), a(_,_))}.

	test(lgt_compare_3_18, true) :-
		{compare(<, a(1,2), a(1,3))}.

	test(lgt_compare_3_19, true) :-
		{compare(<, a(1,2), b(1,2))}.

	% other tests

	test(lgt_compare_3_20, true) :-
		{compare(>, (4,1,0), (4,0,1))}.

	test(lgt_compare_3_21, false) :-
		{compare(>, (4,0,1), (4,1,0))}.

	test(lgt_compare_3_22, false) :-
		{compare(<, (4,1,0), (4,0,1))}.

	test(lgt_compare_3_23, true) :-
		{compare(<, (4,0,1), (4,1,0))}.

	test(lgt_compare_3_24, true) :-
		{compare(>, (4,1,0), (4,0,1))}.

	test(lgt_compare_3_25, false) :-
		{compare(>, (4,0,1), (4,1,0))}.

	test(lgt_compare_3_26, false) :-
		{compare(<, (4,1,0), (4,0,1))}.

	test(lgt_compare_3_27, true) :-
		{compare(<, (4,0,1), (4,1,0))}.

	test(lgt_compare_3_28, true(Order == (<))) :-
		{compare(Order, 1, 1+2)}.

	test(lgt_compare_3_29, true(Order == (>))) :-
		{compare(Order, 1+2, 1)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_compare_3_30, true(Order == (<))) :-
			X = [0,1| X],
			Y = [0,2| Y],
			{compare(Order, a(1,X), a(1,Y))}.

	:- else.

		- test(lgt_compare_3_30, true(Order == (<))) :-
			% STO; Undefined
			X = [0,1| X],
			Y = [0,2| Y],
			{compare(Order, a(1,X), a(1,Y))}.

	:- endif.

:- end_object.

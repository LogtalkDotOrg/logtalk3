%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		date is 2024-10-04,
		comment is 'Unit tests for the ISO Prolog standard term comparison built-in predicates.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.4.1.4

	test(iso_term_comparison_01, true) :-
		{'@=<'(1.0, 1)}.

	test(iso_term_comparison_02, true) :-
		{'@<'(1.0, 1)}.

	test(iso_term_comparison_03, false) :-
		{'\\=='(1, 1)}.

	test(iso_term_comparison_04, true) :-
		{'@=<'(aardvark, zebra)}.

	test(iso_term_comparison_05, true) :-
		{'@=<'(short, short)}.

	test(iso_term_comparison_06, true) :-
		{'@=<'(short, shorter)}.

	test(iso_term_comparison_07, false) :-
		{'@>='(short, shorter)}.

	test(iso_term_comparison_08, false) :-
		{'@<'(foo(a,b), north(a))}.

	test(iso_term_comparison_09, true) :-
		{'@>'(foo(b), foo(a))}.

	test(iso_term_comparison_10, true) :-
		{'@<'(foo(a, _X), foo(b, _Y))}.

	test(iso_term_comparison_11, true) :-
		(	{'@<'(foo(_X, a), foo(_Y, b))} ->
			true
		;	true
		).

	test(iso_term_comparison_12, true) :-
		{'@=<'(X, X)}.

	test(iso_term_comparison_13, true) :-
		{'=='(X, X)}.

	test(iso_term_comparison_14, true) :-
		(	{'@=<'(_X, _Y)} ->
			true
		;	true
		).

	test(iso_term_comparison_15, false) :-
		{'=='(_X, _Y)}.

	test(iso_term_comparison_16, true) :-
		{\==(_, _)}.

	test(iso_term_comparison_17, false) :-
		{'=='(_, _)}.

	test(iso_term_comparison_18, true) :-
		(	{'@=<'(_, _)} ->
			true
		;	true
		).

	test(iso_term_comparison_19, true) :-
		(	{'@=<'(foo(_X, a), foo(_Y, b))} ->
			true
		;	true
		).

	% standard order tests from the Logtalk portability work

	test(lgt_term_comparison_20, true) :-
		{'@<'(_X, 1.1)}.

	test(lgt_term_comparison_21, true) :-
		{'@<'(1.1, 1)}.

	test(lgt_term_comparison_22, true) :-
		{'@<'(1, a)}.

	test(lgt_term_comparison_23, true) :-
		{'@<'(a, a(_))}.

	test(lgt_term_comparison_24, true) :-
		{'@<'(a(_), a(_,_))}.

	test(lgt_term_comparison_25, true) :-
		{'@<'(b(_), a(_,_))}.

	test(lgt_term_comparison_26, true) :-
		{'@<'(a(1,2), a(1,3))}.

	test(lgt_term_comparison_27, true) :-
		{'@<'(a(1,2), b(1,2))}.

	% other tests

	test(lgt_term_comparison_28, true) :-
		{'@>='((4,1,0), (4,0,1))}.

	test(lgt_term_comparison_29, false) :-
		{'@>='((4,0,1), (4,1,0))}.

	test(lgt_term_comparison_30, false) :-
		{'@=<'((4,1,0), (4,0,1))}.

	test(lgt_term_comparison_31, true) :-
		{'@=<'((4,0,1), (4,1,0))}.

	test(lgt_term_comparison_32, true) :-
		{'@>'((4,1,0), (4,0,1))}.

	test(lgt_term_comparison_33, false) :-
		{'@>'((4,0,1), (4,1,0))}.

	test(lgt_term_comparison_34, false) :-
		{'@<'((4,1,0), (4,0,1))}.

	test(lgt_term_comparison_35, true) :-
		{'@<'((4,0,1), (4,1,0))}.

	test(lgt_term_comparison_36, true) :-
		{'@<'(b/0, a//0)}.

	test(lgt_term_comparison_37, false) :-
		{'@<'(a//0, a/0)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_term_comparison_38, true) :-
			{X = f(X), Y = f(Y), X @>= Y}.

		test(lgt_term_comparison_39, false) :-
			{X = f(X), Y = f(Y), X @< Y}.

		test(lgt_term_comparison_40, true) :-
			{X = f(X), Y = f(Y), X == Y}.

		test(lgt_term_comparison_41, true) :-
			{X = s(X,_), Y = s(Y,_), X @< Y}.

		test(lgt_term_comparison_42, false) :-
			{X = s(X,_), Y = s(Y,_), X == Y}.

		test(lgt_term_comparison_43, false) :-
			{X = s(X,_), Y = s(Y,_), X @> Y}.

		test(lgt_term_comparison_44, true) :-
			{X = s(_,X), Y = s(_,Y), X @< Y}.

		test(lgt_term_comparison_45, false) :-
			{X = s(_,X), Y = s(_,Y), X == Y}.

		test(lgt_term_comparison_46, false) :-
			{X = s(_,X), Y = s(_,Y), X @> Y}.

		test(lgt_term_comparison_47, true) :-
			{L1 = [1,2,3| L1], L2 = [1,2,3,1,2,3| L2], L1 == L2}.

	:- else.

		- test(lgt_term_comparison_38, true, [note('STO')]) :-
			% STO; Undefined.
			{X = f(X), Y = f(Y), X @>= Y}.

		- test(lgt_term_comparison_39, false, [note('STO')]) :-
			% STO; Undefined.
			{X = f(X), Y = f(Y), X @< Y}.

		- test(lgt_term_comparison_40, true, [note('STO')]) :-
			% STO; Undefined.
			{X = f(X), Y = f(Y), X == Y}.

		- test(lgt_term_comparison_41, true, [note('STO')]) :-
			{X = s(X,_), Y = s(Y,_), X @< Y}.

		- test(lgt_term_comparison_42, false, [note('STO')]) :-
			{X = s(X,_), Y = s(Y,_), X == Y}.

		- test(lgt_term_comparison_43, false, [note('STO')]) :-
			{X = s(X,_), Y = s(Y,_), X @> Y}.

		- test(lgt_term_comparison_44, true, [note('STO')]) :-
			{X = s(_,X), Y = s(_,Y), X @< Y}.

		- test(lgt_term_comparison_45, false, [note('STO')]) :-
			{X = s(_,X), Y = s(_,Y), X == Y}.

		- test(lgt_term_comparison_46, false, [note('STO')]) :-
			{X = s(_,X), Y = s(_,Y), X @> Y}.

		- test(lgt_term_comparison_47, true, [note('STO')]) :-
			{L1 = [1,2,3| L1], L2 = [1,2,3,1,2,3| L2], L1 == L2}.

	:- endif.

:- end_object.

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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2024-10-04,
		comment is 'Unit tests for the ISO Prolog standard (=)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.1.4

	test(iso_unify_2_01, true) :-
		{'='(1, 1)}.

	test(iso_unify_2_02, true(X == 1)) :-
		{'='(X, 1)}.

	test(iso_unify_2_03, true(X == Y)) :-
		{'='(X, Y)}.

	test(iso_unify_2_04, true) :-
		{'='(_, _)}.

	test(iso_unify_2_05, true((X == abc, Y == abc))) :-
		{('='(X,Y), '='(X,abc))}.

	test(iso_unify_2_06, true((X == def, Y == def))) :-
		{'='(f(X,def), f(def,Y))}.

	test(iso_unify_2_07, false) :-
		{'='(1, 2)}.

	test(iso_unify_2_08, false) :-
		{'='(1, 1.0)}.

	test(iso_unify_2_09, false) :-
		{'='(g(X), f(f(X)))}.

	test(iso_unify_2_10, false) :-
		{'='(f(X,1), f(a(X)))}.

	test(iso_unify_2_11, false) :-
		{'='(f(X,Y,X), f(a(X),a(Y),Y,2))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_unify_2_12, true) :-
			{'='(X,a(X))}.

		test(iso_unify_2_13, false) :-
			{'='(f(X,1), f(a(X),2))}.

		test(iso_unify_2_14, false) :-
			{'='(f(1,X,1), f(2,a(X),2))}.

		test(iso_unify_2_15, false) :-
			{'='(f(1,X), f(2,a(X)))}.

		test(iso_unify_2_16, false) :-
			{'='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

		test(iso_unify_2_17, true) :-
			{L1 = [1,2,3| L1], L2 = [1,2,3,1,2,3| L2], L1 == L2}.

	:- else.

		- test(iso_unify_2_12, true, [note('STO')]) :-
			% STO; Undefined
			{'='(X,a(X))}.

		- test(iso_unify_2_13, false, [note('STO')]) :-
			% STO; Undefined
			{'='(f(X,1), f(a(X),2))}.

		- test(iso_unify_2_14, false, [note('STO')]) :-
			% STO; Undefined
			{'='(f(1,X,1), f(2,a(X),2))}.

		- test(iso_unify_2_15, false, [note('STO')]) :-
			% STO; Undefined
			{'='(f(1,X), f(2,a(X)))}.

		- test(iso_unify_2_16, false, [note('STO')]) :-
			% STO; Undefined
			{'='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

		- test(iso_unify_2_17, true, [note('STO')]) :-
			{L1 = [1,2,3| L1], L2 = [1,2,3,1,2,3| L2], L1 = L2}.

	:- endif.

:- end_object.

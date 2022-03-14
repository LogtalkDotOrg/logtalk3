%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		date is 2022-03-13,
		comment is 'Unit tests for the ISO Prolog standard unify_with_occurs_check/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.2.4

	test(iso_unify_with_occurs_check_2_01, true) :-
		{unify_with_occurs_check(1, 1)}.

	test(iso_unify_with_occurs_check_2_02, true(X == 1)) :-
		{unify_with_occurs_check(X, 1)}.

	test(iso_unify_with_occurs_check_2_03, true(X == Y)) :-
		{unify_with_occurs_check(X, Y)}.

	test(iso_unify_with_occurs_check_2_04, true) :-
		{unify_with_occurs_check(_, _)}.

	test(iso_unify_with_occurs_check_2_05, true(X-Y == abc-abc)) :-
		{unify_with_occurs_check(X, Y), unify_with_occurs_check(X, abc)}.

	test(iso_unify_with_occurs_check_2_06, true(X-Y == def-def)) :-
		{unify_with_occurs_check(f(X,def), f(def,Y))}.

	test(iso_unify_with_occurs_check_2_07, false) :-
		{unify_with_occurs_check(1, 2)}.

	test(iso_unify_with_occurs_check_2_08, false) :-
		{unify_with_occurs_check(1, 1.0)}.

	test(iso_unify_with_occurs_check_2_09, false) :-
		{unify_with_occurs_check(g(X), f(f(X)))}.

	test(iso_unify_with_occurs_check_2_10, false) :-
		{unify_with_occurs_check(f(X,1), f(a(X)))}.

	test(iso_unify_with_occurs_check_2_11, false) :-
		{unify_with_occurs_check(f(X,Y,X), f(a(X),a(Y),Y,2))}.

	test(iso_unify_with_occurs_check_2_12, false) :-
		{unify_with_occurs_check(X, a(X))}.

	test(iso_unify_with_occurs_check_2_13, false) :-
		{unify_with_occurs_check(f(X,1), f(a(X),2))}.

	test(iso_unify_with_occurs_check_2_14, false) :-
		{unify_with_occurs_check(f(1,X,1), f(2,a(X),2))}.

	test(iso_unify_with_occurs_check_2_15, false) :-
		{unify_with_occurs_check(f(1,X), f(2,a(X)))}.

	test(iso_unify_with_occurs_check_2_16, false) :-
		{unify_with_occurs_check(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

	% tests from the Logtalk portability work

	test(lgt_unify_with_occurs_check_2_17, false) :-
		{unify_with_occurs_check(X, [_| X])}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_unify_with_occurs_check_2_18, true(v(A,B,C) == v(1,2,3))) :-
			{L1 = [1,2,3| L1], L2 = [A,B,C| L2], unify_with_occurs_check(L1, L2)}.

	:- else.

		- test(lgt_unify_with_occurs_check_2_18, true(v(A,B,C) == v(1,2,3))) :-
			% STO; Undefined.
			{L1 = [1,2,3| L1], L2 = [A,B,C| L2], unify_with_occurs_check(L1, L2)}.

	:- endif.

:- end_object.

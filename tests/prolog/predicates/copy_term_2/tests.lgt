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
		date is 2021-09-13,
		comment is 'Unit tests for the ISO Prolog standard copy_term/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.4.4

	test(iso_copy_term_2_01, true) :-
		{copy_term(_X, _Y)}.

	test(iso_copy_term_2_02, true) :-
		{copy_term(_X, 3)}.

	test(iso_copy_term_2_03, true) :-
		{copy_term(_, a)}.

	test(iso_copy_term_2_04, true(X == a)) :-
		{copy_term(a+X, X+b)}.

	test(iso_copy_term_2_05, true) :-
		{copy_term(_, _)}.

	test(iso_copy_term_2_06, true(A == B)) :-
		{copy_term(X+X+_Y, A+B+B)}.

	test(iso_copy_term_2_07, false) :-
		{copy_term(a, b)}.

	test(iso_copy_term_2_08, false) :-
		{copy_term(a+X,X+b), copy_term(a+X,X+b)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		test(iso_copy_term_2_09, true) :-
			{copy_term(demoen(X,X), demoen(Y,f(Y)))}.
	:- else.
		- test(iso_copy_term_2_09, true) :-
			% STO; Undefined
			{copy_term(demoen(X,X), demoen(Y,f(Y)))}.
	:- endif.

	% ISO/IEC 13211-1:1995(E) standard, section 8.5.4.1 NOTE

	test(iso_copy_term_2_10, true((X == A, Y == B))) :-
		Term1 = a(X, Y),
		Term2 = a(_, _),
		{copy_term(Term1, Term2)},
		Term1 = a(A, B).

	test(iso_copy_term_2_11, true((X \== Z, X \== W, Y \== W, Y \== Z))) :-
		Term1 = a(X, Y),
		Term2 = a(Z, W),
		{copy_term(Term1, Term2)}.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		date is 2023-04-09,
		comment is 'Unit tests for the ISO Prolog standard subsumes_term/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.2.4.4

	test(iso_subsumes_term_2_01, true) :-
		{subsumes_term(a, a)}.

	test(iso_subsumes_term_2_02, true) :-
		{subsumes_term(f(_X,_Y), f(Z,Z))}.

	test(iso_subsumes_term_2_03, false) :-
		{subsumes_term(f(Z,Z), f(_X,_Y))}.

	test(iso_subsumes_term_2_04, false) :-
		{subsumes_term(g(X), g(f(X)))}.

	test(iso_subsumes_term_2_05, false) :-
		{subsumes_term(X, f(X))}.

	test(iso_subsumes_term_2_06, true) :-
		{subsumes_term(X, Y), subsumes_term(Y, f(X))}.

	% tests from the Logtalk portability work

	test(lgt_subsumes_term_2_07, true) :-
		{subsumes_term(A, A)}.

	test(lgt_subsumes_term_2_08, true) :-
		{subsumes_term(t(A,B), t(A,B))}.

	test(lgt_subsumes_term_2_09, true) :-
		{subsumes_term(A-B, A-B)}.

	test(lgt_subsumes_term_2_10, true) :-
		{subsumes_term(c(A, [e(B)]), c(A, [e(B)]))}.

	test(lgt_subsumes_term_2_11, true) :-
		{subsumes_term(_A, _B)}.

	test(lgt_subsumes_term_2_12, true) :-
		{subsumes_term(t(_A,_B), t(_C,_D))}.

	test(lgt_subsumes_term_2_13, true) :-
		{subsumes_term(_A-_B, _C-_D)}.

	test(lgt_subsumes_term_2_14, true) :-
		{subsumes_term(c(_A, [e(_B)]), c(_C, [e(_D)]))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_subsumes_term_2_15, true) :-
			X = f(X),
			{subsumes_term(f(_), f(X))}.

		test(lgt_subsumes_term_2_16, false) :-
			X = f(X),
			{subsumes_term(f(X), f(_))}.

		test(lgt_subsumes_term_2_17, true) :-
			X = f(X),
			{subsumes_term(X, X)}.

	:- else.

		- test(lgt_subsumes_term_2_15, true, [note('STO')]) :-
			% STO; Undefined.
			X = f(X),
			{subsumes_term(f(_), f(X))}.

		- test(lgt_subsumes_term_2_16, false, [note('STO')]) :-
			% STO; Undefined.
			X = f(X),
			{subsumes_term(f(X), f(_))}.

		- test(lgt_subsumes_term_2_17, true, [note('STO')]) :-
			% STO; Undefined.
			X = f(X),
			{subsumes_term(X, X)}.

	:- endif.

:- end_object.

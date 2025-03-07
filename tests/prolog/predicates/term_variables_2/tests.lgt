%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:7:0,
		author is 'Paulo Moura',
		date is 2024-10-06,
		comment is 'Unit tests for the ISO Prolog standard term_variables/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.5.5.1 NOTE

	quick_check(iso_term_variables_2_01, twice(+term)).

	quick_check(iso_term_variables_2_02, chain(+term)).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.5.5.4

	test(iso_term_variables_2_03, true(Vars == [])) :-
		{term_variables(t, Vars)}.

	test(iso_term_variables_2_04, true(Vars == [A, B, C, D])) :-
		{term_variables(A+B*C/B-D, Vars)}.

	test(iso_term_variables_2_05, error(type_error(list,[_,_|a]))) :-
		{term_variables(t, [_, _|a])}.

	test(iso_term_variables_2_06, true) :-
		{S=B+T, T=A*B, term_variables(S, Vars)},
		^^assertion(Vars == [B, A]),
		^^assertion(T == A*B),
		^^assertion(S == B+A*B).

	test(iso_term_variables_2_07, true) :-
		{T=A*B, S=B+T, term_variables(S, Vars)},
		^^assertion(Vars == [B, A]),
		^^assertion(T == A*B),
		^^assertion(S == B+A*B).

	test(iso_term_variables_2_08, true) :-
		{term_variables(A+B+B, [B|Vars])},
		^^assertion(A == B),
		^^assertion(Vars == [B]).

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_term_variables_2_09, subsumes([_, _], Vars)) :-
			{term_variables(_X+Vars, Vars)}.

	:- else.

		- test(iso_term_variables_2_09, subsumes([_, _], Vars), [note('STO')]) :-
			% STO; Undefined.
			{term_variables(_X+Vars, Vars)}.

	:- endif.

	% tests from the ECLiPSe test suite

	test(eclipse_term_variables_2_10, error(type_error(list,3))) :-
		{term_variables(foo, 3)}.

	test(eclipse_term_variables_2_11, error(type_error(list,[a|b]))) :-
		{term_variables(foo, [a|b])}.

	test(eclipse_term_variables_2_12, true(Vs == [X,Y,Z])) :-
		{term_variables(foo(X,Y,X,Z), Vs)}.

	% tests from the Logtalk portability work

	test(lgt_term_variables_2_13, true(Vs == [Z])) :-
		{term_variables([Z,Z], Vs)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_term_variables_2_14, true(Vars == [])) :-
			{X = f(X,X), term_variables(X, Vars)}.

		test(lgt_term_variables_2_15, true(Vars == [Y])) :-
			{X = f(X,Y), term_variables(X, Vars)}.

	:- else.

		- test(lgt_term_variables_2_14, true(Vars == []), [note('STO')]) :-
			% STO; Undefined.
			{X = f(X,X), term_variables(X, Vars)}.

		- test(lgt_term_variables_2_15, true(Vars == [Y]), [note('STO')]) :-
			{X = f(X,Y), term_variables(X, Vars)}.

	:- endif.

	% tests from the WG17 test suite

	test(wg17_term_variables_2_16, variant(Vs, [_, _])) :-
		term_variables(_ + _, Vs).

	% auxiliary predicates

	twice(T) :-
		{term_variables(T, Vs1), term_variables(T, Vs2), Vs1 == Vs2}.

	chain(T) :-
		{term_variables(T, Vs1), term_variables(Vs1, Vs2), Vs1 == Vs2}.

:- end_object.

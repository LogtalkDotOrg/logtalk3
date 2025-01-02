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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.1.4

:- dynamic(cat/0).
cat.

:- dynamic(dog/0).
dog :- true.

elk(X) :- moose(X).

:- dynamic(legs/2).
legs(A, 6) :- insect(A).
legs(A, 7) :- A, call(A).

:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(r/3).
r(A, B, C) :- a(A), b(B), c(C).
a(_).
b(_).
c(_).

:- dynamic(p/2).
p(a(C), b(C)).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2024-10-06,
		comment is 'Unit tests for the ISO Prolog standard clause/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.1.4

	test(iso_clause_2_01, true) :-
		{clause(cat,true)}.

	test(iso_clause_2_02, true) :-
		{clause(dog,true)}.

	test(iso_clause_2_03, true(Body == insect(I))) :-
		{clause(legs(I,6), Body)}.

	test(iso_clause_2_04, true(Body == (call(C),call(C)))) :-
		{clause(legs(C,7), Body)}.

	test(iso_clause_2_05, true(L == [ant-true, bee-true])) :-
		findall(I-T, {clause(insect(I),T)}, L).

	test(iso_clause_2_06, false) :-
		{clause(x, _Body)}.

	test(iso_clause_2_07, error(instantiation_error)) :-
		{clause(_, _B)}.

	test(iso_clause_2_08, error(type_error(callable,4))) :-
		{clause(4, _B)}.

	test(iso_clause_2_09, true) :-
		% some Prolog systems allows applying clauses/2 to static predicates
		catch(
			{clause(elk(_N), _Body)},
			Error,
			(	Error = error(permission_error(access,private_procedure,elk/1),_)
			;	% the second exception term is used in some of the Prolog compilers supporting modules
				Error = error(permission_error(access,private_procedure,':'(user,elk/1)),_)
			)
		).

	test(iso_clause_2_10, errors([permission_error(access,private_procedure,atom/1), permission_error(access,private_procedure,':'(user,atom/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{clause(atom(_), _Body)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_clause_2_11, true) :-
			{clause(legs(A,6), insect(f(A)))}.

	:- else.

		- test(iso_clause_2_11, true, [note('STO')]) :-
			% STO; Undefined
			{clause(legs(A,6), insect(f(A)))}.

	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_clause_2_12, error(type_error(callable,5))) :-
		{clause(f(_), 5)}.

	% tests from the Logtalk portability work

	test(lgt_clause_2_13, true(Vars == [A,B,C,D,E,F])) :-
		% section 8.8.1.1 - converting a clause to a term must produce renamed copies
		{clause(r(A,B,C), (a(A), b(B), c(C)))},
		{clause(r(D,E,F), (a(D), b(E), c(F)))},
		term_variables(vs(A,B,C,D,E,F), Vars).

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse),
		catch({current_prolog_flag(occurs_check, _)}, _, fail)
	)).

		test(lgt_clause_2_14, false, [setup({set_prolog_flag(occurs_check,true)})]) :-
			{clause(p(A, b(A)), true)}.

		test(lgt_clause_2_15, error(_), [setup({set_prolog_flag(occurs_check,error)})]) :-
			{clause(p(A, b(A)), true)}.

		test(lgt_clause_2_16, true, [setup({set_prolog_flag(occurs_check,false)})]) :-
			{clause(p(A, b(A)), true)}.

	:- else.

		- test(lgt_clause_2_14, false, [setup({set_prolog_flag(occurs_check,true)}), note('STO')]) :-
			{clause(p(A, b(A)), true)}.

		- test(lgt_clause_2_15, error(_), [setup({set_prolog_flag(occurs_check,error)}), note('STO')]) :-
			{clause(p(A, b(A)), true)}.

		- test(lgt_clause_2_16, true, [setup({set_prolog_flag(occurs_check,false)}), note('STO')]) :-
			{clause(p(A, b(A)), true)}.

	:- endif.

:- end_object.

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


% database for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.1 NOTES

:- dynamic(foo/1).
foo(1).
foo(2).
foo(3).

% database for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.4

elk(X) :- moose(X).

:- dynamic(insect/1).
insect(ant).
insect(bee).

% database for tests from the Logtalk portability work

:- dynamic(a/1).
a(1).
a(2).
a(X) :- b(X).

b(3).

:- dynamic(c/2).
c(1, a).
c(1, 'A').
c(2, b).
c(2, 'B').


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:8:0,
		author is 'Paulo Moura',
		date is 2024-09-24,
		comment is 'Unit tests for the ISO Prolog standard retractall/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.1 NOTES

	test(iso_retractall_1_01, true({current_predicate(foo/1)})) :-
		{retractall(foo(_))}.

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.4

	test(iso_retractall_1_02, true) :-
		{retractall(insect(bee))}.

	test(iso_retractall_1_03, true) :-
		{retractall(insect(_))}.

	test(iso_retractall_1_04, true) :-
		{retractall(insect(spider))}.

	test(iso_retractall_1_05, error(type_error(callable,3))) :-
		% try to delay the error to runtime
		three(Three),
		{retractall(Three)}.

	test(iso_retractall_1_06, errors([permission_error(modify,static_procedure,retractall/1), permission_error(modify,static_procedure,':'(user,retractall/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retractall(retractall(_))}.

	% tests from the Logtalk portability work

	throws(lgt_retractall_1_07, error(instantiation_error,_)) :-
		{retractall(_)}.

	test(lgt_retractall_1_08, true) :-
		{retractall(a(_)), \+ a(_)}.

	test(lgt_retractall_1_09, true(L == [b,'B'])) :-
		{	retractall(c(1, _)),
			findall(X, c(_, X), L)
		}.

	% tests from the ECLiPSe test suite

	test(eclipse_retractall_1_10, true(I == beetle(stag))) :-
		{	assertz(insect(fly(house))),
			assertz(insect(beetle(stag))),
			assertz(insect(fly(fruit))),
			retractall(insect(fly(_))),
			\+ insect(fly(_)),
			insect(I)
		}.

	test(eclipse_retractall_1_11, true) :-
		{retractall(mammal(_))}.

	test(eclipse_retractall_1_12, errors([permission_error(modify,static_procedure,elk/1), permission_error(modify,static_procedure,':'(user,elk/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retractall(elk(_))}.

	% tests from the Logtalk portability work

	test(lgt_retractall_1_13, errors([permission_error(modify,static_procedure,true/0), permission_error(modify,static_procedure,':'(user,true/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retractall(true)}.

	test(lgt_retractall_1_14, errors([permission_error(modify,static_procedure,catch/3), permission_error(modify,static_procedure,':'(user,catch/3))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retractall(catch(_,_,_))}.

	test(lgt_retractall_1_15, errors([permission_error(modify,static_procedure,repeat/0), permission_error(modify,static_procedure,':'(user,repeat/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retractall(repeat)}.

	% auxiliary predicates

	three(3).

:- end_object.

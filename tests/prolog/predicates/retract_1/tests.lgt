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


% database for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.4

elk(X) :- moose(X).

:- dynamic(legs/2).
legs(A, 4) :- animal(A).
legs(octopus, 8).
legs(A, 6) :- insect(A).
legs(spider, 8).
legs(B, 2) :- bird(B).

:- dynamic(insect/1).
insect(ant).
insect(bee).

:- dynamic(foo/1).
foo(X) :- call(X) -> call(X).
foo(X) :- call(X), call(X).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2022-04-06,
		comment is 'Unit tests for the ISO Prolog standard retract/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.3.4

	test(iso_retract_1_01, true) :-
		{retract(legs(octopus, 8))}.

	test(iso_retract_1_02, false) :-
		{retract(legs(spider, 6))}.

	test(iso_retract_1_03, true(T == bird(X))) :-
		{retract((legs(X, 2) :- T))}.

	test(iso_retract_1_04, variant(L, [A-4-animal(A), B-6-insect(B), spider-8-true])) :-
		findall(X-Y-Z, {retract((legs(X,Y) :- Z))}, L).

	test(iso_retract_1_05, false) :-
		{retract((legs(_X,_Y) :- _Z))}.

	test(iso_retract_1_06, true(I == ant)) :-
		{	retract(insect(I)),
			retract(insect(bee))
		}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(iso_retract_1_07, true) :-
			{retract((foo(A) :- A,call(A)))}.

	:- else.

		- test(iso_retract_1_07, true) :-
			% STO; Undefined
			{retract((foo(A) :- A,call(A)))}.

	:- endif.

	test(iso_retract_1_08, true(A-B == call(C)-call(C))) :-
		{retract((foo(C) :- A -> B))}.

	test(iso_retract_1_09, error(instantiation_error)) :-
		{retract((_X :- in_eec(_Y)))}.

	test(iso_retract_1_10, error(type_error(callable,4))) :-
		{retract((4 :- _X))}.

	test(iso_retract_1_11, errors([permission_error(modify,static_procedure,atom/1), permission_error(modify,static_procedure,':'(user,atom/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((atom(X) :- X =='[]'))}.

	% tests from the Logtalk portability work

	test(lgt_retract_1_12, error(instantiation_error)) :-
		{retract(_)}.

	test(lgt_retract_1_13, errors([permission_error(modify,static_procedure,elk/1), permission_error(modify,static_procedure,':'(user,elk/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((elk(_) :- _))}.

	% tests from the ECLiPSe test suite

	test(eclipse_retract_1_14, false) :-
		{retract(mammal(_))}.

	% tests from the Logtalk portability work

	test(eclipse_retract_1_15, errors([permission_error(modify,static_procedure,retract/1), permission_error(modify,static_procedure,':'(user,retract/1))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((retract(_) :- _))}.

	test(eclipse_retract_1_16, errors([permission_error(modify,static_procedure,true/0), permission_error(modify,static_procedure,':'(user,true/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract(true)}.

	test(eclipse_retract_1_17, errors([permission_error(modify,static_procedure,catch/3), permission_error(modify,static_procedure,':'(user,catch/3))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((catch(_,_,_) :- _))}.

	test(eclipse_retract_1_18, errors([permission_error(modify,static_procedure,repeat/0), permission_error(modify,static_procedure,':'(user,repeat/0))])) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((repeat :- _))}.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


% databse for tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.9.5.4

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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard retract/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.3.4

	succeeds(iso_retract_1_01) :-
		{retract(legs(octopus, 8))}.

	fails(iso_retract_1_02) :-
		{retract(legs(spider, 6))}.

	succeeds(iso_retract_1_03) :-
		{retract((legs(X, 2) :- T))},
		T == bird(X).

	succeeds(iso_retract_1_04) :-
		findall(X-Y-Z, {retract((legs(X,Y) :- Z))}, L),
		L = [_-4-animal(X), _-6-insect(X), spider-8-true].

	fails(iso_retract_1_05) :-
		{retract((legs(_X,_Y) :- _Z))}.

	succeeds(iso_retract_1_06) :-
		{	retract(insect(I)),
		%	write(I), nl,
			retract(insect(bee))
		},
		I == ant.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_retract_1_07) :-
			{retract((foo(A) :- A,call(A)))}.
	:- else.
		- succeeds(iso_retract_1_07) :-
			% STO; Undefined
			{retract((foo(A) :- A,call(A)))}.
	:- endif.

	succeeds(iso_retract_1_08) :-
		{retract((foo(C) :- A -> B))},
		A == call(C), B == call(C).

	throws(iso_retract_1_09, error(instantiation_error,_)) :-
		{retract((_X :- in_eec(_Y)))}.

	throws(iso_retract_1_10, error(type_error(callable,4),_)) :-
		{retract((4 :- _X))}.

	throws(iso_retract_1_11, [error(permission_error(modify,static_procedure,atom/1),_), error(permission_error(modify,static_procedure,':'(user,atom/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((atom(X) :- X =='[]'))}.

	% tests from the Logtalk portability work

	throws(lgt_retract_1_12, error(instantiation_error,_)) :-
		{retract(_)}.

	throws(lgt_retract_1_13, [error(permission_error(modify,static_procedure,elk/1),_), error(permission_error(modify,static_procedure,':'(user,elk/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{retract((elk(_) :- _))}.

	% tests from the ECLiPSe test suite

	fails(eclipse_retract_1_14) :-
		{retract(mammal(_))}.

:- end_object.

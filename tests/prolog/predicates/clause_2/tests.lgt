%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/06/02,
		comment is 'Unit tests for the ISO Prolog standard clause/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.8.1.4

	succeeds(iso_clause_2_01) :-
		{clause(cat,true)}.

	succeeds(iso_clause_2_02) :-
		{clause(dog,true)}.

	succeeds(iso_clause_2_03) :-
		{clause(legs(I,6), Body)},
		Body == insect(I).

	succeeds(iso_clause_2_04) :-
		{clause(legs(C,7), Body)},
		Body == (call(C),call(C)).

	succeeds(iso_clause_2_05) :-
		findall(I-T, {clause(insect(I),T)}, L),
		L == [ant-true, bee-true].

	fails(iso_clause_2_06) :-
		{clause(x, _Body)}.

	throws(iso_clause_2_07, error(instantiation_error,_)) :-
		{clause(_, _B)}.

	throws(iso_clause_2_08, error(type_error(callable,4),_)) :-
		{clause(4, _B)}.

	succeeds(iso_clause_2_09) :-
		catch(
			{clause(elk(_N), _Body)},
			Error,
			(	Error = error(permission_error(access,private_procedure,elk/1),_)
			;	% the second exception term is used in some of the Prolog compilers supporting modules
				Error = error(permission_error(access,private_procedure,':'(user,elk/1)),_)
			)
		).

	throws(iso_clause_2_10, [error(permission_error(access,private_procedure,atom/1),_), error(permission_error(access,private_procedure,':'(user,atom/1)),_)]) :-
		% the second exception term is used in some of the Prolog compilers supporting modules
		{clause(atom(_), _Body)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_clause_2_11) :-
			{clause(legs(A,6), insect(f(A)))}.
	:- else.
		- succeeds(iso_clause_2_11) :-
			% STO; Undefined
			{clause(legs(A,6), insect(f(A)))}.
	:- endif.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_clause_2_12, error(type_error(callable,5),_)) :-
		{clause(f(_), 5)}.

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
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

	throws(iso_clause_2_09, error(permission_error(access,private_procedure,elk/1),_)) :-
		{clause(elk(_N), _Body)}.

	throws(iso_clause_2_10, error(permission_error(access,private_procedure,atom/1),_)) :-
		{clause(atom(_), _Body)}.

	- succeeds(iso_clause_2_11) :-
		% STO; Undefined
		{clause(legs(A,6), insect(f(A)))}.

	throws(eddbali_clause_2_12, error(type_error(callable,5),_)) :-
		{clause(f(_), 5)}.

:- end_object.

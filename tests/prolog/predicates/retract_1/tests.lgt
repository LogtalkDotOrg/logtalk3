%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/09,
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
		{retract(insect(I)), write(I), nl, retract(insect(bee))},
		I == ant.

	:- if(current_logtalk_flag(coinduction, supported)).
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

	:- if(current_logtalk_flag(prolog_conformance, iso_strict)).
		throws(iso_retract_1_11, error(permission_error(modify,static_procedure,atom/1),_)) :-
			{retract((atom(X) :- X =='[]'))}.
	:- else.
		throws(iso_retract_1_11, [error(permission_error(modify,static_procedure,atom/1),_), error(permission_error(modify,static_procedure,':'(user,atom/1)),_)]) :-
			% the second exception term is used in some of the Prolog compilers supporting modules
			{retract((atom(X) :- X =='[]'))}.
	:- endif.

:- end_object.

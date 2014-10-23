%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.3.4

b(X) :-
	Y = (write(X), X),
	call(Y).

a(1).
a(2).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard call/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.3.4

	succeeds(iso_call_1_01) :-
		{call(!)}.

	fails(iso_call_1_02) :-
		{call(fail)}.

	fails(iso_call_1_03) :-
		{call((fail, _X))}.

	fails(iso_call_1_04) :-
		{call((fail, call(1)))}.

	throws(iso_call_1_05, error(instantiation_error,_)) :-
		{b(_)}.

	throws(iso_call_1_06, [error(type_error(callable,(write(3),3)),_), error(type_error(callable,3),_)]) :-
		% the first exception term is fixed in ISO/IEC 13211-1:1995/Cor.2:2012
		% the second exception term is a common but not conforming alternative
		{b(3)}.

	succeeds(iso_call_1_07) :-
		{(Z = !, call((Z=!, a(X), Z)))},
		Z == !, X == 1.

	succeeds(iso_call_1_08) :-
		findall(Z-X, {call((Z=!, a(X), Z))}, L),
		L == [!-1, !-2].

	throws(iso_call_1_09, error(instantiation_error,_)) :-
		{call((write(3), _X))}.

	throws(iso_call_1_10, error(type_error(callable,1),_)) :-
		{call((write(3), call(1)))}.

	throws(iso_call_1_11, error(instantiation_error,_)) :-
		{call(_X)}.

	throws(iso_call_1_12, error(type_error(callable,1),_)) :-
		{call(1)}.

	throws(iso_call_1_13, [error(type_error(callable,(fail,1)),_), error(type_error(callable,1),_)]) :-
		% the second exception term is a common but not conforming alternative
		{call((fail, 1))}.

	throws(iso_call_1_14, [error(type_error(callable,(write(3),1)),_), error(type_error(callable,1),_)]) :-
		% the second exception term is a common but not conforming alternative
		{call((write(3), 1))}.

	throws(iso_call_1_15, [error(type_error(callable,(1;true)),_), error(type_error(callable,1),_)]) :-
		% the second exception term is a common but not conforming alternative
		{call((1; true))}.

:- end_object.

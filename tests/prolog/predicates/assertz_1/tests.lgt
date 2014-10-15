%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.2.4

:- dynamic(legs/2).
:- dynamic(foo/1).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard assertz/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.2.4

	succeeds(iso_assertz_1_01) :-
		{assertz(legs(spider, 8))}.

	succeeds(iso_assertz_1_02) :-
		{assertz((legs(B, 2):-bird(B)))}.

	succeeds(iso_assertz_1_03) :-
		{assertz((foo(X):- X -> call(X)))}.

	throws(iso_assertz_1_04, error(instantiation_error,_)) :-
		{assertz(_)}.

	throws(iso_assertz_1_05, error(type_error(callable,4),_)) :-
		{assertz(4)}.

	throws(iso_assertz_1_06, error(type_error(callable,4),_)) :-
		{assertz((foo :- 4))}.

	throws(iso_assertz_1_07, error(permission_error(modify,static_procedure,atom/1),_)) :-
		{assertz((atom(_) :- true))}.

:- end_object.

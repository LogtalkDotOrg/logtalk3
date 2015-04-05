%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/04/05,
		comment is 'Unit tests for the ISO Prolog standard sqrt/1 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.7.4

	succeeds(iso_sqrt_1_01) :-
		{X is sqrt(0.0)},
		X == 0.0.

	succeeds(iso_sqrt_1_02) :-
		{X is sqrt(1)},
		X == 1.0.

	succeeds(iso_sqrt_1_03) :-
		{X is sqrt(1.21)},
		X =~= 1.1.

	throws(iso_sqrt_1_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{_X is sqrt(X)}.

	throws(iso_sqrt_1_05, error(evaluation_error(undefined),_)) :-
		{_X is sqrt(-1.0)}.

	throws(iso_sqrt_1_06, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is sqrt(Foo)}.

	throws(lgt_sqrt_1_07, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is sqrt(Foo)}.

	throws(lgt_sqrt_1_08, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is sqrt(Foo)}.

	variable(_).

	foo(0, foo).
	foo(1, foo(_)).
	foo(2, foo(_,_)).

:- end_object.

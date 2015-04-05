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
		comment is 'Unit tests for the ISO Prolog standard atan2/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.13.4

	succeeds(iso_atan2_2_01) :-
		{X is atan2(1, 0)},
		X =~= 1.570796.

	succeeds(iso_atan2_2_02) :-
		{X is atan2(0, -1)},
		X =~= 3.1415927.

	throws(iso_atan2_2_03, error(evaluation_error(undefined),_)) :-
		{_X is atan2(0, 0)}.

	throws(lgt_atan2_2_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{_X is atan2(1, X)}.

	throws(lgt_atan2_2_05, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(X),
		{_X is atan2(X, 0)}.

	throws(lgt_atan2_2_06, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is atan2(1, Foo)}.

	throws(lgt_atan2_2_07, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is atan2(Foo, 0)}.

	throws(lgt_atan2_2_08, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is atan2(1, Foo)}.

	throws(lgt_atan2_2_09, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is atan2(Foo, 0)}.

	throws(lgt_atan2_2_10, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is atan2(1, Foo)}.

	throws(lgt_atan2_2_11, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is atan2(Foo, 0)}.

	variable(_).

	foo(0, foo).
	foo(1, foo(_)).
	foo(2, foo(_,_)).

:- end_object.

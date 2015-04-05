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
		comment is 'Unit tests for the ISO Prolog standard (^)/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.10.4

	succeeds(iso_integer_power_2_01) :-
		{X is ^(0,0)},
		X == 1.

	succeeds(iso_integer_power_2_02) :-
		{X is 3^1.0},
		X == 3.0.

	succeeds(iso_integer_power_2_03) :-
		{X is 3^3},
		X == 27.

	succeeds(iso_integer_power_2_04) :-
		{X is 3^27},
		X == 7625597484987.

	succeeds(iso_integer_power_2_05) :-
		{X is 3^3^3},
		X == 7625597484987.

	throws(iso_integer_power_2_06, error(type_error(float,2),_)) :-
		% the ISO standard specifies an evaluation_error(undefined) but there seems
		% to be consensus that the correct exception is a type_error(float,...)
		{_X is 2^(-1)}.

	succeeds(iso_integer_power_2_07) :-
		{X is 1^(-1)},
		X == 1.

	succeeds(iso_integer_power_2_08) :-
		{X is 0^0},
		X == 1.

	succeeds(iso_integer_power_2_09) :-
		{X is 2^ -1.5},
		X =~= 0.353553.

	throws(lgt_integer_power_2_10, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is 3^N}.

	throws(lgt_integer_power_2_11, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is N^3}.

	throws(lgt_integer_power_2_12, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is 3^Foo}.

	throws(lgt_integer_power_2_13, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is Foo^3}.

	throws(lgt_integer_power_2_14, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is 3^Foo}.

	throws(lgt_integer_power_2_15, error(type_error(evaluable,foo/1),_)) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is Foo^3}.

	throws(lgt_integer_power_2_16, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is 3^Foo}.

	throws(lgt_integer_power_2_17, error(type_error(evaluable,foo/2),_)) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is Foo^3}.

	variable(_).

	foo(0, foo).
	foo(1, foo(_)).
	foo(2, foo(_,_)).

:- end_object.

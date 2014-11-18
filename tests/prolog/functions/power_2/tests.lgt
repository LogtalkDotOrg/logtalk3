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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/18,
		comment is 'Unit tests for the ISO Prolog standard (**)/2 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.1.4

	succeeds(iso_power_2_01) :-
		{X is '**'(5,3)},
		X =~= 125.0.

	succeeds(iso_power_2_02) :-
		{X is '**'(-5.0,3)},
		X =~= -125.0.

	succeeds(iso_power_2_03) :-
		{X is '**'(5,-1)},
		X =~= 0.2.

	throws(iso_power_2_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '**'(77,N)}.

	throws(iso_power_2_05, error(type_error(evaluable,foo/0),_)) :-
		% try to delay the error to runtime
		foo(Foo),
		{_X is '**'(Foo,2)}.

	succeeds(iso_power_2_06) :-
		{X is '**'(5,3.0)},
		X =~= 125.0.

	succeeds(iso_power_2_07) :-
		{X is '**'(0,0.0)},
		X =~= 1.0.

	variable(_).

	foo(foo).

:- end_object.

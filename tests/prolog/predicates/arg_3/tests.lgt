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
		date is 2014/10/14,
		comment is 'Unit tests for the ISO Prolog standard arg/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.2.4

	succeeds(iso_arg_3_01) :-
		{arg(1, foo(a,b), a)}.

	succeeds(iso_arg_3_02) :-
		{arg(1, foo(a,b), X)},
		X == a.

	succeeds(iso_arg_3_03) :-
		{arg(1, foo(X,b), a)},
		X == a.

	succeeds(iso_arg_3_04) :-
		{arg(1, foo(X,b), Y)},
		Y == X.

	fails(iso_arg_3_05) :-
		{arg(1, foo(a,b), b)}.

	fails(iso_arg_3_06) :-
		{arg(0, foo(a,b), foo)}.

	fails(iso_arg_3_07) :-
		{arg(3, foo(3,4), _)}.

	throws(iso_arg_3_08, error(instantiation_error,_)) :-
		{arg(_, foo(a,b), a)}.

	throws(iso_arg_3_09, error(instantiation_error,_)) :-
		{arg(1, _, a)}.

	throws(iso_arg_3_10, error(type_error(compound,atom),_)) :-
		{arg(0, atom, _)}.

	throws(iso_arg_3_11, error(type_error(compound,3),_)) :-
		{arg(0, 3, _)}.

	- succeeds(iso_arg_3_12) :-
		% STO; Undefined
		{arg(1, foo(X), u(X))}.

:- end_object.

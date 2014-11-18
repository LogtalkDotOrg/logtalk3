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
		comment is 'Unit tests for the ISO Prolog standard (\\)/1 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.5.4

	succeeds(iso_bitwise_complement_1_01) :-
		{X is '\\'('\\'(10))},
		X == 10.

	succeeds(iso_bitwise_complement_1_02) :-
		{X is \(\(10))},
		X == 10.

	succeeds(iso_bitwise_complement_1_03) :-
		% assumes two's complement representation for negative integers
		{X is \(10)},
		X == -11.

	throws(iso_bitwise_complement_1_04, error(instantiation_error,_)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is '\\'(N)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(sics_bitwise_complement_1_05, error(type_error(integer,2.5),_)) :-
		{_X is '\\'(2.5)}.

	variable(_).

:- end_object.

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
		comment is 'Unit tests for the ISO Prolog standard (>>)/1 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.4.1.4

	succeeds(iso_bitwise_right_shift_2_01) :-
		{X is '>>'(16, 2)},
		X == 4.

	succeeds(iso_bitwise_right_shift_2_02) :-
		{X is '>>'(19, 2)},
		X == 4.

	succeeds(iso_bitwise_right_shift_2_03) :-
		% assumes two's complement representation for negative integers
		{X is '>>'(-16, 2)},
		X == -4.

	throws(iso_bitwise_right_shift_1_04, error(instantiation_error,_)) :-
		{_X is '>>'(77, _N)}.

	throws(iso_bitwise_right_shift_2_05, error(type_error(evaluable,foo/0),_)) :-
		{_X is '>>'(foo, 2)}.

	throws(iso_bitwise_right_shift_2_06, error(type_error(integer,1.0),_)) :-
		{_X is '>>'(1.0, 2)}.

:- end_object.

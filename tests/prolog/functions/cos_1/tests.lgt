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
		comment is 'Unit tests for the ISO Prolog standard cos/1 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.3.4

	succeeds(iso_cos_1_01) :-
		{X is cos(0.0)},
		X == 1.0.

	throws(iso_cos_1_02, error(instantiation_error,_)) :-
		{_X is cos(_N)}.

	succeeds(iso_cos_1_03) :-
		{X is cos(0)},
		X == 1.0.

	throws(iso_cos_1_04, error(type_error(evaluable,foo/0),_)) :-
		{_X is cos(foo)}.

	succeeds(iso_cos_1_05) :-
		{PI is atan(1.0)*4, X is cos(PI/2.0)},
		X =~= 0, PI =~= 3.14159.

:- end_object.

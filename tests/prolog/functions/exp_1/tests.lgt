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
		comment is 'Unit tests for the ISO Prolog standard exp/1 built-in function.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	:- discontiguous([
		succeeds/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 9.3.5.4

	succeeds(iso_exp_1_01) :-
		{X is exp(0.0)},
		X == 1.0.

	succeeds(iso_exp_1_02) :-
		{E is exp(1.0)},
		E =~= 2.71828.

	throws(iso_exp_1_03, error(instantiation_error,_)) :-
		{_X is exp(_N)}.

	succeeds(iso_exp_1_04) :-
		{X is exp(0)},
		X == 1.0.

	throws(iso_exp_1_05, error(type_error(evaluable,foo/0),_)) :-
		{_X is exp(foo)}.

:- end_object.

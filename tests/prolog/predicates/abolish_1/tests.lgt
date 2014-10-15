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
		comment is 'Unit tests for the ISO Prolog standard abolish/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.9.4.4

	succeeds(iso_abolish_1_01) :-
		{abolish(foo/2)}.

	throws(iso_abolish_1_02, error(instantiation_error,_)) :-
		{abolish(foo/_)}.

	throws(iso_abolish_1_03, error(type_error(predicate_indicator,foo),_)) :-
		{abolish(foo)}.

	throws(iso_abolish_1_04, error(type_error(predicate_indicator,foo(X)),_)) :-
		{abolish(foo(X))}.

	throws(iso_abolish_1_05, error(permission_error(modify,static_procedure,abolish/1),_)) :-
		{abolish(abolish/1)}.

:- end_object.

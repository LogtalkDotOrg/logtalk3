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
		comment is 'Unit tests for the ISO Prolog standard copy_term/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.4.4

	succeeds(iso_copy_term_2_01) :-
		{copy_term(_X, _Y)}.

	succeeds(iso_copy_term_2_02) :-
		{copy_term(_X, 3)}.

	succeeds(iso_copy_term_2_03) :-
		{copy_term(_, a)}.

	succeeds(iso_copy_term_2_04) :-
		{copy_term(a+X, X+b)},
		X == a.

	succeeds(iso_copy_term_2_05) :-
		{copy_term(_, _)}.

	succeeds(iso_copy_term_2_06) :-
		{copy_term(X+X+_Y, A+B+B)},
		A == B.

	fails(iso_copy_term_2_07) :-
		{copy_term(a, b)}.

	fails(iso_copy_term_2_08, error(instantiation_error,_)) :-
		{copy_term(a+X, X+b)}.

:- end_object.

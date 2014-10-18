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
		comment is 'Unit tests for the ISO Prolog standard subsumes_term/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.2.4.4

	succeeds(iso_subsumes_term_2_01) :-
		{subsumes_term(a, a)}.

	succeeds(iso_subsumes_term_2_02) :-
		{subsumes_term(f(_X,_Y), f(Z,Z))}.

	fails(iso_subsumes_term_2_03) :-
		{subsumes_term(f(Z,Z), f(_X,_Y))}.

	fails(iso_subsumes_term_2_04) :-
		{subsumes_term(g(X), g(f(X)))}.

	fails(iso_subsumes_term_2_05) :-
		{subsumes_term(X, f(X))}.

	succeeds(iso_subsumes_term_2_06) :-
		{subsumes_term(X, Y), subsumes_term(Y, f(X))}.

:- end_object.

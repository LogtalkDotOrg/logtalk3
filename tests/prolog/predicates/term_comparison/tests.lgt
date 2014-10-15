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
		comment is 'Unit tests for the ISO Prolog standard term comparison built-in predicates.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.4.1.4

	succeeds(iso_term_comparison_01) :-
		{'@=<'(1.0, 1)}.

	succeeds(iso_term_comparison_02) :-
		{'@<'(1.0, 1)}.

	fails(iso_term_comparison_03) :-
		{'\\=='(1, 1)}.

	succeeds(iso_term_comparison_04) :-
		{'@=<'(aardvark, zebra)}.

	succeeds(iso_term_comparison_05) :-
		{'@=<'(short, short)}.

	succeeds(iso_term_comparison_06) :-
		{'@=<'(short, shorter)}.

	fails(iso_term_comparison_07) :-
		{'@>='(short, shorter)}.

	fails(iso_term_comparison_08) :-
		{'@<'(foo(a,b), north(a))}.

	succeeds(iso_term_comparison_09) :-
		{'@>'(foo(b), foo(a))}.

	succeeds(iso_term_comparison_10) :-
		{'@<'(foo(a, _X), foo(b, _Y))}.

	succeeds(iso_term_comparison_11) :-
		(	{'@<'(foo(_X, a), foo(_Y, b))} ->
			true
		;	true
		).

	succeeds(iso_term_comparison_12) :-
		{'@=<'(X, X)}.

	succeeds(iso_term_comparison_13) :-
		{'=='(X, X)}.

	succeeds(iso_term_comparison_14) :-
		(	{'@=<'(_X, _Y)} ->
			true
		;	true
		).

	fails(iso_term_comparison_15) :-
		{'=='(_X, _Y)}.

	succeeds(iso_term_comparison_16) :-
		{\==(_, _)}.

	fails(iso_term_comparison_17) :-
		{'=='(_, _)}.

	succeeds(iso_term_comparison_18) :-
		(	{'@=<'(_, _)} ->
			true
		;	true
		).

	succeeds(iso_term_comparison_19) :-
		(	{'@=<'(foo(_X, a), foo(_Y, b))} ->
			true
		;	true
		).

:- end_object.

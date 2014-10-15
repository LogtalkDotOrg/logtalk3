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
		comment is 'Unit tests for the ISO Prolog standard (;)/2 control construct.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 7.8.6.4

	succeeds(iso_disjunction_2_01) :-
		{';'(true, fail)}.

	fails(iso_disjunction_2_02) :-
		{';'((!, fail), true)}.

	succeeds(iso_disjunction_2_03) :-
		{';'(!, call(3))}.

	succeeds(iso_disjunction_2_04) :-
		{';'((X=1, !), X=2)},
		X == 1.

	succeeds(iso_disjunction_2_05) :-
		findall(X, {';'(X=1, X=2)}, L),
		L == [1,2].

:- end_object.

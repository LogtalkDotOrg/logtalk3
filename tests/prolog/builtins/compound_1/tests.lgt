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
		comment is 'Unit tests for the ISO Prolog standard compound/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.6.4

	fails(iso_compound_1_01) :-
		{compound(33.3)}.

	fails(iso_compound_1_02) :-
		{compound(-33.3)}.

	succeeds(iso_compound_1_03) :-
		{compound(-a)}.

	fails(iso_compound_1_04) :-
		{compound(_)}.

	fails(iso_compound_1_05) :-
		{compound(a)}.

	succeeds(iso_compound_1_06) :-
		{compound(a(b))}.

	fails(iso_compound_1_07) :-
		{compound([])}.

	succeeds(iso_compound_1_08) :-
		{compound([a])}.

:- end_object.

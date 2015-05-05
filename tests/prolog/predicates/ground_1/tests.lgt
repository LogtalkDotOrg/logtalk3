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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard ground/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.3.10.4

	succeeds(iso_ground_1_01) :-
		{ground(3)}.

	fails(iso_ground_1_02) :-
		{ground(a(1, _))}.

	% tests from the ECLiPSe test suite

	succeeds(eclipse_ground_1_03) :-
		{ground(a)}.

	succeeds(eclipse_ground_1_04) :-
		{ground(f(3))}.

	fails(eclipse_ground_1_05) :-
		{ground(_)}.

	fails(eclipse_ground_1_06) :-
		{ground(f(_))}.

:- end_object.

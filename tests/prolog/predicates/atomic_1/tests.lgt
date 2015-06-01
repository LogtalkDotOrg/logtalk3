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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard atomic/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.5.4

	succeeds(iso_atomic_1_01) :-
		{atomic(atom)}.

	fails(iso_atomic_1_02) :-
		{atomic(a(b))}.

	fails(iso_atomic_1_03) :-
		{atomic(_Var)}.

	succeeds(iso_atomic_1_04) :-
		{atomic(6)}.

	succeeds(iso_atomic_1_05) :-
		{atomic(3.3)}.

	% tests from the Logtalk portability work

	succeeds(lgt_atomic_1_06) :-
		{atomic([])}.

	succeeds(lgt_atomic_1_07) :-
		{atom(!)}.

	succeeds(lgt_atomic_1_08) :-
		{atom({})}.

:- end_object.

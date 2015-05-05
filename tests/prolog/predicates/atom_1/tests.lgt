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
		comment is 'Unit tests for the ISO Prolog standard atom/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.2.4

	succeeds(iso_atom_1_01) :-
		{atom(atom)}.

	succeeds(iso_atom_1_02) :-
		{atom('string')}.

	fails(iso_atom_1_03) :-
		{atom(a(b))}.

	fails(iso_atom_1_04) :-
		{atom(_Var)}.

	succeeds(iso_atom_1_05) :-
		{atom([])}.

	fails(iso_atom_1_06) :-
		{atom(6)}.

	fails(iso_atom_1_07) :-
		{atom(3.3)}.

	succeeds(lgt_atom_1_08) :-
		{atom(!)}.

	succeeds(lgt_atom_1_08) :-
		{atom({})}.

:- end_object.

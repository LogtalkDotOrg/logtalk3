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
		comment is 'Unit tests for the ISO Prolog standard atom_chars/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.4.4

	succeeds(iso_atom_chars_2_01) :-
		{atom_chars('', L)},
		L == [].

	succeeds(iso_atom_chars_2_02) :-
		{atom_chars([], L)},
		L == ['[',']'].

	succeeds(iso_atom_chars_2_03) :-
		{atom_chars('''', L)},
		L == [''''].

	succeeds(iso_atom_chars_2_04) :-
		{atom_chars('ant', L)},
		L == ['a','n','t'].

	succeeds(iso_atom_chars_2_05) :-
		{atom_chars(Str, ['s','o','p'])},
		Str == 'sop'.

	succeeds(iso_atom_chars_2_06) :-
		{atom_chars('North', ['N'| X])},
		X == ['o','r','t','h'].

	fails(iso_atom_chars_2_07) :-
		{atom_chars('soap', ['s','o','p'])}.

	throws(iso_atom_chars_2_08, error(instantiation_error,_)) :-
		{atom_chars(_X, _Y)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	throws(eddbali_atom_chars_2_09, error(instantiation_error,_)) :-
		{atom_chars(_A, [a,_E,c])}.

	throws(eddbali_atom_chars_2_10, error(instantiation_error,_)) :-
		{atom_chars(_A, [a,b|_L])}.

	throws(eddbali_atom_chars_2_11, error(type_error(atom,f(a)),_)) :-
		{atom_chars(f(a), _L)}.

	throws(eddbali_atom_chars_2_12, error(type_error(list,iso),_)) :-
		{atom_chars(_A, iso)}.

	throws(eddbali_atom_chars_2_13, error(type_error(character,f(b)),_)) :-
		{atom_chars(_A, [a,f(b)])}.

	% the following two tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- succeeds(sics_atom_chars_2_14) :-
		{atom_chars('Pécs', L)},
		L == ['P','é','c','s'].

	- succeeds(sics_atom_chars_2_15) :-
		{atom_chars(A, ['P','é','c','s'])},
		A == 'Pécs'.

:- end_object.

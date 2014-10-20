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
		comment is 'Unit tests for the ISO Prolog standard atom_codes/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.5.4

	succeeds(iso_atom_codes_2_01) :-
		{atom_codes('', L)},
		L == [].

	succeeds(iso_atom_codes_2_02) :-
		{atom_codes([], L)},
		L == [0'[, 0']].

	succeeds(iso_atom_codes_2_03) :-
		{atom_codes('''', L)},
		L == [39].

	succeeds(iso_atom_codes_2_04) :-
		{atom_codes('ant', L)},
		L == [0'a, 0'n, 0't].

	succeeds(iso_atom_codes_2_05) :-
		{atom_codes(Str, [0's,0'o,0'p])},
		Str == 'sop'.

	succeeds(iso_atom_codes_2_06) :-
		{atom_codes('North', [0'N| X])},
		X == [0'o,0'r,0't,0'h].

	fails(iso_atom_codes_2_07) :-
		{atom_codes('soap', [0's, 0'o, 0'p])}.

	throws(iso_atom_codes_2_08, error(instantiation_error,_)) :-
		{atom_codes(_X, _Y)}.

	throws(eddbali_atom_codes_2_09, error(type_error(atom,f(a)),_)) :-
		{atom_codes(f(a), _L)}.

	throws(eddbali_atom_codes_2_10, error(type_error(list,0'x),_)) :-
		{atom_codes(_, 0'x)}.

	throws(eddbali_atom_codes_2_11, error(representation_error(character_code),_)) :-
		{atom_codes(_A, [0'i,0's,-1])}.

%	the following tests result in a syntax error with several Prolog compilers
%
%	succeeds(sics_atom_codes_2_12) :-
%		{atom_codes('Pécs', C)},
%		C == [0'P,0'é,0'c,0's].
%
%	succeeds(sics_atom_codes_2_13) :-
%		{atom_codes(A, [0'P,0'é,0'c,0's])},
%		A == 'Pécs'.

:- end_object.

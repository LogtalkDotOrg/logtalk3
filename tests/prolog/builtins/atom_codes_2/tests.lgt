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

:- end_object.

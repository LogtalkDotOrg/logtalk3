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
		comment is 'Unit tests for the ISO Prolog standard char_code/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.6.4

	succeeds(iso_char_code_2_01) :-
		{char_code(a, Code)},
		Code == 0'a.

	succeeds(iso_char_code_2_02) :-
		{char_code(Char, 99), atom_codes(Atom, [99])},
		Char == Atom.

	succeeds(iso_char_code_2_03) :-
		{char_code(Char, 0'c)},
		Char == c.

	succeeds(iso_char_code_2_04) :-
		% the ISO standard also allows a representation_error(character_code)
		{char_code(_Char, 163)}.

	succeeds(iso_char_code_2_05) :-
		{char_code(b, 0'b)}.

	throws(iso_char_code_2_06, error(type_error(character,'ab'),_)) :-
		{char_code('ab', _Code)}.

	throws(iso_char_code_2_07, error(instantiation_error,_)) :-
		{char_code(_Char, _Code)}.

	throws(eddbali_char_code_2_08, error(type_error(integer,x),_)) :-
		{char_code(a, x)}.

	throws(eddbali_char_code_2_09, error(representation_error(character_code),_)) :-
		{char_code(_Char, -2)}.

:- end_object.

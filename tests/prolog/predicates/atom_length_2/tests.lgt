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
		comment is 'Unit tests for the ISO Prolog standard atom_length/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.1.4

	succeeds(iso_atom_length_2_01) :-
		{atom_length('enchanted evening', N)},
		N == 17.

	succeeds(iso_atom_length_2_02) :-
		{atom_length('enchanted\
 evening', N)},
		N == 17.

	succeeds(iso_atom_length_2_03) :-
		{atom_length('', N)},
		N == 0.

	fails(iso_atom_length_2_04) :-
		{atom_length('scarlet', 5)}.

	throws(iso_atom_length_2_05, error(instantiation_error,_)) :-
		{atom_length(_Atom, 4)}.

	throws(iso_atom_length_2_06, error(type_error(atom,1.23),_)) :-
		{atom_length(1.23, 4)}.

	throws(iso_atom_length_2_07, error(type_error(integer,'4'),_)) :-
		{atom_length(atom, '4')}.

:- end_object.

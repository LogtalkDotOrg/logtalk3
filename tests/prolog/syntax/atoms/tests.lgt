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
		date is 2014/11/07,
		comment is 'Unit tests for the ISO Prolog standard atom syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.1.3

	succeeds(iso_atom_01) :-
		^^set_text_input('a. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_02) :-
		^^set_text_input('\'A\'. '),
		{read(T)},
		atom(T).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.2.1

	succeeds(iso_atom_03) :-
		^^set_text_input('\'a''''b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_04) :-
		^^set_text_input('\'a\\\'b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_05) :-
		^^set_text_input('\'a\\\"b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_06) :-
		^^set_text_input('\'a\\\\b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_07) :-
		^^set_text_input('\'a\\123\\b\'. '),
		{read(T)},
		T == 'aSb'.

	succeeds(iso_atom_08) :-
		^^set_text_input('\'a\\x53\\b\'. '),
		{read(T)},
		T == 'aSb'.

	cleanup :-
		^^clean_text_input.

:- end_object.

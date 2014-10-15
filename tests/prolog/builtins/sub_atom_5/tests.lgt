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
		comment is 'Unit tests for the ISO Prolog standard integer/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.3.4

	succeeds(iso_sub_atom_5_01) :-
		{sub_atom(abracadabra, 0, 5, _, S2)},
		S2 == 'abrac'.

	succeeds(iso_sub_atom_5_02) :-
		{sub_atom(abracadabra, _, 5, 0, S2)},
		S2 == 'dabra'.

	succeeds(iso_sub_atom_5_03) :-
		{sub_atom(abracadabra, 3, Length, 3, S2)},
		Length == 5, S2 == 'acada'.

	succeeds(iso_sub_atom_5_04) :-
		findall(Before-After, {sub_atom(abracadabra,Before,2,After,ab)}, L),
		L == [0-9, 7-2].

	succeeds(iso_sub_atom_5_05) :-
		{sub_atom('Banana', 3, 2, _, S2)},
		S2 == 'an'.

	succeeds(iso_sub_atom_5_06) :-
		findall(S2, {sub_atom('charity',_,3,_,S2)}, L),
		L == ['cha', 'har', 'ari', 'rit', 'ity'].

	succeeds(iso_sub_atom_5_07) :-
		findall(Start-Lenght-SubAtom, {sub_atom('ab',Start,Lenght,_,SubAtom)}, L),
		L == [0-0-'', 0-1-'a', 0-2-'ab', 1-0-'', 1-1-'b', 2-0-''].

:- end_object.

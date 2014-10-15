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
		comment is 'Unit tests for the ISO Prolog standard atom_concat/3 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.2.4

	succeeds(iso_atom_concat_3_01) :-
		{atom_concat('hello', ' world', S3)},
		S3 == 'hello world'.

	succeeds(iso_atom_concat_3_02) :-
		{atom_concat('hello', ' world', S3)},
		S3 == 'hello world'.

	fails(iso_atom_concat_3_03) :-
		{atom_concat('hello',' world', 'small world')}.

	succeeds(iso_atom_concat_3_04) :-
		findall(T1-T2, {atom_concat(T1, T2, 'hello')}, L),
		L == [''-'hello', 'h'-'ello', 'he'-'llo', 'hel'-'lo', 'hell'-'o', 'hello'-''].

	throws(iso_atom_concat_3_05, error(instantiation_error,_)) :-
		{atom_concat(small, _V2, _V4)}.

:- end_object.

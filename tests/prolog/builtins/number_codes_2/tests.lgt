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
		comment is 'Unit tests for the ISO Prolog standard number_codes/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.8.4

	succeeds(iso_number_codes_2_01) :-
		{number_codes(33, L)},
		L == [0'3,0'3].

	succeeds(iso_number_codes_2_02) :-
		{number_codes(33, [0'3,0'3])}.

	succeeds(iso_number_codes_2_03) :-
		{number_codes(33.0, L), number_codes(N, L)},
		N == 33.0.

	succeeds(iso_number_codes_2_04) :-
		{number_codes(33.0, [0'3| _L])}.

	succeeds(iso_number_codes_2_05) :-
		{number_codes(A, [0'-,0'2,0'5])},
		A == -25.

	succeeds(iso_number_codes_2_06) :-
		{number_codes(A, [0' , 0'3])},
		A == 3.

	succeeds(iso_number_codes_2_07) :-
		{number_codes(A, [0'0,0'x,0'f])},
		A == 15.

	throws(iso_number_codes_2_08, error(syntax_error,_)) :-
		{number_codes(A, [0'0,39,0'a])},
		A == 0'a.

	succeeds(iso_number_codes_2_09) :-
		{number_codes(A, [0'4,0'.,0'2])},
		A == 4.2.

	succeeds(iso_number_codes_2_10) :-
		{number_codes(A, [0'4,0'2,0'.,0'0,0'e,0'-,0'1])},
		A == 4.2.

:- end_object.

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
		comment is 'Unit tests for the ISO Prolog standard number_chars/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.7.4

	succeeds(iso_number_chars_2_01) :-
		{number_chars(33, L)},
		L == ['3','3'].

	succeeds(iso_number_chars_2_02) :-
		{number_chars(33, ['3','3'])}.

	succeeds(iso_number_chars_2_03) :-
		{number_chars(33.0, L), number_chars(N, L)},
		N == 33.0.

	succeeds(iso_number_chars_2_04) :-
		{number_chars(X, ['3','.','3','E','+','0'])},
		X == 3.3.

	succeeds(iso_number_chars_2_05) :-
		{number_chars(3.3, ['3'| _L])}.

	succeeds(iso_number_chars_2_06) :-
		{number_chars(A, ['-','2','5'])},
		A == -25.

	succeeds(iso_number_chars_2_07) :-
		{number_chars(A, ['\n',' ','3'])},
		A == 3.

	throws(iso_number_chars_2_08, error(syntax_error,_)) :-
		{number_chars(_A, ['3',' '])}.

	succeeds(iso_number_chars_2_09) :-
		{number_chars(A, ['0',x,f])},
		A == 15.

	succeeds(iso_number_chars_2_10) :-
		{number_chars(A, ['0','''',a])},
		A == 0'a.

	succeeds(iso_number_chars_2_11) :-
		{number_chars(A, ['4','.','2'])},
		A == 4.2.

	succeeds(iso_number_chars_2_12) :-
		{number_chars(A, ['4','2','.','0','e','-','1'])},
		A == 4.2.

:- end_object.

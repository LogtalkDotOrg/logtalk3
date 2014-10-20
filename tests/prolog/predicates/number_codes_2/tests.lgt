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

	:- discontiguous([
		succeeds/1, throws/2
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

	succeeds(iso_number_codes_2_08) :-
		{number_codes(A, [0'0,39,0'a])},
		A == 0'a.

	succeeds(iso_number_codes_2_09) :-
		{number_codes(A, [0'4,0'.,0'2])},
		A == 4.2.

	succeeds(iso_number_codes_2_10) :-
		{number_codes(A, [0'4,0'2,0'.,0'0,0'e,0'-,0'1])},
		A == 4.2.

	throws(eddbali_number_codes_2_11, error(instantiation_error,_)) :-
		{number_codes(_A, _L)}.

	throws(eddbali_number_codes_2_12, error(type_error(number,a),_)) :-
		{number_codes(a, _L)}.

	throws(eddbali_number_codes_2_13, error(type_error(list,4),_)) :-
		{number_codes(_A, 4)}.

	throws(eddbali_number_codes_2_14, error(representation_error(character_code),_)) :-
		{number_codes(_A, [0'4,-1])}.

	throws(sics_number_codes_2_15, error(instantiation_error,_)) :-
		{number_codes(_A, [0'a|_L])}.

	throws(sics_number_codes_2_16, error(instantiation_error,_)) :-
		{number_codes(_A, [0'a,_L])}.

	succeeds(sics_number_codes_2_17) :-
		{number_chars(X, [' ','0','x','1','1','1']), number_codes(X, Y)},
		X == 273, Y == [50,55,51].

	succeeds(sics_number_codes_2_18) :-
		{number_chars(X, [' ','0','o','1','1','1']), number_codes(X, Y)},
		X == 73, Y == [55,51].

	succeeds(sics_number_codes_2_19) :-
		{number_chars(X, [' ','0','b','1','1','1']), number_codes(X, Y)},
		X == 7, Y == [55].

	- succeeds(sics_number_codes_2_20) :-
		{number_codes(N, "0'\n")},
		N == 10.

	throws(sics_number_codes_2_21, error(syntax_error(_),_)) :-
		{number_codes(_N, "ä")}.

	throws(sics_number_codes_2_22, error(syntax_error(_),_)) :-
		{number_codes(_X,[0'0,0'x,0'0,0'.,0'0])}.

:- end_object.

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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2015/05/05,
		comment is 'Unit tests for the ISO Prolog standard keysort/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.4.4

	succeeds(iso_keysort_2_01) :-
		{keysort([1-1, 1-1], Sorted)},
		Sorted == [1-1, 1-1].

	succeeds(iso_keysort_2_02) :-
		{keysort([2-99, 1-a, 3-f(_), 1-z, 1-a, 2-44], Sorted)},
		Sorted = [1-a, 1-z, 1-a, 2-99, 2-44, 3-f(_)].

	succeeds(iso_keysort_2_03) :-
		{keysort([X-1,1-1],[2-1,1-1])},
		X == 2.

	- succeeds(iso_keysort_2_04) :-
		% STO; Undefined.
		{Pairs = [1-2|Pairs], keysort(Pairs, _Sorted)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(iso_keysort_2_05) :-
			{keysort([V-V], V)}.
	:- else.
		- succeeds(iso_keysort_2_05) :-
			% STO; Undefined.
			{keysort([V-V], V)}.
	:- endif.

	% tests from the ECLiPSe test suite

	throws(eclipse_keysort_2_06, error(instantiation_error,_)) :-
		{keysort(_, _)}.

	throws(eclipse_keysort_2_07, error(instantiation_error,_)) :-
		{keysort([1-a|_], _)}.

	throws(eclipse_keysort_2_08, error(type_error(list,3),_)) :-
		{keysort(3, _)}.

	throws(eclipse_keysort_2_09, error(type_error(list,[1-a|b]),_)) :-
		{keysort([1-a|b], _)}.

	throws(eclipse_keysort_2_10, error(type_error(list,3),_)) :-
		{keysort([], 3)}.

	throws(eclipse_keysort_2_11, error(type_error(list,[1-a|b]),_)) :-
		{keysort([], [1-a|b])}.

	throws(eclipse_keysort_2_12, error(instantiation_error,_)) :-
		{keysort([_], _)}.

	throws(eclipse_keysort_2_13, error(type_error(pair,1/a),_)) :-
		{keysort([1/a], _)}.

	throws(eclipse_keysort_2_14, error(type_error(pair,1/a),_)) :-
		{keysort([], [1/a])}.

:- end_object.

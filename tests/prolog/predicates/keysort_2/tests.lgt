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

	- succeeds(iso_keysort_2_05) :-
		% STO; Undefined.
		{keysort([V-V], V)}.

:- end_object.

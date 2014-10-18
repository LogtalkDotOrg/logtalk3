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
		comment is 'Unit tests for the ISO Prolog standard sort/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.4.3.4

	succeeds(iso_sort_2_01) :-
		{sort([1, 1], Sorted)},
		Sorted == [1].

	succeeds(iso_sort_2_02) :-
		{sort([1+Y, z, a, V, 1, 2, V, 1, 7.0, 8.0, 1+Y, 1+2, 8.0, -a, -X, a], Sorted)},
		Sorted = [V, 7.0, 8.0, 1, 2, a, z, -X, -a, 1+Y, 1+2].

	fails(iso_sort_2_03) :-
		{sort([1, 1], [1, 1])}.

	- succeeds(iso_sort_2_04) :-
		% STO; Undefined.
		{sort([V], V)}.

	succeeds(iso_sort_2_05) :-
		{sort([f(U),U,U,f(V),f(U),V],L)},
		(	L = [U,V,f(U),f(V)] ->
			true
		;	L = [V,U,f(V),f(U)]
		).

:- end_object.

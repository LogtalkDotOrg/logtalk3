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
		comment is 'Unit tests for the ISO Prolog standard findall/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.1.4

	succeeds(iso_findall_3_01) :-
		{findall(X, (X=1;X=2), S)},
		S == [1,2].

	succeeds(iso_findall_3_02) :-
		{findall(X+_Y, (X=1), S)},
		S == [1+_].

	succeeds(iso_findall_3_03) :-
		{findall(_X,fail,L)},
		L == [].

	succeeds(iso_findall_3_04) :-
		{findall(X,(X=1;X=1),S)},
		S == [1,1].

	fails(iso_findall_3_05) :-
		{findall(X, (X=2;X=1), [1,2])}.

	succeeds(iso_findall_3_06) :-
		{findall(X, (X=1;X=2), [X,Y])}.
		X == 1, Y == 2.

	throws(iso_findall_3_07, error(instantiation_error,_)) :-
		{findall(_X, _Goal, _S)}.

	throws(iso_findall_3_08, error(type_error(callable,4),_)) :-
		{findall(_X, 4, _S)}.

:- end_object.

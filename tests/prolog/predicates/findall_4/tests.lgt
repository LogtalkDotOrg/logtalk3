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
		date is 2015/02/26,
		comment is 'Unit tests for the de facto Prolog standard findall/4 built-in predicate.'
	]).

	succeeds(commons_findall_4_01) :-
		{findall(X, (X=1;X=2), S, [3])},
		S == [1,2,3].

	succeeds(commons_findall_4_02) :-
		{findall(X+_Y, (X=1), S, _)},
		S = [H| _], callable(H), functor(H, (+), 2), arg(1, H, N), N == 1, arg(2, H, V), var(V).

	succeeds(commons_findall_4_03) :-
		{findall(_X, fail, L, [0])},
		L == [0].

	succeeds(commons_findall_4_04) :-
		{findall(X, (X=1;X=1), S, [])},
		S == [1,1].

	fails(commons_findall_4_05) :-
		{findall(X, (X=2;X=1), [1,2], _)}.

	succeeds(commons_findall_4_06) :-
		{findall(X, (X=1;X=2), [X,Y,3], T)},
		X == 1, Y == 2, T == [3].

	throws(commons_findall_4_07, error(instantiation_error,_)) :-
		{findall(_X, _Goal, _S, _T)}.

	throws(commons_findall_4_08, error(type_error(callable,4),_)) :-
		{findall(_X, 4, _S, _T)}.

:- end_object.

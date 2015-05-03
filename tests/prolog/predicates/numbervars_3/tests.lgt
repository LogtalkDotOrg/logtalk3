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
		date is 2015/05/03,
		comment is 'Unit tests for the de facto Prolog standard numbervars/3 built-in predicate.'
	]).

	succeeds(commons_numbervars_3_01) :-
		{numbervars(t, 0, N)},
		N == 0.

	succeeds(commons_numbervars_3_02) :-
		{numbervars(T, 0, N)},
		ground(T), N == 1.

	succeeds(commons_numbervars_3_03) :-
		T = a(_X,_Y,_Z),
		{numbervars(T, 0, N)},
		ground(T), N == 3.

	succeeds(commons_numbervars_3_04) :-
		T = a(_X,_Y,_X),
		{numbervars(T, 0, N)},
		ground(T), N == 2.

	succeeds(commons_numbervars_3_05) :-
		T = a(_X, 1, b(_Y, c(_X), 2), 3, _W),
		{numbervars(T, 3, N)},
		ground(T), N == 6.

	succeeds(commons_numbervars_3_06) :-
		T = a(X,Y,Z),
		{numbervars(T, 0, _)},
		X == '$VAR'(0), Y == '$VAR'(1), Z == '$VAR'(2).

:- end_object.

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
		comment is 'Unit tests for the ISO Prolog standard (=)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.1.4

	succeeds(iso_unify_2_01) :-
		{'='(1, 1)}.

	succeeds(iso_unify_2_02) :-
		{'='(X, 1)},
		X == 1.

	succeeds(iso_unify_2_03) :-
		{'='(X,Y)},
		X == Y.

	succeeds(iso_unify_2_04) :-
		{'='(_,_)}.

	succeeds(iso_unify_2_05) :-
		{('='(X,Y),'='(X,abc))},
		X == abc, Y == abc.

	succeeds(iso_unify_2_06) :-
		{'='(f(X,def),f(def,Y))},
		X == def, Y == def.

	fails(iso_unify_2_07) :-
		{'='(1,2)}.

	fails(iso_unify_2_08) :-
		{'='(1,1.0)}.

	fails(iso_unify_2_09) :-
		{'='(g(X),f(f(X)))}.

	fails(iso_unify_2_10) :-
		{'='(f(X,1),f(a(X)))}.

	fails(iso_unify_2_11) :-
		{'='(f(X,Y,X),f(a(X),a(Y),Y,2))}.

:- end_object.

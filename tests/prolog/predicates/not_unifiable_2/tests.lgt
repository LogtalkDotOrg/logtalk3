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
		comment is 'Unit tests for the ISO Prolog standard (\\=)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.2.3.4

	fails(iso_not_unifiable_2_01) :-
		{'\\='(1, 1)}.

	fails(iso_not_unifiable_2_02) :-
		{\=(_X, 1)}.

	fails(iso_not_unifiable_2_03) :-
		{'\\='(_X, _Y)}.

	fails(iso_not_unifiable_2_04) :-
		{\=(_, _)}.

	fails(iso_not_unifiable_2_05) :-
		{\=(f(_X,def), f(def,_Y))}.

	succeeds(iso_not_unifiable_2_06) :-
		{'\\='(1, 2)}.

	succeeds(iso_not_unifiable_2_07) :-
		{\=(1, 1.0)}.

	succeeds(iso_not_unifiable_2_08) :-
		{'\\='(g(X), f(f(X)))}.

	succeeds(iso_not_unifiable_2_09) :-
		{\=(f(X,1), f(a(X)))}.

	succeeds(iso_not_unifiable_2_10) :-
		{'\\='(f(X,Y,X), f(a(X),a(Y),Y,2))}.

	- fails(iso_not_unifiable_2_11) :-
		% STO; Undefined
		{\=(X, a(X))}.

	- succeeds(iso_not_unifiable_2_12) :-
		% STO; Undefined
		{'\\='(f(X,1), f(a(X),2))}.

	- succeeds(iso_not_unifiable_2_13) :-
		% STO; Undefined
		{'\\='(f(1,X,1), f(2,a(X),2))}.

	- succeeds(iso_not_unifiable_2_14) :-
		% STO; Undefined
		{\=(f(1,X), f(2,a(X)))}.

	- succeeds(iso_not_unifiable_2_15) :-
		% STO; Undefined
		{'\\='(f(X,Y,X,1), f(a(X),a(Y),Y,2))}.

:- end_object.

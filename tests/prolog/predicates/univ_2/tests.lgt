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
		comment is 'Unit tests for the ISO Prolog standard (=..)/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.5.3.4

	succeeds(iso_univ_2_01) :-
		{'=..'(foo(a,b), [foo,a,b])}.

	succeeds(iso_univ_2_02) :-
		{'=..'(X, [foo,a,b])},
		X == foo(a,b).

	succeeds(iso_univ_2_03) :-
		{'=..'(foo(a,b), L)},
		L == [foo,a,b].

	succeeds(iso_univ_2_04) :-
		{'=..'(foo(X,b), [foo,a,Y])},
		X == a,Y == b.

	succeeds(iso_univ_2_05) :-
		{'=..'(1, [1])}.

	fails(iso_univ_2_06) :-
		{'=..'(foo(a,b), [foo,b,a])}.

	throws(iso_univ_2_07, error(instantiation_error,_)) :-
		{'=..'(_X, _Y)}.

	throws(iso_univ_2_08, error(instantiation_error,_)) :-
		{'=..'(_X, [foo,a|_Y])}.

	throws(iso_univ_2_09, error(type_error(list,[foo|bar]),_)) :-
		{'=..'(_X, [foo|bar])}.

	throws(iso_univ_2_10, error(instantiation_error,_)) :-
		{'=..'(_X, [_Foo,bar])}.

	throws(iso_univ_2_11, error(type_error(atom,3),_)) :-
		{'=..'(_X, [3,1])}.

	throws(iso_univ_2_12, error(type_error(atom,1.1),_)) :-
		{'=..'(_X, [1.1,foo])}.

	throws(iso_univ_2_13, error(type_error(atom,a(b)),_)) :-
		{'=..'(_X, [a(b),1])}.

	throws(iso_univ_2_14, error(type_error(list,4),_)) :-
		{'=..'(_X, 4)}.

:- end_object.

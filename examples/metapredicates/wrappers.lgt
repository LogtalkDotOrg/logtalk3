%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% example of defining wrappers for the bagof/3 and setof/3 built-in methods
% where the meta-argument may contain existentially qualified variables

:- object(wrappers).

	:- public(my_setof/3).
	:- meta_predicate(my_setof(*, ^, *)).

	my_setof(Term, Goal, List) :-
		setof(Term, Goal, List).

	:- public(my_bagof/3).
	:- meta_predicate(my_bagof(*, ^, *)).

	my_bagof(Term, Goal, List) :-
		bagof(Term, Goal, List).

:- end_object.


:- object(object).

	:- public(p/1).
	p(L) :-
		wrappers::my_setof(X, Y^p(X, Y), L).

	:- public(q/1).
	q(L) :-
		wrappers::my_setof(X, Y^Z^q(X, Y, Z), L).

	:- public(r/1).
	r(L) :-
		wrappers::my_bagof(X, Y^p(X, Y), L).

	:- public(s/1).
	s(L) :-
		wrappers::my_bagof(X, Y^Z^q(X, Y, Z), L).

	p(2, two).
	p(1, one).
	p(3, three).

	q(2, two, 'TWO').
	q(1, one, 'ONE').
	q(3, three, 'THREE').

:- end_object.

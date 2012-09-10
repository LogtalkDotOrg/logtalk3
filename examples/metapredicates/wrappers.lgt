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

:- end_object.


:- object(object).

	:- public(p/1).
	p(L) :-
		wrappers::my_setof(X, Y^p(X, Y), L).

	p(1, one).
	p(2, two).
	p(3, three).

	:- public(q/1).
	q(L) :-
		wrappers::my_setof(X, Y^Z^q(X, Y, Z), L).

	q(1, one, 'ONE').
	q(2, two, 'TWO').
	q(3, three, 'THREE').

:- end_object.

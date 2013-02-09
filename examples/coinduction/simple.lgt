%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(simple).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/08/31,
		comment is 'Elementary coinduction predicate example.'
	]).

	:- public(p/0).
	:- coinductive(p/0).

	p :- p.

	:- public(p/1).
	:- coinductive(p/1).

	p(X) :- q(X).

	q(X) :- r(X).

	r(X) :- p(X).

	:- public(p/2).
	:- coinductive(p/2).

	p(_, Y) :- 
		findall(T, s(T), Bag),
		member(Y, Bag).

	s(X) :- t(X).

	t(X) :- p(X, _).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.

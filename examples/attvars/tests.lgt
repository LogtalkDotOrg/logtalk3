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
		version is 1.4,
		author is 'Paulo Moura',
		date is 2011/03/13,
		comment is 'Unit tests for the "attvars" example.']).

	:- if(current_object(domain)).
	test(attvars_1) :-
		\+ (domain::domain(X, [a,b]), X = c).

	test(attvars_2) :-
		domain::domain(X, [a,b]), domain::domain(X, [a,c]),
		X == a.

	test(attvars_3) :-
		domain::domain(X, [a,b,c]), domain::domain(X, [a,c]),
		domain::domain(X, List), List == [a,c].
	:- endif.

	:- if(current_object(domain(_))).
	test(attvars_4) :-
		\+ (domain(atom)::domain(X, [a,b]), X = c).

	test(attvars_5) :-
		domain(integer)::domain(X, [1,2]), domain(integer)::domain(X, [1,3]),
		X == 1.

	test(attvars_6) :-
		domain(integer)::domain(X, [1,2,3]), domain(integer)::domain(X, [1,3]),
		domain(integer)::domain(X, List), List == [1,3].

	test(attvars_7) :-
		domain(atom)::domain(X, [a,b,c]), domain(atom)::domain(X, [a,c]), domain(TypeX)::domain(X, LX),
		TypeX == atom, LX == [a, c],
		domain(integer)::domain(Y, [1,2,3]), domain(integer)::domain(Y, [1,3]), domain(TypeY)::domain(Y, LY),
		TypeY = integer, LY = [1, 3].
	:- endif.

:- end_object.

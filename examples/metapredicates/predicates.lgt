%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% some simple predicates to use with library meta-predicates (e.g. fold_left/4
% and partition/4) compiled as plain Prolog code and thus defined in the "user"
% pseudo-object:

sum_squares(X, Y, Z) :-
	Z is X*X + Y*Y.


even_integer(I) :-
	I mod 2 =:= 0.


% a simple object defining some predicates to use with library meta-predicates:

:- object(predicates).

	:- info([
		version is 1.0,
		date is 2008/11/19,
		author is 'Paul Crocker',
		comment is 'Some predicates for testing the library meta-predicates.']).

	:- public(tuple/3).

	tuple((X1, Y1), (X2, Y2), (X, Y)) :-
		X is X1 + X2,
		Y is Y1 + Y2.

	:- public(sum/3).

	sum(X, Y, Z) :-
		Z is X + Y.

	:- public(product/3).

	product(X, Y, Z) :-
		Z is X * Y.

:- end_object.

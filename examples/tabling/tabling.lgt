%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(paths).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/05/14,
		comment is 'Simple tabling example using graph paths.',
		source is 'Direct conversion to Logtalk of a XSB tabling example.']).

	:- public(path/2).
	:- table(path/2).

	path(X,Y) :- path(X,Z), edge(Z,Y).
	path(X,Y) :- edge(X,Y).

	edge(1,2).
	edge(2,2).
	edge(2,4).
	edge(2,3).
	edge(3,5).

:- end_object.


:- object(fibonacci).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/05/28,
		comment is 'Simple tabling example using Fibonacci numbers.',
		source is 'Direct conversion to Logtalk of a B-Prolog tabling example.']).

	:- public(fib/2).
	:- table(fib/2).

	fib(0, 1).
	fib(1, 1).
	fib(N,F) :-
		N > 1,
		N1 is N - 1,
		N2 is N - 2,
		fib(N1, F1),
		fib(N2, F2),
		F is F1 + F2.

:- end_object.

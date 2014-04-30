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
		date is 2014/04/30,
		comment is 'Unit tests for the findall/4 built-in method.'
	]).

	test(findall_3_1) :-
		findall(X, a(X, _), L, [5, 6, 7]),
		L == [1, 2, 3, 4, 5, 6, 7].

	test(findall_3_2) :-
		findall(Y-L, findall(X, a(X, Y), L, [5, 6, 7]), LL),
		LL = [_-[1,2,3,4,5,6,7]].

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.

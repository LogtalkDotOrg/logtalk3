%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/fibonacci" example.']).

	test(fibonacci_1) :-
		fibonacci(1)::fib(24, N),
		N == 75025.

	test(fibonacci_2) :-
		fibonacci(2)::fib(24, N),
		N == 75025.

	test(fibonacci_3) :-
		fibonacci(4)::fib(24, N),
		N == 75025.

	test(fibonacci_4) :-
		fibonacci(8)::fib(24, N),
		N == 75025.

	test(fibonacci_5) :-
		fibonacci(16)::fib(24, N),
		N == 75025.

:- end_object.

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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2012/02/17,
		comment is 'Unit tests for the "chr" example.']).

	:- if(current_object(dom)).
	test(chr_dom_1) :-
		dom::dom(A, [1,2,3]), dom::dom(A, [3,4,5]),
		A =:= 3.
	:- endif.

	:- if(current_object(family)).
	test(chr_family_1) :-
		family::start, family::sibling(peter,mary).
	
	test(chr_family_2) :-
		\+ (family::start, family::sibling(paul,mary)).
	
	test(chr_family_3) :-
		\+ (family::father(X,Y), family::mother(X,Y)).
	:- endif.

	:- if(current_object(fib)).
	test(chr_fib_1) :-
		fib::fib(5, M),
		M =:= 8.
	:- endif.

	:- if(current_object(fibonacci)).
	test(chr_fibonacci_1) :-
		fibonacci::fibonacci(5, M),
		M =:= 8.
	:- endif.

	:- if(current_object(gcd)).
	test(chr_gcd_1) :-
		gcd::gcd(2), gcd::gcd(3).

	test(chr_gcd_2) :-
		X is 37*11*11*7*3, Y is 11*7*5*3, Z is 37*11*5, gcd::gcd(X), gcd::gcd(Y), gcd::gcd(Z).
	:- endif.

	:- if(current_object(leq)).
	test(chr_leq_1) :-
		leq::leq(_X, Y), leq::leq(Y, _Z).
	:- endif.

	:- if(current_object(primes)).
	test(chr_primes_1) :-
		primes::candidate(29).
	:- endif.

	:- if(current_object(foo)).
	test(chr_combining_1) :-
		foo::foo(1).
	:- endif.

:- end_object.

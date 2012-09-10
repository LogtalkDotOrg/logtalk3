%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(sieve2).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/07/03,
		comment is 'Sieve of Eratosthenes coinduction example.']).

	:- public(primes/2).
	primes(N, Prime) :-
		generate_infinite_list(N, List),
		sieve(List, Primes),
		comember(Prime, Primes).

	generate_infinite_list(N, List) :-
		sequence(2, N, List, List).

	sequence(Sup, Sup, [Sup| List], List) :-
		!.
	sequence(Inf, Sup, [Inf| List], Tail) :-
		Next is Inf + 1,
		sequence(Next, Sup, List, Tail).

	:- coinductive(sieve/2).
	sieve([H| T], [H| R]) :-
		filter(H, T, F),
		sieve(F, R).

	:- coinductive(filter/3).
	filter(H, [K| T], L) :-
		(	K > H, K mod H =:= 0 ->
			L = T1
		;	L = [K| T1]
		),
		filter(H, T, T1).

	:- coinductive(comember/2).
	comember(X, L) :-
		drop(X, L, L1),
		comember(X, L1).

	drop(H, [H| T], T).
	drop(H, [_| T], T1) :-
		drop(H, T, T1).

:- end_object.

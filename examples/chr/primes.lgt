%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Sieve of eratosthenes to compute primes
% thom fruehwirth 920218-20, 980311
% christian holzbaur 980207 for Sicstus CHR
%
% ported to hProlog by Tom Schrijvers 
% 
% ported to Logtalk by Paulo Moura


:- object(primes).

	:- public(candidate/1).
	:- chr_constraint(candidate/1).

	:- private(prime/1).
	:- chr_constraint(prime/1).

	candidate(1) <=> true.
	candidate(N) <=> prime(N), N1 is N - 1, candidate(N1).

	absorb @ prime(Y) \ prime(X) <=> 0 is X mod Y | true.

:- end_object.

/*
time(N):-
	cputime(X),
	primes::candidate(N),
	cputime( Now),
	Time is Now-X,
	write(N-Time), nl.

cputime( Ts) :- 
	statistics( runtime, [Tm,_]),
	Ts is Tm/1000.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(primes(_Threads)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2008/2/29,
		comment is 'Simple example for comparing single and multi-threading calculation of prime numbers.',
		parameters is ['Threads' - 'Number of threads to use.']
	]).

	:- threaded.

	:- public(primes/3).
	:- mode(primes(+integer, +integer, -list), one).
	:- info(primes/3, [
		comment is 'Returns a list of all prime numbers in the given interval. No restrictions on the number of threads to use besides those imposed by the backend Prolog compiler.',
		argnames is ['Inf', 'Sup', 'Primes']
	]).

	primes(Inf, Sup, Primes) :-
		parameter(1, Threads),
		Sup > Inf,
		split(Inf, Sup, Threads, Intervals),
		spawn(Intervals, Primes, Goals),
		collect(Goals).

	split(Inf, Sup, N, Intervals) :-
		Width is (Sup - Inf) / N,
		split(1, N, Inf, Inf, Width, [], Intervals).

	split(N, N, Inf, Current, Width, Acc, [Current-Sup| Acc]) :-
		Sup is truncate(Inf + Width*N),
		!.
	split(I, N, Inf, Current, Width, Acc, Intervals) :-
		Sup is truncate(Inf + Width*I),
		Current2 is Sup + 1,
		I2 is I + 1,
		split(I2, N, Inf, Current2, Width, [Current-Sup| Acc], Intervals).

	spawn(Intervals, Primes, Goals) :-
		spawn(Intervals, [], Primes, Goals).

	spawn([], Primes, Primes, []).
	spawn([Inf-Sup| Intervals], Acc, Primes, [primes(Inf, Sup, Acc, Acc2)| Goals]) :-
		threaded_once(primes(Inf, Sup, Acc, Acc2)),
		spawn(Intervals, Acc2, Primes, Goals).

	collect([]).
	collect([primes(Inf, Sup, Acc, Primes)| Goals]) :-
		threaded_exit(primes(Inf, Sup, Acc, Primes)),
		collect(Goals).

	primes(N, M, Primes, Primes) :-
		N > M,
		!.
	primes(N, M, Acc, Primes) :-
		(	is_prime(N) ->
			Primes = [N| Primes2]
		;	Primes = Primes2
		),
	    N2 is N + 1,
		primes(N2, M, Acc, Primes2).

	is_prime(2) :- !.
	is_prime(Prime):-
		Prime > 2,
		Prime mod 2 =:= 1,
		Sqrt is sqrt(Prime),
		is_prime(3, Sqrt, Prime).

	is_prime(N, Sqrt, Prime):-
		(	N > Sqrt ->
			true
		;	Prime mod N > 0,
 			N2 is N + 2,
			is_prime(N2, Sqrt, Prime)
		).

:- end_object.

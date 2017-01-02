%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(interactors).

	:- info([
		version is 1.0,
		author is 'Paul Tarau and Paulo Moura',
		date is 2016/06/15,
		comment is 'Examples of implementing interactors using threaded engines.'
	]).

	:- threaded.

	% interactors initialization 
	:- initialization((
		% the engine names are object-scoped
		threaded_engine_create(_, natural_loop(1), naturals),
		threaded_engine_create(_, prime_loop(1), primes),
		threaded_engine_create(_, sum_loop(0), sums)
	)).

	:- public(natural/1).
	:- mode(natural(-integer), one).
	:- info(natural/1, [
		comment is 'Returns natural numbers in increasing order.',
		argnames is ['Natural']
	]).

	:- public(prime/1).
	:- mode(prime(-integer), one).
	:- info(prime/1, [
		comment is 'Returns prime numbers in increasing order.',
		argnames is ['Prime']
	]).

	:- public(sums/1).
	:- mode(sums(-nonvar), one).
	:- info(sums/1, [
		comment is 'Injects sum goals into an engine and returns sums computed from the engine local state "variables".',
		argnames is ['Prime']
	]).

	natural(N) :-
		threaded_engine_next(naturals, N).

	natural_loop(N) :-
		threaded_engine_yield(N),
		N1 is N + 1,
		natural_loop(N1).

	prime(Prime) :-
		threaded_engine_next(primes, Prime).

	prime_loop(N) :-
		N1 is N + 1,
		(	test_prime(N1) ->
			prime_loop(N1)
		;	threaded_engine_yield(N1)
		),
		prime_loop(N1).

	test_prime(N) :-
		M is truncate(sqrt(N)),
		between(2, M, D),
		N mod D =:= 0.

	sums(Sums) :-
		% inject goal for the engine to execute
		threaded_engine_post(sums, Sum1->Sum2 :- Sum2 is Sum1 + 2),
		threaded_engine_post(sums, Sum1->Sum2 :- Sum2 is Sum1 + 5),
		threaded_engine_next(sums, Sums).

	% the argument of the loop predicate acts as a engine local state variable
	sum_loop(Sum1) :-
		threaded_engine_fetch(Sum1->Sum2 :- Goal),
		call(Goal),
		threaded_engine_yield(Sum1->Sum2),
		sum_loop(Sum2).

:- end_object.

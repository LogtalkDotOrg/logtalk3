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


:- object(sieve).

	:- info([
		version is 1.1,
		author is 'Paulo Moura. Derived from a coroutining solution by Gopal Gupta et al.',
		date is 2011/08/13,
		comment is 'Sieve of Eratosthenes coinduction example.'
	]).

	:- public(primes/2).

	:- coinductive([
		sieve/2, filter/3
	]).

	% computes a coinductive list with all the primes in the 2..N interval
	primes(N, Primes) :-
		generate_infinite_list(N, List),
		sieve(List, Primes).

	% generate a coinductive list with a 2..N repeating patern
	generate_infinite_list(N, List) :-
		sequence(2, N, List, List).

	sequence(Sup, Sup, [Sup| List], List) :-
		!.
	sequence(Inf, Sup, [Inf| List], Tail) :-
		Next is Inf + 1,
		sequence(Next, Sup, List, Tail).

	sieve([H| T], [H| R]) :-
		filter(H, T, F),
		sieve(F, R).

	filter(H, [K| T], L) :-
		(	K > H, K mod H =:= 0 ->
			% throw away the multiple we found
			L = T1
		;	% we must not throw away the integer used for filtering
			% as we must return a filtered coinductive list
			L = [K| T1]
		),
		filter(H, T, T1).

:- end_object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(natural,
	extends(integer)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-02-02,
		comment is 'Natural numbers data type predicates.'
	]).

	:- public(factorial/2).
	:- mode(factorial(+non_negative_integer, -non_negative_integer), one).
	:- info(factorial/2, [
		comment is 'Computes the factorial of a non-negative integer.',
		argnames is ['N', 'Factorial']
	]).

	:- public(binomial/3).
	:- mode(binomial(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(binomial/3, [
		comment is 'Computes the binomial coefficient. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'Binomial']
	]).

	factorial(N, Factorial) :-
		integer(N),
		N >= 0,
		factorial(N, 1, Factorial).

	factorial(0, Factorial, Factorial) :-
		!.
	factorial(N, Factorial0, Factorial) :-
		N1 is N - 1,
		Factorial1 is Factorial0 * N,
		factorial(N1, Factorial1, Factorial).

	binomial(N, K, Binomial) :-
		K =< N,
		K >= 0,
		min_k(N, K, K_opt),
		binomial(K_opt, 0, N, 1, Binomial).

	% take advantage of the fact that C(n,k) = C(n,n-k)
	min_k(N, K, K_opt) :-
    	(	K > N - K
		->	K_opt is N - K
		;	K_opt = K
		).

	binomial(K, K, _, Binomial, Binomial) :-
		!.
	binomial(K, K0, N, Binomial0, Binomial) :-
		K0 < K,
		K1 is K0 + 1,
		Binomial1 is Binomial0 * (N - K0) // K1,
		binomial(K, K1, N, Binomial1, Binomial).

	between(Lower, Upper, Integer) :-
		integer(Lower),
		Lower > 0,
		^^between(Lower, Upper, Integer).

	valid(Natural) :-
		integer(Natural),
		Natural > 0.

	check(Term) :-
		(	integer(Term), Term > 0 ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(natural, Term)
		).

:- end_object.

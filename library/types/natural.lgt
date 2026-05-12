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
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Natural numbers data type predicates.'
	]).

	:- public(factorial/2).
	:- mode(factorial(+non_negative_integer, -non_negative_integer), one).
	:- info(factorial/2, [
		comment is 'Computes the factorial of a non-negative integer.',
		argnames is ['N', 'Factorial']
	]).

	:- public(subfactorial/2).
	:- mode(subfactorial(+non_negative_integer, -non_negative_integer), one).
	:- info(subfactorial/2, [
		comment is 'Computes the subfactorial of a non-negative integer.',
		argnames is ['N', 'Subfactorial']
	]).

	:- public(bell/2).
	:- mode(bell(+non_negative_integer, -non_negative_integer), one).
	:- info(bell/2, [
		comment is 'Computes the Bell number of a non-negative integer.',
		argnames is ['N', 'Bell']
	]).

	:- public(stirling_second/3).
	:- mode(stirling_second(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(stirling_second/3, [
		comment is 'Computes the Stirling number of the second kind. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'Stirling']
	]).

	:- public(binomial/3).
	:- mode(binomial(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(binomial/3, [
		comment is 'Computes the binomial coefficient. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'Binomial']
	]).

	:- uses(list, [
		nth0/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	factorial(N, Factorial) :-
		integer(N),
		N >= 0,
		factorial(N, 1, Factorial).

	subfactorial(0, 1) :-
		!.
	subfactorial(1, 0) :-
		!.
	subfactorial(N, Subfactorial) :-
		integer(N),
		N > 1,
		subfactorial_loop(2, N, 1, 0, Subfactorial).

	bell(N, Bell) :-
		integer(N),
		N >= 0,
		stirling_row(N, Row),
		sum(Row, Bell).

	stirling_second(N, K, Stirling) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		K =< N,
		stirling_row(N, Row),
		nth0(K, Row, Stirling).

	factorial(0, Factorial, Factorial) :-
		!.
	factorial(N, Factorial0, Factorial) :-
		N1 is N - 1,
		Factorial1 is Factorial0 * N,
		factorial(N1, Factorial1, Factorial).

	subfactorial_loop(I, N, Previous2, Previous1, Subfactorial) :-
		Current is (I - 1) * (Previous1 + Previous2),
		(   I =:= N ->
			Subfactorial = Current
		;   I1 is I + 1,
			subfactorial_loop(I1, N, Previous1, Current, Subfactorial)
		).

	stirling_row(0, [1]) :-
		!.
	stirling_row(N, Row) :-
		stirling_row_loop(0, N, [1], Row).

	stirling_row_loop(N, N, Row, Row) :-
		!.
	stirling_row_loop(Current, N, PreviousRow, Row) :-
		Next is Current + 1,
		stirling_next_row(Next, PreviousRow, NextRow),
		stirling_row_loop(Next, N, NextRow, Row).

	stirling_next_row(N, PreviousRow, [0| RowTail]) :-
		stirling_next_row_entries(1, N, PreviousRow, RowTail).

	stirling_next_row_entries(K, K, [_], [1]) :-
		!.
	stirling_next_row_entries(K, N, [Previous0, Previous1| Previouss], [Current| RowTail]) :-
		K < N,
		Current is K * Previous1 + Previous0,
		K1 is K + 1,
		stirling_next_row_entries(K1, N, [Previous1| Previouss], RowTail).

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

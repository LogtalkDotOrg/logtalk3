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
		version is 2:3:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Natural numbers data type predicates.'
	]).

	:- public(factorial/2).
	:- mode(factorial(+non_negative_integer, -non_negative_integer), one).
	:- info(factorial/2, [
		comment is 'Computes the factorial of a non-negative integer.',
		argnames is ['N', 'Factorial']
	]).

	:- public(fibonacci/2).
	:- mode(fibonacci(+non_negative_integer, -non_negative_integer), one).
	:- info(fibonacci/2, [
		comment is 'Computes the Fibonacci number of a non-negative integer.',
		argnames is ['N', 'Fibonacci']
	]).

	:- public(lucas/2).
	:- mode(lucas(+non_negative_integer, -non_negative_integer), one).
	:- info(lucas/2, [
		comment is 'Computes the Lucas number of a non-negative integer.',
		argnames is ['N', 'Lucas']
	]).

	:- public(falling_factorial/3).
	:- mode(falling_factorial(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(falling_factorial/3, [
		comment is 'Computes the falling factorial. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'FallingFactorial']
	]).

	:- public(rising_factorial/3).
	:- mode(rising_factorial(+non_negative_integer, +non_negative_integer, -non_negative_integer), one).
	:- info(rising_factorial/3, [
		comment is 'Computes the rising factorial.',
		argnames is ['N', 'K', 'RisingFactorial']
	]).

	:- public(catalan/2).
	:- mode(catalan(+non_negative_integer, -non_negative_integer), one).
	:- info(catalan/2, [
		comment is 'Computes the Catalan number of a non-negative integer.',
		argnames is ['N', 'Catalan']
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

	:- public(stirling_first/3).
	:- mode(stirling_first(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(stirling_first/3, [
		comment is 'Computes the unsigned Stirling number of the first kind. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'Stirling']
	]).

	:- public(stirling_second/3).
	:- mode(stirling_second(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(stirling_second/3, [
		comment is 'Computes the Stirling number of the second kind. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'Stirling']
	]).

	:- public(partition_number/2).
	:- mode(partition_number(+non_negative_integer, -non_negative_integer), one).
	:- info(partition_number/2, [
		comment is 'Computes the partition number of a non-negative integer.',
		argnames is ['N', 'PartitionNumber']
	]).

	:- public(binomial/3).
	:- mode(binomial(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(binomial/3, [
		comment is 'Computes the binomial coefficient. ``N`` must be greater than or equal to ``K`` (fails otherwise).',
		argnames is ['N', 'K', 'Binomial']
	]).

	:- public(multinomial/2).
	:- mode(multinomial(+list(non_negative_integer), -non_negative_integer), one).
	:- info(multinomial/2, [
		comment is 'Computes the multinomial coefficient for a list of non-negative integers.',
		argnames is ['Integers', 'Multinomial']
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

	fibonacci(0, 0) :-
		!.
	fibonacci(N, Fibonacci) :-
		integer(N),
		N > 0,
		fibonacci_loop(1, N, 0, 1, Fibonacci).

	lucas(0, 2) :-
		!.
	lucas(N, Lucas) :-
		integer(N),
		N > 0,
		lucas_loop(1, N, 2, 1, Lucas).

	falling_factorial(N, K, FallingFactorial) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		K =< N,
		falling_factorial_loop(0, K, N, 1, FallingFactorial).

	rising_factorial(N, K, RisingFactorial) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		rising_factorial_loop(0, K, N, 1, RisingFactorial).

	catalan(N, Catalan) :-
		integer(N),
		N >= 0,
		N2 is 2 * N,
		binomial(N2, N, Binomial),
		Catalan is Binomial // (N + 1).

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

	stirling_first(N, K, Stirling) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		K =< N,
		stirling_first_row(N, Row),
		nth0(K, Row, Stirling).

	stirling_second(N, K, Stirling) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		K =< N,
		stirling_row(N, Row),
		nth0(K, Row, Stirling).

	partition_number(0, 1) :-
		!.
	partition_number(N, PartitionNumber) :-
		integer(N),
		N > 0,
		partition_number_loop(1, N, [1], PartitionNumber).

	factorial(0, Factorial, Factorial) :-
		!.
	factorial(N, Factorial0, Factorial) :-
		N1 is N - 1,
		Factorial1 is Factorial0 * N,
		factorial(N1, Factorial1, Factorial).

	fibonacci_loop(N, N, _, Fibonacci, Fibonacci) :-
		!.
	fibonacci_loop(Index, N, Previous, Current, Fibonacci) :-
		Next is Previous + Current,
		Index1 is Index + 1,
		fibonacci_loop(Index1, N, Current, Next, Fibonacci).

	lucas_loop(N, N, _, Lucas, Lucas) :-
		!.
	lucas_loop(Index, N, Previous, Current, Lucas) :-
		Next is Previous + Current,
		Index1 is Index + 1,
		lucas_loop(Index1, N, Current, Next, Lucas).

	falling_factorial_loop(K, K, _, FallingFactorial, FallingFactorial) :-
		!.
	falling_factorial_loop(Index, K, N, FallingFactorial0, FallingFactorial) :-
		Factor is N - Index,
		FallingFactorial1 is FallingFactorial0 * Factor,
		Index1 is Index + 1,
		falling_factorial_loop(Index1, K, N, FallingFactorial1, FallingFactorial).

	rising_factorial_loop(K, K, _, RisingFactorial, RisingFactorial) :-
		!.
	rising_factorial_loop(Index, K, N, RisingFactorial0, RisingFactorial) :-
		Factor is N + Index,
		RisingFactorial1 is RisingFactorial0 * Factor,
		Index1 is Index + 1,
		rising_factorial_loop(Index1, K, N, RisingFactorial1, RisingFactorial).

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

	stirling_first_row(0, [1]) :-
		!.
	stirling_first_row(N, Row) :-
		stirling_first_row_loop(0, N, [1], Row).

	stirling_row_loop(N, N, Row, Row) :-
		!.
	stirling_row_loop(Current, N, PreviousRow, Row) :-
		Next is Current + 1,
		stirling_next_row(Next, PreviousRow, NextRow),
		stirling_row_loop(Next, N, NextRow, Row).

	stirling_first_row_loop(N, N, Row, Row) :-
		!.
	stirling_first_row_loop(Current, N, PreviousRow, Row) :-
		Next is Current + 1,
		stirling_first_next_row(Next, PreviousRow, NextRow),
		stirling_first_row_loop(Next, N, NextRow, Row).

	stirling_next_row(N, PreviousRow, [0| RowTail]) :-
		stirling_next_row_entries(1, N, PreviousRow, RowTail).

	stirling_first_next_row(N, PreviousRow, [0| RowTail]) :-
		Multiplier is N - 1,
		stirling_first_next_row_entries(1, N, Multiplier, PreviousRow, RowTail).

	stirling_next_row_entries(K, K, [_], [1]) :-
		!.
	stirling_next_row_entries(K, N, [Previous0, Previous1| Previouss], [Current| RowTail]) :-
		K < N,
		Current is K * Previous1 + Previous0,
		K1 is K + 1,
		stirling_next_row_entries(K1, N, [Previous1| Previouss], RowTail).

	stirling_first_next_row_entries(K, K, _, [_], [1]) :-
		!.
	stirling_first_next_row_entries(K, N, Multiplier, [Previous0, Previous1| Previouss], [Current| RowTail]) :-
		K < N,
		Current is Multiplier * Previous1 + Previous0,
		K1 is K + 1,
		stirling_first_next_row_entries(K1, N, Multiplier, [Previous1| Previouss], RowTail).

	partition_number_loop(Current, N, PreviousPartitions, PartitionNumber) :-
		partition_number_value(Current, PreviousPartitions, CurrentPartition),
		(   Current =:= N ->
			PartitionNumber = CurrentPartition
		;   Current1 is Current + 1,
			append_partition_number(PreviousPartitions, CurrentPartition, Partitions),
			partition_number_loop(Current1, N, Partitions, PartitionNumber)
		).

	partition_number_value(Current, Partitions, PartitionNumber) :-
		partition_number_terms(1, Current, Partitions, 0, PartitionNumber).

	partition_number_terms(K, Current, _, PartitionNumber, PartitionNumber) :-
		Pentagonal is K * (3 * K - 1) // 2,
		Pentagonal > Current,
		!.
	partition_number_terms(K, Current, Partitions, PartitionNumber0, PartitionNumber) :-
		Pentagonal1 is K * (3 * K - 1) // 2,
		Index1 is Current - Pentagonal1,
		nth0(Index1, Partitions, Value1),
		partition_number_sign(K, Sign),
		PartitionNumber1 is PartitionNumber0 + Sign * Value1,
		Pentagonal2 is K * (3 * K + 1) // 2,
		(   Pentagonal2 =< Current ->
			Index2 is Current - Pentagonal2,
			nth0(Index2, Partitions, Value2),
			PartitionNumber2 is PartitionNumber1 + Sign * Value2
		;   PartitionNumber2 = PartitionNumber1
		),
		K1 is K + 1,
		partition_number_terms(K1, Current, Partitions, PartitionNumber2, PartitionNumber).

	partition_number_sign(K, 1) :-
		K mod 2 =:= 1,
		!.
	partition_number_sign(_, -1).

	append_partition_number([], PartitionNumber, [PartitionNumber]).
	append_partition_number([Head| Tail], PartitionNumber, [Head| UpdatedTail]) :-
		append_partition_number(Tail, PartitionNumber, UpdatedTail).

	binomial(N, K, Binomial) :-
		K =< N,
		K >= 0,
		min_k(N, K, K_opt),
		binomial(K_opt, 0, N, 1, Binomial).

	multinomial(Integers, Multinomial) :-
		valid_non_negative_integers(Integers),
		sum(Integers, Sum),
		factorial(Sum, Numerator),
		multinomial_denominator(Integers, 1, Denominator),
		Multinomial is Numerator // Denominator.

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

	valid_non_negative_integers([]).
	valid_non_negative_integers([Integer| Integers]) :-
		integer(Integer),
		Integer >= 0,
		valid_non_negative_integers(Integers).

	multinomial_denominator([], Denominator, Denominator).
	multinomial_denominator([Integer| Integers], Denominator0, Denominator) :-
		factorial(Integer, Factorial),
		Denominator1 is Denominator0 * Factorial,
		multinomial_denominator(Integers, Denominator1, Denominator).

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

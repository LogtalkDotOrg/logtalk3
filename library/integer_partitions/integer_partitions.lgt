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


:- object(integer_partitions,
	implements(integer_partitions_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Implementation of integer partition operations.'
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(natural, [
		partition_number/2
	]).

	:- uses(list, [
		length/2, member/2, msort/2, nth0/3
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3 as random_between/3
	]).

	partitions(N, Partitions) :-
		all_partitions(N, default, Partitions).

	partition(N, Partition) :-
		partitions(N, Partitions),
		member(Partition, Partitions).

	partitions(N, K, Partitions) :-
		integer(K),
		!,
		exact_partitions(K, N, default, Partitions).
	partitions(N, Order, Partitions) :-
		all_partitions(N, Order, Partitions).

	partition(N, K, Partition) :-
		integer(K),
		!,
		partitions(N, K, Partitions),
		member(Partition, Partitions).
	partition(N, Order, Partition) :-
		partitions(N, Order, Partitions),
		member(Partition, Partitions).

	partitions(N, K, Order, Partitions) :-
		exact_partitions(K, N, Order, Partitions).

	partition(N, K, Order, Partition) :-
		partitions(N, K, Order, Partitions),
		member(Partition, Partitions).

	distinct_partitions(N, Partitions) :-
		distinct_all_partitions(N, default, Partitions).

	distinct_partition(N, Partition) :-
		distinct_partitions(N, Partitions),
		member(Partition, Partitions).

	distinct_partitions(N, K, Partitions) :-
		integer(K),
		!,
		distinct_exact_partitions(K, N, default, Partitions).
	distinct_partitions(N, Order, Partitions) :-
		distinct_all_partitions(N, Order, Partitions).

	distinct_partition(N, K, Partition) :-
		integer(K),
		!,
		distinct_partitions(N, K, Partitions),
		member(Partition, Partitions).
	distinct_partition(N, Order, Partition) :-
		distinct_partitions(N, Order, Partitions),
		member(Partition, Partitions).

	distinct_partitions(N, K, Order, Partitions) :-
		distinct_exact_partitions(K, N, Order, Partitions).

	distinct_partition(N, K, Order, Partition) :-
		distinct_partitions(N, K, Order, Partitions),
		member(Partition, Partitions).

	count_partitions(N, Count) :-
		partition_number(N, Count).

	count_partitions(N, K, Count) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		K =< N,
		restricted_partition_table(N, Table),
		nth0(N, Table, Row),
		nth0(K, Row, Count).

	count_distinct_partitions(N, Count) :-
		integer(N),
		N >= 0,
		distinct_partition_table_row(N, Row),
		nth0(N, Row, Count).

	count_distinct_partitions(N, K, Count) :-
		integer(N),
		N >= 0,
		integer(K),
		K >= 0,
		distinct_exact_partitions(K, N, default, Partitions),
		length(Partitions, Count).

	nth_partition(N, Index, Partition) :-
		Index >= 0,
		partitions(N, Partitions),
		nth0(Index, Partitions, Partition).

	nth_partition(N, K, Index, Partition) :-
		integer(K),
		!,
		Index >= 0,
		partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition).
	nth_partition(N, Order, Index, Partition) :-
		Index >= 0,
		partitions(N, Order, Partitions),
		nth0(Index, Partitions, Partition).

	nth_partition(N, K, Order, Index, Partition) :-
		Index >= 0,
		partitions(N, K, Order, Partitions),
		nth0(Index, Partitions, Partition).

	partition_index(N, Partition, Index) :-
		partitions(N, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	partition_index(N, K, Partition, Index) :-
		integer(K),
		!,
		partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition),
		!.
	partition_index(N, Order, Partition, Index) :-
		partitions(N, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	partition_index(N, K, Order, Partition, Index) :-
		partitions(N, K, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	nth_distinct_partition(N, Index, Partition) :-
		Index >= 0,
		distinct_partitions(N, Partitions),
		nth0(Index, Partitions, Partition).

	nth_distinct_partition(N, K, Index, Partition) :-
		integer(K),
		!,
		Index >= 0,
		distinct_partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition).
	nth_distinct_partition(N, Order, Index, Partition) :-
		Index >= 0,
		distinct_partitions(N, Order, Partitions),
		nth0(Index, Partitions, Partition).

	nth_distinct_partition(N, K, Order, Index, Partition) :-
		Index >= 0,
		distinct_partitions(N, K, Order, Partitions),
		nth0(Index, Partitions, Partition).

	distinct_partition_index(N, Partition, Index) :-
		distinct_partitions(N, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	distinct_partition_index(N, K, Partition, Index) :-
		integer(K),
		!,
		distinct_partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition),
		!.
	distinct_partition_index(N, Order, Partition, Index) :-
		distinct_partitions(N, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	distinct_partition_index(N, K, Order, Partition, Index) :-
		distinct_partitions(N, K, Order, Partitions),
		nth0(Index, Partitions, Partition),
		!.

	random_partition(N, Partition) :-
		count_partitions(N, Count),
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_partition(N, Index, Partition).

	random_partition(N, K, Partition) :-
		count_partitions(N, K, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_partition(N, K, Index, Partition).

	sample_partitions(N, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, all, N, Samples).

	sample_partitions(N, K, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, exact(K), N, Samples).

	sample_partitions_loop(0, _Mode, _N, []) :-
		!.
	sample_partitions_loop(SampleCount, Mode, N, [Partition| Samples]) :-
		SampleCount > 0,
		random_partition_for_mode(Mode, N, Partition),
		SampleCount1 is SampleCount - 1,
		sample_partitions_loop(SampleCount1, Mode, N, Samples).

	random_distinct_partition(N, Partition) :-
		count_distinct_partitions(N, Count),
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_distinct_partition(N, Index, Partition).

	random_distinct_partition(N, K, Partition) :-
		count_distinct_partitions(N, K, Count),
		Count > 0,
		Count1 is Count - 1,
		random_between(0, Count1, Index),
		nth_distinct_partition(N, K, Index, Partition).

	sample_distinct_partitions(N, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, distinct_all, N, Samples).

	sample_distinct_partitions(N, K, SampleCount, Samples) :-
		SampleCount >= 0,
		sample_partitions_loop(SampleCount, distinct_exact(K), N, Samples).

	random_partition_for_mode(all, N, Partition) :-
		random_partition(N, Partition).
	random_partition_for_mode(exact(K), N, Partition) :-
		random_partition(N, K, Partition).
	random_partition_for_mode(distinct_all, N, Partition) :-
		random_distinct_partition(N, Partition).
	random_partition_for_mode(distinct_exact(K), N, Partition) :-
		random_distinct_partition(N, K, Partition).

	next_partition(N, Partition, Next) :-
		canonical_lexicographic_partitions(N, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Partitions, Next).

	next_partition(N, K, Partition, Next) :-
		canonical_lexicographic_partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Partitions, Next).

	previous_partition(N, Partition, Previous) :-
		canonical_lexicographic_partitions(N, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Partitions, Previous).

	previous_partition(N, K, Partition, Previous) :-
		canonical_lexicographic_partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Partitions, Previous).

	next_distinct_partition(N, Partition, Next) :-
		canonical_lexicographic_distinct_partitions(N, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Partitions, Next).

	next_distinct_partition(N, K, Partition, Next) :-
		canonical_lexicographic_distinct_partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		NextIndex is Index + 1,
		nth0(NextIndex, Partitions, Next).

	previous_distinct_partition(N, Partition, Previous) :-
		canonical_lexicographic_distinct_partitions(N, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Partitions, Previous).

	previous_distinct_partition(N, K, Partition, Previous) :-
		canonical_lexicographic_distinct_partitions(N, K, Partitions),
		nth0(Index, Partitions, Partition),
		!,
		Index > 0,
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Partitions, Previous).

	all_partitions(N, Order, Partitions) :-
		findall(Partition, generate_partition(N, Partition), Partitions0),
		apply_order(Order, Partitions0, Partitions).

	exact_partitions(K, N, Order, Partitions) :-
		findall(Partition, generate_partition(K, N, Partition), Partitions0),
		apply_order(Order, Partitions0, Partitions).

	distinct_all_partitions(N, Order, Partitions) :-
		findall(Partition, generate_distinct_partition(N, Partition), Partitions0),
		apply_order(Order, Partitions0, Partitions).

	distinct_exact_partitions(K, N, Order, Partitions) :-
		findall(Partition, generate_distinct_partition(K, N, Partition), Partitions0),
		apply_order(Order, Partitions0, Partitions).

	% generates partitions of N in non-increasing part order; a part choice
	% is always bounded above by both what remains (N) and by the previous
	% part (Max), which guarantees the non-increasing (canonical) form
	generate_partition(N, Partition) :-
		(	N =:= 0 ->
			Partition = []
		;	N > 0,
			integer_partition(N, N, Partition)
		).

	generate_partition(K, N, Partition) :-
		(	K =:= 0,
			N =:= 0 ->
			Partition = []
		;	integer_partition(N, K, N, Partition)
		).

	integer_partition(0, _Max, []) :-
		!.
	integer_partition(N, Max, [Part| Rest]) :-
		N > 0,
		Upper is min(N, Max),
		Upper >= 1,
		between(1, Upper, Offset),
		Part is Upper - Offset + 1,
		N1 is N - Part,
		integer_partition(N1, Part, Rest).

	integer_partition(N, K, Max, Partition) :-
		(	K =:= 0 ->
			N =:= 0,
			Partition = []
		;	K > 0,
			N >= K,
			Upper is min(Max, N - K + 1),
			Upper >= 1,
			between(1, Upper, Offset),
			Part is Upper - Offset + 1,
			N1 is N - Part,
			K1 is K - 1,
			integer_partition(N1, K1, Part, Rest),
			Partition = [Part| Rest]
		).

	% generates partitions of N into pairwise distinct (strictly decreasing)
	% parts; the recursive bound tightens to Part - 1 (rather than Part) so
	% that no part can be reused
	generate_distinct_partition(N, Partition) :-
		(	N =:= 0 ->
			Partition = []
		;	N > 0,
			distinct_integer_partition(N, N, Partition)
		).

	generate_distinct_partition(K, N, Partition) :-
		(	K =:= 0,
			N =:= 0 ->
			Partition = []
		;	distinct_integer_partition(N, K, N, Partition)
		).

	distinct_integer_partition(0, _Max, []) :-
		!.
	distinct_integer_partition(N, Max, [Part| Rest]) :-
		N > 0,
		Upper is min(N, Max),
		Upper >= 1,
		between(1, Upper, Offset),
		Part is Upper - Offset + 1,
		N1 is N - Part,
		Max1 is Part - 1,
		distinct_integer_partition(N1, Max1, Rest).

	distinct_integer_partition(N, K, Max, Partition) :-
		(	K =:= 0 ->
			N =:= 0,
			Partition = []
		;	K > 0,
			N >= K,
			Upper is min(Max, N - K + 1),
			Upper >= 1,
			between(1, Upper, Offset),
			Part is Upper - Offset + 1,
			N1 is N - Part,
			K1 is K - 1,
			Max1 is Part - 1,
			distinct_integer_partition(N1, K1, Max1, Rest),
			Partition = [Part| Rest]
		).

	% number of partitions of N into exactly K parts, computed with the
	% classic p(n,k) = p(n-1,k-1) + p(n-k,k) recurrence; the full table
	% (rather than just the previous row) is kept because p(n-k,k) can
	% reach arbitrarily far back rows
	restricted_partition_table(N, Table) :-
		restricted_partition_table_loop(0, N, [[1]], Table).

	restricted_partition_table_loop(N, N, Table, Table) :-
		!.
	restricted_partition_table_loop(Current, N, Table0, Table) :-
		Current < N,
		Next is Current + 1,
		restricted_partition_row(Next, Table0, NextRow),
		append_table_row(Table0, NextRow, Table1),
		restricted_partition_table_loop(Next, N, Table1, Table).

	restricted_partition_row(N, Table, Row) :-
		findall(Value, (between(0, N, K), restricted_partition_row_entry(N, K, Table, Value)), Row).

	restricted_partition_row_entry(N, 0, _Table, Value) :-
		!,
		(	N =:= 0 ->
			Value = 1
		;	Value = 0
		).
	restricted_partition_row_entry(N, N, _Table, 1) :-
		!.
	restricted_partition_row_entry(N, K, _Table, 0) :-
		K > N,
		!.
	restricted_partition_row_entry(N, K, Table, Value) :-
		Previous is N - 1,
		nth0(Previous, Table, PreviousRow),
		Index1 is K - 1,
		nth0(Index1, PreviousRow, Term1),
		(	K > N - K ->
			Term2 = 0
		;	Earlier is N - K,
			nth0(Earlier, Table, EarlierRow),
			nth0(K, EarlierRow, Term2)
		),
		Value is Term1 + Term2.

	append_table_row([], Row, [Row]).
	append_table_row([Row0| Rows0], Row, [Row0| Rows]) :-
		append_table_row(Rows0, Row, Rows).

	% number of partitions of N into distinct parts, computed as the number
	% of subsets of {1,...,N} summing to N (a 0/1 knapsack style recurrence
	% that, unlike the restricted-parts count above, only ever needs the
	% previous row)
	distinct_partition_table_row(N, Row) :-
		zero_row(N, ZeroTail),
		FirstRow = [1| ZeroTail],
		distinct_partition_row_loop(0, N, FirstRow, Row).

	distinct_partition_row_loop(N, N, Row, Row) :-
		!.
	distinct_partition_row_loop(I, N, PreviousRow, Row) :-
		I < N,
		I1 is I + 1,
		distinct_partition_row_entries(0, N, I1, PreviousRow, NextRow),
		distinct_partition_row_loop(I1, N, NextRow, Row).

	distinct_partition_row_entries(J, N, _I, _PreviousRow, []) :-
		J > N,
		!.
	distinct_partition_row_entries(J, N, I, PreviousRow, [Value| Values]) :-
		J =< N,
		nth0(J, PreviousRow, Term1),
		(	J >= I ->
			J2 is J - I,
			nth0(J2, PreviousRow, Term2)
		;	Term2 = 0
		),
		Value is Term1 + Term2,
		J1 is J + 1,
		distinct_partition_row_entries(J1, N, I, PreviousRow, Values).

	zero_row(0, []) :-
		!.
	zero_row(N, [0| Zeros]) :-
		N > 0,
		N1 is N - 1,
		zero_row(N1, Zeros).

	canonical_lexicographic_partitions(N, Partitions) :-
		partitions(N, lexicographic, Partitions).

	canonical_lexicographic_partitions(N, K, Partitions) :-
		partitions(N, K, lexicographic, Partitions).

	canonical_lexicographic_distinct_partitions(N, Partitions) :-
		distinct_partitions(N, lexicographic, Partitions).

	canonical_lexicographic_distinct_partitions(N, K, Partitions) :-
		distinct_partitions(N, K, lexicographic, Partitions).

	apply_order(default, Partitions, Partitions).
	apply_order(lexicographic, Partitions, SortedPartitions) :-
		msort(Partitions, SortedPartitions).

:- end_object.

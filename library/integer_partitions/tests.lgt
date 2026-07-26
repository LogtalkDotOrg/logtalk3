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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-07-26,
		comment is 'Unit tests for the "integer_partitions" library.'
	]).

	cover(integer_partitions).

	test(partitions_2_zero, deterministic(Partitions == [[]])) :-
		integer_partitions::partitions(0, Partitions).

	test(partition_2_zero, true(Partitions == [[]])) :-
		findall(Partition, integer_partitions::partition(0, Partition), Partitions).

	test(partitions_2_singleton, deterministic(Partitions == [[1]])) :-
		integer_partitions::partitions(1, Partitions).

	test(partitions_3_exact_one_part, deterministic(Partitions == [[4]])) :-
		integer_partitions::partitions(4, 1, Partitions).

	test(partitions_2_four, deterministic(Partitions == [[4],[3,1],[2,2],[2,1,1],[1,1,1,1]])) :-
		integer_partitions::partitions(4, Partitions).

	test(partitions_3_lexicographic, deterministic(Partitions == [[1,1,1,1],[2,1,1],[2,2],[3,1],[4]])) :-
		integer_partitions::partitions(4, lexicographic, Partitions).

	test(partitions_3_three_parts, deterministic(Partitions == [[4,1,1],[3,2,1],[2,2,2]])) :-
		integer_partitions::partitions(6, 3, Partitions).

	test(partitions_4_three_parts_lexicographic, deterministic(Partitions == [[2,2,2],[3,2,1],[4,1,1]])) :-
		integer_partitions::partitions(6, 3, lexicographic, Partitions).

	test(partition_3_three_parts_all, true(Partitions == [[4,1,1],[3,2,1],[2,2,2]])) :-
		findall(Partition, integer_partitions::partition(6, 3, Partition), Partitions).

	test(partition_3_lexicographic_all, true(Partitions == [[1,1,1,1],[2,1,1],[2,2],[3,1],[4]])) :-
		findall(Partition, integer_partitions::partition(4, lexicographic, Partition), Partitions).

	test(partition_4_three_parts_lexicographic_all, true(Partitions == [[2,2,2],[3,2,1],[4,1,1]])) :-
		findall(Partition, integer_partitions::partition(6, 3, lexicographic, Partition), Partitions).

	test(distinct_partitions_2_six, deterministic(Partitions == [[6],[5,1],[4,2],[3,2,1]])) :-
		integer_partitions::distinct_partitions(6, Partitions).

	test(distinct_partitions_3_three_parts, deterministic(Partitions == [[6,2,1],[5,3,1],[4,3,2]])) :-
		integer_partitions::distinct_partitions(9, 3, Partitions).

	test(distinct_partitions_3_lexicographic, deterministic(Partitions == [[3,2,1],[4,2],[5,1],[6]])) :-
		integer_partitions::distinct_partitions(6, lexicographic, Partitions).

	test(distinct_partitions_4_three_parts_lexicographic, deterministic(Partitions == [[4,3,2],[5,3,1],[6,2,1]])) :-
		integer_partitions::distinct_partitions(9, 3, lexicographic, Partitions).

	test(distinct_partition_2_all, true(Partitions == [[6],[5,1],[4,2],[3,2,1]])) :-
		findall(Partition, integer_partitions::distinct_partition(6, Partition), Partitions).

	test(distinct_partition_3_three_parts_all, true(Partitions == [[6,2,1],[5,3,1],[4,3,2]])) :-
		findall(Partition, integer_partitions::distinct_partition(9, 3, Partition), Partitions).

	test(distinct_partition_3_lexicographic_all, true(Partitions == [[3,2,1],[4,2],[5,1],[6]])) :-
		findall(Partition, integer_partitions::distinct_partition(6, lexicographic, Partition), Partitions).

	test(distinct_partition_4_three_parts_lexicographic_all, true(Partitions == [[4,3,2],[5,3,1],[6,2,1]])) :-
		findall(Partition, integer_partitions::distinct_partition(9, 3, lexicographic, Partition), Partitions).

	test(count_partitions_2_four, deterministic(Count == 5)) :-
		integer_partitions::count_partitions(4, Count).

	test(count_partitions_2_zero, deterministic(Count == 1)) :-
		integer_partitions::count_partitions(0, Count).

	test(count_partitions_2_seven, deterministic(Count == 15)) :-
		integer_partitions::count_partitions(7, Count).

	test(count_partitions_3_three_parts, deterministic(Count == 3)) :-
		integer_partitions::count_partitions(6, 3, Count).

	test(count_partitions_3_k_greater_than_n, fail) :-
		integer_partitions::count_partitions(3, 5, _).

	test(count_distinct_partitions_2_six, deterministic(Count == 4)) :-
		integer_partitions::count_distinct_partitions(6, Count).

	test(count_distinct_partitions_2_zero, deterministic(Count == 1)) :-
		integer_partitions::count_distinct_partitions(0, Count).

	test(count_distinct_partitions_2_eight, deterministic(Count == 6)) :-
		integer_partitions::count_distinct_partitions(8, Count).

	test(count_distinct_partitions_3_three_parts, deterministic(Count == 3)) :-
		integer_partitions::count_distinct_partitions(9, 3, Count).

	test(nth_partition_3_default, deterministic(Partition == [2,2])) :-
		integer_partitions::nth_partition(4, 2, Partition).

	test(nth_partition_4_exact_default, deterministic(Partition == [3,2,1])) :-
		integer_partitions::nth_partition(6, 3, 1, Partition).

	test(nth_partition_4_lexicographic, deterministic(Partition == [3,1])) :-
		integer_partitions::nth_partition(4, lexicographic, 3, Partition).

	test(nth_partition_5_exact_lexicographic, deterministic(Partition == [3,2,1])) :-
		integer_partitions::nth_partition(6, 3, lexicographic, 1, Partition).

	test(partition_index_3_default, deterministic(Index == 2)) :-
		integer_partitions::partition_index(4, [2,2], Index).

	test(partition_index_4_exact_default, deterministic(Index == 1)) :-
		integer_partitions::partition_index(6, 3, [3,2,1], Index).

	test(partition_index_4_lexicographic, deterministic(Index == 3)) :-
		integer_partitions::partition_index(4, lexicographic, [3,1], Index).

	test(partition_index_5_exact_lexicographic, deterministic(Index == 2)) :-
		integer_partitions::partition_index(6, 3, lexicographic, [4,1,1], Index).

	test(nth_distinct_partition_3_default, deterministic(Partition == [4,2])) :-
		integer_partitions::nth_distinct_partition(6, 2, Partition).

	test(nth_distinct_partition_4_exact_default, deterministic(Partition == [5,3,1])) :-
		integer_partitions::nth_distinct_partition(9, 3, 1, Partition).

	test(nth_distinct_partition_4_lexicographic, deterministic(Partition == [3,2,1])) :-
		integer_partitions::nth_distinct_partition(6, lexicographic, 0, Partition).

	test(nth_distinct_partition_5_exact_lexicographic, deterministic(Partition == [6,2,1])) :-
		integer_partitions::nth_distinct_partition(9, 3, lexicographic, 2, Partition).

	test(distinct_partition_index_3_default, deterministic(Index == 3)) :-
		integer_partitions::distinct_partition_index(6, [3,2,1], Index).

	test(distinct_partition_index_4_exact_default, deterministic(Index == 2)) :-
		integer_partitions::distinct_partition_index(9, 3, [4,3,2], Index).

	test(distinct_partition_index_4_lexicographic, deterministic(Index == 2)) :-
		integer_partitions::distinct_partition_index(6, lexicographic, [5,1], Index).

	test(distinct_partition_index_5_exact_lexicographic, deterministic(Index == 1)) :-
		integer_partitions::distinct_partition_index(9, 3, lexicographic, [5,3,1], Index).

	test(partitions_exact_zero_zero, deterministic(Partitions == [[]])) :-
		integer_partitions::partitions(0, 0, Partitions).

	test(partitions_exact_one_zero, deterministic(Partitions == [])) :-
		integer_partitions::partitions(1, 0, Partitions).

	test(distinct_partitions_exact_zero_zero, deterministic(Partitions == [[]])) :-
		integer_partitions::distinct_partitions(0, 0, Partitions).

	test(nth_partition_out_of_range, fail) :-
		integer_partitions::nth_partition(4, 10, _).

	test(partition_index_missing, fail) :-
		integer_partitions::partition_index(4, [3,3], _).

	test(nth_distinct_partition_out_of_range, fail) :-
		integer_partitions::nth_distinct_partition(6, 10, _).

	test(distinct_partition_index_missing, fail) :-
		integer_partitions::distinct_partition_index(6, [2,2,2], _).

	test(random_partition_2_exists, true(integer_partitions::partition(10, Partition))) :-
		integer_partitions::random_partition(10, Partition).

	test(random_partition_3_exists, true(integer_partitions::partition(10, 4, Partition))) :-
		integer_partitions::random_partition(10, 4, Partition).

	test(sample_partitions_3_zero, deterministic(Samples == [])) :-
		integer_partitions::sample_partitions(10, 0, Samples).

	test(sample_partitions_4_count, true((length(Samples, 3), forall(list::member(Sample, Samples), integer_partitions::partition(10, 4, Sample))))) :-
		integer_partitions::sample_partitions(10, 4, 3, Samples).

	test(random_distinct_partition_2_exists, true(integer_partitions::distinct_partition(10, Partition))) :-
		integer_partitions::random_distinct_partition(10, Partition).

	test(random_distinct_partition_3_exists, true(integer_partitions::distinct_partition(10, 4, Partition))) :-
		integer_partitions::random_distinct_partition(10, 4, Partition).

	test(sample_distinct_partitions_3_zero, deterministic(Samples == [])) :-
		integer_partitions::sample_distinct_partitions(10, 0, Samples).

	test(sample_distinct_partitions_4_count, true((length(Samples, 3), forall(list::member(Sample, Samples), integer_partitions::distinct_partition(10, 3, Sample))))) :-
		integer_partitions::sample_distinct_partitions(10, 3, 3, Samples).

	test(next_partition_3, deterministic(Next == [2,1,1])) :-
		integer_partitions::next_partition(4, [1,1,1,1], Next).

	test(next_partition_4_exact, deterministic(Next == [3,2,1])) :-
		integer_partitions::next_partition(6, 3, [2,2,2], Next).

	test(previous_partition_3, deterministic(Previous == [3,1])) :-
		integer_partitions::previous_partition(4, [4], Previous).

	test(previous_partition_4_exact, deterministic(Previous == [3,2,1])) :-
		integer_partitions::previous_partition(6, 3, [4,1,1], Previous).

	test(next_partition_first_has_no_previous, fail) :-
		integer_partitions::previous_partition(4, [1,1,1,1], _).

	test(next_partition_last_has_no_next, fail) :-
		integer_partitions::next_partition(4, [4], _).

	test(next_distinct_partition_3, deterministic(Next == [5,3,1])) :-
		integer_partitions::next_distinct_partition(9, [4,3,2], Next).

	test(next_distinct_partition_4_exact, deterministic(Next == [5,3,1])) :-
		integer_partitions::next_distinct_partition(9, 3, [4,3,2], Next).

	test(previous_distinct_partition_3, deterministic(Previous == [4,3,2])) :-
		integer_partitions::previous_distinct_partition(9, [5,3,1], Previous).

	test(previous_distinct_partition_4_exact, deterministic(Previous == [5,3,1])) :-
		integer_partitions::previous_distinct_partition(9, 3, [6,2,1], Previous).

:- end_object.

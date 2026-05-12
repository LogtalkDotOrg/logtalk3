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
		date is 2026-05-12,
		comment is 'Unit tests for the "partitions" library.'
	]).

	cover(partitions).

	test(partitions_2_empty, deterministic(Partitions == [[]])) :-
		partitions::partitions([], Partitions).

	test(partition_2_empty, true(Partitions == [[]])) :-
		findall(Partition, partitions::partition([], Partition), Partitions).

	test(partitions_2_singleton, deterministic(Partitions == [[[a]]])) :-
		partitions::partitions([a], Partitions).

	test(partitions_3_singleton_one_block, deterministic(Partitions == [[[a]]])) :-
		partitions::partitions(1, [a], Partitions).

	test(partitions_2_three, deterministic(Partitions == [[[a,b,c]],[[a,b],[c]],[[a,c],[b]],[[a],[b,c]],[[a],[b],[c]]])) :-
		partitions::partitions([a,b,c], Partitions).

	test(partitions_3_lexicographic, deterministic(Partitions == [[[a],[b],[c]],[[a],[b,c]],[[a,b],[c]],[[a,b,c]],[[a,c],[b]]])) :-
		partitions::partitions([a,b,c], lexicographic, Partitions).

	test(partitions_3_two_blocks, deterministic(Partitions == [[[a,b],[c]],[[a,c],[b]],[[a],[b,c]]])) :-
		partitions::partitions(2, [a,b,c], Partitions).

	test(partitions_4_two_blocks_lexicographic, deterministic(Partitions == [[[a],[b,c]],[[a,b],[c]],[[a,c],[b]]])) :-
		partitions::partitions(2, [a,b,c], lexicographic, Partitions).

	test(partition_3_two_blocks_all, true(Partitions == [[[a,b],[c]],[[a,c],[b]],[[a],[b,c]]])) :-
		findall(Partition, partitions::partition(2, [a,b,c], Partition), Partitions).

	test(partition_3_lexicographic_all, true(Partitions == [[[a],[b],[c]],[[a],[b,c]],[[a,b],[c]],[[a,b,c]],[[a,c],[b]]])) :-
		findall(Partition, partitions::partition([a,b,c], lexicographic, Partition), Partitions).

	test(partition_4_two_blocks_lexicographic_all, true(Partitions == [[[a],[b,c]],[[a,b],[c]],[[a,c],[b]]])) :-
		findall(Partition, partitions::partition(2, [a,b,c], lexicographic, Partition), Partitions).

	test(distinct_partitions_2_duplicates, deterministic(Partitions == [[[a,a,b]],[[a,a],[b]],[[a],[a,b]],[[a],[a],[b]]])) :-
		partitions::distinct_partitions([a,a,b], Partitions).

	test(distinct_partitions_3_duplicates_two_blocks, deterministic(Partitions == [[[a,a],[b]],[[a],[a,b]]])) :-
		partitions::distinct_partitions(2, [a,a,b], Partitions).

	test(distinct_partitions_3_lexicographic_duplicates, deterministic(Partitions == [[[a],[a],[b]],[[a],[a,b]],[[a,a],[b]],[[a,a,b]]])) :-
		partitions::distinct_partitions([a,a,b], lexicographic, Partitions).

	test(distinct_partitions_4_duplicates_two_blocks_lexicographic, deterministic(Partitions == [[[a],[a,b]],[[a,a],[b]]])) :-
		partitions::distinct_partitions(2, [a,a,b], lexicographic, Partitions).

	test(distinct_partition_2_duplicates_all, true(Partitions == [[[a,a,b]],[[a,a],[b]],[[a],[a,b]],[[a],[a],[b]]])) :-
		findall(Partition, partitions::distinct_partition([a,a,b], Partition), Partitions).

	test(distinct_partition_3_duplicates_two_blocks_all, true(Partitions == [[[a,a],[b]],[[a],[a,b]]])) :-
		findall(Partition, partitions::distinct_partition(2, [a,a,b], Partition), Partitions).

	test(distinct_partition_3_lexicographic_duplicates_all, true(Partitions == [[[a],[a],[b]],[[a],[a,b]],[[a,a],[b]],[[a,a,b]]])) :-
		findall(Partition, partitions::distinct_partition([a,a,b], lexicographic, Partition), Partitions).

	test(distinct_partition_4_duplicates_two_blocks_lexicographic_all, true(Partitions == [[[a],[a,b]],[[a,a],[b]]])) :-
		findall(Partition, partitions::distinct_partition(2, [a,a,b], lexicographic, Partition), Partitions).

	test(count_partitions_2_three, deterministic(Count == 5)) :-
		partitions::count_partitions([a,b,c], Count).

	test(nth_partition_3_default, deterministic(Partition == [[a,c],[b]])) :-
		partitions::nth_partition([a,b,c], 2, Partition).

	test(nth_partition_4_exact_default, deterministic(Partition == [[a],[b,c]])) :-
		partitions::nth_partition(2, [a,b,c], 2, Partition).

	test(nth_partition_4_lexicographic, deterministic(Partition == [[a,b,c]])) :-
		partitions::nth_partition([a,b,c], lexicographic, 3, Partition).

	test(nth_partition_5_exact_lexicographic, deterministic(Partition == [[a,c],[b]])) :-
		partitions::nth_partition(2, [a,b,c], lexicographic, 2, Partition).

	test(partition_index_3_default, deterministic(Index == 4)) :-
		partitions::partition_index([a,b,c], [[a],[b],[c]], Index).

	test(partition_index_4_exact_default, deterministic(Index == 1)) :-
		partitions::partition_index(2, [a,b,c], [[a,c],[b]], Index).

	test(partition_index_4_lexicographic, deterministic(Index == 1)) :-
		partitions::partition_index([a,b,c], lexicographic, [[a],[b,c]], Index).

	test(partition_index_5_exact_lexicographic, deterministic(Index == 0)) :-
		partitions::partition_index(2, [a,b,c], lexicographic, [[a],[b,c]], Index).

	test(count_partitions_3_three_two_blocks, deterministic(Count == 3)) :-
		partitions::count_partitions(2, [a,b,c], Count).

	test(count_distinct_partitions_2_duplicates, deterministic(Count == 4)) :-
		partitions::count_distinct_partitions([a,a,b], Count).

	test(nth_distinct_partition_3_default, deterministic(Partition == [[a],[a,b]])) :-
		partitions::nth_distinct_partition([a,a,b], 2, Partition).

	test(nth_distinct_partition_4_exact_default, deterministic(Partition == [[a],[a,b]])) :-
		partitions::nth_distinct_partition(2, [a,a,b], 1, Partition).

	test(nth_distinct_partition_4_lexicographic, deterministic(Partition == [[a],[a,b]])) :-
		partitions::nth_distinct_partition([a,a,b], lexicographic, 1, Partition).

	test(nth_distinct_partition_5_exact_lexicographic, deterministic(Partition == [[a,a],[b]])) :-
		partitions::nth_distinct_partition(2, [a,a,b], lexicographic, 1, Partition).

	test(distinct_partition_index_3_default, deterministic(Index == 3)) :-
		partitions::distinct_partition_index([a,a,b], [[a],[a],[b]], Index).

	test(distinct_partition_index_4_exact_default, deterministic(Index == 0)) :-
		partitions::distinct_partition_index(2, [a,a,b], [[a,a],[b]], Index).

	test(distinct_partition_index_4_lexicographic, deterministic(Index == 2)) :-
		partitions::distinct_partition_index([a,a,b], lexicographic, [[a,a],[b]], Index).

	test(distinct_partition_index_5_exact_lexicographic, deterministic(Index == 0)) :-
		partitions::distinct_partition_index(2, [a,a,b], lexicographic, [[a],[a,b]], Index).

	test(count_distinct_partitions_2_singleton_all_distinct, deterministic(Count == 1)) :-
		partitions::count_distinct_partitions([a], Count).

	test(count_distinct_partitions_2_two_all_distinct, deterministic(Count == 2)) :-
		partitions::count_distinct_partitions([a,b], Count).

	test(count_distinct_partitions_2_all_distinct, deterministic(Count == 5)) :-
		partitions::count_distinct_partitions([a,b,c], Count).

	test(count_distinct_partitions_3_duplicates_two_blocks, deterministic(Count == 2)) :-
		partitions::count_distinct_partitions(2, [a,a,b], Count).

	test(count_distinct_partitions_3_all_distinct_two_blocks, deterministic(Count == 3)) :-
		partitions::count_distinct_partitions(2, [a,b,c], Count).

	test(count_partitions_empty, deterministic(Count == 1)) :-
		partitions::count_partitions([], Count).

	test(partitions_exact_empty_zero, deterministic(Partitions == [[]])) :-
		partitions::partitions(0, [], Partitions).

	test(partitions_exact_empty_one, deterministic(Partitions == [])) :-
		partitions::partitions(1, [], Partitions).

	test(nth_partition_out_of_range, fail) :-
		partitions::nth_partition([a,b], 2, _).

	test(partition_index_missing, fail) :-
		partitions::partition_index([a,b], [[a],[c]], _).

	test(nth_distinct_partition_out_of_range, fail) :-
		partitions::nth_distinct_partition([a,a], 2, _).

	test(distinct_partition_index_missing, fail) :-
		partitions::distinct_partition_index([a,a,b], [[b],[a],[a]], _).

	test(random_partition_2_exists, true(partitions::partition([a,b,c], Partition))) :-
		partitions::random_partition([a,b,c], Partition).

	test(random_partition_3_exists, true(partitions::partition(2, [a,b,c], Partition))) :-
		partitions::random_partition(2, [a,b,c], Partition).

	test(sample_partitions_3_zero, deterministic(Samples == [])) :-
		partitions::sample_partitions([a,b,c], 0, Samples).

	test(sample_partitions_4_count, true((length(Samples, 3), forall(list::member(Sample, Samples), partitions::partition(2, [a,b,c], Sample))))) :-
		partitions::sample_partitions(2, [a,b,c], 3, Samples).

	test(random_distinct_partition_2_exists, true(partitions::distinct_partition([a,a,b], Partition))) :-
		partitions::random_distinct_partition([a,a,b], Partition).

	test(random_distinct_partition_3_exists, true(partitions::distinct_partition(2, [a,a,b], Partition))) :-
		partitions::random_distinct_partition(2, [a,a,b], Partition).

	test(sample_distinct_partitions_3_zero, deterministic(Samples == [])) :-
		partitions::sample_distinct_partitions([a,a,b], 0, Samples).

	test(sample_distinct_partitions_4_count, true((length(Samples, 3), forall(list::member(Sample, Samples), partitions::distinct_partition(2, [a,a,b], Sample))))) :-
		partitions::sample_distinct_partitions(2, [a,a,b], 3, Samples).

	test(next_partition_3_duplicates, deterministic(Next == [[a,a],[b]])) :-
		partitions::next_partition([a,a,b], [[a],[a,b]], Next).

	test(next_partition_4_exact, deterministic(Next == [[a,c],[b]])) :-
		partitions::next_partition(2, [a,b,c], [[a,b],[c]], Next).

	test(previous_partition_3_duplicates, deterministic(Previous == [[a],[a,b]])) :-
		partitions::previous_partition([a,a,b], [[a,a],[b]], Previous).

	test(previous_partition_4_exact, deterministic(Previous == [[a,b],[c]])) :-
		partitions::previous_partition(2, [a,b,c], [[a,c],[b]], Previous).

:- end_object.

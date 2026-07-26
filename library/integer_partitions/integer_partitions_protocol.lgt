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


:- protocol(integer_partitions_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Protocol for integer partition operations.'
	]).

	:- public(partitions/2).
	:- mode(partitions(+non_negative_integer, -list), one).
	:- info(partitions/2, [
		comment is 'Generates all partitions of a non-negative integer using default order. Partitions are represented as non-increasing lists of positive integers that sum to the given integer. Zero has one partition, represented by the empty list of parts.',
		argnames is ['N', 'Partitions']
	]).

	:- public(partition/2).
	:- mode(partition(+non_negative_integer, -list), one_or_more).
	:- info(partition/2, [
		comment is 'True iff the second argument is a partition of a non-negative integer using default order.',
		argnames is ['N', 'Partition']
	]).

	:- public(partitions/3).
	:- mode(partitions(+non_negative_integer, +atom, -list), one).
	:- mode(partitions(+non_negative_integer, +non_negative_integer, -list), one).
	:- info(partitions/3, [
		comment is 'Generates either all partitions of an integer with the given order (when the second argument is ``default`` or ``lexicographic``) or all partitions with an exact number of parts using default order (when the second argument is an integer). Note that, unlike the ``partitions`` library (where the size constraint can occupy the first argument position, distinguishing it from a list by type), here the integer being partitioned and the exact part count are both integers, so the part count is always given as the second argument, distinguished from the order by type.',
		argnames is ['N', 'KOrOrder', 'Partitions']
	]).

	:- public(partition/3).
	:- mode(partition(+non_negative_integer, +atom, -list), one_or_more).
	:- mode(partition(+non_negative_integer, +non_negative_integer, -list), one_or_more).
	:- info(partition/3, [
		comment is 'True iff the third argument is a partition generated either in the given order or with the given exact number of parts using default order.',
		argnames is ['N', 'KOrOrder', 'Partition']
	]).

	:- public(partitions/4).
	:- mode(partitions(+non_negative_integer, +non_negative_integer, +atom, -list), one).
	:- info(partitions/4, [
		comment is 'Generates all partitions of an integer with an exact number of parts and the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Partitions']
	]).

	:- public(partition/4).
	:- mode(partition(+non_negative_integer, +non_negative_integer, +atom, -list), one_or_more).
	:- info(partition/4, [
		comment is 'True iff the fourth argument is a partition with an exact number of parts and the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Partition']
	]).

	:- public(distinct_partitions/2).
	:- mode(distinct_partitions(+non_negative_integer, -list), one).
	:- info(distinct_partitions/2, [
		comment is 'Generates all partitions of a non-negative integer into distinct (non-repeating) parts using default order. Unlike the ``partitions`` library, where "distinct" deduplicates equal-valued results from a position-sensitive generator, an integer partition has no position-sensitive input to begin with; here "distinct" is the standard integer partition notion of parts that are pairwise different (also known as strict partitions).',
		argnames is ['N', 'Partitions']
	]).

	:- public(distinct_partition/2).
	:- mode(distinct_partition(+non_negative_integer, -list), one_or_more).
	:- info(distinct_partition/2, [
		comment is 'True iff the second argument is a partition of a non-negative integer into distinct parts using default order.',
		argnames is ['N', 'Partition']
	]).

	:- public(distinct_partitions/3).
	:- mode(distinct_partitions(+non_negative_integer, +atom, -list), one).
	:- mode(distinct_partitions(+non_negative_integer, +non_negative_integer, -list), one).
	:- info(distinct_partitions/3, [
		comment is 'Generates either all partitions into distinct parts with the given order (when the second argument is ``default`` or ``lexicographic``) or all partitions into an exact number of distinct parts using default order (when the second argument is an integer).',
		argnames is ['N', 'KOrOrder', 'Partitions']
	]).

	:- public(distinct_partition/3).
	:- mode(distinct_partition(+non_negative_integer, +atom, -list), one_or_more).
	:- mode(distinct_partition(+non_negative_integer, +non_negative_integer, -list), one_or_more).
	:- info(distinct_partition/3, [
		comment is 'True iff the third argument is a partition into distinct parts generated either in the given order or with the given exact number of parts using default order.',
		argnames is ['N', 'KOrOrder', 'Partition']
	]).

	:- public(distinct_partitions/4).
	:- mode(distinct_partitions(+non_negative_integer, +non_negative_integer, +atom, -list), one).
	:- info(distinct_partitions/4, [
		comment is 'Generates all partitions of an integer into an exact number of distinct parts and the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Partitions']
	]).

	:- public(distinct_partition/4).
	:- mode(distinct_partition(+non_negative_integer, +non_negative_integer, +atom, -list), one_or_more).
	:- info(distinct_partition/4, [
		comment is 'True iff the fourth argument is a partition into an exact number of distinct parts and the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Partition']
	]).

	:- public(count_partitions/2).
	:- mode(count_partitions(+non_negative_integer, -non_negative_integer), one).
	:- info(count_partitions/2, [
		comment is 'Counts the number of partitions of a non-negative integer.',
		argnames is ['N', 'Count']
	]).

	:- public(count_partitions/3).
	:- mode(count_partitions(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(count_partitions/3, [
		comment is 'Counts the number of partitions of a non-negative integer with an exact number of parts.',
		argnames is ['N', 'K', 'Count']
	]).

	:- public(count_distinct_partitions/2).
	:- mode(count_distinct_partitions(+non_negative_integer, -non_negative_integer), one).
	:- info(count_distinct_partitions/2, [
		comment is 'Counts the number of partitions of a non-negative integer into distinct parts.',
		argnames is ['N', 'Count']
	]).

	:- public(count_distinct_partitions/3).
	:- mode(count_distinct_partitions(+non_negative_integer, +non_negative_integer, -non_negative_integer), zero_or_one).
	:- info(count_distinct_partitions/3, [
		comment is 'Counts the number of partitions of a non-negative integer into an exact number of distinct parts.',
		argnames is ['N', 'K', 'Count']
	]).

	:- public(nth_partition/3).
	:- mode(nth_partition(+non_negative_integer, +integer, -list), zero_or_one).
	:- info(nth_partition/3, [
		comment is 'Returns the partition at a given zero-based index using default order.',
		argnames is ['N', 'Index', 'Partition']
	]).

	:- public(nth_partition/4).
	:- mode(nth_partition(+non_negative_integer, +atom, +integer, -list), zero_or_one).
	:- mode(nth_partition(+non_negative_integer, +non_negative_integer, +integer, -list), zero_or_one).
	:- info(nth_partition/4, [
		comment is 'Returns either the partition at a given zero-based index in the given order (when the second argument is ``default`` or ``lexicographic``) or the partition with an exact number of parts at a given zero-based index using default order (when the second argument is an integer).',
		argnames is ['N', 'KOrOrder', 'Index', 'Partition']
	]).

	:- public(nth_partition/5).
	:- mode(nth_partition(+non_negative_integer, +non_negative_integer, +atom, +integer, -list), zero_or_one).
	:- info(nth_partition/5, [
		comment is 'Returns the partition with an exact number of parts at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Index', 'Partition']
	]).

	:- public(partition_index/3).
	:- mode(partition_index(+non_negative_integer, +list, -integer), zero_or_one).
	:- info(partition_index/3, [
		comment is 'Returns the zero-based index of a partition using default order.',
		argnames is ['N', 'Partition', 'Index']
	]).

	:- public(partition_index/4).
	:- mode(partition_index(+non_negative_integer, +atom, +list, -integer), zero_or_one).
	:- mode(partition_index(+non_negative_integer, +non_negative_integer, +list, -integer), zero_or_one).
	:- info(partition_index/4, [
		comment is 'Returns either the zero-based index of a partition in the given order (when the second argument is ``default`` or ``lexicographic``) or the zero-based index of a partition with an exact number of parts using default order (when the second argument is an integer).',
		argnames is ['N', 'KOrOrder', 'Partition', 'Index']
	]).

	:- public(partition_index/5).
	:- mode(partition_index(+non_negative_integer, +non_negative_integer, +atom, +list, -integer), zero_or_one).
	:- info(partition_index/5, [
		comment is 'Returns the zero-based index of a partition with an exact number of parts in the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Partition', 'Index']
	]).

	:- public(nth_distinct_partition/3).
	:- mode(nth_distinct_partition(+non_negative_integer, +integer, -list), zero_or_one).
	:- info(nth_distinct_partition/3, [
		comment is 'Returns the partition into distinct parts at a given zero-based index using default order.',
		argnames is ['N', 'Index', 'Partition']
	]).

	:- public(nth_distinct_partition/4).
	:- mode(nth_distinct_partition(+non_negative_integer, +atom, +integer, -list), zero_or_one).
	:- mode(nth_distinct_partition(+non_negative_integer, +non_negative_integer, +integer, -list), zero_or_one).
	:- info(nth_distinct_partition/4, [
		comment is 'Returns either the partition into distinct parts at a given zero-based index in the given order (when the second argument is ``default`` or ``lexicographic``) or the partition with an exact number of distinct parts at a given zero-based index using default order (when the second argument is an integer).',
		argnames is ['N', 'KOrOrder', 'Index', 'Partition']
	]).

	:- public(nth_distinct_partition/5).
	:- mode(nth_distinct_partition(+non_negative_integer, +non_negative_integer, +atom, +integer, -list), zero_or_one).
	:- info(nth_distinct_partition/5, [
		comment is 'Returns the partition with an exact number of distinct parts at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Index', 'Partition']
	]).

	:- public(distinct_partition_index/3).
	:- mode(distinct_partition_index(+non_negative_integer, +list, -integer), zero_or_one).
	:- info(distinct_partition_index/3, [
		comment is 'Returns the zero-based index of a partition into distinct parts using default order.',
		argnames is ['N', 'Partition', 'Index']
	]).

	:- public(distinct_partition_index/4).
	:- mode(distinct_partition_index(+non_negative_integer, +atom, +list, -integer), zero_or_one).
	:- mode(distinct_partition_index(+non_negative_integer, +non_negative_integer, +list, -integer), zero_or_one).
	:- info(distinct_partition_index/4, [
		comment is 'Returns either the zero-based index of a partition into distinct parts in the given order (when the second argument is ``default`` or ``lexicographic``) or the zero-based index of a partition with an exact number of distinct parts using default order (when the second argument is an integer).',
		argnames is ['N', 'KOrOrder', 'Partition', 'Index']
	]).

	:- public(distinct_partition_index/5).
	:- mode(distinct_partition_index(+non_negative_integer, +non_negative_integer, +atom, +list, -integer), zero_or_one).
	:- info(distinct_partition_index/5, [
		comment is 'Returns the zero-based index of a partition with an exact number of distinct parts in the given order: ``default`` or ``lexicographic``.',
		argnames is ['N', 'K', 'Order', 'Partition', 'Index']
	]).

	:- public(random_partition/2).
	:- mode(random_partition(+non_negative_integer, -list), one).
	:- info(random_partition/2, [
		comment is 'Returns a random partition of a non-negative integer.',
		argnames is ['N', 'Partition']
	]).

	:- public(random_partition/3).
	:- mode(random_partition(+non_negative_integer, +non_negative_integer, -list), zero_or_one).
	:- info(random_partition/3, [
		comment is 'Returns a random partition of a non-negative integer with an exact number of parts.',
		argnames is ['N', 'K', 'Partition']
	]).

	:- public(sample_partitions/3).
	:- mode(sample_partitions(+non_negative_integer, +non_negative_integer, -list), zero_or_one).
	:- info(sample_partitions/3, [
		comment is 'Returns SampleCount random partitions of a non-negative integer, sampled with replacement.',
		argnames is ['N', 'SampleCount', 'Samples']
	]).

	:- public(sample_partitions/4).
	:- mode(sample_partitions(+non_negative_integer, +non_negative_integer, +non_negative_integer, -list), zero_or_one).
	:- info(sample_partitions/4, [
		comment is 'Returns SampleCount random partitions of a non-negative integer with an exact number of parts, sampled with replacement.',
		argnames is ['N', 'K', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_partition/2).
	:- mode(random_distinct_partition(+non_negative_integer, -list), one).
	:- info(random_distinct_partition/2, [
		comment is 'Returns a random partition of a non-negative integer into distinct parts.',
		argnames is ['N', 'Partition']
	]).

	:- public(random_distinct_partition/3).
	:- mode(random_distinct_partition(+non_negative_integer, +non_negative_integer, -list), zero_or_one).
	:- info(random_distinct_partition/3, [
		comment is 'Returns a random partition of a non-negative integer with an exact number of distinct parts.',
		argnames is ['N', 'K', 'Partition']
	]).

	:- public(sample_distinct_partitions/3).
	:- mode(sample_distinct_partitions(+non_negative_integer, +non_negative_integer, -list), zero_or_one).
	:- info(sample_distinct_partitions/3, [
		comment is 'Returns SampleCount random partitions of a non-negative integer into distinct parts, sampled with replacement.',
		argnames is ['N', 'SampleCount', 'Samples']
	]).

	:- public(sample_distinct_partitions/4).
	:- mode(sample_distinct_partitions(+non_negative_integer, +non_negative_integer, +non_negative_integer, -list), zero_or_one).
	:- info(sample_distinct_partitions/4, [
		comment is 'Returns SampleCount random partitions of a non-negative integer with an exact number of distinct parts, sampled with replacement.',
		argnames is ['N', 'K', 'SampleCount', 'Samples']
	]).

	:- public(next_partition/3).
	:- mode(next_partition(+non_negative_integer, +list, -list), zero_or_one).
	:- info(next_partition/3, [
		comment is 'Returns the next partition value of a non-negative integer in lexicographic order.',
		argnames is ['N', 'Partition', 'Next']
	]).

	:- public(next_partition/4).
	:- mode(next_partition(+non_negative_integer, +non_negative_integer, +list, -list), zero_or_one).
	:- info(next_partition/4, [
		comment is 'Returns the next partition value of a non-negative integer with an exact number of parts in lexicographic order.',
		argnames is ['N', 'K', 'Partition', 'Next']
	]).

	:- public(previous_partition/3).
	:- mode(previous_partition(+non_negative_integer, +list, -list), zero_or_one).
	:- info(previous_partition/3, [
		comment is 'Returns the previous partition value of a non-negative integer in lexicographic order.',
		argnames is ['N', 'Partition', 'Previous']
	]).

	:- public(previous_partition/4).
	:- mode(previous_partition(+non_negative_integer, +non_negative_integer, +list, -list), zero_or_one).
	:- info(previous_partition/4, [
		comment is 'Returns the previous partition value of a non-negative integer with an exact number of parts in lexicographic order.',
		argnames is ['N', 'K', 'Partition', 'Previous']
	]).

	:- public(next_distinct_partition/3).
	:- mode(next_distinct_partition(+non_negative_integer, +list, -list), zero_or_one).
	:- info(next_distinct_partition/3, [
		comment is 'Returns the next partition-into-distinct-parts value of a non-negative integer in lexicographic order.',
		argnames is ['N', 'Partition', 'Next']
	]).

	:- public(next_distinct_partition/4).
	:- mode(next_distinct_partition(+non_negative_integer, +non_negative_integer, +list, -list), zero_or_one).
	:- info(next_distinct_partition/4, [
		comment is 'Returns the next partition-into-distinct-parts value of a non-negative integer with an exact number of parts in lexicographic order.',
		argnames is ['N', 'K', 'Partition', 'Next']
	]).

	:- public(previous_distinct_partition/3).
	:- mode(previous_distinct_partition(+non_negative_integer, +list, -list), zero_or_one).
	:- info(previous_distinct_partition/3, [
		comment is 'Returns the previous partition-into-distinct-parts value of a non-negative integer in lexicographic order.',
		argnames is ['N', 'Partition', 'Previous']
	]).

	:- public(previous_distinct_partition/4).
	:- mode(previous_distinct_partition(+non_negative_integer, +non_negative_integer, +list, -list), zero_or_one).
	:- info(previous_distinct_partition/4, [
		comment is 'Returns the previous partition-into-distinct-parts value of a non-negative integer with an exact number of parts in lexicographic order.',
		argnames is ['N', 'K', 'Partition', 'Previous']
	]).

:- end_protocol.

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


:- protocol(partitions_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for set partition operations over lists.'
	]).

	:- public(partitions/2).
	:- mode(partitions(+list, -list), one).
	:- info(partitions/2, [
		comment is 'Generates all set partitions of a list using default order. Partitions are represented as lists of non-empty blocks. The empty list has one partition, represented by the empty list of blocks.',
		argnames is ['List', 'Partitions']
	]).

	:- public(partition/2).
	:- mode(partition(+list, -list), one_or_more).
	:- info(partition/2, [
		comment is 'True iff the second argument is a set partition of a list using default order.',
		argnames is ['List', 'Partition']
	]).

	:- public(partitions/3).
	:- mode(partitions(+list, +atom, -list), one).
	:- mode(partitions(+integer, +list, -list), one).
	:- info(partitions/3, [
		comment is 'Generates either all set partitions with the given order (when the second argument is ``default`` or ``lexicographic``) or all set partitions with an exact number of blocks using default order (when the first argument is an integer).',
		argnames is ['ListOrK', 'OrderOrList', 'Partitions']
	]).

	:- public(partition/3).
	:- mode(partition(+list, +atom, -list), one_or_more).
	:- mode(partition(+integer, +list, -list), one_or_more).
	:- info(partition/3, [
		comment is 'True iff the third argument is a set partition generated either in the given order or with the given exact number of blocks using default order.',
		argnames is ['ListOrK', 'OrderOrList', 'Partition']
	]).

	:- public(partitions/4).
	:- mode(partitions(+integer, +list, +atom, -list), one).
	:- info(partitions/4, [
		comment is 'Generates all set partitions with an exact number of blocks and the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Partitions']
	]).

	:- public(partition/4).
	:- mode(partition(+integer, +list, +atom, -list), one_or_more).
	:- info(partition/4, [
		comment is 'True iff the fourth argument is a set partition with an exact number of blocks and the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Partition']
	]).

	:- public(distinct_partitions/2).
	:- mode(distinct_partitions(+list, -list), one).
	:- info(distinct_partitions/2, [
		comment is 'Generates all distinct set partitions of a list (deduplicating equal-valued partitions after canonicalizing block order) using default order.',
		argnames is ['List', 'Partitions']
	]).

	:- public(distinct_partition/2).
	:- mode(distinct_partition(+list, -list), one_or_more).
	:- info(distinct_partition/2, [
		comment is 'True iff the second argument is a distinct set partition of a list using default order.',
		argnames is ['List', 'Partition']
	]).

	:- public(distinct_partitions/3).
	:- mode(distinct_partitions(+list, +atom, -list), one).
	:- mode(distinct_partitions(+integer, +list, -list), one).
	:- info(distinct_partitions/3, [
		comment is 'Generates either all distinct set partitions with the given order (when the second argument is ``default`` or ``lexicographic``) or all distinct set partitions with an exact number of blocks using default order (when the first argument is an integer).',
		argnames is ['ListOrK', 'OrderOrList', 'Partitions']
	]).

	:- public(distinct_partition/3).
	:- mode(distinct_partition(+list, +atom, -list), one_or_more).
	:- mode(distinct_partition(+integer, +list, -list), one_or_more).
	:- info(distinct_partition/3, [
		comment is 'True iff the third argument is a distinct set partition generated either in the given order or with the given exact number of blocks using default order.',
		argnames is ['ListOrK', 'OrderOrList', 'Partition']
	]).

	:- public(distinct_partitions/4).
	:- mode(distinct_partitions(+integer, +list, +atom, -list), one).
	:- info(distinct_partitions/4, [
		comment is 'Generates all distinct set partitions with an exact number of blocks and the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Partitions']
	]).

	:- public(distinct_partition/4).
	:- mode(distinct_partition(+integer, +list, +atom, -list), one_or_more).
	:- info(distinct_partition/4, [
		comment is 'True iff the fourth argument is a distinct set partition with an exact number of blocks and the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Partition']
	]).

	:- public(count_partitions/2).
	:- mode(count_partitions(+list, -integer), one).
	:- info(count_partitions/2, [
		comment is 'Counts the number of set partitions of a list.',
		argnames is ['List', 'Count']
	]).

	:- public(nth_partition/3).
	:- mode(nth_partition(+list, +integer, -list), zero_or_one).
	:- info(nth_partition/3, [
		comment is 'Returns the set partition at a given zero-based index using default order.',
		argnames is ['List', 'Index', 'Partition']
	]).

	:- public(nth_partition/4).
	:- mode(nth_partition(+list, +atom, +integer, -list), zero_or_one).
	:- mode(nth_partition(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_partition/4, [
		comment is 'Returns either the set partition at a given zero-based index in the given order (when the second argument is ``default`` or ``lexicographic``) or the set partition with an exact number of blocks at a given zero-based index using default order (when the first argument is an integer).',
		argnames is ['ListOrK', 'OrderOrList', 'Index', 'Partition']
	]).

	:- public(nth_partition/5).
	:- mode(nth_partition(+integer, +list, +atom, +integer, -list), zero_or_one).
	:- info(nth_partition/5, [
		comment is 'Returns the set partition with an exact number of blocks at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Index', 'Partition']
	]).

	:- public(partition_index/3).
	:- mode(partition_index(+list, +list, -integer), zero_or_one).
	:- info(partition_index/3, [
		comment is 'Returns the zero-based index of a set partition using default order.',
		argnames is ['List', 'Partition', 'Index']
	]).

	:- public(partition_index/4).
	:- mode(partition_index(+list, +atom, +list, -integer), zero_or_one).
	:- mode(partition_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(partition_index/4, [
		comment is 'Returns either the zero-based index of a set partition in the given order (when the second argument is ``default`` or ``lexicographic``) or the zero-based index of a set partition with an exact number of blocks using default order (when the first argument is an integer).',
		argnames is ['ListOrK', 'OrderOrList', 'Partition', 'Index']
	]).

	:- public(partition_index/5).
	:- mode(partition_index(+integer, +list, +atom, +list, -integer), zero_or_one).
	:- info(partition_index/5, [
		comment is 'Returns the zero-based index of a set partition with an exact number of blocks in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Partition', 'Index']
	]).

	:- public(count_partitions/3).
	:- mode(count_partitions(+integer, +list, -integer), zero_or_one).
	:- info(count_partitions/3, [
		comment is 'Counts the number of set partitions of a list with an exact number of blocks.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(count_distinct_partitions/2).
	:- mode(count_distinct_partitions(+list, -integer), one).
	:- info(count_distinct_partitions/2, [
		comment is 'Counts the number of distinct set partitions of a list (deduplicating equal-valued partitions).',
		argnames is ['List', 'Count']
	]).

	:- public(nth_distinct_partition/3).
	:- mode(nth_distinct_partition(+list, +integer, -list), zero_or_one).
	:- info(nth_distinct_partition/3, [
		comment is 'Returns the distinct set partition at a given zero-based index using default order.',
		argnames is ['List', 'Index', 'Partition']
	]).

	:- public(nth_distinct_partition/4).
	:- mode(nth_distinct_partition(+list, +atom, +integer, -list), zero_or_one).
	:- mode(nth_distinct_partition(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_distinct_partition/4, [
		comment is 'Returns either the distinct set partition at a given zero-based index in the given order (when the second argument is ``default`` or ``lexicographic``) or the distinct set partition with an exact number of blocks at a given zero-based index using default order (when the first argument is an integer).',
		argnames is ['ListOrK', 'OrderOrList', 'Index', 'Partition']
	]).

	:- public(nth_distinct_partition/5).
	:- mode(nth_distinct_partition(+integer, +list, +atom, +integer, -list), zero_or_one).
	:- info(nth_distinct_partition/5, [
		comment is 'Returns the distinct set partition with an exact number of blocks at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Index', 'Partition']
	]).

	:- public(distinct_partition_index/3).
	:- mode(distinct_partition_index(+list, +list, -integer), zero_or_one).
	:- info(distinct_partition_index/3, [
		comment is 'Returns the zero-based index of a distinct set partition using default order.',
		argnames is ['List', 'Partition', 'Index']
	]).

	:- public(distinct_partition_index/4).
	:- mode(distinct_partition_index(+list, +atom, +list, -integer), zero_or_one).
	:- mode(distinct_partition_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(distinct_partition_index/4, [
		comment is 'Returns either the zero-based index of a distinct set partition in the given order (when the second argument is ``default`` or ``lexicographic``) or the zero-based index of a distinct set partition with an exact number of blocks using default order (when the first argument is an integer).',
		argnames is ['ListOrK', 'OrderOrList', 'Partition', 'Index']
	]).

	:- public(distinct_partition_index/5).
	:- mode(distinct_partition_index(+integer, +list, +atom, +list, -integer), zero_or_one).
	:- info(distinct_partition_index/5, [
		comment is 'Returns the zero-based index of a distinct set partition with an exact number of blocks in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Partition', 'Index']
	]).

	:- public(count_distinct_partitions/3).
	:- mode(count_distinct_partitions(+integer, +list, -integer), zero_or_one).
	:- info(count_distinct_partitions/3, [
		comment is 'Counts the number of distinct set partitions of a list with an exact number of blocks.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(random_partition/2).
	:- mode(random_partition(+list, -list), one).
	:- info(random_partition/2, [
		comment is 'Returns a random set partition of a list.',
		argnames is ['List', 'Partition']
	]).

	:- public(random_partition/3).
	:- mode(random_partition(+integer, +list, -list), zero_or_one).
	:- info(random_partition/3, [
		comment is 'Returns a random set partition of a list with an exact number of blocks.',
		argnames is ['K', 'List', 'Partition']
	]).

	:- public(sample_partitions/3).
	:- mode(sample_partitions(+list, +integer, -list), zero_or_one).
	:- info(sample_partitions/3, [
		comment is 'Returns SampleCount random set partitions of a list, sampled with replacement.',
		argnames is ['List', 'SampleCount', 'Samples']
	]).

	:- public(sample_partitions/4).
	:- mode(sample_partitions(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_partitions/4, [
		comment is 'Returns SampleCount random set partitions of a list with an exact number of blocks, sampled with replacement.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_partition/2).
	:- mode(random_distinct_partition(+list, -list), one).
	:- info(random_distinct_partition/2, [
		comment is 'Returns a random distinct set partition of a list (deduplicating equal-valued partitions).',
		argnames is ['List', 'Partition']
	]).

	:- public(random_distinct_partition/3).
	:- mode(random_distinct_partition(+integer, +list, -list), zero_or_one).
	:- info(random_distinct_partition/3, [
		comment is 'Returns a random distinct set partition of a list with an exact number of blocks (deduplicating equal-valued partitions).',
		argnames is ['K', 'List', 'Partition']
	]).

	:- public(sample_distinct_partitions/3).
	:- mode(sample_distinct_partitions(+list, +integer, -list), zero_or_one).
	:- info(sample_distinct_partitions/3, [
		comment is 'Returns SampleCount random distinct set partitions of a list, sampled with replacement after deduplicating equal-valued partitions.',
		argnames is ['List', 'SampleCount', 'Samples']
	]).

	:- public(sample_distinct_partitions/4).
	:- mode(sample_distinct_partitions(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_distinct_partitions/4, [
		comment is 'Returns SampleCount random distinct set partitions of a list with an exact number of blocks, sampled with replacement after deduplicating equal-valued partitions.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(next_partition/3).
	:- mode(next_partition(+list, +list, -list), zero_or_one).
	:- info(next_partition/3, [
		comment is 'Returns the next distinct set partition value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Partition', 'Next']
	]).

	:- public(next_partition/4).
	:- mode(next_partition(+integer, +list, +list, -list), zero_or_one).
	:- info(next_partition/4, [
		comment is 'Returns the next distinct set partition value with an exact number of blocks in lexicographic order induced by the second argument.',
		argnames is ['K', 'List', 'Partition', 'Next']
	]).

	:- public(previous_partition/3).
	:- mode(previous_partition(+list, +list, -list), zero_or_one).
	:- info(previous_partition/3, [
		comment is 'Returns the previous distinct set partition value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Partition', 'Previous']
	]).

	:- public(previous_partition/4).
	:- mode(previous_partition(+integer, +list, +list, -list), zero_or_one).
	:- info(previous_partition/4, [
		comment is 'Returns the previous distinct set partition value with an exact number of blocks in lexicographic order induced by the second argument.',
		argnames is ['K', 'List', 'Partition', 'Previous']
	]).

:- end_protocol.

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


:- protocol(arrangements_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for arrangements operations over lists.'
	]).

	:- public(arrangements/3).
	:- mode(arrangements(+integer, +list, -list), one).
	:- info(arrangements/3, [
		comment is 'Generates all K-arrangements (ordered K-element selections with replacement) of a list using default order.',
		argnames is ['K', 'List', 'Arrangements']
	]).

	:- public(arrangement/3).
	:- mode(arrangement(+integer, +list, -list), one_or_more).
	:- info(arrangement/3, [
		comment is 'True iff the third argument is a K-arrangement with replacement of a list using default order.',
		argnames is ['K', 'List', 'Arrangement']
	]).

	:- public(arrangements/4).
	:- mode(arrangements(+integer, +list, +atom, -list), one).
	:- info(arrangements/4, [
		comment is 'Generates all K-arrangements with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Arrangements']
	]).

	:- public(arrangement/4).
	:- mode(arrangement(+integer, +list, +atom, -list), one_or_more).
	:- info(arrangement/4, [
		comment is 'True iff the fourth argument is a K-arrangement with replacement with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Arrangement']
	]).

	:- public(distinct_arrangements/3).
	:- mode(distinct_arrangements(+integer, +list, -list), one).
	:- info(distinct_arrangements/3, [
		comment is 'Generates all distinct K-arrangements (deduplicating repeated values in the input list) using default order.',
		argnames is ['K', 'List', 'Arrangements']
	]).

	:- public(distinct_arrangement/3).
	:- mode(distinct_arrangement(+integer, +list, -list), one_or_more).
	:- info(distinct_arrangement/3, [
		comment is 'True iff the third argument is a distinct K-arrangement with replacement of a list using default order.',
		argnames is ['K', 'List', 'Arrangement']
	]).

	:- public(distinct_arrangements/4).
	:- mode(distinct_arrangements(+integer, +list, +atom, -list), one).
	:- info(distinct_arrangements/4, [
		comment is 'Generates all distinct K-arrangements with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Arrangements']
	]).

	:- public(distinct_arrangement/4).
	:- mode(distinct_arrangement(+integer, +list, +atom, -list), one_or_more).
	:- info(distinct_arrangement/4, [
		comment is 'True iff the fourth argument is a distinct K-arrangement with replacement with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Arrangement']
	]).

	:- public(nth_arrangement/4).
	:- mode(nth_arrangement(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_arrangement/4, [
		comment is 'Returns the K-arrangement at a given zero-based index using default order.',
		argnames is ['K', 'List', 'Index', 'Arrangement']
	]).

	:- public(nth_arrangement/5).
	:- mode(nth_arrangement(+integer, +list, +atom, +integer, -list), zero_or_one).
	:- info(nth_arrangement/5, [
		comment is 'Returns the K-arrangement at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Index', 'Arrangement']
	]).

	:- public(arrangement_index/4).
	:- mode(arrangement_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(arrangement_index/4, [
		comment is 'Returns the zero-based index of a K-arrangement using default order.',
		argnames is ['K', 'List', 'Arrangement', 'Index']
	]).

	:- public(arrangement_index/5).
	:- mode(arrangement_index(+integer, +list, +atom, +list, -integer), zero_or_one).
	:- info(arrangement_index/5, [
		comment is 'Returns the zero-based index of a K-arrangement in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Arrangement', 'Index']
	]).

	:- public(count_arrangements/3).
	:- mode(count_arrangements(+integer, +list, -integer), one).
	:- info(count_arrangements/3, [
		comment is 'Counts the number of K-arrangements with replacement of a list.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(count_distinct_arrangements/3).
	:- mode(count_distinct_arrangements(+integer, +list, -integer), one).
	:- info(count_distinct_arrangements/3, [
		comment is 'Counts the number of distinct K-arrangements with replacement of a list (deduplicating repeated values in the input list).',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(nth_distinct_arrangement/4).
	:- mode(nth_distinct_arrangement(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_distinct_arrangement/4, [
		comment is 'Returns the distinct K-arrangement at a given zero-based index in default generation order.',
		argnames is ['K', 'List', 'Index', 'Arrangement']
	]).

	:- public(distinct_arrangement_index/4).
	:- mode(distinct_arrangement_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(distinct_arrangement_index/4, [
		comment is 'Returns the zero-based index of a distinct K-arrangement in default generation order.',
		argnames is ['K', 'List', 'Arrangement', 'Index']
	]).

	:- public(random_arrangement/3).
	:- mode(random_arrangement(+integer, +list, -list), zero_or_one).
	:- info(random_arrangement/3, [
		comment is 'Returns a random K-arrangement with replacement of a list.',
		argnames is ['K', 'List', 'Arrangement']
	]).

	:- public(sample_arrangements/4).
	:- mode(sample_arrangements(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_arrangements/4, [
		comment is 'Returns SampleCount random K-arrangements with replacement of a list, sampled with replacement.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_arrangement/3).
	:- mode(random_distinct_arrangement(+integer, +list, -list), zero_or_one).
	:- info(random_distinct_arrangement/3, [
		comment is 'Returns a random distinct K-arrangement with replacement of a list (deduplicating repeated values in the input list).',
		argnames is ['K', 'List', 'Arrangement']
	]).

	:- public(sample_distinct_arrangements/4).
	:- mode(sample_distinct_arrangements(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_distinct_arrangements/4, [
		comment is 'Returns SampleCount random distinct K-arrangements with replacement of a list, sampled with replacement after deduplicating repeated values in the input list.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(next_arrangement/3).
	:- mode(next_arrangement(+list, +list, -list), zero_or_one).
	:- info(next_arrangement/3, [
		comment is 'Returns the next distinct arrangement value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Arrangement', 'Next']
	]).

	:- public(previous_arrangement/3).
	:- mode(previous_arrangement(+list, +list, -list), zero_or_one).
	:- info(previous_arrangement/3, [
		comment is 'Returns the previous distinct arrangement value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Arrangement', 'Previous']
	]).

:- end_protocol.

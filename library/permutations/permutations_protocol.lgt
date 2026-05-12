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


:- protocol(permutations_protocol).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for permutations operations over lists.'
	]).

	:- public(permutations/2).
	:- mode(permutations(+list, -list), one).
	:- info(permutations/2, [
		comment is 'Generates all permutations of a list using default order.',
		argnames is ['List', 'Permutations']
	]).

	:- public(permutation/2).
	:- mode(permutation(+list, -list), one_or_more).
	:- info(permutation/2, [
		comment is 'True iff the second argument is a permutation of the first argument using default order.',
		argnames is ['List', 'Permutation']
	]).

	:- public(permutations/3).
	:- mode(permutations(+list, +atom, -list), one).
	:- info(permutations/3, [
		comment is 'Generates all permutations with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Permutations']
	]).

	:- public(permutation/3).
	:- mode(permutation(+list, +atom, -list), one_or_more).
	:- info(permutation/3, [
		comment is 'True iff the third argument is a permutation with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Permutation']
	]).

	:- public(distinct_permutations/2).
	:- mode(distinct_permutations(+list, -list), one).
	:- info(distinct_permutations/2, [
		comment is 'Generates all distinct permutations of a list (deduplicating repeated values in the input list) using default order.',
		argnames is ['List', 'Permutations']
	]).

	:- public(distinct_permutation/2).
	:- mode(distinct_permutation(+list, -list), one_or_more).
	:- info(distinct_permutation/2, [
		comment is 'True iff the second argument is a distinct permutation of the first argument using default order.',
		argnames is ['List', 'Permutation']
	]).

	:- public(distinct_permutations/3).
	:- mode(distinct_permutations(+list, +atom, -list), one).
	:- info(distinct_permutations/3, [
		comment is 'Generates all distinct permutations with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Permutations']
	]).

	:- public(distinct_permutation/3).
	:- mode(distinct_permutation(+list, +atom, -list), one_or_more).
	:- info(distinct_permutation/3, [
		comment is 'True iff the third argument is a distinct permutation with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Permutation']
	]).

	:- public(k_permutations/3).
	:- mode(k_permutations(+integer, +list, -list), one).
	:- info(k_permutations/3, [
		comment is 'Generates all K-permutations (ordered selections) of a list using default order.',
		argnames is ['K', 'List', 'Permutations']
	]).

	:- public(k_permutation/3).
	:- mode(k_permutation(+integer, +list, -list), one_or_more).
	:- info(k_permutation/3, [
		comment is 'True iff the third argument is a K-permutation (ordered selection) of a list using default order.',
		argnames is ['K', 'List', 'Permutation']
	]).

	:- public(k_permutations/4).
	:- mode(k_permutations(+integer, +list, +atom, -list), one).
	:- info(k_permutations/4, [
		comment is 'Generates all K-permutations with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Permutations']
	]).

	:- public(k_permutation/4).
	:- mode(k_permutation(+integer, +list, +atom, -list), one_or_more).
	:- info(k_permutation/4, [
		comment is 'True iff the fourth argument is a K-permutation with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Permutation']
	]).

	:- public(next_permutation/2).
	:- mode(next_permutation(+list, -list), zero_or_one).
	:- info(next_permutation/2, [
		comment is 'Returns the next permutation value in lexicographic order.',
		argnames is ['Permutation', 'Next']
	]).

	:- public(previous_permutation/2).
	:- mode(previous_permutation(+list, -list), zero_or_one).
	:- info(previous_permutation/2, [
		comment is 'Returns the previous permutation value in lexicographic order.',
		argnames is ['Permutation', 'Previous']
	]).

	:- public(nth_permutation/3).
	:- mode(nth_permutation(+list, +integer, -list), zero_or_one).
	:- info(nth_permutation/3, [
		comment is 'Returns the permutation at a given zero-based index using default order.',
		argnames is ['List', 'Index', 'Permutation']
	]).

	:- public(nth_permutation/4).
	:- mode(nth_permutation(+list, +atom, +integer, -list), zero_or_one).
	:- info(nth_permutation/4, [
		comment is 'Returns the permutation at a given zero-based index in the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Index', 'Permutation']
	]).

	:- public(permutation_index/3).
	:- mode(permutation_index(+list, +list, -integer), zero_or_one).
	:- info(permutation_index/3, [
		comment is 'Returns the zero-based index of a permutation using default order.',
		argnames is ['List', 'Permutation', 'Index']
	]).

	:- public(permutation_index/4).
	:- mode(permutation_index(+list, +atom, +list, -integer), zero_or_one).
	:- info(permutation_index/4, [
		comment is 'Returns the zero-based index of a permutation in the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Permutation', 'Index']
	]).

	:- public(count_permutations/2).
	:- mode(count_permutations(+list, -integer), one).
	:- info(count_permutations/2, [
		comment is 'Counts the number of permutations of a list.',
		argnames is ['List', 'Count']
	]).

	:- public(count_distinct_permutations/2).
	:- mode(count_distinct_permutations(+list, -integer), one).
	:- info(count_distinct_permutations/2, [
		comment is 'Counts the number of distinct permutations of a list (deduplicating equal-valued permutations).',
		argnames is ['List', 'Count']
	]).

	:- public(nth_distinct_permutation/3).
	:- mode(nth_distinct_permutation(+list, +integer, -list), zero_or_one).
	:- info(nth_distinct_permutation/3, [
		comment is 'Returns the distinct permutation at a given zero-based index in default generation order.',
		argnames is ['List', 'Index', 'Permutation']
	]).

	:- public(distinct_permutation_index/3).
	:- mode(distinct_permutation_index(+list, +list, -integer), zero_or_one).
	:- info(distinct_permutation_index/3, [
		comment is 'Returns the zero-based index of a distinct permutation in default generation order.',
		argnames is ['List', 'Permutation', 'Index']
	]).

	:- public(random_permutation/2).
	:- mode(random_permutation(+list, -list), one).
	:- info(random_permutation/2, [
		comment is 'Returns a random permutation of a list.',
		argnames is ['List', 'Permutation']
	]).

	:- public(sample_permutations/3).
	:- mode(sample_permutations(+list, +integer, -list), zero_or_one).
	:- info(sample_permutations/3, [
		comment is 'Returns SampleCount random permutations of a list, sampled with replacement.',
		argnames is ['List', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_permutation/2).
	:- mode(random_distinct_permutation(+list, -list), one).
	:- info(random_distinct_permutation/2, [
		comment is 'Returns a random distinct permutation of a list (deduplicating equal-valued permutations).',
		argnames is ['List', 'Permutation']
	]).

	:- public(sample_distinct_permutations/3).
	:- mode(sample_distinct_permutations(+list, +integer, -list), zero_or_one).
	:- info(sample_distinct_permutations/3, [
		comment is 'Returns SampleCount random distinct permutations of a list, sampled with replacement after deduplicating equal-valued permutations.',
		argnames is ['List', 'SampleCount', 'Samples']
	]).

:- end_protocol.

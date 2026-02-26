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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Protocol for permutations operations over lists.'
	]).

	:- public(permutations/2).
	:- mode(permutations(+list, -list), one).
	:- info(permutations/2, [
		comment is 'Generates all permutations of a list.',
		argnames is ['List', 'Permutations']
	]).

	:- public(permutation/2).
	:- mode(permutation(+list, -list), one_or_more).
	:- info(permutation/2, [
		comment is 'True iff the second argument is a permutation of the first argument.',
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
		comment is 'Generates all distinct permutations of a list (deduplicating repeated values in the input list).',
		argnames is ['List', 'Permutations']
	]).

	:- public(distinct_permutation/2).
	:- mode(distinct_permutation(+list, -list), one_or_more).
	:- info(distinct_permutation/2, [
		comment is 'True iff the second argument is a distinct permutation of the first argument.',
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
		comment is 'Generates all K-permutations (ordered selections) of a list.',
		argnames is ['K', 'List', 'Permutations']
	]).

	:- public(k_permutation/3).
	:- mode(k_permutation(+integer, +list, -list), one_or_more).
	:- info(k_permutation/3, [
		comment is 'True iff the third argument is a K-permutation (ordered selection) of a list.',
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

	:- public(cartesian_product/3).
	:- mode(cartesian_product(+integer, +list, -list), one).
	:- info(cartesian_product/3, [
		comment is 'Generates all K-element tuples from a list with replacement where order matters.',
		argnames is ['K', 'List', 'Tuples']
	]).

	:- public(derangements/2).
	:- mode(derangements(+list, -list), one).
	:- info(derangements/2, [
		comment is 'Generates all derangements of a list.',
		argnames is ['List', 'Derangements']
	]).

	:- public(derangement/2).
	:- mode(derangement(+list, -list), one_or_more).
	:- info(derangement/2, [
		comment is 'True iff the second argument is a derangement of the first argument.',
		argnames is ['List', 'Derangement']
	]).

	:- public(next_permutation/2).
	:- mode(next_permutation(+list, -list), zero_or_one).
	:- info(next_permutation/2, [
		comment is 'Returns the next permutation in lexicographic order.',
		argnames is ['Permutation', 'Next']
	]).

	:- public(previous_permutation/2).
	:- mode(previous_permutation(+list, -list), zero_or_one).
	:- info(previous_permutation/2, [
		comment is 'Returns the previous permutation in lexicographic order.',
		argnames is ['Permutation', 'Previous']
	]).

	:- public(nth_permutation/3).
	:- mode(nth_permutation(+list, +integer, -list), zero_or_one).
	:- info(nth_permutation/3, [
		comment is 'Returns the permutation at a given zero-based index.',
		argnames is ['List', 'Index', 'Permutation']
	]).

	:- public(permutation_index/3).
	:- mode(permutation_index(+list, +list, -integer), zero_or_one).
	:- info(permutation_index/3, [
		comment is 'Returns the zero-based index of a permutation.',
		argnames is ['List', 'Permutation', 'Index']
	]).

	:- public(count_permutations/2).
	:- mode(count_permutations(+list, -integer), one).
	:- info(count_permutations/2, [
		comment is 'Counts the number of permutations of a list.',
		argnames is ['List', 'Count']
	]).

	:- public(random_permutation/2).
	:- mode(random_permutation(+list, -list), one).
	:- info(random_permutation/2, [
		comment is 'Returns a random permutation of a list.',
		argnames is ['List', 'Permutation']
	]).

:- end_protocol.

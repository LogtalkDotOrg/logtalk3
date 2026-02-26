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


:- protocol(combinations_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Protocol for combinations operations over lists.'
	]).

	:- public(combinations/3).
	:- mode(combinations(+integer, +list, -list), one).
	:- info(combinations/3, [
		comment is 'Generates all K-element combinations of a list.',
		argnames is ['K', 'List', 'Combinations']
	]).

	:- public(combination/3).
	:- mode(combination(+integer, +list, -list), one_or_more).
	:- info(combination/3, [
		comment is 'True iff the third argument is a K-element combination of a list.',
		argnames is ['K', 'List', 'Combination']
	]).

	:- public(combinations/4).
	:- mode(combinations(+integer, +list, +atom, -list), one).
	:- info(combinations/4, [
		comment is 'Generates all K-element combinations with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combinations']
	]).

	:- public(combination/4).
	:- mode(combination(+integer, +list, +atom, -list), one_or_more).
	:- info(combination/4, [
		comment is 'True iff the fourth argument is a K-element combination with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combination']
	]).

	:- public(combinations_with_replacement/3).
	:- mode(combinations_with_replacement(+integer, +list, -list), one).
	:- info(combinations_with_replacement/3, [
		comment is 'Generates all K-element combinations with replacement.',
		argnames is ['K', 'List', 'Combinations']
	]).

	:- public(combinations_with_replacement/4).
	:- mode(combinations_with_replacement(+integer, +list, +atom, -list), one).
	:- info(combinations_with_replacement/4, [
		comment is 'Generates all K-element combinations with replacement with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combinations']
	]).

	:- public(combination_with_replacement/3).
	:- mode(combination_with_replacement(+integer, +list, -list), one_or_more).
	:- info(combination_with_replacement/3, [
		comment is 'True iff the third argument is a K-element combination with replacement.',
		argnames is ['K', 'List', 'Combination']
	]).

	:- public(combination_with_replacement/4).
	:- mode(combination_with_replacement(+integer, +list, +atom, -list), one_or_more).
	:- info(combination_with_replacement/4, [
		comment is 'True iff the fourth argument is a K-element combination with replacement with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combination']
	]).

	:- public(distinct_combinations/3).
	:- mode(distinct_combinations(+integer, +list, -list), one).
	:- info(distinct_combinations/3, [
		comment is 'Generates all distinct K-element combinations of a list (deduplicating equal-valued combinations).',
		argnames is ['K', 'List', 'Combinations']
	]).

	:- public(distinct_combination/3).
	:- mode(distinct_combination(+integer, +list, -list), one_or_more).
	:- info(distinct_combination/3, [
		comment is 'True iff the third argument is a distinct K-element combination of a list.',
		argnames is ['K', 'List', 'Combination']
	]).

	:- public(distinct_combinations/4).
	:- mode(distinct_combinations(+integer, +list, +atom, -list), one).
	:- info(distinct_combinations/4, [
		comment is 'Generates all distinct K-element combinations with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combinations']
	]).

	:- public(distinct_combination/4).
	:- mode(distinct_combination(+integer, +list, +atom, -list), one_or_more).
	:- info(distinct_combination/4, [
		comment is 'True iff the fourth argument is a distinct K-element combination with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combination']
	]).

	:- public(nth_combination/4).
	:- mode(nth_combination(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_combination/4, [
		comment is 'Returns the combination at a given zero-based index.',
		argnames is ['K', 'List', 'Index', 'Combination']
	]).

	:- public(combination_index/4).
	:- mode(combination_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(combination_index/4, [
		comment is 'Returns the zero-based index of a combination.',
		argnames is ['K', 'List', 'Combination', 'Index']
	]).

	:- public(count_combinations/3).
	:- mode(count_combinations(+integer, +list, -integer), one).
	:- info(count_combinations/3, [
		comment is 'Counts the number of K-element combinations of a list.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(count_combinations_with_replacement/3).
	:- mode(count_combinations_with_replacement(+integer, +list, -integer), one).
	:- info(count_combinations_with_replacement/3, [
		comment is 'Counts the number of K-element combinations with replacement of a list.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(random_combination/3).
	:- mode(random_combination(+integer, +list, -list), zero_or_one).
	:- info(random_combination/3, [
		comment is 'Returns a random K-element combination of a list.',
		argnames is ['K', 'List', 'Combination']
	]).

:- end_protocol.

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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for combinations operations over lists.'
	]).

	:- public(combinations/3).
	:- mode(combinations(+integer, +list, -list), one).
	:- info(combinations/3, [
		comment is 'Generates all K-element combinations of a list using default order.',
		argnames is ['K', 'List', 'Combinations']
	]).

	:- public(combination/3).
	:- mode(combination(+integer, +list, -list), one_or_more).
	:- info(combination/3, [
		comment is 'True iff the third argument is a K-element combination of a list using default order.',
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

	:- public(distinct_combinations/3).
	:- mode(distinct_combinations(+integer, +list, -list), one).
	:- info(distinct_combinations/3, [
		comment is 'Generates all distinct K-element combinations of a list (deduplicating equal-valued combinations) using default order.',
		argnames is ['K', 'List', 'Combinations']
	]).

	:- public(distinct_combination/3).
	:- mode(distinct_combination(+integer, +list, -list), one_or_more).
	:- info(distinct_combination/3, [
		comment is 'True iff the third argument is a distinct K-element combination of a list using default order.',
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
		comment is 'Returns the combination at a given zero-based index using default order.',
		argnames is ['K', 'List', 'Index', 'Combination']
	]).

	:- public(nth_combination/5).
	:- mode(nth_combination(+integer, +list, +atom, +integer, -list), zero_or_one).
	:- info(nth_combination/5, [
		comment is 'Returns the combination at a given zero-based index in the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Index', 'Combination']
	]).

	:- public(combination_index/4).
	:- mode(combination_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(combination_index/4, [
		comment is 'Returns the zero-based index of a combination using default order.',
		argnames is ['K', 'List', 'Combination', 'Index']
	]).

	:- public(combination_index/5).
	:- mode(combination_index(+integer, +list, +atom, +list, -integer), zero_or_one).
	:- info(combination_index/5, [
		comment is 'Returns the zero-based index of a combination in the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['K', 'List', 'Order', 'Combination', 'Index']
	]).

	:- public(count_combinations/3).
	:- mode(count_combinations(+integer, +list, -integer), one).
	:- info(count_combinations/3, [
		comment is 'Counts the number of K-element combinations of a list.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(count_distinct_combinations/3).
	:- mode(count_distinct_combinations(+integer, +list, -integer), one).
	:- info(count_distinct_combinations/3, [
		comment is 'Counts the number of distinct K-element combinations of a list (deduplicating equal-valued combinations).',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(nth_distinct_combination/4).
	:- mode(nth_distinct_combination(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_distinct_combination/4, [
		comment is 'Returns the distinct combination at a given zero-based index in default generation order.',
		argnames is ['K', 'List', 'Index', 'Combination']
	]).

	:- public(distinct_combination_index/4).
	:- mode(distinct_combination_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(distinct_combination_index/4, [
		comment is 'Returns the zero-based index of a distinct combination in default generation order.',
		argnames is ['K', 'List', 'Combination', 'Index']
	]).

	:- public(random_combination/3).
	:- mode(random_combination(+integer, +list, -list), zero_or_one).
	:- info(random_combination/3, [
		comment is 'Returns a random K-element combination of a list.',
		argnames is ['K', 'List', 'Combination']
	]).

	:- public(sample_combinations/4).
	:- mode(sample_combinations(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_combinations/4, [
		comment is 'Returns SampleCount random K-element combinations of a list, sampled with replacement.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_combination/3).
	:- mode(random_distinct_combination(+integer, +list, -list), zero_or_one).
	:- info(random_distinct_combination/3, [
		comment is 'Returns a random distinct K-element combination of a list (deduplicating equal-valued combinations).',
		argnames is ['K', 'List', 'Combination']
	]).

	:- public(sample_distinct_combinations/4).
	:- mode(sample_distinct_combinations(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_distinct_combinations/4, [
		comment is 'Returns SampleCount random distinct K-element combinations of a list, sampled with replacement after deduplicating equal-valued combinations.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(next_combination/3).
	:- mode(next_combination(+list, +list, -list), zero_or_one).
	:- info(next_combination/3, [
		comment is 'Returns the next distinct combination value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Combination', 'Next']
	]).

	:- public(previous_combination/3).
	:- mode(previous_combination(+list, +list, -list), zero_or_one).
	:- info(previous_combination/3, [
		comment is 'Returns the previous distinct combination value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Combination', 'Previous']
	]).

:- end_protocol.

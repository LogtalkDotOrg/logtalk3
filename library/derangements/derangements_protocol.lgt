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


:- protocol(derangements_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for derangement operations over lists.'
	]).

	:- public(derangements/2).
	:- mode(derangements(+list, -list), one).
	:- info(derangements/2, [
		comment is 'Generates all derangements of a list using default order.',
		argnames is ['List', 'Derangements']
	]).

	:- public(derangement/2).
	:- mode(derangement(+list, -list), zero_or_more).
	:- info(derangement/2, [
		comment is 'True iff the second argument is a derangement of the first argument using default order.',
		argnames is ['List', 'Derangement']
	]).

	:- public(derangements/3).
	:- mode(derangements(+list, +atom, -list), one).
	:- info(derangements/3, [
		comment is 'Generates all derangements with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Derangements']
	]).

	:- public(derangement/3).
	:- mode(derangement(+list, +atom, -list), zero_or_more).
	:- info(derangement/3, [
		comment is 'True iff the third argument is a derangement with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Derangement']
	]).

	:- public(distinct_derangements/2).
	:- mode(distinct_derangements(+list, -list), one).
	:- info(distinct_derangements/2, [
		comment is 'Generates all distinct derangements of a list using default order.',
		argnames is ['List', 'Derangements']
	]).

	:- public(distinct_derangement/2).
	:- mode(distinct_derangement(+list, -list), zero_or_more).
	:- info(distinct_derangement/2, [
		comment is 'True iff the second argument is a distinct derangement of the first argument using default order.',
		argnames is ['List', 'Derangement']
	]).

	:- public(distinct_derangements/3).
	:- mode(distinct_derangements(+list, +atom, -list), one).
	:- info(distinct_derangements/3, [
		comment is 'Generates all distinct derangements with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Derangements']
	]).

	:- public(distinct_derangement/3).
	:- mode(distinct_derangement(+list, +atom, -list), zero_or_more).
	:- info(distinct_derangement/3, [
		comment is 'True iff the third argument is a distinct derangement with the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Derangement']
	]).

	:- public(nth_derangement/3).
	:- mode(nth_derangement(+list, +integer, -list), zero_or_one).
	:- info(nth_derangement/3, [
		comment is 'Returns the derangement at a given zero-based index using default order.',
		argnames is ['List', 'Index', 'Derangement']
	]).

	:- public(nth_derangement/4).
	:- mode(nth_derangement(+list, +atom, +integer, -list), zero_or_one).
	:- info(nth_derangement/4, [
		comment is 'Returns the derangement at a given zero-based index in the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Index', 'Derangement']
	]).

	:- public(derangement_index/3).
	:- mode(derangement_index(+list, +list, -integer), zero_or_one).
	:- info(derangement_index/3, [
		comment is 'Returns the zero-based index of a derangement using default order.',
		argnames is ['List', 'Derangement', 'Index']
	]).

	:- public(derangement_index/4).
	:- mode(derangement_index(+list, +atom, +list, -integer), zero_or_one).
	:- info(derangement_index/4, [
		comment is 'Returns the zero-based index of a derangement in the given order: ``default``, ``lexicographic``, or ``shortlex``.',
		argnames is ['List', 'Order', 'Derangement', 'Index']
	]).

	:- public(count_derangements/2).
	:- mode(count_derangements(+list, -integer), one).
	:- info(count_derangements/2, [
		comment is 'Counts the number of derangements of a list.',
		argnames is ['List', 'Count']
	]).

	:- public(count_partial_derangements/3).
	:- mode(count_partial_derangements(+integer, +list, -integer), one).
	:- info(count_partial_derangements/3, [
		comment is 'Counts the number of permutations of a list with exactly the given number of fixed points.',
		argnames is ['FixedPoints', 'List', 'Count']
	]).

	:- public(count_distinct_derangements/2).
	:- mode(count_distinct_derangements(+list, -integer), one).
	:- info(count_distinct_derangements/2, [
		comment is 'Counts the number of distinct derangements of a list (deduplicating equal-valued derangements).',
		argnames is ['List', 'Count']
	]).

	:- public(nth_distinct_derangement/3).
	:- mode(nth_distinct_derangement(+list, +integer, -list), zero_or_one).
	:- info(nth_distinct_derangement/3, [
		comment is 'Returns the distinct derangement at a given zero-based index in default generation order.',
		argnames is ['List', 'Index', 'Derangement']
	]).

	:- public(distinct_derangement_index/3).
	:- mode(distinct_derangement_index(+list, +list, -integer), zero_or_one).
	:- info(distinct_derangement_index/3, [
		comment is 'Returns the zero-based index of a distinct derangement in default generation order.',
		argnames is ['List', 'Derangement', 'Index']
	]).

	:- public(random_derangement/2).
	:- mode(random_derangement(+list, -list), zero_or_one).
	:- info(random_derangement/2, [
		comment is 'Returns a random derangement of a list.',
		argnames is ['List', 'Derangement']
	]).

	:- public(sample_derangements/3).
	:- mode(sample_derangements(+list, +integer, -list), zero_or_one).
	:- info(sample_derangements/3, [
		comment is 'Returns SampleCount random derangements of a list, sampled with replacement.',
		argnames is ['List', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_derangement/2).
	:- mode(random_distinct_derangement(+list, -list), zero_or_one).
	:- info(random_distinct_derangement/2, [
		comment is 'Returns a random distinct derangement of a list (deduplicating equal-valued derangements).',
		argnames is ['List', 'Derangement']
	]).

	:- public(sample_distinct_derangements/3).
	:- mode(sample_distinct_derangements(+list, +integer, -list), zero_or_one).
	:- info(sample_distinct_derangements/3, [
		comment is 'Returns SampleCount random distinct derangements of a list, sampled with replacement after deduplicating equal-valued derangements.',
		argnames is ['List', 'SampleCount', 'Samples']
	]).

	:- public(next_derangement/3).
	:- mode(next_derangement(+list, +list, -list), zero_or_one).
	:- info(next_derangement/3, [
		comment is 'Returns the next distinct derangement value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Derangement', 'Next']
	]).

	:- public(previous_derangement/3).
	:- mode(previous_derangement(+list, +list, -list), zero_or_one).
	:- info(previous_derangement/3, [
		comment is 'Returns the previous distinct derangement value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Derangement', 'Previous']
	]).

:- end_protocol.
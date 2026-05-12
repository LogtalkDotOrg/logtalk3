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


:- protocol(multisets_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for multiset operations over lists.'
	]).

	:- public(multisets/3).
	:- mode(multisets(+integer, +list, -list), one).
	:- info(multisets/3, [
		comment is 'Generates all K-multisets (unordered K-element selections with replacement) of a list using default order.',
		argnames is ['K', 'List', 'Multisets']
	]).

	:- public(multiset/3).
	:- mode(multiset(+integer, +list, -list), one_or_more).
	:- info(multiset/3, [
		comment is 'True iff the third argument is a K-multiset with replacement of a list using default order.',
		argnames is ['K', 'List', 'Multiset']
	]).

	:- public(multisets/4).
	:- mode(multisets(+integer, +list, +atom, -list), one).
	:- info(multisets/4, [
		comment is 'Generates all K-multisets with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Multisets']
	]).

	:- public(multiset/4).
	:- mode(multiset(+integer, +list, +atom, -list), one_or_more).
	:- info(multiset/4, [
		comment is 'True iff the fourth argument is a K-multiset with replacement with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Multiset']
	]).

	:- public(distinct_multisets/3).
	:- mode(distinct_multisets(+integer, +list, -list), one).
	:- info(distinct_multisets/3, [
		comment is 'Generates all distinct K-multisets (deduplicating repeated values in the input list) using default order.',
		argnames is ['K', 'List', 'Multisets']
	]).

	:- public(distinct_multiset/3).
	:- mode(distinct_multiset(+integer, +list, -list), one_or_more).
	:- info(distinct_multiset/3, [
		comment is 'True iff the third argument is a distinct K-multiset with replacement of a list using default order.',
		argnames is ['K', 'List', 'Multiset']
	]).

	:- public(distinct_multisets/4).
	:- mode(distinct_multisets(+integer, +list, +atom, -list), one).
	:- info(distinct_multisets/4, [
		comment is 'Generates all distinct K-multisets with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Multisets']
	]).

	:- public(distinct_multiset/4).
	:- mode(distinct_multiset(+integer, +list, +atom, -list), one_or_more).
	:- info(distinct_multiset/4, [
		comment is 'True iff the fourth argument is a distinct K-multiset with replacement with the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Multiset']
	]).

	:- public(nth_multiset/4).
	:- mode(nth_multiset(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_multiset/4, [
		comment is 'Returns the K-multiset at a given zero-based index using default order.',
		argnames is ['K', 'List', 'Index', 'Multiset']
	]).

	:- public(nth_multiset/5).
	:- mode(nth_multiset(+integer, +list, +atom, +integer, -list), zero_or_one).
	:- info(nth_multiset/5, [
		comment is 'Returns the K-multiset at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Index', 'Multiset']
	]).

	:- public(multiset_index/4).
	:- mode(multiset_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(multiset_index/4, [
		comment is 'Returns the zero-based index of a K-multiset using default order.',
		argnames is ['K', 'List', 'Multiset', 'Index']
	]).

	:- public(multiset_index/5).
	:- mode(multiset_index(+integer, +list, +atom, +list, -integer), zero_or_one).
	:- info(multiset_index/5, [
		comment is 'Returns the zero-based index of a K-multiset in the given order: ``default`` or ``lexicographic``.',
		argnames is ['K', 'List', 'Order', 'Multiset', 'Index']
	]).

	:- public(count_multisets/3).
	:- mode(count_multisets(+integer, +list, -integer), one).
	:- info(count_multisets/3, [
		comment is 'Counts the number of K-multisets with replacement of a list.',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(count_distinct_multisets/3).
	:- mode(count_distinct_multisets(+integer, +list, -integer), one).
	:- info(count_distinct_multisets/3, [
		comment is 'Counts the number of distinct K-multisets with replacement of a list (deduplicating repeated values in the input list).',
		argnames is ['K', 'List', 'Count']
	]).

	:- public(nth_distinct_multiset/4).
	:- mode(nth_distinct_multiset(+integer, +list, +integer, -list), zero_or_one).
	:- info(nth_distinct_multiset/4, [
		comment is 'Returns the distinct K-multiset at a given zero-based index in default generation order.',
		argnames is ['K', 'List', 'Index', 'Multiset']
	]).

	:- public(distinct_multiset_index/4).
	:- mode(distinct_multiset_index(+integer, +list, +list, -integer), zero_or_one).
	:- info(distinct_multiset_index/4, [
		comment is 'Returns the zero-based index of a distinct K-multiset in default generation order.',
		argnames is ['K', 'List', 'Multiset', 'Index']
	]).

	:- public(random_multiset/3).
	:- mode(random_multiset(+integer, +list, -list), zero_or_one).
	:- info(random_multiset/3, [
		comment is 'Returns a random K-multiset with replacement of a list.',
		argnames is ['K', 'List', 'Multiset']
	]).

	:- public(sample_multisets/4).
	:- mode(sample_multisets(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_multisets/4, [
		comment is 'Returns SampleCount random K-multisets with replacement of a list, sampled with replacement.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_multiset/3).
	:- mode(random_distinct_multiset(+integer, +list, -list), zero_or_one).
	:- info(random_distinct_multiset/3, [
		comment is 'Returns a random distinct K-multiset with replacement of a list (deduplicating repeated values in the input list).',
		argnames is ['K', 'List', 'Multiset']
	]).

	:- public(sample_distinct_multisets/4).
	:- mode(sample_distinct_multisets(+integer, +list, +integer, -list), zero_or_one).
	:- info(sample_distinct_multisets/4, [
		comment is 'Returns SampleCount random distinct K-multisets with replacement of a list, sampled with replacement after deduplicating repeated values in the input list.',
		argnames is ['K', 'List', 'SampleCount', 'Samples']
	]).

	:- public(next_multiset/3).
	:- mode(next_multiset(+list, +list, -list), zero_or_one).
	:- info(next_multiset/3, [
		comment is 'Returns the next distinct multiset value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Multiset', 'Next']
	]).

	:- public(previous_multiset/3).
	:- mode(previous_multiset(+list, +list, -list), zero_or_one).
	:- info(previous_multiset/3, [
		comment is 'Returns the previous distinct multiset value in lexicographic order induced by the first argument.',
		argnames is ['List', 'Multiset', 'Previous']
	]).

:- end_protocol.

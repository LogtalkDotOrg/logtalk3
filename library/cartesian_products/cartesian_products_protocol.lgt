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


:- protocol(cartesian_products_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Protocol for Cartesian-product operations over lists.'
	]).

	:- public(cartesian_product/2).
	:- mode(cartesian_product(+list(list), -list), one).
	:- info(cartesian_product/2, [
		comment is 'Generates the Cartesian product of a list of lists using default order.',
		argnames is ['Lists', 'Product']
	]).

	:- public(cartesian_tuple/2).
	:- mode(cartesian_tuple(+list(list), -list), one_or_more).
	:- info(cartesian_tuple/2, [
		comment is 'True iff the second argument is a tuple obtained by selecting one element from each list in the first argument using default order.',
		argnames is ['Lists', 'Tuple']
	]).

	:- public(cartesian_product/3).
	:- mode(cartesian_product(+list(list), +atom, -list), one).
	:- info(cartesian_product/3, [
		comment is 'Generates the Cartesian product with the given order: ``default`` or ``lexicographic``.',
		argnames is ['Lists', 'Order', 'Product']
	]).

	:- public(cartesian_tuple/3).
	:- mode(cartesian_tuple(+list(list), +atom, -list), one_or_more).
	:- info(cartesian_tuple/3, [
		comment is 'True iff the third argument is a Cartesian-product tuple with the given order: ``default`` or ``lexicographic``.',
		argnames is ['Lists', 'Order', 'Tuple']
	]).

	:- public(distinct_cartesian_product/2).
	:- mode(distinct_cartesian_product(+list(list), -list), one).
	:- info(distinct_cartesian_product/2, [
		comment is 'Generates the Cartesian product after deduplicating repeated values in each factor list using default order.',
		argnames is ['Lists', 'Product']
	]).

	:- public(distinct_cartesian_tuple/2).
	:- mode(distinct_cartesian_tuple(+list(list), -list), one_or_more).
	:- info(distinct_cartesian_tuple/2, [
		comment is 'True iff the second argument is a tuple in the distinct Cartesian product of the factor lists using default order.',
		argnames is ['Lists', 'Tuple']
	]).

	:- public(distinct_cartesian_product/3).
	:- mode(distinct_cartesian_product(+list(list), +atom, -list), one).
	:- info(distinct_cartesian_product/3, [
		comment is 'Generates the distinct Cartesian product with the given order: ``default`` or ``lexicographic``.',
		argnames is ['Lists', 'Order', 'Product']
	]).

	:- public(distinct_cartesian_tuple/3).
	:- mode(distinct_cartesian_tuple(+list(list), +atom, -list), one_or_more).
	:- info(distinct_cartesian_tuple/3, [
		comment is 'True iff the third argument is a tuple in the distinct Cartesian product with the given order: ``default`` or ``lexicographic``.',
		argnames is ['Lists', 'Order', 'Tuple']
	]).

	:- public(count_cartesian_product/2).
	:- mode(count_cartesian_product(+list(list), -integer), one).
	:- info(count_cartesian_product/2, [
		comment is 'Counts the number of tuples in the Cartesian product of a list of lists.',
		argnames is ['Lists', 'Count']
	]).

	:- public(count_distinct_cartesian_product/2).
	:- mode(count_distinct_cartesian_product(+list(list), -integer), one).
	:- info(count_distinct_cartesian_product/2, [
		comment is 'Counts the number of tuples in the distinct Cartesian product after deduplicating repeated values in each factor list.',
		argnames is ['Lists', 'Count']
	]).

	:- public(nth_cartesian_tuple/3).
	:- mode(nth_cartesian_tuple(+list(list), +integer, -list), zero_or_one).
	:- info(nth_cartesian_tuple/3, [
		comment is 'Returns the Cartesian-product tuple at a given zero-based index using default order.',
		argnames is ['Lists', 'Index', 'Tuple']
	]).

	:- public(nth_cartesian_tuple/4).
	:- mode(nth_cartesian_tuple(+list(list), +atom, +integer, -list), zero_or_one).
	:- info(nth_cartesian_tuple/4, [
		comment is 'Returns the Cartesian-product tuple at a given zero-based index in the given order: ``default`` or ``lexicographic``.',
		argnames is ['Lists', 'Order', 'Index', 'Tuple']
	]).

	:- public(cartesian_tuple_index/3).
	:- mode(cartesian_tuple_index(+list(list), +list, -integer), zero_or_one).
	:- info(cartesian_tuple_index/3, [
		comment is 'Returns the zero-based index of a Cartesian-product tuple using default order.',
		argnames is ['Lists', 'Tuple', 'Index']
	]).

	:- public(cartesian_tuple_index/4).
	:- mode(cartesian_tuple_index(+list(list), +atom, +list, -integer), zero_or_one).
	:- info(cartesian_tuple_index/4, [
		comment is 'Returns the zero-based index of a Cartesian-product tuple in the given order: ``default`` or ``lexicographic``.',
		argnames is ['Lists', 'Order', 'Tuple', 'Index']
	]).

	:- public(nth_distinct_cartesian_tuple/3).
	:- mode(nth_distinct_cartesian_tuple(+list(list), +integer, -list), zero_or_one).
	:- info(nth_distinct_cartesian_tuple/3, [
		comment is 'Returns the distinct Cartesian-product tuple at a given zero-based index in default generation order.',
		argnames is ['Lists', 'Index', 'Tuple']
	]).

	:- public(distinct_cartesian_tuple_index/3).
	:- mode(distinct_cartesian_tuple_index(+list(list), +list, -integer), zero_or_one).
	:- info(distinct_cartesian_tuple_index/3, [
		comment is 'Returns the zero-based index of a distinct Cartesian-product tuple in default generation order.',
		argnames is ['Lists', 'Tuple', 'Index']
	]).

	:- public(random_cartesian_tuple/2).
	:- mode(random_cartesian_tuple(+list(list), -list), zero_or_one).
	:- info(random_cartesian_tuple/2, [
		comment is 'Returns a random Cartesian-product tuple.',
		argnames is ['Lists', 'Tuple']
	]).

	:- public(sample_cartesian_tuples/3).
	:- mode(sample_cartesian_tuples(+list(list), +integer, -list), zero_or_one).
	:- info(sample_cartesian_tuples/3, [
		comment is 'Returns SampleCount random Cartesian-product tuples, sampled with replacement.',
		argnames is ['Lists', 'SampleCount', 'Samples']
	]).

	:- public(random_distinct_cartesian_tuple/2).
	:- mode(random_distinct_cartesian_tuple(+list(list), -list), zero_or_one).
	:- info(random_distinct_cartesian_tuple/2, [
		comment is 'Returns a random tuple from the distinct Cartesian product after deduplicating repeated values in each factor list.',
		argnames is ['Lists', 'Tuple']
	]).

	:- public(sample_distinct_cartesian_tuples/3).
	:- mode(sample_distinct_cartesian_tuples(+list(list), +integer, -list), zero_or_one).
	:- info(sample_distinct_cartesian_tuples/3, [
		comment is 'Returns SampleCount random tuples from the distinct Cartesian product, sampled with replacement.',
		argnames is ['Lists', 'SampleCount', 'Samples']
	]).

	:- public(next_cartesian_tuple/3).
	:- mode(next_cartesian_tuple(+list(list), +list, -list), zero_or_one).
	:- info(next_cartesian_tuple/3, [
		comment is 'Returns the next distinct Cartesian-product tuple in lexicographic order induced by the factor lists.',
		argnames is ['Lists', 'Tuple', 'Next']
	]).

	:- public(previous_cartesian_tuple/3).
	:- mode(previous_cartesian_tuple(+list(list), +list, -list), zero_or_one).
	:- info(previous_cartesian_tuple/3, [
		comment is 'Returns the previous distinct Cartesian-product tuple in lexicographic order induced by the factor lists.',
		argnames is ['Lists', 'Tuple', 'Previous']
	]).

:- end_protocol.

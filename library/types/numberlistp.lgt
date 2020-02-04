%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(numberlistp).

	:- info([
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2019-03-02,
		comment is 'List of numbers protocol.',
		see_also is [numberlist, listp, varlistp]
	]).

	:- public(max/2).
	:- mode(max(+list(number), -number), zero_or_one).
	:- info(max/2, [
		comment is 'Determines the list maximum value using arithmetic order. Fails if the list is empty.',
		argnames is ['List', 'Maximum']
	]).

	:- public(min/2).
	:- mode(min(+list(number), -number), zero_or_one).
	:- info(min/2, [
		comment is 'Determines the minimum value in a list using arithmetic order. Fails if the list is empty.',
		argnames is ['List', 'Minimum']
	]).

	:- public(product/2).
	:- mode(product(+list(number), -number), zero_or_one).
	:- info(product/2, [
		comment is 'Calculates the product of all list numbers. Fails if the list is empty.',
		argnames is ['List', 'Product']
	]).

	:- public(sum/2).
	:- mode(sum(+list(number), -number), one).
	:- info(sum/2, [
		comment is 'Calculates the sum of all list numbers. Returns the integer zero if the list is empty.',
		argnames is ['List', 'Sum']
	]).

	:- public(average/2).
	:- mode(average(+list(number), -float), zero_or_one).
	:- info(average/2, [
		comment is 'Calculates the average (i.e. arithmetic mean) of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Average']
	]).

	:- public(euclidean_norm/2).
	:- mode(euclidean_norm(+list(number), -float), zero_or_one).
	:- info(euclidean_norm/2, [
		comment is 'Calculates the Euclidean norm of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Norm']
	]).

	:- public(chebyshev_norm/2).
	:- mode(chebyshev_norm(+list(integer), -integer), zero_or_one).
	:- mode(chebyshev_norm(+list(float), -float), zero_or_one).
	:- info(chebyshev_norm/2, [
		comment is 'Calculates the Chebyshev norm of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Norm']
	]).

	:- public(manhattan_norm/2).
	:- mode(manhattan_norm(+list(integer), -integer), zero_or_one).
	:- mode(manhattan_norm(+list(float), -float), zero_or_one).
	:- info(manhattan_norm/2, [
		comment is 'Calculates the Manhattan norm of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Norm']
	]).

	:- public(euclidean_distance/3).
	:- mode(euclidean_distance(+list(number), +list(number), -float), zero_or_one).
	:- info(euclidean_distance/3, [
		comment is 'Calculates the Euclidean distance between two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(chebyshev_distance/3).
	:- mode(chebyshev_distance(+list(integer), +list(integer), -integer), zero_or_one).
	:- mode(chebyshev_distance(+list(float), +list(float), -float), zero_or_one).
	:- info(chebyshev_distance/3, [
		comment is 'Calculates the Chebyshev distance between two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(manhattan_distance/3).
	:- mode(manhattan_distance(+list(integer), +list(integer), -integer), zero_or_one).
	:- mode(manhattan_distance(+list(float), +list(float), -float), zero_or_one).
	:- info(manhattan_distance/3, [
		comment is 'Calculates the Manhattan distance between two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Distance']
	]).

	:- public(scalar_product/3).
	:- mode(scalar_product(+list(integer), +list(integer), -integer), zero_or_one).
	:- mode(scalar_product(+list(float), +list(float), -float), zero_or_one).
	:- info(scalar_product/3, [
		comment is 'Calculates the scalar product of two lists of numbers. Fails if the two lists are empty or not of the same length.',
		argnames is ['List1', 'List2', 'Product']
	]).

	:- public(normalize_range/2).
	:- mode(normalize_range(+list(number), -list(float)), one).
	:- info(normalize_range/2, [
		comment is 'Normalizes a list of numbers into the ``[0.0,1.0]`` range. Caller must handle arithmetic exceptions if the input list if not normalizable.',
		argnames is ['List', 'NormalizedList']
	]).

	:- public(normalize_range/4).
	:- mode(normalize_range(+list(number), +number, +number, -list(float)), one).
	:- info(normalize_range/4, [
		comment is 'Normalizes a list of numbers into the given range. Caller must handle arithmetic exceptions if the input list if not normalizable.',
		argnames is ['List', 'Minimum', 'Maximum', 'NormalizedList']
	]).

	:- public(normalize_unit/2).
	:- mode(normalize_unit(+list(number), -list(float)), one).
	:- info(normalize_unit/2, [
		comment is 'Normalizes a list of numbers returning its unit vector (i.e. a list with Euclidean norm equal to one). Caller must handle arithmetic exceptions if the input list if not normalizable.',
		argnames is ['List', 'NormalizedList']
	]).

	:- public(normalize_scalar/2).
	:- mode(normalize_scalar(+list(number), -list(float)), one).
	:- info(normalize_scalar/2, [
		comment is 'Normalizes a list of numbers such that the sum of all numbers is equal to one. Caller must handle arithmetic exceptions if the input list if not normalizable.',
		argnames is ['List', 'NormalizedList']
	]).

	:- public(rescale/3).
	:- mode(rescale(+list(integer), +integer, -list(integer)), one).
	:- mode(rescale(+list(number), +float, -list(float)), one).
	:- info(rescale/3, [
		comment is 'Rescales all numbers in a list by the given factor.',
		argnames is ['List', 'Factor', 'RescaledList']
	]).

:- end_protocol.

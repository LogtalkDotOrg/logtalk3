%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(statisticsp).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2022-06-20,
		comment is 'Statistical calculations over a list of numbers protocol.',
		see_also is [statistics, sample, population]
	]).

	:- public(product/2).
	:- mode(product(+list(number), -number), zero_or_one).
	:- info(product/2, [
		comment is 'Calculates the product of all list numbers. Fails if the list is empty.',
		argnames is ['List', 'Product']
	]).

	:- public(sum/2).
	:- mode(sum(+list(number), -number), zero_or_one).
	:- info(sum/2, [
		comment is 'Calculates the sum of all list numbers. Fails if the list is empty.',
		argnames is ['List', 'Sum']
	]).

	:- public(min/2).
	:- mode(min(+list, -number), zero_or_one).
	:- info(min/2, [
		comment is 'Determines the minimum value in a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Minimum']
	]).

	:- public(max/2).
	:- mode(max(+list, -number), zero_or_one).
	:- info(max/2, [
		comment is 'Determines the list maximum value in a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Maximum']
	]).

	:- public(min_max/3).
	:- mode(min_max(+list(number), -number, -number), zero_or_one).
	:- info(min_max/3, [
		comment is 'Determines the minimum and maximum values in a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Minimum', 'Maximum']
	]).


	:- public(range/2).
	:- mode(range(+list, -number), zero_or_one).
	:- info(range/2, [
		comment is 'Range is the length of the smallest interval which contains all the numbers in List. Fails if the list is empty.',
		argnames is ['List', 'Range']
	]).

	:- public(arithmetic_mean/2).
	:- mode(arithmetic_mean(+list(number), -float), zero_or_one).
	:- info(arithmetic_mean/2, [
		comment is 'Calculates the arithmetic mean of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Mean']
	]).

	:- public(geometric_mean/2).
	:- mode(geometric_mean(+list(number), -float), zero_or_one).
	:- info(geometric_mean/2, [
		comment is 'Calculates the geometric mean of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Mean']
	]).

	:- public(harmonic_mean/2).
	:- mode(harmonic_mean(+list(number), -float), zero_or_one).
	:- info(harmonic_mean/2, [
		comment is 'Calculates the harmonic mean of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Mean']
	]).

	:- public(weighted_mean/3).
	:- mode(weighted_mean(+list(number), +list(number), -float), zero_or_one).
	:- info(weighted_mean/3, [
		comment is 'Calculates the weighted mean of a list of numbers. Fails if the list is empty or if the two lists have different lengths. Wights are assume to be non-negative.',
		argnames is ['Weights', 'List', 'Mean']
	]).

	:- public(median/2).
	:- mode(median(+list(number), -float), zero_or_one).
	:- info(median/2, [
		comment is 'Calculates the median of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Median']
	]).

	:- public(modes/2).
	:- mode(modes(+list(number), -list(number)), zero_or_one).
	:- info(modes/2, [
		comment is 'Returns the list of modes of a list of numbers in ascending order. Fails if the list is empty.',
		argnames is ['List', 'Modes']
	]).

	:- public(average_deviation/3).
	:- mode(average_deviation(+list(number), +float, -float), zero_or_one).
	:- info(average_deviation/3, [
		comment is 'Calculates the average absolute deviation of a list of numbers given a central tendency (e.g., mean, median, or mode). Fails if the list is empty.',
		argnames is ['List', 'CentralTendency', 'Deviation']
	]).

	:- public(mean_deviation/2).
	:- mode(mean_deviation(+list(number), -float), zero_or_one).
	:- info(mean_deviation/2, [
		comment is 'Calculates the mean absolute deviation of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Deviation']
	]).

	:- public(median_deviation/2).
	:- mode(median_deviation(+list(number), -float), zero_or_one).
	:- info(median_deviation/2, [
		comment is 'Calculates the median absolute deviation of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Deviation']
	]).

	:- public(standard_deviation/2).
	:- mode(standard_deviation(+list(number), -float), zero_or_one).
	:- info(standard_deviation/2, [
		comment is 'Calculates the standard deviation of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Deviation']
	]).

	:- public(coefficient_of_variation/2).
	:- mode(coefficient_of_variation(+list(number), -float), zero_or_one).
	:- info(coefficient_of_variation/2, [
		comment is 'Calculates the coefficient of variation of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Coefficient']
	]).

	:- public(relative_standard_deviation/2).
	:- mode(relative_standard_deviation(+list(number), -float), zero_or_one).
	:- info(relative_standard_deviation/2, [
		comment is 'Calculates the relative standard deviation of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Percentage']
	]).

	:- public(skewness/2).
	:- mode(skewness(+list(number), -float), zero_or_one).
	:- info(skewness/2, [
		comment is 'Calculates the (moment) skewness of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Skewness']
	]).

	:- public(kurtosis/2).
	:- mode(kurtosis(+list(number), -float), zero_or_one).
	:- info(kurtosis/2, [
		comment is 'Calculates the (excess) kurtosis of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Kurtosis']
	]).

	:- public(variance/2).
	:- mode(variance(+list(number), -float), zero_or_one).
	:- info(variance/2, [
		comment is 'Calculates the unbiased variance of a list of numbers. Fails if the list is empty.',
		argnames is ['List', 'Variance']
	]).

	:- public(z_normalization/2).
	:- mode(z_normalization(+list(number), -list(float)), zero_or_one).
	:- info(z_normalization/2, [
		comment is 'Normalizes a list of number such that for the resulting list the mean of is close to zero and the standard deviation is close to 1. Fails if the list is empty.',
		argnames is ['List', 'NormalizedList']
	]).

	:- public(fractile/3).
	:- mode(fractile(+float, +list(integer), -integer), zero_or_one).
	:- mode(fractile(+float, +list(float), -float), zero_or_one).
	:- info(fractile/3, [
		comment is 'Calculates the smallest value in a list of numbers such that the list elements in its fraction ``P`` are less or equal to that value (with ``P`` in the open interval ``(0.0, 1.0)``). Fails if the list is empty.',
		argnames is ['P', 'List', 'Fractile']
	]).

	:- public(valid/1).
	:- mode(valid(@nonvar), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is a closed list of numbers.',
		argnames is ['Term']
	]).

:- end_protocol.

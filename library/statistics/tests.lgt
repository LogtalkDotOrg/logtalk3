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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2022-06-20,
		comment is 'Unit tests for the "statistics" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(statistics).
	cover(sample).
	cover(population).

	% product/2 tests

	test(sample_product_2_01, false) :-
		sample::product([], _).

	test(sample_product_2_02, deterministic(Product == 6)) :-
		sample::product([1,2,3], Product).

	% sum/2 tests

	test(sample_sum_2_01, false) :-
		sample::sum([], _).

	test(sample_sum_2_02, deterministic(Sum == 6)) :-
		sample::sum([1,2,3], Sum).

	% max/2 tests

	test(sample_max_2_01, false) :-
		sample::max([], _).

	test(sample_max_2_02, deterministic(Max == 3)) :-
		sample::max([1,2,3], Max).

	% min/2 tests

	test(sample_min_2_01, false) :-
		sample::min([], _).

	test(sample_min_2_02, deterministic(Min == 1)) :-
		sample::min([1,2,3], Min).

	% min_max/3 tests

	test(sample_min_max_3_01, false) :-
		sample::min_max([], _, _).

	test(sample_min_max_3_02, deterministic(Min-Max == 1-3)) :-
		sample::min_max([1,2,3], Min, Max).

	% range/2 tests

	test(sample_range_2_01, false) :-
		sample::range([], _).

	test(sample_range_2_02, deterministic(Range == 35)) :-
		sample::range([35,36,46,68,70], Range).

	% standard_deviation/2 tests

	test(sample_standard_deviation_2_01, false) :-
		sample::standard_deviation([], _).

	test(sample_standard_deviation_2_02, deterministic(StandardDeviation =~= 17.0)) :-
		sample::standard_deviation([35,36,46,68,70], StandardDeviation).

	test(population_standard_deviation_2_02, deterministic(StandardDeviation =~= 15.205262246999)) :-
		population::standard_deviation([35,36,46,68,70], StandardDeviation).

	% arithmetic_mean/2 tests

	test(sample_arithmetic_mean_2_01, false) :-
		sample::arithmetic_mean([], _).

	test(sample_arithmetic_mean_2_02, deterministic(Mean =~= 51.0)) :-
		sample::arithmetic_mean([35,36,46,68,70], Mean).

	% geometric_mean/2 tests

	test(sample_geometric_mean_2_01, false) :-
		sample::geometric_mean([], _).

	test(sample_geometric_mean_2_02, deterministic(Mean =~= 48.769361068906)) :-
		sample::geometric_mean([35,36,46,68,70], Mean).

	% harmonic_mean/2 tests

	test(sample_harmonic_mean_2_01, false) :-
		sample::harmonic_mean([], _).

	test(sample_harmonic_mean_2_02, deterministic(Mean =~= 46.694089547712)) :-
		sample::harmonic_mean([35,36,46,68,70], Mean).

	% weighted_mean/3 tests

	test(sample_weighted_mean_3_01, false) :-
		sample::weighted_mean([], [], _).

	test(sample_weighted_mean_3_02, deterministic(Mean =~= 83.0)) :-
		sample::weighted_mean([0.4, 0.4, 0.2], [80, 80, 95], Mean).

	% median/2 tests

	test(sample_median_2_01, false) :-
		sample::median([], _).

	test(sample_median_2_02, deterministic(Median =~= 41.0)) :-
		sample::median([35,36,46,68], Median).

	test(sample_median_2_03, deterministic(Median =~= 46.0)) :-
		sample::median([35,36,46,68,70], Median).

	% modes/2 tests

	test(numberlist_modes_2_01, false) :-
		sample::modes([], _).

	test(numberlist_modes_2_02, deterministic(Modes == [1])) :-
		sample::modes([1,0,1,2,2,1], Modes).

	test(numberlist_modes_2_03, deterministic(Modes == [2,3])) :-
		sample::modes([1,2,1,2,2,3,3,4,3], Modes).

	test(numberlist_modes_2_04, deterministic(Modes == [1,2,3])) :-
		sample::modes([1,2,3], Modes).

	% average_deviation/3 tests

	test(sample_average_deviation_3_01, false) :-
		sample::average_deviation([], _, _).

	test(sample_average_deviation_3_02, deterministic(Deviation =~= 14.4)) :-
		sample::arithmetic_mean([35,36,46,68,70], Mean),
		sample::average_deviation([35,36,46,68,70], Mean, Deviation).

	% mean_deviation/2 tests

	test(sample_mean_deviation_2_01, false) :-
		sample::mean_deviation([], _).

	test(sample_mean_deviation_2_02, deterministic(Deviation =~= 14.4)) :-
		sample::mean_deviation([35,36,46,68,70], Deviation).

	% median_deviation/2 tests

	test(sample_median_deviation_2_01, false) :-
		sample::median_deviation([], _).

	test(sample_median_deviation_2_02, deterministic(Deviation =~= 13.4)) :-
		sample::median_deviation([35,36,46,68,70], Deviation).

	% coefficient_of_variation/2 tests

	test(sample_coefficient_of_variation_2_01, false) :-
		sample::coefficient_of_variation([], _).

	test(sample_coefficient_of_variation_2_02, deterministic(Variation =~= 0.3333333333333333)) :-
		sample::coefficient_of_variation([35,36,46,68,70], Variation).

	% relative_standard_deviation/2 tests

	test(sample_relative_standard_deviation_2_01, false) :-
		sample::relative_standard_deviation([], _).

	test(sample_relative_standard_deviation_2_02, deterministic(Deviation =~= 33.33333333333333)) :-
		sample::relative_standard_deviation([35,36,46,68,70], Deviation).

	% skewness/2 tests

	test(sample_skewness_2_01, false) :-
		sample::skewness([], _).

	test(sample_skewness_2_02, deterministic(Skewness =~= 0.16999796458375738)) :-
		sample::skewness([35,36,46,68,70], Skewness).

	test(population_skewness_2_02, deterministic(Skewness =~= 0.354162)) :-
		population::skewness([35,36,46,68,70], Skewness).

	% kurtosis/2 tests

	test(sample_kurtosis_2_01, false) :-
		sample::kurtosis([], _).

	test(sample_kurtosis_2_02, deterministic(Kurtosis =~= -2.208275762981765)) :-
		sample::kurtosis([35,36,46,68,70], Kurtosis).

	test(population_kurtosis_2_03, deterministic(Kurtosis =~= -3.05172)) :-
		population::kurtosis([35,36,46,68,70], Kurtosis).

	% variance/2 tests

	test(sample_variance_2_01, false) :-
		sample::variance([], _).

	test(sample_variance_2_02, deterministic(Variance =~= 289.0)) :-
		sample::variance([35,36,46,68,70], Variance).

	test(population_variance_2_03, deterministic(Variance =~= 231.2)) :-
		population::variance([35,36,46,68,70], Variance).

	% z_normalization/2 tests

	test(sample_z_normalization_2_01, false) :-
		sample::z_normalization([], _).

	test(sample_z_normalization_2_02, deterministic(ZScores =~= [-0.9411764705882353,-0.8823529411764706,-0.29411764705882354,1.0,1.1176470588235294])) :-
		sample::z_normalization([35,36,46,68,70], ZScores).

	% fractile/3 tests

	test(sample_fractile_3_01, false) :-
		sample::fractile(0.5, [], _).

	test(sample_fractile_3_02, false) :-
		sample::fractile(0.0, [1,2,3], _).

	test(sample_fractile_3_03, false) :-
		sample::fractile(1.0, [1,2,3], _).

	test(sample_fractile_3_04, deterministic(Fractile == 7)) :-
		sample::fractile(0.25, [3, 6, 7, 8, 8, 10, 13, 15, 16, 20], Fractile).

	test(sample_fractile_3_05, deterministic(Fractile == 8)) :-
		sample::fractile(0.5, [3, 6, 7, 8, 8, 10, 13, 15, 16, 20], Fractile).

	test(sample_fractile_3_06, deterministic(Fractile == 15)) :-
		sample::fractile(0.75, [3, 6, 7, 8, 8, 10, 13, 15, 16, 20], Fractile).

	% valid/1 tests

	test(sample_valid_1_01, false) :-
		sample::valid(_).

	test(sample_valid_1_02, false) :-
		sample::valid([1,2,3| _]).

	test(sample_valid_1_03, deterministic) :-
		sample::valid([1,2,3]).

:- end_object.

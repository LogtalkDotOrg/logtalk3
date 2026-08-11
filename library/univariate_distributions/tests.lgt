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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-11,
		comment is 'Unit tests for the "univariate_distributions" library.'
	]).

	cover(univariate_distributions(_)).

	test(standard_normal_samples_reproducible, deterministic(Samples1 == Samples2)) :-
		fast_random(xoshiro128pp)::reset_seed,
		univariate_distributions(fast_random(xoshiro128pp))::standard_normal_samples(3, Samples1),
		fast_random(xoshiro128pp)::reset_seed,
		univariate_distributions(fast_random(xoshiro128pp))::standard_normal_samples(3, Samples2).

	test(normal_samples_reproducible, deterministic(Samples1 == Samples2)) :-
		fast_random(xoshiro128pp)::reset_seed,
		univariate_distributions(fast_random(xoshiro128pp))::normal_samples(3, 2.0, 4.0, Samples1),
		fast_random(xoshiro128pp)::reset_seed,
		univariate_distributions(fast_random(xoshiro128pp))::normal_samples(3, 2.0, 4.0, Samples2).

	test(normal_samples_zero_count, deterministic(Samples == [])) :-
		univariate_distributions(fast_random(xoshiro128pp))::normal_samples(0, 2.0, 4.0, Samples).

	test(normal_zero_deviation, deterministic(Value =:= 2.0)) :-
		univariate_distributions(fast_random(xoshiro128pp))::normal(2.0, 0.0, Value).

	test(standard_normal_density_zero, deterministic(abs(Density - 0.3989422804014327) =< 1.0e-15)) :-
		univariate_distributions(fast_random)::standard_normal_density(0.0, Density).

	test(standard_normal_log_density_zero, deterministic(abs(LogDensity + 0.9189385332046727) =< 1.0e-15)) :-
		univariate_distributions(fast_random)::standard_normal_log_density(0.0, LogDensity).

	test(standard_normal_distribution_zero, deterministic(abs(Probability - 0.5) =< 1.0e-9)) :-
		univariate_distributions(fast_random)::standard_normal_distribution(0.0, Probability).

	test(standard_normal_distribution_symmetry, deterministic(abs(Sum - 1.0) =< 1.0e-12)) :-
		univariate_distributions(fast_random)::standard_normal_distribution(1.5, PositiveProbability),
		univariate_distributions(fast_random)::standard_normal_distribution(-1.5, NegativeProbability),
		Sum is PositiveProbability + NegativeProbability.

	test(standard_normal_quantile_median, deterministic(abs(Quantile) =< 1.0e-15)) :-
		univariate_distributions(fast_random)::standard_normal_quantile(0.5, Quantile).

	test(standard_normal_quantile_upper_975, deterministic(abs(Quantile - 1.959963986120195) =< 1.0e-7)) :-
		univariate_distributions(fast_random)::standard_normal_quantile(0.975, Quantile).

	test(standard_normal_quantile_lower_tail, deterministic(abs(Quantile + 4.753424308822899) =< 1.0e-7)) :-
		univariate_distributions(fast_random)::standard_normal_quantile(0.000001, Quantile).

	test(normal_density, deterministic(abs(Density - 0.19947114020071635) =< 1.0e-15)) :-
		univariate_distributions(fast_random)::normal_density(3.0, 3.0, 2.0, Density).

	test(normal_log_density, deterministic(abs(LogDensity + 1.612085713764618) =< 1.0e-15)) :-
		univariate_distributions(fast_random)::normal_log_density(3.0, 3.0, 2.0, LogDensity).

	test(normal_distribution, deterministic(abs(Probability - 0.5) =< 1.0e-9)) :-
		univariate_distributions(fast_random)::normal_distribution(3.0, 3.0, 2.0, Probability).

	test(normal_quantile, deterministic(abs(Quantile - 3.0) =< 1.0e-15)) :-
		univariate_distributions(fast_random)::normal_quantile(0.5, 3.0, 2.0, Quantile).

	test(evaluation_preserves_random_seed, deterministic(Seed0 == Seed)) :-
		fast_random::get_seed(Seed0),
		univariate_distributions(fast_random)::standard_normal_density(1.0, _),
		univariate_distributions(fast_random)::standard_normal_distribution(1.0, _),
		univariate_distributions(fast_random)::standard_normal_quantile(0.75, _),
		fast_random::get_seed(Seed).

	test(standard_normal_quantile_zero_error, error(domain_error(open_probability, 0.0))) :-
		univariate_distributions(fast_random)::standard_normal_quantile(0.0, _).

	test(standard_normal_quantile_one_error, error(domain_error(open_probability, 1.0))) :-
		univariate_distributions(fast_random)::standard_normal_quantile(1.0, _).

	test(standard_normal_quantile_type_error, error(type_error(number, probability))) :-
		univariate_distributions(fast_random)::standard_normal_quantile(probability, _).

	test(normal_density_zero_deviation_error, error(domain_error(positive_number, 0.0))) :-
		univariate_distributions(fast_random)::normal_density(0.0, 0.0, 0.0, _).

	test(normal_samples_negative_count_error, error(domain_error(non_negative_integer, -1))) :-
		univariate_distributions(fast_random)::normal_samples(-1, 0.0, 1.0, _).

:- end_object.

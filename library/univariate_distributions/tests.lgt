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

	test(standard_t_predicates, deterministic) :-
		univariate_distributions(fast_random)::standard_t(1.5, Sample),
		univariate_distributions(fast_random)::standard_t_samples(2, 1.5, Samples),
		univariate_distributions(fast_random)::standard_t_density(0.0, 1.0, Density),
		univariate_distributions(fast_random)::standard_t_log_density(0.0, 1.0, LogDensity),
		univariate_distributions(fast_random)::standard_t_distribution(0.0, 1.0, Distribution),
		univariate_distributions(fast_random)::standard_t_quantile(0.5, 1.0, Quantile),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 0.3183098861837907) =< 1.0e-12),
		^^assertion(abs(LogDensity + 1.1447298858494002) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.5) =< 1.0e-12),
		^^assertion(abs(Quantile) =< 1.0e-12).

	test(location_scale_t_predicates, deterministic) :-
		univariate_distributions(fast_random)::t(3.0, 2.0, 1.5, Sample),
		univariate_distributions(fast_random)::t_samples(2, 3.0, 2.0, 1.5, Samples),
		univariate_distributions(fast_random)::t_density(3.0, 3.0, 2.0, 1.0, Density),
		univariate_distributions(fast_random)::t_log_density(3.0, 3.0, 2.0, 1.0, LogDensity),
		univariate_distributions(fast_random)::t_distribution(3.0, 3.0, 2.0, 1.0, Distribution),
		univariate_distributions(fast_random)::t_quantile(0.5, 3.0, 2.0, 1.0, Quantile),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 0.15915494309189535) =< 1.0e-12),
		^^assertion(abs(LogDensity + 1.8378770664093453) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.5) =< 1.0e-12),
		^^assertion(abs(Quantile - 3.0) =< 1.0e-12).

	test(chi_squared_predicates, deterministic) :-
		univariate_distributions(fast_random)::chi_squared(2.5, Sample),
		univariate_distributions(fast_random)::chi_squared_samples(2, 2.5, Samples),
		univariate_distributions(fast_random)::chi_squared_density(2.0, 2.0, Density),
		univariate_distributions(fast_random)::chi_squared_log_density(2.0, 2.0, LogDensity),
		univariate_distributions(fast_random)::chi_squared_distribution(2.0, 2.0, Distribution),
		univariate_distributions(fast_random)::chi_squared_quantile(0.5, 2.0, Quantile),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 0.18393972058572117) =< 1.0e-12),
		^^assertion(abs(LogDensity + 1.6931471805599453) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.6321205588285577) =< 1.0e-12),
		^^assertion(abs(Quantile - 1.3862943611198906) =< 1.0e-10).

	test(gamma_predicates, deterministic) :-
		univariate_distributions(fast_random)::gamma(2.0, 3.0, Sample),
		univariate_distributions(fast_random)::gamma_samples(2, 2.0, 3.0, Samples),
		univariate_distributions(fast_random)::gamma_density(3.0, 2.0, 3.0, Density),
		univariate_distributions(fast_random)::gamma_log_density(3.0, 2.0, 3.0, LogDensity),
		univariate_distributions(fast_random)::gamma_distribution(3.0, 2.0, 3.0, Distribution),
		univariate_distributions(fast_random)::gamma_quantile(0.5, 2.0, 3.0, Quantile),
		univariate_distributions(fast_random)::gamma_distribution(Quantile, 2.0, 3.0, RoundTrip),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 0.12262648039048077) =< 1.0e-12),
		^^assertion(abs(exp(LogDensity) - Density) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.26424111765711533) =< 1.0e-12),
		^^assertion(abs(RoundTrip - 0.5) =< 1.0e-10).

	test(beta_predicates, deterministic) :-
		univariate_distributions(fast_random)::beta(2.0, 2.0, Sample),
		univariate_distributions(fast_random)::beta_samples(2, 2.0, 2.0, Samples),
		univariate_distributions(fast_random)::beta_density(0.5, 2.0, 2.0, Density),
		univariate_distributions(fast_random)::beta_log_density(0.5, 2.0, 2.0, LogDensity),
		univariate_distributions(fast_random)::beta_distribution(0.5, 2.0, 2.0, Distribution),
		univariate_distributions(fast_random)::beta_quantile(0.5, 2.0, 2.0, Quantile),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 1.5) =< 1.0e-12),
		^^assertion(abs(LogDensity - 0.4054651081081644) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.5) =< 1.0e-12),
		^^assertion(abs(Quantile - 0.5) =< 1.0e-12).

	test(exponential_predicates, deterministic) :-
		univariate_distributions(fast_random)::exponential(2.0, Sample),
		univariate_distributions(fast_random)::exponential_samples(2, 2.0, Samples),
		univariate_distributions(fast_random)::exponential_density(2.0, 2.0, Density),
		univariate_distributions(fast_random)::exponential_log_density(2.0, 2.0, LogDensity),
		univariate_distributions(fast_random)::exponential_distribution(2.0, 2.0, Distribution),
		univariate_distributions(fast_random)::exponential_quantile(0.5, 2.0, Quantile),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 0.18393972058572117) =< 1.0e-12),
		^^assertion(abs(LogDensity + 1.6931471805599453) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.6321205588285577) =< 1.0e-12),
		^^assertion(abs(Quantile - 1.3862943611198906) =< 1.0e-12).

	test(fisher_predicates, deterministic) :-
		univariate_distributions(fast_random)::fisher(2.5, 3.5, Sample),
		univariate_distributions(fast_random)::fisher_samples(2, 2.5, 3.5, Samples),
		univariate_distributions(fast_random)::fisher_density(1.0, 2.0, 2.0, Density),
		univariate_distributions(fast_random)::fisher_log_density(1.0, 2.0, 2.0, LogDensity),
		univariate_distributions(fast_random)::fisher_distribution(1.0, 2.0, 2.0, Distribution),
		univariate_distributions(fast_random)::fisher_quantile(0.5, 2.0, 2.0, Quantile),
		^^assertion(number(Sample)),
		^^assertion((ground(Samples), Samples = [_, _])),
		^^assertion(abs(Density - 0.25) =< 1.0e-12),
		^^assertion(abs(LogDensity + 1.3862943611198906) =< 1.0e-12),
		^^assertion(abs(Distribution - 0.5) =< 1.0e-12),
		^^assertion(abs(Quantile - 1.0) =< 1.0e-10).

:- end_object.
